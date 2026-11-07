# SQLite archive for source-independent review.  Payloads are JSON serialized
# R objects (jsonlite::serializeJSON), not RDS; this preserves NaN/Inf/-Inf and
# NULL without executing arbitrary R code on load.

nm_archive_require <- function() {
  miss <- c("DBI", "RSQLite", "jsonlite")[!vapply(c("DBI", "RSQLite", "jsonlite"), requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) stop("archive requires packages: ", paste(miss, collapse = ", "))
}

nm_archive_normalize_source <- function(x) {
  if (is.null(x)) {
    return(list(root = NULL, relative = NA_character_, files = data.frame(path = character(), role = character(), stringsAsFactors = FALSE)))
  }
  y <- x
  y["root"] <- list(NULL)
  if (!is.null(y$path) && is.null(y$relative)) {
    y$relative <- basename(as.character(y$path))
    y$path <- NULL
  }
  y
}

nm_archive_frame_paths <- function(d, root = NULL) {
  if (!is.data.frame(d) || !".source_file" %in% names(d)) {
    return(d)
  }
  vals <- as.character(d$.source_file)
  absolute <- grepl("^(?:[A-Za-z]:[/\\\\]|[/\\\\])", vals)
  if (any(absolute)) d$.source_file[absolute] <- if (!is.null(root)) vapply(vals[absolute], nm_relpath, character(1), root = root) else basename(vals[absolute])
  d
}

nm_archive_project <- function(project, include_observations = TRUE) {
  if (is.null(project) || !is.list(project)) stop("project must be a project list")
  p <- project
  p$schema_version <- 2L
  p$models <- lapply(p$models %||% list(), function(m) {
    source_root <- if (is.list(m$source)) m$source$root %||% NULL else NULL
    m$source <- nm_archive_normalize_source(m$source)
    m$live_root <- NULL
    if (!isTRUE(include_observations)) m$stored_tables <- NULL
    m$table_catalog <- names(m$stored_tables) %||% character()
    m$data <- nm_archive_frame_paths(m$data, source_root)
    if (!isTRUE(include_observations)) m$data <- data.frame(.source_line = integer(), .source_file = character(), stringsAsFactors = FALSE)
    m
  })
  p$bootstraps <- lapply(p$bootstraps %||% list(), function(b) {
    b$source <- nm_archive_normalize_source(b$source)
    b
  })
  p$vpcs <- lapply(p$vpcs %||% list(), function(v) {
    source_root <- if (is.list(v$source)) v$source$root %||% NULL else NULL
    v$source <- nm_archive_normalize_source(v$source)
    v$bins <- nm_archive_frame_paths(v$bins, source_root)
    v["observations"] <- list(nm_archive_frame_paths(v$observations, source_root))
    if (!isTRUE(include_observations)) v["observations"] <- list(NULL)
    v
  })
  if (!isTRUE(include_observations) && !is.null(p$view$inspector)) {
    p$view$inspector$ids <- NULL
    p$view$inspector$filters <- NULL
  }
  p
}

nm_archive_payloads <- function(project) {
  list(schema_version = as.integer(project$schema_version %||% 1L), models = project$models %||% list(), bootstraps = project$bootstraps %||% list(), vpcs = project$vpcs %||% list(), manifest = project$manifest %||% data.frame(stringsAsFactors = FALSE), notices = as.character(project$notices %||% character()), view = project$view %||% list())
}

nm_archive_row_counts <- function(project) {
  models <- project$models %||% list()
  boots <- project$bootstraps %||% list()
  vpcs <- project$vpcs %||% list()
  rows <- data.frame(kind = character(), id = character(), table_name = character(), nrow = integer(), stringsAsFactors = FALSE)
  add <- function(kind, id, table_name, n) rows <<- rbind(rows, data.frame(kind = kind, id = as.character(id %||% ""), table_name = table_name, nrow = as.integer(n %||% 0L), stringsAsFactors = FALSE))
  for (m in models) {
    id <- m$id %||% ""
    add("model", id, "data", if (is.data.frame(m$data)) nrow(m$data) else 0L)
    add("model", id, "parameters", if (is.data.frame(m$parameters)) nrow(m$parameters) else 0L)
    for (file in names(m$stored_tables)) {
      tables <- m$stored_tables[[file]]
      for (i in seq_along(tables)) add("model", id, paste0("table ", i, ": ", file), nrow(tables[[i]]))
    }
  }
  for (b in boots) {
    id <- b$id %||% ""
    add("bootstrap", id, "raw", if (is.data.frame(b$raw)) nrow(b$raw) else 0L)
    add("bootstrap", id, "parameters", if (is.data.frame(b$parameters)) nrow(b$parameters) else 0L)
    add("bootstrap", id, "summary", if (is.data.frame(b$summary)) nrow(b$summary) else 0L)
  }
  for (v in vpcs) {
    id <- v$id %||% ""
    add("vpc", id, "bins", if (is.data.frame(v$bins)) nrow(v$bins) else 0L)
    add("vpc", id, "observations", if (is.data.frame(v$observations)) nrow(v$observations) else 0L)
  }
  add("project", "", "manifest", if (is.data.frame(project$manifest)) nrow(project$manifest) else 0L)
  rows
}

nm_archive_json <- function(x) jsonlite::serializeJSON(x, digits = 17L, pretty = FALSE)

nm_archive_digest <- function(x) {
  if (requireNamespace("digest", quietly = TRUE)) as.character(digest::digest(x, algo = "md5", serialize = FALSE)) else sprintf("fallback-%08x", as.integer(sum(as.integer(charToRaw(x))) %% 2^31))
}

nm_archive_validate_counts <- function(project, counts) {
  actual <- nm_archive_row_counts(project)
  if (!nrow(counts)) {
    return(invisible(TRUE))
  }
  key <- paste(actual$kind, actual$id, actual$table_name, sep = "\u001f")
  key2 <- paste(counts$kind, counts$id, counts$table_name, sep = "\u001f")
  idx <- match(key2, key)
  if (anyNA(idx) || any(actual$nrow[idx] != counts$nrow)) stop("archive row-count validation failed")
  invisible(TRUE)
}

nm_archive_connect <- function(path, readonly = FALSE) {
  nm_archive_require()
  DBI::dbConnect(RSQLite::SQLite(), dbname = path, flags = if (isTRUE(readonly)) RSQLite::SQLITE_RO else RSQLite::SQLITE_RWC)
}

nm_archive_write <- function(con, project) {
  DBI::dbExecute(con, "CREATE TABLE archive_meta (key TEXT PRIMARY KEY, value TEXT NOT NULL)")
  DBI::dbExecute(con, "CREATE TABLE archive_payload (kind TEXT PRIMARY KEY, payload TEXT NOT NULL, checksum TEXT NOT NULL)")
  DBI::dbExecute(con, "CREATE TABLE archive_counts (kind TEXT NOT NULL, id TEXT NOT NULL, table_name TEXT NOT NULL, nrow INTEGER NOT NULL)")
  DBI::dbExecute(con, "INSERT INTO archive_meta(key,value) VALUES ('schema_version','2'), ('format','nmreview-json-v2')")
  for (nm in names(nm_archive_payloads(project))) {
    payload <- nm_archive_json(nm_archive_payloads(project)[[nm]])
    DBI::dbExecute(con, "INSERT INTO archive_payload(kind,payload,checksum) VALUES (?,?,?)", params = list(nm, payload, nm_archive_digest(payload)))
  }
  counts <- nm_archive_row_counts(project)
  if (nrow(counts)) DBI::dbWriteTable(con, "archive_counts", counts, append = TRUE, row.names = FALSE)
}

nm_archive_read <- function(path) {
  con <- nm_archive_connect(path, readonly = TRUE)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  if (!DBI::dbExistsTable(con, "archive_meta") || !DBI::dbExistsTable(con, "archive_payload") || !DBI::dbExistsTable(con, "archive_counts")) stop("not an nmreview archive")
  meta <- DBI::dbGetQuery(con, "SELECT key,value FROM archive_meta")
  ver <- meta$value[match("schema_version", meta$key)]
  if (is.na(ver) || !ver %in% c("1", "2")) stop("unsupported archive schema version")
  pp <- DBI::dbGetQuery(con, "SELECT kind,payload,checksum FROM archive_payload")
  if (any(vapply(seq_len(nrow(pp)), function(i) !identical(as.character(pp$checksum[[i]]), nm_archive_digest(pp$payload[[i]])), logical(1)))) stop("archive payload checksum validation failed")
  obj <- stats::setNames(lapply(pp$payload, jsonlite::unserializeJSON), pp$kind)
  project <- list(schema_version = as.integer(ver), models = obj$models %||% list(), bootstraps = obj$bootstraps %||% list(), vpcs = obj$vpcs %||% list(), manifest = obj$manifest %||% data.frame(stringsAsFactors = FALSE), notices = obj$notices %||% character(), view = obj$view %||% list())
  counts <- DBI::dbGetQuery(con, "SELECT kind,id,table_name,nrow FROM archive_counts")
  nm_archive_validate_counts(project, counts)
  structure(project, class = c("nmreview_project", "list"))
}

nm_archive_commit <- function(tmp, target) {
  if (file.exists(target)) stop("Archive already exists. Choose a new filename.")
  if (!file.rename(tmp, target)) stop("cannot commit archive")
  invisible(TRUE)
}

save_archive <- function(project, path, include_observations = TRUE) {
  nm_archive_require()
  if (missing(path) || !length(path)) stop("archive path is required")
  target <- normalizePath(path[[1L]], winslash = "/", mustWork = FALSE)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  lock <- paste0(target, ".lock")
  if (!dir.create(lock, showWarnings = FALSE)) stop("Another archive save owns this filename. Choose a different filename.")
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  if (file.exists(target)) stop("Archive already exists. Choose a new filename.")
  p <- nm_archive_project(project, include_observations = include_observations)
  tmp <- tempfile(pattern = paste0(".", basename(target), ".tmp-"), tmpdir = dirname(target), fileext = ".sqlite")
  committed <- FALSE
  on.exit(if (file.exists(tmp)) unlink(tmp, force = TRUE), add = TRUE)
  con <- NULL
  tryCatch(
    {
      con <- nm_archive_connect(tmp, readonly = FALSE)
      DBI::dbWithTransaction(con, nm_archive_write(con, p))
      DBI::dbDisconnect(con)
      con <- NULL
      nm_archive_read(tmp)
      nm_archive_commit(tmp, target)
      committed <- TRUE
    },
    error = function(e) {
      if (!is.null(con)) try(DBI::dbDisconnect(con), silent = TRUE)
      stop(e)
    }
  )
  if (!committed || !file.exists(target)) stop("archive commit did not complete")
  normalizePath(target, winslash = "/", mustWork = TRUE)
}

archive_summary <- function(project) {
  if (is.character(project) && length(project) == 1L && file.exists(project)) project <- load_archive(project)
  if (is.null(project) || !is.list(project)) stop("project must be a project list or archive path")
  rows <- list()
  k <- 0L
  add <- function(kind, id, label, rows_n, source, notices) {
    k <<- k + 1L
    rows[[k]] <<- data.frame(kind = kind, id = as.character(id %||% ""), label = as.character(label %||% ""), rows = as.integer(rows_n %||% 0L), source = as.character(source %||% ""), notices = paste(as.character(notices %||% character()), collapse = "; "), stringsAsFactors = FALSE)
  }
  for (m in project$models %||% list()) add("model", m$id, m$label, if (is.data.frame(m$data)) nrow(m$data) else 0L, m$source$relative %||% "", m$notices)
  for (b in project$bootstraps %||% list()) add("bootstrap", b$id, b$model, if (is.data.frame(b$parameters)) nrow(b$parameters) else if (is.data.frame(b$raw)) nrow(b$raw) else 0L, b$source$relative %||% "", b$notices)
  for (v in project$vpcs %||% list()) add("vpc", v$id, v$model, if (is.data.frame(v$bins)) nrow(v$bins) else 0L, v$source$relative %||% "", v$notices)
  if (!length(rows)) data.frame(kind = character(), id = character(), label = character(), rows = integer(), source = character(), notices = character(), stringsAsFactors = FALSE) else do.call(rbind, rows)
}

load_archive <- function(path) {
  if (missing(path) || !length(path) || !file.exists(path)) stop("archive path does not exist")
  nm_archive_read(path)
}
