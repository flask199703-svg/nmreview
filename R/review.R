# Structured diagnostics from a single identified estimation table.

nm_records <- function(lines, keyword) {
  clean <- sub(";.*$", "", lines)
  starts <- grep("^\\s*\\$[A-Za-z]", clean)
  ends <- c(starts[-1L] - 1L, length(clean))
  i <- which(grepl(paste0("^\\s*\\$(?:", keyword, ")\\b"), clean[starts], ignore.case = TRUE, perl = TRUE))
  vapply(i, function(j) paste(trimws(clean[starts[[j]]:ends[[j]]]), collapse = " "), "")
}

nm_parameter_key <- function(x) {
  z <- toupper(gsub("\\s+", "", x))
  z <- sub("^THETA\\(([0-9]+)\\)$", "THETA\\1", z)
  z
}

nm_theta_bounds <- function(parameters, control) {
  theta <- nm_records(control, "THETA")
  index <- match(paste0("THETA", seq_along(theta)), nm_parameter_key(parameters$parameter))
  if (!length(theta) || anyNA(index) ||
      sum(grepl("^THETA", nm_parameter_key(parameters$parameter))) != length(theta)) return(parameters)
  tokens <- lapply(theta, function(record) {
    body <- trimws(sub("^\\s*\\$THETA\\b", "", record, ignore.case = TRUE))
    body <- trimws(sub("\\s+FIX(?:ED)?\\s*$", "", body, ignore.case = TRUE, perl = TRUE))
    if (!grepl("^\\([^()]+\\)$", body)) return(nm_num(body))
    nm_num(strsplit(trimws(sub("^\\((.*)\\)$", "\\1", body)), "[,[:space:]]+")[[1L]])
  })
  if (any(vapply(tokens, function(z) !length(z) || length(z) > 3L || anyNA(z), logical(1)))) return(parameters)
  parameters$lower <- parameters$upper <- rep(NA_real_, nrow(parameters))
  for (i in seq_along(tokens)) {
    z <- tokens[[i]]
    if (length(z) >= 2L) parameters$lower[index[[i]]] <- z[[1L]]
    if (length(z) == 3L) parameters$upper[index[[i]]] <- z[[3L]]
  }
  parameters
}

nm_matrix_keys <- function(lines, family) {
  records <- nm_records(lines, family)
  position <- 0L
  previous <- 0L
  free <- character()
  known <- character()
  for (record in records) {
    s <- toupper(trimws(sub("^\\s*\\$\\w+", "", record)))
    block <- grepl("^BLOCK\\b", s)
    size <- regmatches(s, regexec("^(?:BLOCK|DIAGONAL)\\s*\\(\\s*([0-9]+)\\s*\\)", s, perl = TRUE))[[1L]]
    same <- grepl("\\bSAME\\b", s)
    if (same && !grepl("^(?:BLOCK\\s*(?:\\(\\s*[0-9]+\\s*\\))?\\s+)?SAME\\s*$", s, perl = TRUE)) return(NULL)
    body <- sub("^(?:BLOCK|DIAGONAL)\\s*\\(\\s*[0-9]+\\s*\\)", "", s, perl = TRUE)
    body <- gsub("\\b(?:FIX(?:ED)?|VARIANCE|COVARIANCE|CORRELATION|STANDARD|CHOLESKY)\\b", "", body, perl = TRUE)
    numbers <- regmatches(body, gregexpr("[-+]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[ED][-+]?[0-9]+)?", body, perl = TRUE))[[1L]]
    rest <- gsub("[-+]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[ED][-+]?[0-9]+)?|[\\s(),]", "", body, perl = TRUE)
    if (!same && nzchar(rest)) return(NULL)
    n <- if (length(size)) as.integer(size[[2L]]) else if (same) previous else length(numbers)
    if (is.na(n) || n < 1L || n > 200L || (block && !length(size) && !same)) return(NULL)
    if (same && n != previous) return(NULL)
    if (!same && length(numbers) != if (block) n * (n + 1) / 2 else n) return(NULL)
    indices <- position + seq_len(n)
    keys <- unlist(lapply(indices, function(i) {
      j <- if (block || same) indices[indices <= i] else i
      sprintf("%s(%d,%d)", family, i, j)
    }), use.names = FALSE)
    known <- c(known, keys)
    if (!same) free <- c(free, keys)
    position <- position + n
    previous <- n
  }
  list(countable = free, known = known, size = position)
}

nm_aic_count <- function(parameters, control) {
  unknown <- c(THETA = NA_integer_, OMEGA = NA_integer_, SIGMA = NA_integer_)
  if (!length(control) || !nrow(parameters) || any(grepl("^\\s*\\$(?:INCLUDE|MSFI|ABBREV)\\b", control, ignore.case = TRUE, perl = TRUE))) return(unknown)
  key <- nm_parameter_key(parameters$parameter)
  if (anyDuplicated(key) || any(!grepl("^(?:THETA[0-9]+|(?:OMEGA|SIGMA)\\([0-9]+,[0-9]+\\))$", key, perl = TRUE))) return(unknown)
  omega <- nm_matrix_keys(control, "OMEGA")
  sigma <- nm_matrix_keys(control, "SIGMA")
  if (is.null(omega) || is.null(sigma)) return(unknown)
  theta <- key[startsWith(key, "THETA")]
  if (!length(theta) || any(grepl("\\b(?:SAME|NUMBERPOINTS)\\b", nm_records(control, "THETA"), ignore.case = TRUE))) return(unknown)
  slots <- list(THETA = theta, OMEGA = omega$countable, SIGMA = sigma$countable)
  if (!all(c(omega$known, sigma$known) %in% key)) return(unknown)
  vapply(slots, function(slot) {
    i <- match(slot, key)
    if (anyNA(i) || anyNA(parameters$fixed[i]) || any(!is.finite(parameters$estimate[i]))) return(NA_integer_)
    as.integer(sum(!parameters$fixed[i]))
  }, integer(1))
}

nm_stage_header <- function(header, name) {
  pattern <- paste0("(?i)\\b", name, "\\s*=\\s*([0-9]+)")
  v <- regmatches(header, regexec(pattern, header, perl = TRUE))[[1L]]
  if (length(v) > 1L) as.integer(v[[2L]]) else NA_integer_
}

nm_stage_method <- function(header, fallback) {
  if (!nzchar(header)) return(fallback)
  method <- trimws(sub(":\\s*(?:Goal Function|Problem)\\s*=.*$", "", sub("^\\s*TABLE\\s+NO\\.\\s*[0-9]+\\s*:\\s*", "", header, ignore.case = TRUE), ignore.case = TRUE, perl = TRUE))
  if (nzchar(method)) method else fallback
}

nm_stage_listing <- function(lines, number, single) {
  starts <- grep("^\\s*#TBLN\\s*:", lines, ignore.case = TRUE)
  if (!length(starts)) return(if (single) lines else character())
  ids <- nm_num(sub("^\\s*#TBLN\\s*:\\s*", "", lines[starts], ignore.case = TRUE))
  hit <- which(ids == number)
  if (length(hit) != 1L) return(character())
  ends <- c(starts[-1L] - 1L, length(lines))
  index <- starts[[hit]]:ends[[hit]]
  structure(lines[index], source_lines = attr(lines, "source_lines")[index])
}

nm_shrinkage_rows <- function(metric, definition, value, source, line = NA_integer_, index = seq_along(value)) {
  if (!length(value)) return(NULL)
  data.frame(Metric = metric, Definition = definition, Index = index, Value = value, Source = source, Line = line, stringsAsFactors = FALSE)
}

nm_shrinkage_listing <- function(lines, source) {
  pattern <- "^\\s*((?:ETA|EPS|EBV)shrink(?:SD|VR)?\\s*\\(%\\)|ETABAR|RELATIVEINF\\s*\\(%\\))(?::\\s*|\\s+)(.*)$"
  line_number <- function(i) (attr(lines, "source_lines") %||% rep(NA_integer_, length(lines)))[i]
  hit <- grep(pattern, lines, ignore.case = TRUE, perl = TRUE)
  rows <- lapply(hit, function(i) {
    match <- regmatches(lines[[i]], regexec(pattern, lines[[i]], ignore.case = TRUE, perl = TRUE))[[1L]]
    label <- toupper(match[[2L]])
    metric <- if (label == "ETABAR") "ETABAR" else if (startsWith(label, "RELATIVE")) "ETA" else substr(label, 1, 3)
    definition <- if (label == "ETABAR") "Mean" else if (startsWith(label, "RELATIVE")) "Relative information" else if (grepl("VR", label)) "Variance" else "SD"
    nm_shrinkage_rows(metric, definition, nm_num(nm_header_tokens(match[[3L]])), source, line_number(i))
  })
  etabar <- hit[grepl("^\\s*ETABAR\\s*:", lines[hit], ignore.case = TRUE)]
  if (length(etabar) == 1L) {
    extra <- seq_along(lines)
    extra <- extra[extra > etabar & extra <= etabar + 8L]
    for (j in extra) {
      z <- regmatches(lines[[j]], regexec("^\\s*(SE|N|P\\s*VAL(?:UE)?\\.?)\\s*:\\s*(.*)$", lines[[j]], ignore.case = TRUE, perl = TRUE))[[1L]]
      if (!length(z)) next
      label <- toupper(z[[2L]])
      rows[[length(rows) + 1L]] <- nm_shrinkage_rows(if (label == "N") "ETA" else "ETABAR",
        switch(label, N = "Subjects", SE = "SE", "p-value"),
        nm_num(nm_header_tokens(z[[3L]])), source, line_number(j))
    }
  }
  do.call(rbind, rows)
}

nm_stage_sidecar <- function(tables, number, problem, single) {
  hit <- Filter(function(d) {
    header <- attr(d, "table_header") %||% ""
    p <- nm_stage_header(header, "Problem")
    s <- nm_stage_header(header, "Subproblem")
    n <- attr(d, "table_number") %||% NA_integer_
    (is.na(s) || s == 0L) && (is.na(p) || p == problem) &&
      (isTRUE(n == number) || (single && length(tables) == 1L && is.na(n)))
  }, tables)
  if (length(hit) == 1L) hit[[1L]] else NULL
}

nm_shrinkage_sidecar <- function(d, source, eps_count) {
  if (is.null(d) || !"TYPE" %in% names(d)) return(NULL)
  types <- c("ETABAR", "ETABAR", "ETABAR", "ETA", "EPS", "EBV", "ETA", "ETA", "EBV", "EPS", "ETA")
  defs <- c("Mean", "SE", "p-value", "SD", "SD", "SD", "Subjects", "Variance", "Variance", "Variance", "Relative information")
  do.call(rbind, lapply(seq_len(nrow(d)), function(i) {
    t <- nm_num(d$TYPE[[i]])
    if (!is.finite(t) || t != as.integer(t) || t < 1L || t > length(types) || sum(d$TYPE == t, na.rm = TRUE) != 1L) return(NULL)
    eps <- types[[t]] == "EPS"
    cols <- grep(if (eps) "^EPS\\(?[0-9]+\\)?$" else "^ETA\\(?[0-9]+\\)?$", names(d), ignore.case = TRUE, value = TRUE)
    if (eps && !length(cols) && is.finite(eps_count)) {
      cols <- grep("^ETA\\(?[0-9]+\\)?$", names(d), ignore.case = TRUE, value = TRUE)
      cols <- cols[as.integer(gsub("[^0-9]", "", cols)) <= eps_count]
    }
    indices <- as.integer(gsub("[^0-9]", "", cols))
    nm_shrinkage_rows(types[[t]], defs[[t]], nm_num(unlist(d[i, cols, drop = FALSE], use.names = FALSE)), source, d$.source_line[[i]], indices)
  }))
}

nm_merge_shrinkage <- function(primary, backup) {
  if (is.null(primary)) return(list(rows = backup, conflict = FALSE))
  if (is.null(backup)) return(list(rows = primary, conflict = FALSE))
  key <- function(d) paste(d$Metric, d$Definition, d$Index)
  p <- match(key(backup), key(primary))
  different <- !is.na(p) & is.finite(backup$Value) & is.finite(primary$Value[p]) &
    abs(backup$Value - primary$Value[p]) > 0.0001 * pmax(1, abs(primary$Value[p]))
  keep <- is.na(p) | !is.finite(primary$Value[p]) | different
  primary$Conflict <- key(primary) %in% key(backup)[different]
  backup$Conflict <- different
  replace <- unique(p[!is.na(p) & !is.finite(primary$Value[p]) & is.finite(backup$Value)])
  list(rows = rbind(primary[setdiff(seq_len(nrow(primary)), replace), , drop = FALSE], backup[keep, , drop = FALSE]), conflict = any(different))
}

nm_review_matrix <- function(d, parameters, kind, covariance = NULL) {
  if (is.null(d) || !nrow(d) || ncol(d) < 4L) return(NULL)
  rows <- nm_parameter_key(d[[1L]])
  columns <- setdiff(names(d)[-1L], c(".source_file", ".source_line"))
  names_key <- nm_parameter_key(columns)
  active <- nm_parameter_key(parameters$parameter[which(parameters$fixed %in% FALSE)])
  shape <- length(active) > 0L && !anyNA(parameters$fixed) && !anyDuplicated(rows) && !anyDuplicated(names_key) &&
    all(active %in% rows) && all(active %in% names_key)
  if (!shape) return(list(kind = kind, status = "Incomplete or unknown fixed parameters"))
  matrix <- vapply(columns[match(active, names_key)], function(k) nm_num(d[[k]])[match(active, rows)], numeric(length(active)))
  matrix <- matrix(matrix, nrow = length(active), dimnames = list(active, active))
  if (any(!is.finite(matrix)) || !isSymmetric(unname(matrix), tol = 1e-6)) return(list(kind = kind, status = "Incomplete or asymmetric"))
  diagonal_se <- NULL
  if (startsWith(kind, "Correlation")) {
    diagonal <- diag(matrix)
    se <- parameters$se[match(active, nm_parameter_key(parameters$parameter))]
    matches_se <- function(v) length(v) == length(diagonal) && all(is.finite(v)) &&
      all(v > 0) && all(abs(diagonal - v) <= 1e-5 * pmax(1e-8, v))
    cov_se <- if (!is.null(covariance$values) && all(diag(covariance$values) > 0)) sqrt(diag(covariance$values)) else numeric()
    if (matches_se(se) || matches_se(cov_se)) {
      diagonal_se <- stats::setNames(diagonal, active)
    } else if (any(abs(diagonal - 1) > 1e-6)) {
      return(list(kind = kind, status = "Correlation diagonal could not be identified"))
    }
    if (any(abs(matrix[row(matrix) != col(matrix)]) > 1.000001)) return(list(kind = kind, status = "Invalid correlation coefficient"))
    diag(matrix) <- 1
  }
  values <- eigen((matrix + t(matrix)) / 2, symmetric = TRUE, only.values = TRUE)$values
  tol <- max(abs(values)) * sqrt(.Machine$double.eps)
  status <- if (min(values) < -tol) "Not positive definite" else if (min(values) <= tol) "Singular" else "Available"
  list(kind = kind, status = status, values = matrix, minimum = min(values),
       condition = if (status == "Available") max(values) / min(values) else NA_real_,
       source = d$.source_file[[1L]], diagonal_se = diagonal_se)
}

nm_review_context <- function(control, path, files, root, cache) {
  data <- nm_records(control, "DATA")
  input <- nm_records(control, "INPUT")
  fields <- regmatches(data, regexec("^\\s*\\$DATA\\s+(\"[^\"]+\"|'[^']+'|[^[:space:]]+)(.*)$", data, ignore.case = TRUE, perl = TRUE))
  match <- if (length(fields) == 1L) fields[[1L]] else character()
  file <- character()
  if (length(match) == 3L && !is.na(path)) {
    declared <- nm_clean_name(match[[2L]])
    if (!grepl("^(?:[A-Za-z]:[/\\\\]|[/\\\\])", declared, perl = TRUE)) declared <- file.path(dirname(path), declared)
    target <- normalizePath(declared, winslash = "/", mustWork = FALSE)
    file <- files[nm_normalize_slashes(files) == target]
    if (.Platform$OS.type == "windows" && !length(file)) file <- files[tolower(nm_normalize_slashes(files)) == tolower(target)]
  }
  signature <- NA_character_
  if (length(file) == 1L && length(input) == 1L && !any(grepl("^\\s*\\$(?:INCLUDE|MSFI)\\b", control, ignore.case = TRUE, perl = TRUE))) {
    if (!exists(file, cache, inherits = FALSE)) {
      assign(file, tryCatch(digest::digest(file, algo = "sha256", file = TRUE, serialize = FALSE), error = function(e) NA_character_), cache)
    }
    hash <- get(file, cache, inherits = FALSE)
    if (!is.na(hash)) signature <- paste(hash, trimws(match[[3L]]), input, sep = " | ")
  }
  declaration <- function(key) {
    hits <- grep(paste0("^\\s*;\\s*NMREVIEW\\s+", key, "\\s*="), control, value = TRUE, ignore.case = TRUE)
    if (length(hits) == 1L) trimws(sub("^[^=]+=", "", hits)) else NA_character_
  }
  list(data_signature = signature, data_source = if (length(file) == 1L) nm_relpath(file, root) else NA_character_,
       likelihood = declaration("LIKELIHOOD"), dv_scale = declaration("DV_SCALE"),
       parent = declaration("PARENT"))
}

nm_review_model <- function(model, problem, listing, ext, files, catalog, root, cache) {
  control <- problem$text
  model$review_context <- nm_review_context(control, if (length(problem$control_path)) problem$control_path[[1L]] else NA_character_, catalog, root, cache)
  model$likelihood <- model$review_context$likelihood
  model$dv_scale <- model$review_context$dv_scale
  model$control_code <- if (length(control) && sum(nchar(control, type = "bytes")) <= 128 * 1024) control else character()
  resolve <- function(paths, declared) unique(unlist(lapply(declared, nm_resolve_named_file, candidates = paths, base_dir = dirname(problem$control_path %||% "")), use.names = FALSE))
  tables <- resolve(catalog, problem$table_file)
  model$table_catalog <- vapply(tables, nm_relpath, "", root = root)
  model$live_root <- root
  sections <- if (length(ext) && !is.na(ext)) nm_read_ext_sections(ext) else list()
  single <- length(sections) == 1L && !isTRUE(problem$multiple_est)
  sidecar <- function(extension) {
    path <- files[endsWith(tolower(files), extension)]
    if (length(path) == 1L) list(path = nm_relpath(path, root), tables = nm_read_table_file(path)) else list(path = "", tables = list())
  }
  shk <- sidecar(".shk")
  cor <- sidecar(".cor")
  cov <- sidecar(".cov")
  sigma <- nm_matrix_keys(control, "SIGMA")
  if (is.null(sigma)) sigma <- list(size = NA_integer_)
  if (!length(sections) && !isTRUE(problem$multiple_est)) {
    sh <- nm_shrinkage_listing(listing, source_label(model$source))
    if (!is.null(sh)) model$review <- list(shrinkage = sh, status = model$status, aic_reason = "No identified final EXT estimate")
    return(model)
  }
  stages <- lapply(seq_along(sections), function(i) {
    d <- sections[[i]]
    header <- attr(d, "table_header") %||% ""
    p <- nm_stage_header(header, "Problem")
    sub <- nm_stage_header(header, "Subproblem")
    number <- attr(d, "table_number") %||% NA_integer_
    if (!single && (is.na(p) || is.na(number))) return(NULL)
    if ((!is.na(p) && p != problem$problem) || (!is.na(sub) && sub != 0L)) return(NULL)
    result <- nm_ext_parameters(ext, number, sections = sections)
    if (!nrow(result$parameters)) return(NULL)
    result$parameters <- nm_theta_bounds(result$parameters, control)
    lines <- nm_stage_listing(listing, number, single)
    status <- if (length(lines)) nm_extract_listing(lines)$status else "unknown"
    method <- nm_stage_method(header, model$method)
    control_est <- nm_records(control, "EST|ESTIMATION")
    estimation_only <- "\\b(?:BAYES|MAP|EONLY\\s*=\\s*1|MAXEVAL(?:S)?\\s*=\\s*0)\\b"
    prior <- "^\\s*\\$(?:PRIOR|THETAP|OMEGAP|SIGMAP)\\b"
    not_likelihood <- !length(control_est) ||
      any(grepl(estimation_only, c(control_est, header), ignore.case = TRUE, perl = TRUE)) ||
      any(grepl(prior, control, ignore.case = TRUE, perl = TRUE))
    supported <- grepl("FIRST ORDER|LAPLAC|FOCE|CONDITIONAL|SAEM|IMPORTANCE|^IMP\\b", method, ignore.case = TRUE, perl = TRUE)
    goal <- !grepl("Goal Function\\s*=", header, ignore.case = TRUE) ||
      grepl("Goal Function\\s*=\\s*(?:MINIMUM VALUE OF OBJECTIVE FUNCTION|[- ]*2\\s*LOG LIKELIHOOD)", header, ignore.case = TRUE, perl = TRUE)
    reason <- ""
    if (status != "success") reason <- "Successful estimation not established"
    else if (!is.finite(result$ofv)) reason <- "Final objective unavailable"
    else if (not_likelihood) reason <- "Likelihood at estimated parameters not established"
    else if (!supported || !goal) reason <- "Unsupported objective or estimation method"
    shrink <- nm_merge_shrinkage(
      nm_shrinkage_sidecar(nm_stage_sidecar(shk$tables, number, problem$problem, single), shk$path, sigma$size),
      nm_shrinkage_listing(lines, model$source$files$path[match("listing", model$source$files$role)])
    )
    covariance_matrix <- nm_review_matrix(nm_stage_sidecar(cov$tables, number, problem$problem, single), result$parameters, "Covariance of estimates")
    list(number = if (is.na(number)) 1L else number, method = method, header = header,
      parameters = result$parameters, ofv = result$ofv, status = status,
      covariance = nm_covariance(character(), root, lines, control),
      termination = nm_termination_excerpt(lines), shrinkage = shrink$rows, conflict = shrink$conflict,
      counts = nm_aic_count(result$parameters, control), aic_reason = reason,
      condition_reported = result$condition_reported,
      source = nm_relpath(ext, root), final_line = result$final_line,
      trace = result$trace, iteration_rows = result$iteration_rows,
      correlation = nm_review_matrix(nm_stage_sidecar(cor$tables, number, problem$problem, single), result$parameters, "Correlation of estimates", covariance_matrix),
      covariance_matrix = covariance_matrix)
  })
  model$stages <- Filter(Negate(is.null), stages)
  if (single && length(model$stages) == 1L) {
    model$review <- model$stages[[1L]]
    model$parameters <- model$review$parameters
  }
  model
}

nm_select_stage <- function(model, selection) {
  if (is.null(model) || !length(selection) || !nzchar(selection)) return(model)
  stages <- Filter(function(z) identical(as.character(z$number), selection), model$stages)
  if (length(stages) != 1L) return(model)
  s <- stages[[1L]]
  model$review <- s
  for (k in c("parameters", "ofv", "method", "status", "covariance", "termination")) model[[k]] <- s[[k]]
  model$table_number <- s$number
  model$notices <- unique(c(model$notices[!grepl("Select an identified|multiple EXT tables", model$notices)],
    if (length(model$stages) > 1L) "The diagnostic table is shown independently of the selected estimation table."))
  model
}

nm_model_aic <- function(model, k_override = NULL) {
  review <- model$review
  absent <- list(value = NA_real_, k = NA_integer_, counts = NULL, reason = "Final likelihood or parameter count unavailable", manual = FALSE)
  if (is.null(review)) return(absent)
  if (nzchar(review$aic_reason %||% "")) {
    absent$reason <- review$aic_reason
    return(absent)
  }
  counts <- review$counts
  k <- if (length(counts) && !anyNA(counts)) sum(counts) else NA_real_
  manual <- is.na(k) && length(k_override) == 1L && is.finite(k_override) && k_override >= 0 && k_override == floor(k_override)
  if (manual) k <- k_override
  if (!is.finite(k) || length(review$ofv) != 1L || !is.finite(review$ofv)) return(absent)
  list(value = review$ofv + 2 * k, k = k, counts = counts, reason = "", manual = manual)
}

nm_aic_comparison <- function(reference, candidate, a, b, assume = FALSE) {
  withheld <- function(reason) list(delta = NA_real_, reason = reason)
  if (!is.finite(a$value) || !is.finite(b$value)) return(withheld("AIC unavailable for one or both runs"))
  x <- reference$review_context
  y <- candidate$review_context
  keys <- c("data_signature", "likelihood", "dv_scale")
  known <- function(z) length(z) == 1L && !is.na(z) && nzchar(z)
  both <- vapply(keys, function(k) known(x[[k]]) && known(y[[k]]), logical(1))
  different <- keys[both & vapply(keys, function(k) !identical(x[[k]], y[[k]]), logical(1))]
  if (length(different)) return(withheld(paste("Different", paste(gsub("_", " ", different), collapse = ", "))))
  if (!identical(tolower(reference$review$method), tolower(candidate$review$method))) return(withheld("Likelihood evaluation methods differ"))
  if (!all(both)) {
    if (!isTRUE(assume)) return(withheld("Fitted data, response scale or likelihood conditions unconfirmed"))
    if (!both[[1L]] && !isTRUE(compare_models(reference, candidate)$comparable)) return(withheld("Fitted data not verified and observation matching unavailable"))
  }
  list(delta = b$value - a$value, reason = if (!all(both)) "User-confirmed comparison assumptions" else "Input signature matched; likelihood and scale declared")
}
