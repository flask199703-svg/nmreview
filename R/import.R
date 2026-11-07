# NONMEM / PsN import helpers for the local review application.
# The reader is intentionally dependency-light so it can be tested without
# starting Shiny or installing the optional plotting packages.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

nm_normalize_slashes <- function(x) gsub("\\\\", "/", x)

nm_relpath <- function(path, root) {
  if (is.null(path) || !length(path) || is.na(path[[1L]])) {
    return(NA_character_)
  }
  p <- nm_normalize_slashes(normalizePath(path[[1L]], winslash = "/", mustWork = FALSE))
  r <- nm_normalize_slashes(normalizePath(root, winslash = "/", mustWork = FALSE))
  if (identical(tolower(substr(p, 1L, nchar(r))), tolower(r)) &&
    (nchar(p) == nchar(r) || substr(p, nchar(r) + 1L, nchar(r) + 1L) == "/")) {
    out <- substring(p, nchar(r) + 1L)
    sub("^/", "", out)
  } else {
    basename(p)
  }
}

nm_read_lines <- function(path) {
  con <- file(path, open = "r", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  readLines(con, warn = FALSE, encoding = "UTF-8")
}

# Diagnostics can be very large. Keep only a bounded, source-linked preview
# and never attempt to infer numerical quantities from a diagnostic file here.
nm_read_preview <- function(path, max_lines = 240L, max_bytes = 65536L) {
  if (is.null(path) || !length(path) || is.na(path) || !file.exists(path)) {
    return(list(text = character(), truncated = FALSE))
  }
  con <- file(path, open = "r", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  x <- readLines(con,
    n = as.integer(max_lines) + 1L, warn = FALSE,
    encoding = "UTF-8"
  )
  truncated <- length(x) > max_lines
  if (length(x) > max_lines) x <- x[seq_len(max_lines)]
  if (length(x)) {
    nb <- nchar(x, type = "bytes")
    if (sum(nb) > max_bytes) {
      keep <- character()
      used <- 0L
      for (line in x) {
        left <- max_bytes - used
        if (left <= 0L) break
        line_bytes <- nchar(line, type = "bytes")
        if (line_bytes <= left) {
          keep <- c(keep, line)
          used <- used + line_bytes
        } else {
          keep <- c(keep, substr(line, 1L, left))
          break
        }
      }
      x <- keep
      truncated <- TRUE
    }
  }
  list(text = x, truncated = isTRUE(truncated))
}

nm_num <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  if (length(x) == 0L) {
    return(numeric())
  }
  z <- trimws(as.character(x))
  z[z %in% c("", ".", "NA", "N/A", "NULL", "NaN")] <- NA_character_
  z <- gsub("[dD]", "e", z)
  suppressWarnings(as.numeric(z))
}

nm_one_num <- function(x) {
  n <- nm_num(x)
  if (!length(n) || is.na(n[[1L]])) NA_real_ else n[[1L]]
}

nm_numericish <- function(x, threshold = 0.8) {
  if (is.numeric(x) || is.integer(x)) {
    return(as.numeric(x))
  }
  z <- trimws(as.character(x))
  if (!length(z)) {
    return(x)
  }
  n <- nm_num(z)
  ok <- !is.na(n) | is.na(z) | z %in% c("", ".", "NA", "N/A", "NULL", "NaN")
  if (mean(ok) >= threshold && any(!is.na(n))) n else x
}

nm_safe_md5 <- function(path) {
  if (requireNamespace("digest", quietly = TRUE)) {
    tryCatch(as.character(digest::digest(file = path, algo = "md5", serialize = FALSE)),
      error = function(e) NA_character_
    )
  } else {
    as.character(unname(tools::md5sum(path)))
  }
}

nm_empty_data <- function() {
  data.frame(
    .source_line = integer(), .source_file = character(),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

nm_clean_name <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("^['\"]|['\"]$", "", x)
  x <- gsub("[[:space:]]+", "_", x)
  x
}

nm_norm_name <- function(x) toupper(gsub("[^A-Za-z0-9]", "", as.character(x)))

nm_find_col <- function(df, candidates) {
  if (is.null(df) || !ncol(df)) {
    return(NA_integer_)
  }
  exact <- match(as.character(candidates), names(df), nomatch = 0L)
  if (any(exact > 0L)) {
    return(exact[which(exact > 0L)[[1L]]])
  }
  n <- nm_norm_name(names(df))
  q <- nm_norm_name(candidates)
  q <- q[nzchar(q)]
  n[!nzchar(n)] <- NA_character_
  idx <- match(q, n, nomatch = 0L)
  if (any(idx > 0L)) idx[which(idx > 0L)[[1L]]] else NA_integer_
}

nm_pick_col <- function(df, candidates) {
  i <- nm_find_col(df, candidates)
  if (is.na(i)) NULL else df[[i]]
}

nm_header_tokens <- function(line) {
  toks <- strsplit(trimws(line), "[[:space:]]+")[[1L]]
  toks[nzchar(toks)]
}

nm_is_number_token <- function(x) !is.na(nm_one_num(x))

nm_parse_table_section <- function(lines, start_line = 1L, source_file = NA_character_) {
  if (!length(lines)) {
    return(NULL)
  }
  lines <- sub("\\r$", "", lines)
  keep <- which(nzchar(trimws(lines)) & !grepl("^\\s*(#|;)", lines))
  if (!length(keep)) {
    return(NULL)
  }
  lines2 <- lines[keep]
  line_numbers <- start_line - 1L + keep
  marker <- grepl("^\\s*TABLE\\s+NO\\.", lines2, ignore.case = TRUE)
  if (any(marker)) {
    lines2 <- lines2[!marker]
    line_numbers <- line_numbers[!marker]
  }
  if (length(lines2) < 2L) {
    return(NULL)
  }
  header_i <- NA_integer_
  for (i in seq_len(min(length(lines2) - 1L, 20L))) {
    ht <- nm_header_tokens(lines2[[i]])
    if (!length(ht)) next
    nt <- nm_header_tokens(lines2[[i + 1L]])
    if (length(nt) >= length(ht) && mean(vapply(nt[seq_len(min(length(nt), length(ht)))], nm_is_number_token, logical(1))) > .5) {
      header_i <- i
      break
    }
  }
  if (is.na(header_i)) {
    marker_i <- which(vapply(lines2, function(z) any(grepl("^(ID|TIME|DV|PRED|IPRED|CWRES|IWRES|ITERATION|THETA|OMEGA|SIGMA|TYPE|NAME)$", nm_header_tokens(z), ignore.case = TRUE)), logical(1)))
    if (length(marker_i)) header_i <- marker_i[[1L]]
  }
  if (is.na(header_i) || header_i >= length(lines2)) {
    return(NULL)
  }
  header <- nm_clean_name(nm_header_tokens(lines2[[header_i]]))
  header <- make.unique(header, sep = "_")
  rows <- lines2[(header_i + 1L):length(lines2)]
  row_lines <- line_numbers[(header_i + 1L):length(lines2)]
  if (!length(rows)) {
    return(NULL)
  }
  vals0 <- lapply(rows, nm_header_tokens)
  len0 <- vapply(vals0, length, integer(1))
  vals <- vals0[len0 >= 1L]
  row_lines <- row_lines[len0 >= 1L]
  if (!length(vals)) {
    return(NULL)
  }
  out <- as.data.frame(lapply(seq_along(header), function(j) vapply(vals, function(v) if (length(v) >= j) v[[j]] else NA_character_, character(1))), stringsAsFactors = FALSE, check.names = FALSE)
  names(out) <- header
  identifiers <- toupper(header) %in% c("ID", "USUBJID", "SUBJID", "STUDY", "STUDYID")
  out[!identifiers] <- lapply(out[!identifiers], nm_numericish)
  if (any(identifiers)) out[identifiers] <- lapply(out[identifiers], function(x) {
    x[is.na(x) | x %in% c("", ".", "NA", "*")] <- NA_character_
    x
  })
  out$.source_line <- as.integer(row_lines)
  out$.source_file <- as.character(source_file)
  out
}

nm_read_table_file <- function(path) {
  lines <- tryCatch(nm_read_lines(path), error = function(e) character())
  if (!length(lines)) {
    return(list())
  }
  markers <- which(grepl("^\\s*TABLE\\s+NO\\.", lines, ignore.case = TRUE))
  if (!length(markers)) {
    x <- nm_parse_table_section(lines, 1L, path)
    if (is.null(x)) {
      return(list())
    }
    attr(x, "table_number") <- NA_integer_
    return(list(x))
  }
  ends <- c(markers[-1L] - 1L, length(lines))
  ans <- list()
  for (i in seq_along(markers)) {
    x <- nm_parse_table_section(lines[markers[[i]]:ends[[i]]], markers[[i]], path)
    if (!is.null(x) && nrow(x)) {
      marker_num <- regmatches(
        lines[[markers[[i]]]],
        regexpr("[0-9]+", lines[[markers[[i]]]], perl = TRUE)
      )
      attr(x, "table_number") <- if (length(marker_num) && nzchar(marker_num)) {
        as.integer(marker_num)
      } else {
        NA_integer_
      }
      attr(x, "table_header") <- lines[[markers[[i]]]]
      ans[[length(ans) + 1L]] <- x
    }
  }
  ans
}

nm_read_ext_sections <- function(path) {
  sec <- nm_read_table_file(path)
  if (length(sec)) {
    return(sec)
  }
  lines <- tryCatch(nm_read_lines(path), error = function(e) character())
  x <- nm_parse_table_section(lines, 1L, path)
  if (is.null(x)) {
    return(list())
  }
  attr(x, "table_number") <- NA_integer_
  list(x)
}

nm_file_candidates <- function(root) {
  all <- list.files(root, recursive = TRUE, all.files = FALSE, full.names = TRUE, include.dirs = FALSE)
  if (!length(all)) {
    return(character())
  }
  ii <- file.info(all)
  all[!ii$isdir & !is.na(ii$size)]
}

nm_is_nested_psn_replica <- function(path, root) {
  rel <- tolower(nm_relpath(path, root))
  parts <- strsplit(rel, "/", fixed = TRUE)[[1L]]
  if (length(parts) <= 1L) {
    return(FALSE)
  }
  dirs <- parts[-length(parts)]
  any(grepl("^(rundir|raw_results|m[0-9]+|sim[0-9]*|rep(licate)?[0-9]*|bootstrap(_[0-9]+)?|vpc(_[0-9]+)?|npc(_[0-9]+)?)$", dirs))
}

nm_parse_control_sections <- function(path, fallback_name = basename(path)) {
  lines <- tryCatch(nm_read_lines(path), error = function(e) character())
  starts <- which(grepl("^\\s*\\$PROBLEM\\b", lines, ignore.case = TRUE))
  if (!length(starts)) starts <- 1L
  ends <- c(starts[-1L] - 1L, length(lines))
  ans <- vector("list", length(starts))
  for (i in seq_along(starts)) {
    block <- lines[starts[[i]]:ends[[i]]]
    pi <- grep("^\\s*\\$PROBLEM\\b", block, ignore.case = TRUE)
    p_line <- if (length(pi)) block[[pi[[1L]]]] else ""
    label <- trimws(sub("^\\s*\\$PROBLEM\\s*", "", p_line, ignore.case = TRUE))
    if (!nzchar(label)) label <- paste0("Problem ", i)
    command_block <- function(tag) {
      st <- grep(paste0("^\\s*\\$", tag, "\\b"), block, ignore.case = TRUE)
      if (!length(st)) {
        return(character())
      }
      out <- character()
      for (j in st) {
        en <- which(seq_along(block) > j & grepl("^\\s*\\$[A-Za-z]", block))
        en <- if (length(en)) en[[1L]] - 1L else length(block)
        out <- c(out, block[j:en])
      }
      out
    }
    est_starts <- grep("^\\s*\\$(?:EST|ESTIMATION)\\b", block,
      ignore.case = TRUE, perl = TRUE
    )
    est_lines <- command_block("(?:EST|ESTIMATION)")
    est <- paste(est_lines, collapse = " ")
    method <- ""
    if (nzchar(est)) {
      em <- regmatches(est, regexpr("(?i)(FOCE(?:-I|\\s+INTERACTION)?|LAPLACIAN|SAEM|IMP|BAYES|MCMC|ITS|COND(?:\\s+INTERACTION)?)", est, perl = TRUE))
      if (length(em) && nzchar(em)) method <- toupper(trimws(em))
    }
    ext_file <- character()
    if (nzchar(est)) {
      mm <- regmatches(est, gregexpr("(?i)FILE\\s*=\\s*('[^']+'|\"[^\"]+\"|[^[:space:]]+)", est, perl = TRUE))[[1L]]
      if (length(mm)) ext_file <- nm_clean_name(sub("(?i)^.*?=\\s*", "", mm[[length(mm)]], perl = TRUE))
    }
    table_lines <- command_block("TABLE")
    table_text <- paste(table_lines, collapse = " ")
    table_file <- character()
    if (nzchar(table_text)) {
      mm <- regmatches(table_text, gregexpr("(?i)FILE\\s*=\\s*('[^']+'|\"[^\"]+\"|[^[:space:]]+)", table_text, perl = TRUE))[[1L]]
      if (length(mm)) table_file <- vapply(mm, function(z) nm_clean_name(sub("(?i)^.*?=\\s*", "", z, perl = TRUE)), character(1))
    }
    sim <- any(grepl("^\\s*\\$SIMULATION\\b|\\bsimulation\\b|\\bsimulate\\b", block, ignore.case = TRUE))
    ans[[i]] <- list(
      problem = i, label = label, method = method, ext_file = ext_file, table_file = table_file,
      simulation = sim, multiple_est = length(est_starts) > 1L,
      firstonly = any(grepl("\\bFIRSTONLY\\b", table_text, ignore.case = TRUE)), text = block, control_path = path
    )
  }
  ans
}

nm_listing_blocks <- function(path) {
  if (is.null(path) || !length(path) || is.na(path) || !file.exists(path)) {
    return(list())
  }
  lines <- tryCatch(nm_read_lines(path), error = function(e) character())
  if (!length(lines)) {
    return(list())
  }
  marker <- which(grepl("(?i)(?:PROBLEM\\s*(?:NO\\.?|NUMBER)?\\s*[:=]?\\s*|#PROB(?:LEM)?\\s*)[0-9]+", lines, perl = TRUE))
  if (!length(marker)) {
    attr(lines, "source_lines") <- seq_along(lines)
    return(list(all = lines))
  }
  ids <- vapply(lines[marker], function(z) {
    m <- regmatches(z, regexpr("[0-9]+", z, perl = TRUE))
    if (length(m)) as.integer(m) else NA_integer_
  }, integer(1))
  keep <- !duplicated(ids) & !is.na(ids)
  marker <- marker[keep]
  ids <- ids[keep]
  ends <- c(marker[-1L] - 1L, length(lines))
  out <- list()
  for (i in seq_along(marker)) {
    index <- marker[[i]]:ends[[i]]
    out[[as.character(ids[[i]])]] <- structure(lines[index], source_lines = index)
  }
  out
}

nm_extract_listing <- function(lines) {
  if (!length(lines)) {
    return(list(status = "unknown", method = "", ofv = NA_real_, nind = NA_integer_, nobs = NA_integer_, text = ""))
  }
  get_num <- function(patterns) {
    for (p in patterns) {
      hit <- grep(p, lines, ignore.case = TRUE, value = TRUE, perl = TRUE)
      if (length(hit)) {
        z <- regmatches(hit[[length(hit)]], gregexpr("[-+]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eEdD][-+]?[0-9]+)?", hit[[length(hit)]], perl = TRUE))[[1L]]
        if (length(z)) {
          return(nm_one_num(z[[length(z)]]))
        }
      }
    }
    NA_real_
  }
  ofv <- get_num(c("#\\s*OBJV", "OBJECTIVE\\s+FUNCTION(?:\\s+VALUE)?", "FINAL\\s+OBJECTIVE", "OFV"))
  nind <- get_num(c("NUMBER\\s+OF\\s+INDIVIDUALS", "NO\\.?\\s+OF\\s+INDIVIDUALS", "SUBJECTS"))
  nobs <- get_num(c("NUMBER\\s+OF\\s+OBSERVATIONS", "NO\\.?\\s+OF\\s+OBSERVATIONS", "NO\\.?\\s+OF\\s+OBS(?:ERVATION)?\\s+RECS?"))
  term <- nm_termination_excerpt(lines)
  has <- function(pattern) any(grepl(pattern, term, ignore.case = TRUE, perl = TRUE))
  status <- "unknown"
  if (has("MINIMIZATION\\s+(?:TERMINATED|FAILED|UNSUCCESSFUL)|ESTIMATION\\s+(?:FAILED|UNSUCCESSFUL)|^\\s*0?(?:ERROR IN|RUN ABORTED|ANALYSIS ABORTED)")) status <- "failed"
  else if (has("EXPECTATION ONLY PROCESS WAS COMPLETED")) status <- "evaluation only"
  else if (has("MINIMIZATION\\s+SUCCESSFUL|ESTIMATION\\s+SUCCESSFUL|SUCCESSFULLY\\s+MINIMIZED")) status <- "success"
  else if (has("(?:STOCHASTIC|ACCUMULATION|OPTIMIZATION) (?:PORTION )?WAS COMPLETED")) status <- "completed"
  else if (!has("MINIMIZATION|ESTIMATION|#TERM") && !any(grepl("^\\s*#METH\\b", lines))) status <- "not_run"
  method <- ""
  mh <- grep("(?i)#\\s*METH|METHOD|ESTIMATION METHOD", lines, value = TRUE, perl = TRUE)
  if (length(mh)) method <- trimws(sub("^.*?(?:#\\s*METH|METHOD|ESTIMATION METHOD)\\s*[:=]?\\s*", "", mh[[length(mh)]], ignore.case = TRUE, perl = TRUE))
  list(
    status = status, method = method, ofv = ofv,
    nind = if (is.na(nind)) NA_integer_ else as.integer(round(nind)), nobs = if (is.na(nobs)) NA_integer_ else as.integer(round(nobs)), text = paste(lines, collapse = "\n")
  )
}

nm_resolve_named_file <- function(named, candidates, base_dir = NULL) {
  if (!length(named)) {
    return(character())
  }
  named <- nm_clean_name(named[[1L]])
  if (!nzchar(named)) {
    return(character())
  }
  cand <- candidates
  norm <- nm_normalize_slashes(cand)
  target <- named
  absolute <- grepl("^(?:[A-Za-z]:[/\\\\]|[/\\\\])", named)
  if (!absolute && !is.null(base_dir)) target <- file.path(base_dir, named)
  target_norm <- nm_normalize_slashes(normalizePath(target, winslash = "/", mustWork = FALSE))
  exact <- which(tolower(norm) == tolower(target_norm))
  if (length(exact)) {
    return(cand[[exact[[1L]]]])
  }
  # A declared path outside the selected root is not silently imported.  A
  # basename fallback is allowed only when it is unique among candidates.
  idx <- which(tolower(basename(norm)) == tolower(basename(named)))
  if (!length(idx) && !grepl("\\.[A-Za-z0-9]+$", named)) idx <- which(tolower(tools::file_path_sans_ext(basename(norm))) == tolower(named))
  if (length(idx) == 1L) cand[[idx[[1L]]]] else character()
}

nm_find_related <- function(candidates, stem, extensions = character()) {
  if (!length(candidates)) {
    return(character())
  }
  b <- basename(candidates)
  noext <- tools::file_path_sans_ext(b)
  idx <- which(tolower(b) == tolower(stem) | tolower(noext) == tolower(stem))
  if (length(extensions) && length(idx)) idx <- idx[tolower(tools::file_ext(b[idx])) %in% tolower(sub("^\\.", "", extensions))]
  candidates[idx]
}

nm_choose_table <- function(candidates, declared = character(), run_stem = "") {
  if (!length(candidates)) {
    return(list(data = NULL, path = NA_character_, table_number = NA_integer_, notice = character()))
  }
  notes <- character()
  chosen <- character()
  declared_paths <- character()
  if (length(declared)) {
    for (d in declared) {
      x <- nm_resolve_named_file(d, candidates)
      if (length(x)) declared_paths <- c(declared_paths, x)
    }
  }
  declared_paths <- unique(declared_paths)
  if (length(declared_paths)) chosen <- declared_paths[[1L]]
  if (!length(chosen) && nzchar(run_stem)) {
    x <- nm_find_related(candidates, run_stem)
    if (length(x) == 1L) chosen <- x
  }
  if (!length(chosen) && length(candidates) == 1L) chosen <- candidates[[1L]]
  if (!length(chosen)) {
    return(list(data = NULL, path = NA_character_, table_number = NA_integer_, notice = "multiple table candidates; declaration unresolved"))
  }
  table_paths <- if (length(declared_paths)) declared_paths else chosen[[1L]]
  table_score <- function(d) sum(nm_norm_name(names(d)) %in% c("ID", "TIME", "DV", "PRED", "IPRED", "CWRES", "IWRES", "WRES", "EVID"))
  all_sections <- lapply(table_paths, function(path) {
    sec <- nm_read_table_file(path)
    if (!length(sec)) {
      return(NULL)
    }
    score <- vapply(sec, table_score, numeric(1))
    pick <- which(score == max(score))[[1L]]
    list(data = sec[[pick]], index = pick, multi = length(sec) > 1L)
  })
  good <- !vapply(all_sections, is.null, logical(1))
  if (!any(good)) {
    return(list(data = NULL, path = chosen[[1L]], table_number = NA_integer_, notice = "diagnostic table could not be read"))
  }
  if (sum(good) > 1L) {
    notes <- c(notes, "multiple declared tables; selected strongest diagnostic table without row merge")
    sc <- vapply(all_sections, function(d) if (is.null(d)) -Inf else table_score(d$data), numeric(1))
    pick <- which(sc == max(sc))[[1L]]
    chosen <- table_paths[[pick]]
    sec <- all_sections[[pick]]
  } else {
    pick <- which(good)[[1L]]
    chosen <- table_paths[[pick]]
    sec <- all_sections[[pick]]
  }
  if (isTRUE(sec$multi)) notes <- c(notes, "multiple TABLE sections; selected strongest diagnostic section")
  list(data = sec$data, path = chosen, table_number = as.integer(sec$index), notice = notes)
}

nm_ext_parameters <- function(ext_path, preferred_table = NA_integer_, sections = NULL) {
  empty <- data.frame(parameter = character(), estimate = numeric(), se = numeric(), rse = numeric(), fixed = logical(), stringsAsFactors = FALSE)
  if (is.null(ext_path) || !length(ext_path) || is.na(ext_path) || !file.exists(ext_path)) {
    return(list(parameters = empty, ofv = NA_real_, table_number = NA_integer_, notices = character()))
  }
  sec <- sections %||% nm_read_ext_sections(ext_path)
  if (!length(sec)) {
    return(list(parameters = empty, ofv = NA_real_, table_number = NA_integer_, notices = "EXT could not be parsed"))
  }
  table_numbers <- vapply(sec, function(x) {
    z <- attr(x, "table_number", exact = TRUE)
    if (is.null(z) || length(z) != 1L) NA_integer_ else as.integer(z)
  }, integer(1))
  if (length(sec) > 1L) {
    k <- integer()
    if (!is.na(preferred_table)) k <- which(!is.na(table_numbers) & table_numbers == as.integer(preferred_table))
    if (length(k) != 1L) {
      return(list(
        parameters = empty, ofv = NA_real_, table_number = NA_integer_,
        notices = "multiple EXT tables require explicit table metadata; final estimate withheld"
      ))
    }
  } else {
    k <- 1L
  }
  d <- sec[[k]]
  iter_i <- nm_find_col(d, c("ITERATION", "ITER", "ITERNO"))
  if (is.na(iter_i)) iter_i <- 1L
  iter <- nm_num(d[[iter_i]])
  fin <- which(!is.na(iter) & iter == -1000000000)
  se_row <- which(!is.na(iter) & iter == -1000000001)
  fixed_row <- which(!is.na(iter) & iter == -1000000006)
  notes <- character()
  if (length(fin) != 1L) notes <- c(notes, "EXT requires exactly one final-estimate sentinel row (-1000000000)")
  if (!length(se_row)) notes <- c(notes, "EXT has no standard-error sentinel row (-1000000001)")
  table_number <- table_numbers[[k]]
  if (is.na(table_number)) table_number <- k
  if (length(fin) != 1L) {
    return(list(parameters = empty, ofv = NA_real_, table_number = table_number, notices = notes))
  }
  fin <- fin[[1L]]
  se_row <- if (length(se_row) == 1L) se_row[[1L]] else NA_integer_
  ofv_i <- nm_find_col(d, c("OBJ", "OFV", "OBJECTIVE", "OBJV"))
  excluded <- c(iter_i, ofv_i)
  excluded <- excluded[!is.na(excluded)]
  pcols <- setdiff(seq_len(ncol(d)), excluded)
  pcols <- pcols[!grepl("^(FIX|SE|RSE|STD|COV|COR)$", nm_norm_name(names(d)[pcols]))]
  pcols <- pcols[!grepl("^\\.", names(d)[pcols])]
  estimates <- vapply(pcols, function(j) nm_one_num(d[[j]][fin]), numeric(1))
  ses <- vapply(pcols, function(j) if (is.na(se_row)) NA_real_ else nm_one_num(d[[j]][se_row]), numeric(1))
  nm <- names(d)[pcols]
  fixed <- rep(NA, length(pcols))
  if (length(fixed_row) == 1L) {
    fixed <- vapply(pcols, function(j) {
      v <- nm_one_num(d[[j]][fixed_row[[1L]]])
      if (is.na(v) || !v %in% c(0, 1)) NA else v == 1
    }, logical(1))
  }
  params <- data.frame(parameter = as.character(nm), estimate = estimates, se = ses, rse = ifelse(is.na(ses) | is.na(estimates) | estimates == 0, NA_real_, abs(ses / estimates) * 100), fixed = as.logical(fixed), stringsAsFactors = FALSE, check.names = FALSE)
  params$se[which(params$fixed)] <- NA_real_
  params$rse[which(params$fixed)] <- NA_real_
  extra <- function(sentinel, cols = pcols) {
    i <- which(iter == sentinel)
    if (length(i) != 1L) return(rep(NA_real_, length(cols)))
    vapply(cols, function(j) nm_one_num(d[[j]][i]), numeric(1))
  }
  params$initial <- extra(0)
  params$final_derivative <- extra(-1000000008)
  reported <- extra(-1000000003, setdiff(seq_len(ncol(d)), c(iter_i, grep("^\\.", names(d))))[1L])
  iterations <- which(is.finite(iter) & iter > -1000000000)
  keep <- if (length(iterations) > 100L) iterations[unique(round(seq(1, length(iterations), length.out = 100L)))] else iterations
  trace <- if (!is.na(ofv_i)) data.frame(Iteration = iter[keep], Objective = nm_num(d[[ofv_i]][keep])) else data.frame()
  ofv <- if (is.na(ofv_i)) NA_real_ else nm_one_num(d[[ofv_i]][fin])
  list(parameters = params, ofv = ofv, table_number = table_number, notices = notes,
    condition_reported = reported, header = attr(d, "table_header") %||% "",
    trace = trace, iteration_rows = length(iterations), final_line = d$.source_line[[fin]])
}

nm_covariance <- function(files, root, listing_lines = character(), control_lines = character()) {
  cov <- files[grepl("\\.(cov|cor|shk)$", files, ignore.case = TRUE)]
  lines <- if (length(cov)) tryCatch(nm_read_lines(cov[[1L]]), error = function(e) character()) else character()
  txt <- paste(c(listing_lines, control_lines, lines), collapse = "\n")
  if (grepl("covariance[^\n]*(?:failed|not obtained|unsuccessful)|(?:failed|not obtained|unsuccessful)[^\n]*covariance|(?<!NO)ABORT", txt, ignore.case = TRUE, perl = TRUE)) st <- "failed" else if (grepl("covariance[^\n]*(?:successful|calculated|obtained)|(?:successful|calculated|obtained)[^\n]*covariance", txt, ignore.case = TRUE, perl = TRUE)) st <- "success" else if (grepl("\\$COVARIANCE|\\$COV\\b", txt, ignore.case = TRUE, perl = TRUE)) st <- "unknown" else st <- "not_run"
  list(status = st, source = if (length(cov)) nm_relpath(cov[[1L]], root) else NA_character_, data = data.frame(), notices = character())
}

nm_termination_excerpt <- function(lines, max_lines = 80L) {
  if (!length(lines)) {
    return(character())
  }
  starts <- grep("^\\s*#TERM\\b", lines, ignore.case = TRUE, perl = TRUE)
  ends <- grep("^\\s*#TERE\\b", lines, ignore.case = TRUE, perl = TRUE)
  if (length(starts)) {
    first <- starts[[1L]]
    end_candidates <- ends[ends >= first]
    last <- if (length(end_candidates)) end_candidates[[1L]] else min(length(lines), first + max_lines - 1L)
    return(lines[seq.int(first, min(last, first + max_lines - 1L))])
  }
  hit <- grep("termination|terminated|minimization|estimation|covariance step|objective function|\\babort\\b|\\berror\\b",
    lines,
    ignore.case = TRUE, perl = TRUE
  )
  if (!length(hit)) {
    return(character())
  }
  lines[utils::head(hit, max_lines)]
}

nm_diagnostic_text <- function(files, root) {
  out <- list()
  specs <- c(shrinkage = "\\.shk$", correlation = "\\.cor$", gradients = "\\.grd$")
  for (nm in names(specs)) {
    hit <- files[grepl(specs[[nm]], files, ignore.case = TRUE, perl = TRUE)]
    if (!length(hit)) next
    hit <- sort(hit)[[1L]]
    preview <- nm_read_preview(hit)
    out[[nm]] <- list(
      source = nm_relpath(hit, root), text = preview$text,
      truncated = isTRUE(preview$truncated)
    )
  }
  out
}

nm_control_signature <- function(lines) {
  if (!length(lines)) {
    return(NA_character_)
  }
  z <- lines[grepl("^\\s*\\$(ERROR|THETA|OMEGA|SIGMA|EST|ESTIMATION|TABLE|DATA|INPUT|SUBROUTINES)", lines, ignore.case = TRUE)]
  if (!length(z)) {
    return(NA_character_)
  }
  z <- gsub("[A-Za-z]:[/\\\\][^[:space:]]+", "<path>", z)
  paste(toupper(trimws(z)), collapse = " | ")
}

nm_model_from_sources <- function(root, control, listing, ext, table_candidates, all_files, problem_info, catalog, cache) {
  notices <- character()
  stem_path <- if (length(control) && !is.na(control)) control else if (length(listing) && !is.na(listing)) listing else ext
  stem <- tools::file_path_sans_ext(basename(stem_path))
  all_files <- nm_find_related(all_files, stem)
  lb <- nm_listing_blocks(listing)
  block <- lb[[as.character(problem_info$problem)]] %||% lb$all %||% character()
  li <- nm_extract_listing(block)
  if (nzchar(problem_info$method) && nzchar(li$method) && !grepl(tolower(problem_info$method), tolower(li$method), fixed = TRUE)) notices <- c(notices, "control and listing estimation methods differ")
  method <- problem_info$method
  if (!nzchar(method)) method <- li$method
  if (!nzchar(method)) method <- "unknown"
  ext_info <- nm_ext_parameters(ext, NA_integer_)
  notices <- c(notices, ext_info$notices)
  ext_problem <- nm_stage_header(ext_info$header %||% "", "Problem")
  ext_sub <- nm_stage_header(ext_info$header %||% "", "Subproblem")
  ambiguous <- isTRUE(problem_info$multiple_est) || any(grepl("multiple EXT tables", ext_info$notices)) ||
    (!is.na(ext_problem) && ext_problem != problem_info$problem) || (!is.na(ext_sub) && ext_sub != 0L)
  if (ambiguous) {
    notices <- c(notices, "Select an identified estimation table; final parameters and OFV withheld")
    ext_info$parameters <- ext_info$parameters[0, , drop = FALSE]
    ext_info$ofv <- NA_real_
    ext_info$table_number <- NA_integer_
  }
  table_info <- nm_choose_table(table_candidates, problem_info$table_file, stem)
  notices <- c(notices, table_info$notice)
  data <- table_info$data %||% nm_empty_data()
  if (nrow(data) && !".source_line" %in% names(data)) data$.source_line <- seq_len(nrow(data))
  if (nrow(data) && ".source_file" %in% names(data)) data$.source_file <- vapply(as.character(data$.source_file), nm_relpath, character(1), root = root)
  id <- nm_pick_col(data, "ID")
  evid <- nm_pick_col(data, "EVID")
  dv <- nm_pick_col(data, "DV")
  nind <- li$nind
  nobs <- li$nobs
  if (is.na(nind) && length(id)) nind <- length(unique(id[!is.na(id)]))
  if (is.na(nobs) && length(dv)) {
    ok <- !is.na(nm_num(dv))
    if (length(evid)) ok <- ok & (is.na(nm_num(evid)) | nm_num(evid) == 0)
    nobs <- sum(ok)
  }
  if (problem_info$firstonly) notices <- c(notices, "FIRSTONLY may reduce the table to one row per subject")
  sf <- data.frame(path = character(), role = character(), stringsAsFactors = FALSE)
  add_source <- function(p, role) if (length(p) && !is.na(p) && file.exists(p)) sf <<- rbind(sf, data.frame(path = nm_relpath(p, root), role = role, stringsAsFactors = FALSE))
  add_source(control, "control")
  add_source(listing, "listing")
  add_source(ext, "ext")
  add_source(table_info$path, "table")
  table_num <- if (isTRUE(problem_info$multiple_est)) NA_integer_ else if (!is.na(ext_info$table_number)) ext_info$table_number else table_info$table_number
  ofv_value <- if (ambiguous) NA_real_ else if (!is.na(ext_info$ofv)) ext_info$ofv else li$ofv
  main_path <- if (length(control) && !is.na(control)) control else if (length(listing) && !is.na(listing)) listing else ext
  model <- list(
    id = paste0(tools::file_path_sans_ext(nm_relpath(stem_path, root)), ":p", problem_info$problem), label = problem_info$label,
    source = list(root = NULL, relative = nm_relpath(main_path, root), files = sf),
    problem = as.integer(problem_info$problem), table_number = as.integer(table_num), method = as.character(method),
    status = as.character(li$status), covariance = nm_covariance(all_files, root, block, problem_info$text), ofv = ofv_value,
    nind = if (is.na(nind)) NA_integer_ else as.integer(nind), nobs = if (is.na(nobs)) NA_integer_ else as.integer(nobs),
    reported_nind = li$nind, reported_nobs = li$nobs,
    parameters = ext_info$parameters, data = data, termination = nm_termination_excerpt(block),
    diagnostic_text = nm_diagnostic_text(all_files, root),
    notices = unique(notices[nzchar(notices)]), likelihood = NA_character_, dv_scale = NA_character_, control_signature = nm_control_signature(problem_info$text)
  )
  nm_review_model(model, problem_info, block, ext, all_files, catalog, root, cache)
}

nm_project_file_role <- function(path) {
  b <- tolower(basename(path))
  e <- tolower(tools::file_ext(path))
  if (grepl("^raw_results_.*\\.csv$", b)) {
    return("bootstrap_raw")
  }
  if (b == "bootstrap_results.csv") {
    return("bootstrap_summary")
  }
  if (b == "vpc_results.csv") {
    return("vpc_results")
  }
  if (grepl("^meta\\.(ya?ml|json)$", b)) {
    return("metadata")
  }
  if (e %in% c("mod", "ctl")) {
    return("control")
  }
  if (e %in% c("lst", "res", "out")) {
    return("listing")
  }
  if (e == "ext") {
    return("ext")
  }
  if (e %in% c("tab", "csv", "txt")) {
    return("table_or_data")
  }
  if (e %in% c("cov", "cor", "shk", "phi", "grd")) {
    return("diagnostic")
  }
  "other"
}

import_project <- function(path) {
  if (missing(path) || length(path) != 1L || !dir.exists(path)) stop("project path must be an existing directory")
  root <- normalizePath(path, winslash = "/", mustWork = TRUE)
  files <- nm_file_candidates(root)
  if (!length(files)) stop("project directory contains no files")
  rel <- vapply(files, nm_relpath, character(1), root = root)
  fi <- file.info(files)
  md5 <- vapply(seq_along(files), function(i) if (is.na(fi$size[[i]]) || fi$size[[i]] > 5 * 1024^2) NA_character_ else nm_safe_md5(files[[i]]), character(1))
  manifest <- data.frame(path = nm_normalize_slashes(rel), bytes = as.numeric(fi$size), md5 = md5, role = vapply(files, nm_project_file_role, character(1)), stringsAsFactors = FALSE, check.names = FALSE)
  notices <- character()
  # Additional analyses are parsed before ordinary model discovery so their
  # nested worker output never becomes a duplicate fitted model.
  boot_dirs <- unique(dirname(files[grepl("(^|/)(bootstrap|bootrun|bootstrap_[^/]+)(/|$)|(^|/)raw_results_.*\\.csv$|(^|/)bootstrap_results\\.csv$", nm_normalize_slashes(tolower(files)), perl = TRUE)]))
  vpc_dirs <- unique(dirname(files[grepl("(^|/)(vpc|vpcrun|npc|vpc_[^/]+)(/|$)|(^|/)vpc_results\\.csv$", nm_normalize_slashes(tolower(files)), perl = TRUE)]))
  bootstraps <- list()
  for (d in boot_dirs) {
    if (dir.exists(d)) {
      b <- tryCatch(read_bootstrap(d), error = function(e) {
        notices <<- c(notices, paste0("bootstrap read failed: ", nm_relpath(d, root), ": ", conditionMessage(e)))
        NULL
      })
      if (!is.null(b)) bootstraps[[length(bootstraps) + 1L]] <- b
    }
  }
  vpcs <- list()
  for (d in vpc_dirs) {
    if (dir.exists(d)) {
      v <- tryCatch(read_vpc(d), error = function(e) {
        notices <<- c(notices, paste0("VPC read failed: ", nm_relpath(d, root), ": ", conditionMessage(e)))
        NULL
      })
      if (!is.null(v)) vpcs[[length(vpcs) + 1L]] <- v
    }
  }
  controls <- files[grepl("\\.(mod|ctl)$", files, ignore.case = TRUE)]
  listings <- files[grepl("\\.(lst|res|out)$", files, ignore.case = TRUE)]
  exts <- files[grepl("\\.ext$", files, ignore.case = TRUE)]
  controls <- controls[!vapply(controls, nm_is_nested_psn_replica, logical(1), root = root)]
  listings <- listings[!vapply(listings, nm_is_nested_psn_replica, logical(1), root = root)]
  exts <- exts[!vapply(exts, nm_is_nested_psn_replica, logical(1), root = root)]
  model_files <- unique(c(controls, listings, exts))
  group_keys <- unique(paste(dirname(model_files), tolower(tools::file_path_sans_ext(basename(model_files))), sep = "\u001f"))
  models <- list()
  review_cache <- new.env(parent = emptyenv())
  for (g in group_keys) {
    gp <- strsplit(g, "\u001f", fixed = TRUE)[[1L]]
    base_dir <- gp[[1L]]
    stem <- gp[[2L]]
    pick_group <- function(pool, exts) {
      x <- pool[dirname(pool) == base_dir & tolower(tools::file_path_sans_ext(basename(pool))) == stem]
      if (length(exts)) x <- x[tolower(tools::file_ext(x)) %in% tolower(sub("^\\.", "", exts))]
      if (length(x)) x[[1L]] else character()
    }
    ctl <- pick_group(controls, c(".mod", ".ctl"))
    lst <- pick_group(listings, c(".lst", ".res", ".out"))
    ext <- pick_group(exts, ".ext")
    info <- if (length(ctl)) nm_parse_control_sections(ctl, stem) else list(list(problem = 1L, label = stem, method = "", ext_file = character(), table_file = character(), simulation = FALSE, firstonly = FALSE, text = character(), control_path = ctl))
    for (p in info) {
      ext_p <- character()
      if (length(p$ext_file)) ext_p <- nm_resolve_named_file(p$ext_file, exts, base_dir)
      if (!length(ext_p)) ext_p <- ext
      table_cand <- files[dirname(files) == base_dir & !grepl("\\.(mod|ctl|lst|res|out|ext|cov|cor|shk|phi|grd)$", files, ignore.case = TRUE)]
      declared <- character()
      if (length(p$table_file)) {
        for (d in p$table_file) {
          x <- nm_resolve_named_file(d, table_cand, base_dir)
          if (length(x)) declared <- c(declared, x)
        }
      }
      table_cand <- unique(c(declared, table_cand))
      table_cand <- table_cand[!grepl("(^|/)(raw_results_|bootstrap_results|vpc_results)", nm_normalize_slashes(tolower(table_cand)), perl = TRUE)]
      is_simulation <- isTRUE(p$simulation) || (length(lst) && grepl("\\bsimulation\\b|\\bsimulate\\b", paste(nm_listing_blocks(lst)[[as.character(p$problem)]] %||% character(), collapse = " "), ignore.case = TRUE) && !grepl("MINIMIZATION\\s+SUCCESSFUL", paste(nm_listing_blocks(lst)[[as.character(p$problem)]] %||% character(), collapse = " "), ignore.case = TRUE))
      if (is_simulation) {
        notices <- c(notices, paste0("simulation Problem ", p$problem, " excluded from fitted model list: ", stem))
        next
      }
      group_files <- files[dirname(files) == base_dir]
      m <- nm_model_from_sources(root, if (length(ctl)) ctl else NA_character_, if (length(lst)) lst else NA_character_, if (length(ext_p)) ext_p[[1L]] else NA_character_, table_cand, group_files, p, files, review_cache)
      models[[length(models) + 1L]] <- m
    }
  }
  if (!length(models)) notices <- c(notices, "no fitted model detected")
  if (length(models)) {
    names_only <- vapply(models, function(m) basename(m$id), "")
    duplicated_name <- duplicated(names_only) | duplicated(names_only, fromLast = TRUE)
    for (i in which(!duplicated_name)) models[[i]]$id <- names_only[[i]]
  }
  structure(list(schema_version = 1L, models = models, bootstraps = bootstraps, vpcs = vpcs, manifest = manifest, notices = unique(notices[nzchar(notices)])), class = c("nmreview_project", "list"))
}

# ---- Bootstrap reader ----------------------------------------------------

nm_read_csv_preserve <- function(path) {
  if (!file.exists(path)) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  d <- tryCatch(utils::read.csv(path, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, na.strings = character(), strip.white = TRUE), error = function(e) NULL)
  if (is.null(d)) d <- tryCatch(utils::read.table(path, header = TRUE, sep = ",", fill = TRUE, quote = "\"", comment.char = "", check.names = FALSE, stringsAsFactors = FALSE), error = function(e) data.frame(stringsAsFactors = FALSE))
  d[] <- lapply(d, function(x) {
    if (is.numeric(x)) {
      return(x)
    }
    y <- as.character(x)
    miss <- trimws(y) %in% c("", "NA", "N/A", ".")
    z <- nm_num(y)
    ok <- !is.na(z) | miss
    if (any(!miss) && all(ok)) {
      z[miss] <- NA_real_
      z
    } else {
      x
    }
  })
  d$.source_line <- seq_len(nrow(d)) + 1L
  d
}

nm_parse_metadata_file <- function(path) {
  if (is.null(path) || !length(path) || is.na(path) || !file.exists(path)) {
    return(list())
  }
  if (tolower(tools::file_ext(path)) == "json" && requireNamespace("jsonlite", quietly = TRUE)) {
    return(tryCatch(jsonlite::read_json(path, simplifyVector = TRUE), error = function(e) list()))
  }
  lines <- tryCatch(nm_read_lines(path), error = function(e) character())
  out <- list()
  for (ln in lines) {
    if (grepl("^\\s*-[A-Za-z_][A-Za-z0-9_]*=", ln)) ln <- sub("^\\s*-([^=]+)=", "\\1:", ln)
    if (!grepl("^\\s*[^#][^:]*:", ln)) next
    kv <- strsplit(sub("^\\s*", "", ln), ":", fixed = TRUE)[[1L]]
    if (length(kv) < 2L) next
    key <- trimws(kv[[1L]])
    val <- trimws(paste(kv[-1L], collapse = ":"))
    val <- sub("^['\"]|['\"]$", "", val)
    low <- tolower(val)
    out[[key]] <- if (low %in% c("true", "false")) low == "true" else if (low %in% c("null", "na", "unknown")) NA_character_ else if (!is.na(nm_one_num(val))) nm_one_num(val) else val
  }
  out
}

nm_psn_bool <- function(x) {
  if (!length(x)) {
    return(NA)
  }
  z <- tolower(trimws(as.character(x[[1L]])))
  if (z %in% c("1", "true", "yes", "on")) TRUE else if (z %in% c("0", "false", "no", "off")) FALSE else NA
}

nm_psn_metadata <- function(paths) {
  out <- list()
  for (p in paths) {
    v <- nm_parse_metadata_file(p)
    out[names(v)] <- v
  }
  out
}

# PsN stores separate named CSV tables, not one rectangular CSV dataset.
nm_bootstrap_aggregate <- function(path, parameters) {
  lines <- nm_read_lines(path)
  markers <- which(trimws(lines) %in% c(
    "means", "bias", "medians", "standard.errors",
    "standard.error.confidence.intervals", "percentile.confidence.intervals", "diagnostic.means"
  ))
  if (!length(markers)) {
    return(NULL)
  }
  sections <- list()
  for (j in seq_along(markers)) {
    a <- markers[[j]]
    b <- if (j < length(markers)) markers[[j + 1L]] - 1L else length(lines)
    if (b <= a + 1L) next
    d <- utils::read.csv(
      text = paste(lines[(a + 1L):b], collapse = "\n"),
      check.names = FALSE, stringsAsFactors = FALSE, strip.white = TRUE
    )
    names(d) <- trimws(names(d))
    sections[[trimws(lines[[a]])]] <- d
  }
  med <- sections$medians
  if (is.null(med)) {
    return(NULL)
  }
  pars <- if (nrow(parameters)) unique(parameters$parameter) else names(med)[nzchar(names(med)) & !grepl("^ofv$|^se|^shrinkage|^EI[0-9]+$", names(med), ignore.case = TRUE)]
  ci <- sections$percentile.confidence.intervals
  get <- function(d, par, row = 1L) {
    if (is.null(d) || !par %in% names(d) || !length(row) || is.na(row) || row > nrow(d)) {
      return(NA_real_)
    }
    nm_one_num(d[[par]][row])
  }
  cirow <- function(pct) if (is.null(ci)) NA_integer_ else match(pct, nm_num(gsub("%", "", ci[[1L]], fixed = TRUE)))
  rows <- lapply(pars, function(par) {
    x <- parameters[parameters$parameter == par, , drop = FALSE]
    orig <- x$estimate[x$is_original]
    data.frame(
      parameter = par, original = if (length(orig)) orig[[1L]] else NA_real_,
      median = get(med, par), lower = get(ci, par, cirow(2.5)), upper = get(ci, par, cirow(97.5)),
      n = NA_integer_, mean = get(sections$means, par), stringsAsFactors = FALSE
    )
  })
  list(
    summary = do.call(rbind, rows), sections = sections,
    method = "stored PsN percentile confidence intervals (2.5%, 97.5%); stored medians; aggregate sample count unavailable"
  )
}

nm_iteration_value <- function(x) {
  y <- trimws(as.character(x))
  num <- nm_num(y)
  if (all(is.na(num) == (y %in% c("", "NA", "N/A", "."))) && any(!is.na(num))) as.integer(round(num)) else y
}

nm_original_flag <- function(x) {
  z <- tolower(trimws(as.character(x)))
  out <- z %in% c("original", "orig", "base", "model", "reference", "ref")
  n <- nm_num(z)
  if (any(!is.na(n) & n == 0) && any(!is.na(n) & n > 0)) out <- out | (!is.na(n) & n == 0)
  out
}

nm_status_columns <- function(raw) {
  n <- nm_norm_name(names(raw))
  which(grepl("STATUS|SUCCESS|TERMIN|MINIM|COV|BOUND|FAIL|ERROR|INCLUDED|ACCEPT|REASON", n))
}

nm_option_boundary <- function(settings) {
  if (!length(settings)) {
    return(NA)
  }
  keys <- tolower(names(settings))
  i <- which(keys %in% c("exclude_boundary", "boundary", "boundary_values", "include_boundary", "skip_estimate_near_boundary"))
  if (!length(i)) {
    return(NA)
  }
  v <- settings[[i[[1L]]]]
  if (is.logical(v)) {
    return(if (keys[[i[[1L]]]] == "include_boundary") !v else v)
  }
  low <- tolower(as.character(v))
  if (low %in% c("true", "yes", "exclude", "excluded", "1")) TRUE else if (low %in% c("false", "no", "include", "included", "0")) FALSE else NA
}

nm_bootstrap_long <- function(raw, settings) {
  empty <- data.frame(iteration = character(), parameter = character(), estimate = numeric(), included = logical(), reason = character(), is_original = logical(), stringsAsFactors = FALSE)
  if (!nrow(raw)) {
    return(empty)
  }
  iter_i <- nm_find_col(raw, c("iteration", "iter", "run", "replicate", "sample", "id"))
  if (is.na(iter_i)) iter_i <- 1L
  iter <- nm_iteration_value(raw[[iter_i]])
  orig <- nm_original_flag(raw[[iter_i]])
  status_i <- nm_status_columns(raw)
  status_txt <- if (length(status_i)) apply(as.data.frame(raw[status_i], stringsAsFactors = FALSE), 1L, function(x) paste(as.character(x), collapse = " ")) else rep("", nrow(raw))
  explicit_i <- nm_find_col(raw, c("included", "include", "accepted", "keep"))
  reason_i <- nm_find_col(raw, c("reason", "failure_reason", "exclude_reason"))
  explicit <- if (is.na(explicit_i)) {
    rep(NA, nrow(raw))
  } else {
    lo <- tolower(trimws(as.character(raw[[explicit_i]])))
    ifelse(lo %in% c("true", "yes", "1", "included", "accepted", "keep"), TRUE, ifelse(lo %in% c("false", "no", "0", "excluded", "failed", "drop"), FALSE, NA))
  }
  raw_reason <- if (is.na(reason_i)) rep("", nrow(raw)) else as.character(raw[[reason_i]])
  boundary_opt <- nm_option_boundary(settings)
  param_i <- nm_find_col(raw, c("parameter", "param", "name"))
  est_i <- nm_find_col(raw, c("estimate", "value", "theta_value"))
  out <- list()
  k <- 0L
  add_row <- function(i, p, est) {
    st <- tolower(status_txt[[i]])
    rs <- trimws(raw_reason[[i]] %||% "")
    inc <- explicit[[i]]
    if (grepl("fail|error|unsuccess|not[ _-]?conver|terminated|abort", st, perl = TRUE)) {
      inc <- FALSE
      rs <- paste(c(rs, "status indicates failure"), collapse = "; ")
    }
    # Apply only recorded exclusion policies; a covariance step not run is
    # distinct from an attempted, unsuccessful covariance step.
    unknown <- FALSE
    flag <- function(cands) {
      j <- nm_find_col(raw, cands)
      if (is.na(j)) NA_real_ else nm_one_num(raw[[j]][[i]])
    }
    apply_policy <- function(trigger, key, label) {
      if (!isTRUE(trigger)) {
        return(invisible(NULL))
      }
      opt <- nm_psn_bool(settings[[key]])
      rs <<- paste(c(rs, label), collapse = "; ")
      if (isTRUE(opt)) {
        inc <<- FALSE
      } else if (is.na(opt) && is.na(explicit[[i]])) unknown <<- TRUE
    }
    apply_policy(
      flag("minimization_successful") == 0,
      "skip_minimization_terminated", "minimization unsuccessful"
    )
    covrun <- flag("covariance_step_run")
    apply_policy(
      flag("covariance_step_successful") == 0 && !identical(covrun, 0),
      "skip_covariance_step_terminated", "covariance step unsuccessful"
    )
    if (identical(covrun, 0)) rs <- paste(c(rs, "covariance step not run"), collapse = "; ")
    apply_policy(
      flag("covariance_step_warnings") != 0,
      "skip_with_covstep_warnings", "covariance step warning"
    )
    boundary <- isTRUE(flag(c("estimate_near_boundary", "boundary")) != 0) || grepl("bound|boundary", st)
    if (boundary) {
      if (isTRUE(boundary_opt)) {
        inc <- FALSE
      } else if (is.na(boundary_opt) && is.na(explicit[[i]])) unknown <- TRUE
      rs <- paste(c(rs, if (isTRUE(boundary_opt)) "boundary excluded by option" else if (is.na(boundary_opt)) "boundary status unknown" else "boundary retained by option"), collapse = "; ")
    }
    if (is.na(inc) && !unknown) inc <- TRUE
    if (unknown && !identical(inc, FALSE)) {
      inc <- NA
      rs <- paste(c(rs, "inclusion policy unknown"), collapse = "; ")
    }
    if (isTRUE(orig[[i]])) {
      inc <- FALSE
      rs <- paste(c(rs[nzchar(rs)], "original model excluded from bootstrap replicates"), collapse = "; ")
    }
    if (is.na(est) && !isTRUE(orig[[i]])) {
      inc <- FALSE
      rs <- paste(c(rs, "missing estimate"), collapse = "; ")
    }
    if (!nzchar(rs)) rs <- if (is.na(inc)) "inclusion status unknown" else ""
    k <<- k + 1L
    out[[k]] <<- data.frame(iteration = iter[[i]], parameter = p, estimate = as.numeric(est), included = as.logical(inc), reason = rs, is_original = as.logical(orig[[i]]), stringsAsFactors = FALSE)
  }
  if (!is.na(param_i) && !is.na(est_i)) {
    for (i in seq_len(nrow(raw))) add_row(i, as.character(raw[[param_i]][[i]]), nm_one_num(raw[[est_i]][[i]]))
  } else {
    find_many <- function(cands) which(nm_norm_name(names(raw)) %in% nm_norm_name(cands))
    excluded <- unique(c(iter_i, status_i, explicit_i, reason_i, find_many(c("model", "problem", "subproblem", "covariance_step_run", "minimization_successful", "covariance_step_successful", "covariance_step_warnings", "estimate_near_boundary", "rounding_errors", "zero_gradients", "final_zero_gradients", "hessian_reset", "s_matrix_singular", "significant_digits", "condition_number", "est_methods", "model_run_time", "subprob_est_time", "subprob_cov_time", "ofv", "source_line"))))
    excluded <- excluded[!is.na(excluded)]
    pcols <- setdiff(seq_len(ncol(raw)), excluded)
    pcols <- pcols[!grepl("^SE|^SHRINKAGE|^EI[0-9]+$", nm_norm_name(names(raw)[pcols]))]
    for (i in seq_len(nrow(raw))) for (j in pcols) add_row(i, names(raw)[[j]], nm_one_num(raw[[j]][[i]]))
  }
  if (!length(out)) empty else do.call(rbind, out)
}

nm_bootstrap_summary <- function(parameters, aggregate = data.frame()) {
  if (nrow(aggregate)) {
    n <- nm_find_col(aggregate, c("parameter", "param", "name"))
    if (!is.na(n)) {
      pick <- function(cands) {
        i <- nm_find_col(aggregate, cands)
        if (is.na(i)) rep(NA_real_, nrow(aggregate)) else nm_num(aggregate[[i]])
      }
      return(data.frame(parameter = as.character(aggregate[[n]]), original = pick(c("original", "orig", "base")), median = pick(c("median", "50", "p50")), lower = pick(c("lower", "2.5", "p2.5", "ci_lower")), upper = pick(c("upper", "97.5", "p97.5", "ci_upper")), n = as.integer(pick(c("n", "n_included", "number"))), stringsAsFactors = FALSE, check.names = FALSE))
    }
  }
  if (!nrow(parameters)) {
    return(data.frame(parameter = character(), original = numeric(), median = numeric(), lower = numeric(), upper = numeric(), n = integer(), stringsAsFactors = FALSE))
  }
  pars <- unique(as.character(parameters$parameter))
  ans <- vector("list", length(pars))
  for (i in seq_along(pars)) {
    p <- pars[[i]]
    d <- parameters[parameters$parameter == p, , drop = FALSE]
    o <- d$is_original & !is.na(d$estimate)
    original <- if (any(o)) d$estimate[which(o)[[1L]]] else NA_real_
    v <- d$estimate[!d$is_original & d$included %in% TRUE & is.finite(d$estimate)]
    q <- if (length(v)) stats::quantile(v, probs = c(.025, .5, .975), names = FALSE, na.rm = TRUE, type = 7) else c(NA_real_, NA_real_, NA_real_)
    ans[[i]] <- data.frame(parameter = p, original = original, median = q[[2L]], lower = q[[1L]], upper = q[[3L]], n = length(v), stringsAsFactors = FALSE)
  }
  do.call(rbind, ans)
}

read_bootstrap <- function(path) {
  if (missing(path) || !length(path)) stop("bootstrap path is required")
  root <- if (dir.exists(path)) normalizePath(path, winslash = "/", mustWork = TRUE) else normalizePath(dirname(path), winslash = "/", mustWork = TRUE)
  files <- nm_file_candidates(root)
  raw_paths <- files[grepl("^raw_results_.*\\.csv$", basename(files), ignore.case = TRUE)]
  agg_paths <- files[tolower(basename(files)) == "bootstrap_results.csv"]
  meta_paths <- files[grepl("^(meta\\.(ya?ml|json)|version_and_option_info\\.txt)$", basename(files), ignore.case = TRUE)]
  if (!length(raw_paths) && !length(agg_paths)) stop("bootstrap_results.csv or raw_results_<model>.csv not found")
  raw_path <- if (length(raw_paths)) raw_paths[[1L]] else NA_character_
  aggregate <- if (length(agg_paths)) nm_read_csv_preserve(agg_paths[[1L]]) else data.frame(stringsAsFactors = FALSE)
  raw <- if (!is.na(raw_path)) nm_read_csv_preserve(raw_path) else data.frame(stringsAsFactors = FALSE)
  settings <- nm_psn_metadata(meta_paths)
  params <- nm_bootstrap_long(raw, settings)
  stored <- if (length(agg_paths)) nm_bootstrap_aggregate(agg_paths[[1L]], params) else NULL
  summary <- if (!is.null(stored)) stored$summary else nm_bootstrap_summary(params, aggregate)
  if (!is.null(stored)) {
    settings$summary_method <- stored$method
    settings$confidence_interval <- 95
    settings$stored_sections <- stored$sections
  }
  notes <- character()
  if (!length(agg_paths)) {
    settings$summary_method <- "recomputed R quantile type7"
    notes <- c(notes, "summary recomputed from raw_results; not a stored PsN interval")
  } else if (is.null(settings$summary_method)) settings$summary_method <- "stored aggregate summary"
  if (!is.null(stored)) notes <- c(notes, "stored aggregate retained separately from raw-fit inclusion; stored n unavailable")
  policy_keys <- c("skip_minimization_terminated", "skip_covariance_step_terminated", "skip_estimate_near_boundary", "skip_with_covstep_warnings")
  if (any(!policy_keys %in% names(settings))) notes <- c(notes, "one or more bootstrap exclusion controls unavailable; defaults not inferred")
  model_id <- if (length(settings$model)) as.character(settings$model) else if (!is.na(raw_path)) sub("^raw_results_", "", tools::file_path_sans_ext(basename(raw_path)), ignore.case = TRUE) else "unknown"
  if (!length(raw_paths)) notes <- c(notes, "raw_results replicates unavailable; aggregate only")
  if (nrow(raw) && any(params$is_original)) notes <- c(notes, "original model row excluded from bootstrap replicates")
  list(id = paste0("bootstrap:", model_id, ":", basename(root)), model = model_id, source = list(root = NULL, relative = basename(root), files = unique(nm_normalize_slashes(basename(c(raw_path, agg_paths, meta_paths))))), raw = raw, parameters = params, summary = summary, settings = settings, notices = unique(notes[nzchar(notes)]))
}

# ---- VPC reader ----------------------------------------------------------

nm_vpc_header_like <- function(line) {
  grepl("median\\.idv", line, ignore.case = TRUE) ||
    (grepl("percentile|quantile", line, ignore.case = TRUE) &&
      grepl("observed|obs", line, ignore.case = TRUE))
}

nm_read_vpc_sections <- function(path) {
  lines <- tryCatch(nm_read_lines(path), error = function(e) character())
  if (!length(lines)) {
    return(list())
  }
  hdr <- which(vapply(lines, nm_vpc_header_like, logical(1)))
  if (!length(hdr)) {
    d <- nm_read_csv_preserve(path)
    return(if (nrow(d)) list(d) else list())
  }
  ends <- c(hdr[-1L] - 1L, length(lines))
  out <- list()
  for (i in seq_along(hdr)) {
    h <- hdr[[i]]
    end <- ends[[i]]
    breaks <- which(!nzchar(trimws(lines[h:end])) |
      (grepl(",", lines[[h]], fixed = TRUE) & !grepl(",", lines[h:end], fixed = TRUE)))
    if (length(breaks)) end <- h + breaks[[1L]] - 2L
    block <- lines[h:end]
    d <- if (grepl(",", block[[1L]], fixed = TRUE)) {
      td <- tryCatch(utils::read.csv(text = paste(block, collapse = "\n"), check.names = FALSE, stringsAsFactors = FALSE, comment.char = ""), error = function(e) NULL)
      if (is.null(td)) {
        NULL
      } else {
        names(td) <- trimws(names(td))
        td[] <- lapply(td, nm_numericish)
        td$.source_line <- seq_len(nrow(td)) + h
        td$.source_file <- path
        pre <- lines[seq_len(max(1L, h - 1L))]
        pre <- pre[grepl("VPC results strata", pre, ignore.case = TRUE)]
        td$.stratum <- if (length(pre)) sub("^.*VPC results strata[^=]*=\\s*", "", pre[[length(pre)]], ignore.case = TRUE) else NA_character_
        td
      }
    } else {
      nm_parse_table_section(block, h, path)
    }
    if (!is.null(d) && nrow(d)) out[[length(out) + 1L]] <- d
  }
  out
}

nm_vpc_standardize <- function(sections) {
  empty <- data.frame(stratum = character(), lower = numeric(), upper = numeric(), x = numeric(), percentile = numeric(), observed = numeric(), simulated = numeric(), ci_lower = numeric(), ci_upper = numeric(), n = integer(), .source_line = integer(), .source_file = character(), stringsAsFactors = FALSE)
  if (!length(sections)) {
    return(empty)
  }
  norm_col <- function(d, cands) {
    i <- nm_find_col(d, cands)
    if (is.na(i)) rep(NA_real_, nrow(d)) else nm_num(d[[i]])
  }
  rows <- list()
  k <- 0L
  for (d in sections) {
    si <- nm_find_col(d, c("stratum", ".stratum", "stratification", "strat", "dvid", "dose"))
    strata <- if (is.na(si)) rep(NA_character_, nrow(d)) else as.character(d[[si]])
    lower <- norm_col(d, c("lower", "bin_lower", "bin_min", "xmin", grep("^<[^=]", names(d), value = TRUE)))
    upper <- norm_col(d, c("upper", "bin_upper", "bin_max", "xmax", "<="))
    x <- norm_col(d, c("x", "bin", "bin_mid", "mid", "median.idv"))
    n <- norm_col(d, c("n", "nobs", "n_observed", "observations", "no. of obs"))
    pct <- norm_col(d, c("percentile", "quantile", "q", "perc"))
    obs <- norm_col(d, c("observed", "obs", "obs_median", "obs_50", "observed_median"))
    sim <- norm_col(d, c("simulated", "sim", "sim_median", "sim_50", "pred_median"))
    clo <- norm_col(d, c("ci_lower", "lower_ci", "sim_lower", "sim_lo", "sim_2.5", "sim_025"))
    chi <- norm_col(d, c("ci_upper", "upper_ci", "sim_upper", "sim_hi", "sim_97.5", "sim_975"))
    wide <- grep("%[[:space:]]*real$", tolower(names(d)))
    wide_pct <- if (length(wide)) as.numeric(sub("%.*$", "", names(d)[wide])) else numeric()
    add <- function(i, pp, oo, ss, ll, uu) {
      k <<- k + 1L
      rows[[k]] <<- data.frame(stratum = strata[[i]], lower = lower[[i]], upper = upper[[i]], x = x[[i]], percentile = pp, observed = oo, simulated = ss, ci_lower = ll, ci_upper = uu, n = if (is.na(n[[i]])) NA_integer_ else as.integer(round(n[[i]])), .source_line = if (".source_line" %in% names(d)) d$.source_line[[i]] else NA_integer_, .source_file = if (".source_file" %in% names(d)) as.character(d$.source_file[[i]]) else NA_character_, stringsAsFactors = FALSE)
    }
    if (length(wide)) {
      # PsN wide output has '< TIME', '<=', 'median.idv', 'no. of obs' and
      # triples such as '10% real', '10% sim', '95%CI for 10% from/to'.
      for (i in seq_len(nrow(d))) {
        for (pp in wide_pct) {
          real_i <- which(grepl(paste0("^", pp, "%[[:space:]]*real$"), tolower(names(d))))
          sim_i <- which(grepl(paste0("^", pp, "%[[:space:]]*sim$"), tolower(names(d))))
          lo_i <- which(grepl(paste0("^[0-9.]+%ci for ", pp, "% from$"), tolower(names(d))))
          hi_i <- which(grepl(paste0("^[0-9.]+%ci for ", pp, "% to$"), tolower(names(d))))
          add(i, pp, if (length(real_i)) nm_one_num(d[[real_i[[1L]]]][i]) else NA_real_, if (length(sim_i)) nm_one_num(d[[sim_i[[1L]]]][i]) else NA_real_, if (length(lo_i)) nm_one_num(d[[lo_i[[1L]]]][i]) else NA_real_, if (length(hi_i)) nm_one_num(d[[hi_i[[1L]]]][i]) else NA_real_)
        }
      }
    } else {
      for (i in seq_len(nrow(d))) add(i, pct[[i]], obs[[i]], sim[[i]], clo[[i]], chi[[i]])
    }
  }
  if (!length(rows)) empty else do.call(rbind, rows)
}

nm_vpc_correction <- function(meta, files) {
  parsed <- logical()
  if (length(meta)) {
    n <- tolower(names(meta))
    i <- which(grepl("^pred.?corr$|^prediction.?correction$|^corrected$", n))
    if (length(i)) {
      v <- meta[[i[[1L]]]]
      low <- tolower(trimws(as.character(v)))
      parsed <- c(parsed, if (low %in% c("true", "yes", "on", "1")) TRUE else if (low %in% c("false", "no", "off", "0")) FALSE else NA)
    }
  }
  all_lines <- unlist(lapply(files, function(p) if (file.exists(p)) tryCatch(nm_read_lines(p), error = function(e) character()) else character()), use.names = FALSE)
  h <- grep("prediction[ -]?correction", all_lines, ignore.case = TRUE, value = TRUE)
  if (length(h)) {
    for (v in h) {
      tail <- trimws(sub("^.*prediction[ -]?correction", "", v, ignore.case = TRUE))
      low <- tolower(tail)
      parsed <- c(parsed, if (!nzchar(low) || grepl("^[=:,;[:space:]\"]*(true|yes|on|1)([^0-9]|$)", low, perl = TRUE)) TRUE else if (grepl("^[=:,;[:space:]\"]*(false|no|off|0)([^0-9]|$)", low, perl = TRUE)) FALSE else NA)
    }
  }
  if (!length(parsed) || all(is.na(parsed))) {
    return(list(value = NA, notice = if (length(h)) "prediction-correction marker could not be interpreted" else "prediction-correction source unavailable"))
  }
  if (any(parsed %in% TRUE) && any(parsed %in% FALSE)) {
    return(list(value = NA, notice = "metadata and VPC output prediction-correction markers conflict"))
  }
  list(value = parsed[which(!is.na(parsed))[[1L]]], notice = character())
}

read_vpc <- function(path) {
  if (missing(path) || !length(path)) stop("VPC path is required")
  root <- if (dir.exists(path)) normalizePath(path, winslash = "/", mustWork = TRUE) else normalizePath(dirname(path), winslash = "/", mustWork = TRUE)
  files <- nm_file_candidates(root)
  result_paths <- files[tolower(basename(files)) == "vpc_results.csv"]
  if (!length(result_paths) && !dir.exists(path) && file.exists(path)) result_paths <- path
  if (!length(result_paths)) stop("vpc_results.csv not found")
  meta_paths <- files[grepl("^(meta\\.(ya?ml|json)|version_and_option_info\\.txt)$", basename(files), ignore.case = TRUE)]
  meta <- nm_psn_metadata(meta_paths)
  runlines <- nm_read_lines(result_paths[[1L]])
  ri <- grep("^VPC run info", runlines)
  if (length(ri) && length(runlines) >= ri[[1L]] + 2L) {
    rd <- utils::read.csv(text = paste(runlines[ri[[1L]] + 1:2], collapse = "\n"), check.names = FALSE)
    names(rd) <- trimws(names(rd))
    meta$run_info <- as.list(rd[1L, , drop = FALSE])
  }
  sections <- nm_read_vpc_sections(result_paths[[1L]])
  bins <- nm_vpc_standardize(sections)
  if (nrow(bins) && ".source_file" %in% names(bins)) bins$.source_file <- vapply(as.character(bins$.source_file), nm_relpath, character(1), root = root)
  corr <- nm_vpc_correction(meta, c(result_paths, meta_paths))
  obs_paths <- files[grepl("^vpctab[0-9]*(\\.(csv|tab|txt))?$", basename(files), ignore.case = TRUE)]
  obs <- NULL
  if (length(obs_paths)) {
    first <- readLines(obs_paths[[1L]], n = 1L, warn = FALSE)
    od <- if (grepl(",", first, fixed = TRUE)) list(nm_read_csv_preserve(obs_paths[[1L]])) else nm_read_table_file(obs_paths[[1L]])
    if (length(od)) obs <- od[[1L]]
    if (is.null(obs)) obs <- nm_read_csv_preserve(obs_paths[[1L]])
    if (is.data.frame(obs) && ".source_file" %in% names(obs)) obs$.source_file <- vapply(as.character(obs$.source_file), nm_relpath, character(1), root = root)
  }
  model_id <- if (length(meta$model)) as.character(meta$model) else sub("^vpc_results_?", "", tools::file_path_sans_ext(basename(root)), ignore.case = TRUE)
  notes <- corr$notice
  if (!nrow(bins)) notes <- c(notes, "vpc_results.csv yielded no bin summary")
  list(id = paste0("vpc:", model_id, ":", basename(root)), model = model_id, source = list(root = NULL, relative = basename(root), files = nm_normalize_slashes(basename(c(result_paths, meta_paths, obs_paths)))), bins = bins, observations = obs, corrected = corr$value, settings = meta, notices = unique(notes[nzchar(notes)]))
}

# Return rows that can be treated as observations by the UI.  The original
# data remain untouched in model$data; this helper only provides a stable view
# and key for point linking.
observation_data <- function(model) {
  d <- model$data
  if (is.null(d) || !is.data.frame(d)) {
    return(data.frame(.key = character(), .source_line = integer(), stringsAsFactors = FALSE))
  }
  keep <- rep(TRUE, nrow(d))
  ei <- nm_find_col(d, "EVID")
  mi <- nm_find_col(d, "MDV")
  bi <- nm_find_col(d, c("BLQ", "BQL"))
  if (!is.na(ei)) {
    v <- nm_num(d[[ei]])
    keep <- keep & !is.na(v) & v == 0
  }
  if (!is.na(mi)) {
    v <- nm_num(d[[mi]])
    keep <- keep & !is.na(v) & v == 0
  }
  if (!is.na(bi)) {
    v <- d[[bi]]
    lo <- tolower(as.character(v))
    keep <- keep & !(lo %in% c("1", "true", "yes", "blq", "bql"))
  }
  di <- nm_find_col(d, "DV")
  if (!is.na(di)) {
    v <- nm_num(d[[di]])
    keep <- keep & !is.na(v) & is.finite(v)
  }
  out <- d[keep, , drop = FALSE]
  out$.row_index <- which(keep)
  out$.key <- nm_observation_keys(out)$key
  out
}

nm_observation_keys <- function(d) {
  if (is.null(d) || !nrow(d)) {
    return(list(key = character(), columns = character(), ambiguous = FALSE, reason = "empty"))
  }
  rn <- nm_norm_name(names(d))
  find <- function(x) {
    i <- match(nm_norm_name(x), rn, nomatch = 0L)
    if (any(i > 0L)) names(d)[i[which(i > 0L)[[1L]]]] else NA_character_
  }
  rowc <- find(c("ROW", ".row", "ROW_ID", "ROWID", "IDROW", ".key"))
  key_cols <- character()
  if (!is.na(rowc)) {
    key_cols <- rowc
  } else {
    options <- list(c("ID", "TIME", "DVID", "OCC", "EVID", "DV"), c("ID", "TIME", "DVID", "EVID", "DV"), c("ID", "TIME", "DVID", "DV"), c("ID", "TIME", "EVID", "DV"), c("ID", "TIME", "DV"))
    for (op in options) {
      cols <- vapply(op, find, character(1))
      if (all(!is.na(cols))) {
        key_cols <- cols
        break
      }
    }
  }
  if (!length(key_cols)) {
    return(list(key = rep(NA_character_, nrow(d)), columns = character(), ambiguous = TRUE, reason = "no verified key"))
  }
  vals <- lapply(key_cols, function(nm) {
    x <- d[[nm]]
    if (is.numeric(x)) format(x, digits = 16, trim = TRUE, scientific = TRUE) else trimws(as.character(x))
  })
  key <- do.call(paste, c(vals, sep = "|"))
  bad <- Reduce(`|`, lapply(vals, function(x) is.na(x) | !nzchar(x)))
  key[bad] <- NA_character_
  dup <- anyDuplicated(key[!is.na(key)]) > 0L
  list(key = key, columns = key_cols, ambiguous = dup || any(is.na(key)), reason = if (dup) "duplicate key" else if (any(is.na(key))) "missing key" else "")
}
