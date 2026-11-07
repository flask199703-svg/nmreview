# Model comparison and observation-key handling.

nm_cmp_norm <- function(x) {
  if (is.null(x) || !length(x)) {
    return(NA_character_)
  }
  if (is.numeric(x)) {
    return(format(x[[1L]], digits = 16, trim = TRUE, scientific = TRUE))
  }
  z <- trimws(as.character(x[[1L]]))
  if (!nzchar(z) || is.na(z)) NA_character_ else z
}

nm_cmp_data <- function(model) {
  if (!is.null(model$data) && is.data.frame(model$data)) {
    d <- model$data
  } else {
    d <- data.frame(stringsAsFactors = FALSE)
  }
  if (!nrow(d)) {
    return(list(data = d, key = character(), columns = character(), ambiguous = FALSE, reason = "empty"))
  }
  # Keep the exact original row index so the UI can link a matched point back
  # to the source model$data row.  observation_data applies the same EVID/MDV/
  # BLQ rule that the UI uses for plots.
  od <- if (exists("observation_data", mode = "function")) observation_data(model) else d
  if (!nrow(od)) {
    return(list(data = od, key = character(), columns = character(), ambiguous = FALSE, reason = "empty observations"))
  }
  if (!".row_index" %in% names(od)) od$.row_index <- as.integer(rownames(od))
  if (exists("nm_observation_keys", mode = "function")) {
    kk <- nm_observation_keys(od)
  } else {
    n <- toupper(gsub("[^A-Za-z0-9]", "", names(od)))
    find <- function(x) {
      i <- match(toupper(gsub("[^A-Za-z0-9]", "", x)), n, nomatch = 0L)
      if (any(i > 0L)) names(od)[i[which(i > 0L)[[1L]]]] else NA_character_
    }
    rowc <- find("ROW")
    cols <- if (!is.na(rowc)) {
      rowc
    } else {
      op <- c("ID", "TIME", "DVID", "OCC", "EVID", "DV")
      z <- vapply(op, find, character(1))
      if (all(!is.na(z))) z else character()
    }
    if (!length(cols)) {
      kk <- list(key = rep(NA_character_, nrow(od)), columns = character(), ambiguous = TRUE, reason = "no verified key")
    } else {
      vv <- lapply(cols, function(nm) vapply(od[[nm]], nm_cmp_norm, character(1)))
      key <- do.call(paste, c(vv, sep = "|"))
      bad <- Reduce(`|`, lapply(vv, function(v) is.na(v) | !nzchar(v)))
      key[bad] <- NA_character_
      kk <- list(key = key, columns = cols, ambiguous = anyDuplicated(key[!is.na(key)]) > 0L || anyNA(key), reason = "")
    }
  }
  list(data = od, key = kk$key, columns = kk$columns, ambiguous = isTRUE(kk$ambiguous), reason = kk$reason %||% "")
}

nm_compare_metadata <- function(reference, candidate) {
  reasons <- character()
  r_method <- tolower(trimws(as.character(reference$method %||% "")))
  c_method <- tolower(trimws(as.character(candidate$method %||% "")))
  if (nzchar(r_method) && nzchar(c_method) && r_method != c_method) reasons <- c(reasons, paste0("estimation method differs (", reference$method, " vs ", candidate$method, ")"))
  # control_signature describes the full model formula and is expected to
  # differ when comparing candidate models.  Only explicit likelihood/scale
  # metadata gates an OFV comparison.
  for (field in c("likelihood", "dv_scale")) {
    rv <- reference[[field]]
    cv <- candidate[[field]]
    known_r <- length(rv) && !is.na(rv[[1L]]) && nzchar(as.character(rv[[1L]]))
    known_c <- length(cv) && !is.na(cv[[1L]]) && nzchar(as.character(cv[[1L]]))
    if (known_r && known_c && !identical(as.character(rv[[1L]]), as.character(cv[[1L]]))) reasons <- c(reasons, paste0(field, " differs"))
    if (xor(known_r, known_c)) reasons <- c(reasons, paste0(field, " is unavailable for one model"))
  }
  reasons
}

compare_models <- function(reference, candidate) {
  if (is.null(reference) || is.null(candidate)) stop("reference and candidate models are required")
  r <- nm_cmp_data(reference)
  c <- nm_cmp_data(candidate)
  matched <- data.frame(key = character(), reference_row = integer(), candidate_row = integer(), stringsAsFactors = FALSE)
  reasons <- character()
  key_columns <- character()
  if (isTRUE(r$ambiguous) || isTRUE(c$ambiguous)) {
    reasons <- c(reasons, paste0("observation matching withheld: ", if (nzchar(r$reason)) r$reason else "ambiguous reference key", "; ", if (nzchar(c$reason)) c$reason else "ambiguous candidate key"))
  } else if (!length(r$key) || !length(c$key) || !length(r$columns) || !length(c$columns)) {
    reasons <- c(reasons, "observation matching withheld: no verified unique ROW or composite key")
  } else if (!identical(nm_norm_name(r$columns), nm_norm_name(c$columns))) {
    # Different key recipes can still be valid when they produce exactly the
    # same key strings, but exposing this difference is safer than silently
    # treating same row counts as alignment.
    if (!setequal(nm_norm_name(r$columns), nm_norm_name(c$columns))) reasons <- c(reasons, "observation key definitions differ") else key_columns <- unique(c(r$columns, c$columns))
  } else {
    key_columns <- r$columns
  }
  if (!length(reasons) && length(key_columns)) {
    ri <- match(intersect(r$key, c$key), r$key)
    ci <- match(intersect(r$key, c$key), c$key)
    keys <- intersect(r$key, c$key)
    if (length(keys)) {
      # A ROW identifier is only a link hint.  Verify durable subject/time/
      # endpoint/event/DV values before using it as an observation match.
      verify_names <- c("ID", "TIME", "DVID", "OCC", "EVID", "DV")
      find_both <- function(nm) {
        ir <- nm_find_col(r$data, nm)
        ic <- nm_find_col(c$data, nm)
        if (is.na(ir) || is.na(ic)) NULL else c(ir, ic)
      }
      mismatch <- FALSE
      for (nm in verify_names) {
        ij <- find_both(nm)
        if (!is.null(ij)) {
          rv <- vapply(r$data[[ij[[1L]]]][ri], nm_cmp_norm, character(1))
          cv <- vapply(c$data[[ij[[2L]]]][ci], nm_cmp_norm, character(1))
          mismatch <- mismatch || any(rv != cv, na.rm = TRUE) || any(xor(is.na(rv), is.na(cv)))
        }
      }
      if (mismatch && (length(r$columns) == 1L || length(c$columns) == 1L)) reasons <- c(reasons, "observation key values differ; matching withheld") else matched <- data.frame(key = keys, reference_row = as.integer(r$data$.row_index[ri]), candidate_row = as.integer(c$data$.row_index[ci]), stringsAsFactors = FALSE)
    }
    if (length(setdiff(r$key, c$key)) || length(setdiff(c$key, r$key))) reasons <- c(reasons, "observation key sets differ")
    if (!nrow(matched)) reasons <- c(reasons, "no common observations after key verification")
  }
  metadata_reasons <- nm_compare_metadata(reference, candidate)
  reasons <- c(reasons, metadata_reasons)
  metadata_known <- all(vapply(c("likelihood", "dv_scale"), function(field) {
    rv <- reference[[field]]
    cv <- candidate[[field]]
    length(rv) && length(cv) && !is.na(rv[[1L]]) && !is.na(cv[[1L]]) && nzchar(as.character(rv[[1L]])) && nzchar(as.character(cv[[1L]]))
  }, logical(1)))
  comparison_warning <- if (!metadata_known && !length(metadata_reasons)) "likelihood/DV scale metadata unavailable; delta is descriptive and requires review" else ""
  rs <- tolower(as.character(reference$status %||% ""))
  cs <- tolower(as.character(candidate$status %||% ""))
  if (rs %in% c("failed", "not_run")) reasons <- c(reasons, "reference model did not finish successfully")
  if (cs %in% c("failed", "not_run")) reasons <- c(reasons, "candidate model did not finish successfully")
  if (rs %in% c("", "unknown")) reasons <- c(reasons, "reference termination status is unconfirmed")
  if (cs %in% c("", "unknown")) reasons <- c(reasons, "candidate termination status is unconfirmed")
  # `delta` is candidate - reference and is withheld whenever the data or
  # analysis definition cannot support a meaningful OFV comparison.
  comparable <- nrow(matched) > 0L && !length(reasons)
  delta <- NA_real_
  if (comparable && length(reference$ofv) && length(candidate$ofv) && !is.na(reference$ofv[[1L]]) && !is.na(candidate$ofv[[1L]])) {
    delta <- as.numeric(candidate$ofv[[1L]] - reference$ofv[[1L]])
  } else if (comparable) {
    comparable <- FALSE
    reasons <- c(reasons, "OFV unavailable for one model")
  }
  list(comparable = isTRUE(comparable), reason = if (length(reasons)) paste(unique(reasons), collapse = "; ") else "", warning = comparison_warning, metadata_verified = metadata_known, delta = delta, matched = matched, key_columns = key_columns, reference_rows = nrow(r$data), candidate_rows = nrow(c$data))
}
