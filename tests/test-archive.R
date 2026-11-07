library(nmreview)

run_archive_checks <- function() {
  directory <- tempfile("nmreview-archive-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  project <- demo_project()
  project$models[[1]]$data$nonfinite <- rep(c(NA_real_, NaN, Inf, -Inf), length.out = nrow(project$models[[1]]$data))
  project$models[[1]]$precision_check <- c(1e-20, pi, .Machine$double.xmin, .Machine$double.xmax)
  # Source references deliberately point to unavailable files. Loading must
  # depend entirely on the SQLite archive.
  project$models[[1]]$source$relative <- "unavailable/run101.mod"
  archive <- file.path(directory, "review.sqlite")
  save_archive(project, archive)
  reopened <- load_archive(archive)
  stopifnot(identical(project$models[[1]]$data, reopened$models[[1]]$data))
  stopifnot(identical(project$models[[1]]$precision_check, reopened$models[[1]]$precision_check))
  stopifnot(identical(project$bootstraps, reopened$bootstraps))
  for (i in seq_along(project$vpcs)) {
    original <- project$vpcs[[i]]
    stored <- reopened$vpcs[[i]]
    columns <- setdiff(names(original$bins), ".source_file")
    stopifnot(identical(original$bins[, columns], stored$bins[, columns]))
    stopifnot(identical(basename(original$bins$.source_file), stored$bins$.source_file))
    stopifnot(identical(original$corrected, stored$corrected))
    stopifnot(identical(original$settings, stored$settings))
    stopifnot(is.null(stored$observations))
  }
  original_fingerprint <- unname(tools::md5sum(archive))
  duplicate <- try(save_archive(project, archive), silent = TRUE)
  stopifnot(inherits(duplicate, "try-error"))
  stopifnot(identical(original_fingerprint, unname(tools::md5sum(archive))))
  locked_target <- file.path(directory, "busy.sqlite")
  dir.create(paste0(locked_target, ".lock"))
  busy <- try(save_archive(project, locked_target), silent = TRUE)
  stopifnot(inherits(busy, "try-error"), !file.exists(locked_target))

  summary_archive <- file.path(directory, "summary.sqlite")
  save_archive(project, summary_archive, include_observations = FALSE)
  summary <- load_archive(summary_archive)
  stopifnot(all(vapply(summary$models, function(model) nrow(model$data) == 0L, logical(1))))
  stopifnot(length(summary$bootstraps) == 2L, length(summary$vpcs) == 3L)
  stopifnot(all(vapply(summary$vpcs, function(vpc) nrow(vpc$bins) == 18L, logical(1))))

  damaged_counts <- file.path(directory, "bad-counts.sqlite")
  file.copy(archive, damaged_counts)
  con <- DBI::dbConnect(RSQLite::SQLite(), damaged_counts)
  DBI::dbExecute(con, "UPDATE archive_counts SET nrow = nrow + 1 WHERE rowid = (SELECT min(rowid) FROM archive_counts)")
  DBI::dbDisconnect(con)
  stopifnot(inherits(try(load_archive(damaged_counts), silent = TRUE), "try-error"))
  damaged_payload <- file.path(directory, "bad-payload.sqlite")
  file.copy(archive, damaged_payload)
  con <- DBI::dbConnect(RSQLite::SQLite(), damaged_payload)
  DBI::dbExecute(con, "UPDATE archive_payload SET payload = payload || ' ' WHERE kind = 'models'")
  DBI::dbDisconnect(con)
  stopifnot(inherits(try(load_archive(damaged_payload), silent = TRUE), "try-error"))
}

run_archive_checks()
