# Base-R integration checks for the bundled synthetic review project.
candidates <- c(
  getwd(),
  normalizePath(file.path(getwd(), ".."),
    winslash = "/",
    mustWork = FALSE
  ),
  normalizePath(file.path(getwd(), "..", ".."),
    winslash = "/",
    mustWork = FALSE
  )
)
source_root <- candidates[file.exists(file.path(candidates, "R", "import.R"))][1]
package_available <- requireNamespace("nmreview", quietly = TRUE)

if (!is.na(source_root)) {
  setwd(source_root)
  source(file.path(source_root, "R", "import.R"), local = .GlobalEnv)
  source(file.path(source_root, "R", "demo.R"), local = .GlobalEnv)
  demo_root <- file.path(source_root, "inst", "extdata", "demo")
  ext_parameter_reader <- get("nm_ext_parameters", envir = .GlobalEnv)
} else {
  if (!package_available) stop("Run this test from the package root or its tests directory")
  project_demo <- nmreview::demo_project
  demo_root <- system.file("extdata", "demo", package = "nmreview")
  ext_parameter_reader <- getFromNamespace("nm_ext_parameters", "nmreview")
}

project <- if (!is.na(source_root)) get("demo_project", envir = .GlobalEnv)() else project_demo()
stopifnot(inherits(project, "nmreview_project"))
stopifnot(length(project$models) == 3L)
stopifnot(length(project$bootstraps) == 2L)
stopifnot(length(project$vpcs) == 3L)

# When an installed package is available, also verify its bundled offline copy
# from outside the source checkout, where system.file() must be used.
if (package_available) {
  original_wd <- getwd()
  setwd(tempdir())
  installed_project <- nmreview::demo_project()
  setwd(original_wd)
  stopifnot(length(installed_project$models) == 3L)
  stopifnot(length(installed_project$bootstraps) == 2L)
  stopifnot(length(installed_project$vpcs) == 3L)
  installed_pc <- Filter(
    function(x) isTRUE(x$corrected),
    installed_project$vpcs
  )
  installed_ordinary <- Filter(
    function(x) isFALSE(x$corrected),
    installed_project$vpcs
  )
  stopifnot(length(installed_pc) == 2L, length(installed_ordinary) == 1L)
  stopifnot(all(vapply(
    installed_pc, function(x) nrow(x$bins) == 18L,
    logical(1)
  )))
  stopifnot(all(vapply(
    installed_pc, function(x) all(x$bins$n > 0),
    logical(1)
  )))
}

model_name <- function(x) sub(":p[0-9]+$", "", x$id)
models <- setNames(project$models, vapply(project$models, model_name, character(1)))
stopifnot(all(c("run101", "run102", "run103") %in% names(models)))
stopifnot(all(vapply(models, function(x) x$nind == 24L, logical(1))))
stopifnot(all(vapply(models, function(x) x$nobs == 192L, logical(1))))
stopifnot(all(vapply(models, function(x) x$status == "success", logical(1))))
stopifnot(all(vapply(models, function(x) x$table_number == 1L, logical(1))))
stopifnot(models$run102$ofv < models$run101$ofv)
required_table_columns <- c(
  "ROW", "ID", "TIME", "DV", "PRED", "IPRED",
  "CWRES", "EVID", "MDV"
)
stopifnot(all(vapply(models, function(x) {
  all(required_table_columns %in% names(x$data))
}, logical(1))))

for (name in c("run101", "run102", "run103")) {
  listing_file <- file.path(demo_root, name, paste0(name, ".lst"))
  ext_file <- file.path(demo_root, name, paste0(name, ".ext"))
  stopifnot(nzchar(listing_file), nzchar(ext_file))
  listing_lines <- readLines(listing_file, warn = FALSE)
  ext_lines <- readLines(ext_file, warn = FALSE)
  for (marker in c("#PROB", "#TBLN", "#METH", "#TERM", "#OBJV")) {
    stopifnot(any(grepl(marker, listing_lines, fixed = TRUE)))
  }

  # Keep the NONMEM sentinel values distinct as text before numeric parsing.
  ext_rows <- lapply(ext_lines, function(line) {
    strsplit(trimws(line), "[[:space:]]+")[[1L]]
  })
  numeric_rows <- vapply(ext_rows, function(row) {
    length(row) > 0L && grepl("^-?[0-9]+$", row[[1L]])
  }, logical(1))
  ext_rows <- ext_rows[numeric_rows]
  iteration <- vapply(ext_rows, `[[`, character(1), 1L)
  sentinel_ids <- c("-1000000000", "-1000000001", "-1000000006")
  sentinel_rows <- setNames(lapply(sentinel_ids, function(id) {
    matches <- which(iteration == id)
    stopifnot(length(matches) == 1L)
    ext_rows[[matches[[1L]]]]
  }), sentinel_ids)

  header <- strsplit(trimws(ext_lines[[3L]]), "[[:space:]]+")[[1L]]
  parameter_names <- header[2L:(length(header) - 1L)]
  final_values <- as.numeric(sentinel_rows[["-1000000000"]][2L:(length(header) - 1L)])
  se_values <- as.numeric(sentinel_rows[["-1000000001"]][2L:(length(header) - 1L)])
  fixed_values <- as.numeric(sentinel_rows[["-1000000006"]][2L:(length(header) - 1L)])
  raw_parameters <- ext_parameter_reader(ext_file)$parameters
  raw_parameter_index <- match(parameter_names, raw_parameters$parameter)
  stopifnot(all(!is.na(raw_parameter_index)))
  stopifnot(isTRUE(all.equal(raw_parameters$estimate[raw_parameter_index],
    final_values,
    tolerance = 1e-12,
    check.attributes = FALSE
  )))
  expected_fixed <- fixed_values == 1
  stopifnot(identical(
    raw_parameters$fixed[raw_parameter_index],
    expected_fixed
  ))
  se_values[expected_fixed] <- NA_real_
  stopifnot(isTRUE(all.equal(raw_parameters$se[raw_parameter_index], se_values,
    tolerance = 1e-12, check.attributes = FALSE
  )))

  parameters <- models[[name]]$parameters
  parameter_index <- match(parameter_names, parameters$parameter)
  stopifnot(all(!is.na(parameter_index)))
  stopifnot(isTRUE(all.equal(parameters$estimate[parameter_index], final_values,
    tolerance = 1e-12, check.attributes = FALSE
  )))
  stopifnot(identical(parameters$fixed[parameter_index], expected_fixed))
  se_comparison <- all.equal(parameters$se[parameter_index], se_values,
    tolerance = 1e-12, check.attributes = FALSE
  )
  if (!isTRUE(se_comparison)) {
    stop(sprintf(
      "Imported SE values differ from EXT sentinel row in %s: %s",
      name, paste(se_comparison, collapse = "; ")
    ), call. = FALSE)
  }
}

obs <- function(x) x$data[x$data$EVID == 0 & x$data$MDV == 0, , drop = FALSE]
a <- obs(models$run101)
b <- obs(models$run102)
c <- obs(models$run103)
stopifnot(nrow(a) == 192L, nrow(b) == 192L, nrow(c) == 192L)
stopifnot(identical(a$ROW, b$ROW))
stopifnot(length(unique(a$ROW)) == 192L)
stopifnot(sum(models$run101$data$EVID == 1) == 24L)
stopifnot(sum(models$run102$data$EVID == 1) == 24L)

# Equal counts do not mask the 24 changed row identities in run103.
stopifnot(length(intersect(a$ROW, c$ROW)) == 168L)
stopifnot(sum(!a$ROW %in% c$ROW) == 24L)
stopifnot(sum(!c$ROW %in% a$ROW) == 24L)

# Three intentionally shifted observations have smaller absolute CWRES in run102.
outlier <- a$OUTMAG != 0
stopifnot(sum(outlier) == 3L)
stopifnot(sum(abs(a$CWRES[outlier]) > 3) >= 2L)
stopifnot(all(abs(b$CWRES[outlier]) < abs(a$CWRES[outlier])))

bootstrap_name <- function(x) x$model
bootstraps <- setNames(
  project$bootstraps,
  vapply(project$bootstraps, bootstrap_name, character(1))
)
for (name in c("run101", "run102")) {
  boot <- bootstraps[[name]]
  stopifnot(nrow(boot$raw) == 9L)
  stopifnot(sum(boot$raw$model == 0) == 1L)
  stopifnot(sum(boot$raw$model > 0) == 8L)
  stopifnot(sum(boot$raw$minimization_successful == 0, na.rm = TRUE) == 1L)
  stopifnot(sum(boot$raw$covariance_step_successful == 0, na.rm = TRUE) == 2L)
  stopifnot(sum(boot$raw$estimate_near_boundary == 1, na.rm = TRUE) == 1L)
  stopifnot(sum(boot$raw$covariance_step_warnings == 1, na.rm = TRUE) == 1L)
  stopifnot(sum(boot$raw$rounding_errors == 1, na.rm = TRUE) == 1L)
  stopifnot(identical(as.numeric(boot$settings$skip_minimization_terminated), 1))
  stopifnot(identical(as.numeric(boot$settings$skip_covariance_step_terminated), 1))
  stopifnot(identical(as.numeric(boot$settings$skip_estimate_near_boundary), 1))
  stopifnot(identical(as.numeric(boot$settings$skip_with_covstep_warnings), 0))
  stopifnot(any(boot$parameters$is_original))
  kept <- unique(as.numeric(boot$parameters$iteration[
    !boot$parameters$is_original & boot$parameters$included %in% TRUE
  ]))
  stopifnot(setequal(kept, c(1, 2, 6, 7, 8)))
  stopifnot(all(boot$summary$n == 5L))
}

vpc_name <- function(x) x$model
vpcs <- setNames(project$vpcs, vapply(project$vpcs, vpc_name, character(1)))
for (name in c("run101", "run102")) {
  vpc <- vpcs[[name]]
  stopifnot(isTRUE(vpc$corrected))
  stopifnot(identical(as.numeric(vpc$settings$predcorr), 1))
  bins <- vpc$bins
  valid <- is.finite(bins$lower) & is.finite(bins$upper) &
    is.finite(bins$percentile) & is.finite(bins$observed) &
    is.finite(bins$simulated) & is.finite(bins$ci_lower) &
    is.finite(bins$ci_upper) & is.finite(bins$n)
  bins <- bins[valid, , drop = FALSE]
  stopifnot(nrow(bins) == 18L)
  stopifnot(length(unique(bins$stratum)) == 2L)
  stopifnot(all(bins$n > 0))
  stopifnot(all(bins$upper > bins$lower))
  stopifnot(all(bins$observed > 0), all(bins$simulated > 0))
  stopifnot(all(bins$ci_lower <= bins$simulated & bins$simulated <= bins$ci_upper))
  stopifnot(all(table(bins$stratum) == 9L))
  stopifnot(all(table(bins$percentile) == 6L))
  stopifnot(setequal(unique(bins$percentile), c(10, 50, 90)))
}

# The ordinary VPC must remain explicitly uncorrected despite the shared filename.
ordinary <- vpcs[["run103"]]
stopifnot(isFALSE(ordinary$corrected))
stopifnot(isTRUE(as.numeric(ordinary$settings$predcorr) == 0))
ordinary_file <- file.path(demo_root, "run103", "vpc", "vpc_results.csv")
stopifnot(nzchar(ordinary_file), file.exists(ordinary_file))
ordinary_lines <- readLines(ordinary_file, warn = FALSE)
stopifnot(!any(grepl("Prediction correction", ordinary_lines, fixed = TRUE)))
