# Offline base-R backend checks.

run_backend_tests <- function() {
  if (!exists("import_project", mode = "function")) {
    roots <- unique(normalizePath(c(getwd(), dirname(getwd()), file.path(getwd(), "..", "..")), winslash = "/", mustWork = FALSE))
    roots <- roots[file.exists(file.path(roots, "R", "import.R"))]
    if (length(roots)) {
      root <- roots[[1L]]
      for (file in list.files(file.path(root, "R"), full.names = TRUE)) source(file, encoding = "UTF-8")
    } else if (requireNamespace("nmreview", quietly = TRUE)) library(nmreview) else stop("nmreview package or checkout source not found")
  }
  td <- tempfile("nmreview-backend-")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(c("$PROBLEM Synthetic test", "$INPUT ROW ID TIME DV EVID MDV", "$SIGMA 1 FIX", "$ESTIMATION METHOD=1 INTERACTION", "$TABLE ROW ID TIME DV EVID MDV FILE=sdtab001"), file.path(td, "run001.mod"), useBytes = TRUE)
  writeLines(c("PROBLEM NO.: 1", "TOT. NO. OF OBS RECS: 3", "TOT. NO. OF INDIVIDUALS: 2", "#METH: First Order Conditional Estimation with Interaction", "0MINIMIZATION SUCCESSFUL", "COVARIANCE STEP SUCCESSFUL", "#OBJV: -12.5"), file.path(td, "run001.lst"), useBytes = TRUE)
  writeLines(c("TABLE NO. 1", "ITERATION THETA1 OBJ", "0 1.0 -10", "-1000000000 2.0 -12.5", "-1000000001 0.2 0"), file.path(td, "run001.ext"), useBytes = TRUE)
  writeLines(c("ROW ID TIME DV EVID MDV", "1 1 1 0 1 0", "2 1 2 2 0 0", "3 2 3 3 0 0", "4 2 4 4 0 0"), file.path(td, "sdtab001"), useBytes = TRUE)
  bdir <- file.path(td, "bootstrap")
  dir.create(bdir)
  writeLines(c("iteration,parameter,estimate,minimization_successful,estimate_near_boundary", "0,THETA1,2,1,0", "1,THETA1,2.1,1,0", "2,THETA1,2.2,0,0", "3,THETA1,2.3,1,1"), file.path(bdir, "raw_results_run001.csv"), useBytes = TRUE)
  vdir <- file.path(td, "vpc")
  dir.create(vdir)
  writeLines(c("stratum,lower,upper,x,percentile,observed,simulated,ci_lower,ci_upper,n", "A,0,1,.5,50,1,1.1,1,1.2,2"), file.path(vdir, "vpc_results.csv"), useBytes = TRUE)
  p <- import_project(td)
  stopifnot(p$schema_version == 1L, length(p$models) == 1L, length(p$bootstraps) == 1L, length(p$vpcs) == 1L)
  m <- p$models[[1L]]
  stopifnot(identical(m$nind, 2L), identical(m$nobs, 3L), isTRUE(all.equal(m$ofv, -12.5)), nrow(m$parameters) == 1L, nrow(m$data) == 4L, identical(m$covariance$status, "success"))
  stopifnot(nrow(p$bootstraps[[1L]]$parameters) == 4L, any(p$bootstraps[[1L]]$parameters$is_original), all(!p$bootstraps[[1L]]$parameters$included[p$bootstraps[[1L]]$parameters$is_original]), sum(!p$bootstraps[[1L]]$parameters$is_original & p$bootstraps[[1L]]$parameters$included %in% TRUE) == 1L)
  stopifnot(nrow(p$vpcs[[1L]]$bins) == 1L, identical(p$vpcs[[1L]]$bins$observed[[1L]], 1))
  ref <- m
  cand <- m
  cand$id <- "candidate"
  cand$ofv <- -10.5
  cand$data <- cand$data[c(4, 2, 1, 3), , drop = FALSE]
  z <- compare_models(ref, cand)
  stopifnot(isTRUE(z$comparable), isTRUE(all.equal(z$delta, 2)), nrow(z$matched) == 3L, all(c("key", "reference_row", "candidate_row") %in% names(z$matched)))
  cand$data$TIME[1L] <- cand$data$TIME[1L] + 100
  z2 <- compare_models(ref, cand)
  stopifnot(!isTRUE(z2$comparable), is.na(z2$delta))
  m$data$finite_test <- c(NaN, Inf, -Inf, NA_real_)
  p$models[[1L]] <- m
  ar <- file.path(td, "review.sqlite")
  save_archive(p, ar, include_observations = TRUE)
  q <- load_archive(ar)
  stopifnot(nrow(q$models[[1L]]$data) == nrow(m$data), is.nan(q$models[[1L]]$data$finite_test[[1L]]), is.infinite(q$models[[1L]]$data$finite_test[[2L]]))
  ar2 <- file.path(td, "review-noobs.sqlite")
  save_archive(p, ar2, include_observations = FALSE)
  q2 <- load_archive(ar2)
  stopifnot(nrow(q2$models[[1L]]$data) == 0L, nrow(q2$bootstraps[[1L]]$summary) >= 1L, nrow(archive_summary(q2)) == 3L)
  invisible(TRUE)
}

if (identical(environment(), globalenv())) {
  run_backend_tests()
  cat("backend tests passed\n")
}
