# Independently written miniature PsN layouts; no patient or upstream data.
roots <- c(getwd(), dirname(getwd()), file.path(getwd(), "..", ".."))
root <- roots[file.exists(file.path(roots, "R", "import.R"))][1L]
if (!is.na(root)) {
  source(file.path(root, "R", "import.R"))
} else {
  library(nmreview)
  for (n in c("nm_one_num", "nm_bootstrap_long", "nm_parse_metadata_file")) {
    assign(n, getFromNamespace(n, "nmreview"))
  }
}
run_psn_tests <- function() {
  td <- tempfile("psn-layout-")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  put <- function(name, lines) writeLines(lines, file.path(td, name))
  stopifnot(
    identical(nm_one_num(-1403.905186336465), -1403.905186336465),
    identical(nm_one_num("1.234D-3"), .001234)
  )
  put("version_and_option_info.txt", c(
    "Command:",
    "/bin/vpc -samples=1 -predcorr=0 -rplots=-1", "-predcorr=0",
    "-confidence_interval=90", "-idv=TAD", "-dv=CONC"
  ))
  put("vpc_results.csv", c(
    "VPC run info",
    '\"\",\" observations\",\" simulations\",\"Independent variable\",\"Dependent variable\"',
    '\"\",4,12,TAD,CONC', "", "VPC results strata GROUP = low", "Continuous data",
    '\"\",\" < TAD\",\" <=\",\" median.idv\",\" no. of obs\",\" 2.5% real\",\" 2.5% sim\",\"90%CI for 2.5% from\",\"90%CI for 2.5% to\",\"50% real\",\"50% sim\",\"90%CI for 50% from\",\"90%CI for 50% to\"',
    '\"first interval is closed\",0,2,1,2,3,4,2,5,8,9,7,11',
    "Diagnostics VPC", '\"\",\"False pos (%)\"', '\"PI 90%\",99', "",
    "VPC results strata GROUP = high", "Continuous data",
    '\"\",\" < TAD\",\" <=\",\" median.idv\",\" no. of obs\",\"50% real\",\"50% sim\",\"90%CI for 50% from\",\"90%CI for 50% to\"',
    '\"\",0,2,1,2,18,19,17,21', "", "NPC results", "bin 1: 4 observations",
    '\"\",\"points below PI (count)\"', '\"0% PI\",99'
  ))
  put("vpctab", c("ID,DV,TAD", "1,8,1", "2,18,1"))
  v <- read_vpc(td)
  stopifnot(
    nrow(v$bins) == 3L, identical(v$corrected, FALSE),
    identical(v$settings$predcorr, 0), v$settings$run_info$observations == 4,
    v$settings$run_info$simulations == 12, nrow(v$observations) == 2,
    identical(v$bins$observed, c(3, 8, 18)), identical(v$bins$ci_lower, c(2, 7, 17)),
    identical(v$bins$stratum, c("low", "low", "high")),
    identical(v$bins$.source_line, c(8L, 8L, 16L))
  )
  put("version_and_option_info.txt", c(
    "-skip_minimization_terminated=1",
    "-skip_covariance_step_terminated=0", "-skip_estimate_near_boundary=1",
    "-skip_with_covstep_warnings=0"
  ))
  put("raw_results_sample.csv", c(
    "model,minimization_successful,covariance_step_run,covariance_step_successful,estimate_near_boundary,CL,V,seCL,shrinkage_eta1(%),EI1",
    "0,1,1,1,0,2,20,0.1,8,4", "1,1,0,0,0,3,30,NA,NA,NA",
    "2,0,0,0,0,4,40,NA,NA,NA", "3,1,1,0,0,5,50,NA,NA,NA",
    "4,1,0,0,1,6,60,NA,NA,NA"
  ))
  put("bootstrap_results.csv", c(
    "means", '\"\",\" CL\",\" V\",\"seCL\",\"EI1\"',
    '\"\",4,40,0,0', "medians", '\"\",\" CL\",\" V\",\"seCL\",\"EI1\"', '\"\",4.5,45,0,0',
    "percentile.confidence.intervals", '\"\",\" CL\",\" V\",\"seCL\",\"EI1\"',
    '\"2.5%\",2.5,25,0,0', '\"97.5%\",5.5,55,0,0'
  ))
  b <- read_bootstrap(td)
  stopifnot(
    nrow(b$raw) == 5L, nrow(b$parameters) == 10L,
    setequal(b$parameters$parameter, c("CL", "V")),
    all(b$parameters$included[b$parameters$iteration %in% c(1, 3)]),
    all(!b$parameters$included[b$parameters$iteration %in% c(0, 2, 4)]),
    identical(b$summary$median, c(4.5, 45)), identical(b$summary$lower, c(2.5, 25)),
    all(is.na(b$summary$n)), any(grepl("not run", b$parameters$reason))
  )
  # Unknown policies cannot be silently replaced by contemporary PsN defaults.
  u <- nm_bootstrap_long(b$raw, list())
  stopifnot(all(is.na(u$included[u$iteration %in% c(2, 3, 4)])))
  unlink(file.path(td, "raw_results_sample.csv"))
  a <- read_bootstrap(td)
  stopifnot(
    nrow(a$parameters) == 0L, nrow(a$summary) == 2L,
    all(is.na(a$summary$original)), identical(a$summary$upper, c(5.5, 55))
  )
  invisible(TRUE)
}
run_psn_tests()
cat("PsN layout tests passed\n")

# Optional acceptance against separately obtained pinned upstream examples.
# The offline test above never downloads or redistributes their data.
upstream <- Sys.getenv("NMREVIEW_UPSTREAM_FIXTURES")
if (nzchar(upstream)) {
  xp <- import_project(file.path(upstream, "xpose"))
  stopifnot(length(xp$models) == 1L)
  m <- xp$models[[1L]]
  d <- observation_data(m)
  stopifnot(
    m$nind == 74, m$nobs == 476, nrow(d) == 476,
    identical(m$ofv, -1403.905186336465), sum(abs(d$CWRES) > 3) == 4,
    d$ID[which.max(abs(d$CWRES))] == 703,
    d$.source_line[which.max(abs(d$CWRES))] == 348
  )
  psn <- file.path(upstream, "PsN", "test", "test_files", "SO", "parser_psn")
  b <- read_bootstrap(file.path(psn, "bootrun", "rundir"))
  stopifnot(
    nrow(b$raw) == 4L, nrow(b$parameters) == 20L,
    nrow(b$summary) == 5L, sum(b$parameters$included) == 15L,
    sum(b$parameters$is_original) == 5L,
    b$summary$median[b$summary$parameter == "CL"] == .00438462,
    all(is.na(b$summary$lower)), all(is.na(b$summary$upper)), all(is.na(b$summary$n))
  )
  v <- read_vpc(file.path(psn, "vpcrun", "rundir"))
  med <- v$bins[v$bins$percentile == 50, ]
  stopifnot(
    nrow(v$bins) == 18L, length(unique(v$bins$percentile)) == 9L,
    nrow(med) == 2L, sum(med$n) == 14L, nrow(v$observations) == 14L,
    identical(v$corrected, FALSE), v$settings$run_info$observations == 14,
    v$settings$run_info$simulations == 20,
    med$observed[[1L]] == 18.1, med$simulated[[1L]] == 17.0925,
    med$ci_lower[[1L]] == 14.611, med$ci_upper[[1L]] == 27.726
  )
  cat("Pinned upstream acceptance passed: xpose 1/74/476/4/703/348; bootstrap 4x5=20, 3 included fits; VPC 2x9=18, 14 observations, 20 simulations\n")
}
