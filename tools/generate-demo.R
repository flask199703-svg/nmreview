# Rebuild the entirely synthetic review fixtures. This script uses base R only.
args <- commandArgs(trailingOnly = FALSE)
script_arg <- args[grepl("^--file=", args)][1]
if (is.na(script_arg)) {
  stop("Run this file with Rscript tools/generate-demo.R", call. = FALSE)
}
script_path <- normalizePath(sub("^--file=", "", script_arg), winslash = "/")
project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/")
demo_root <- file.path(project_root, "inst", "extdata", "demo")

set.seed(20261107)

dir.create(demo_root, recursive = TRUE, showWarnings = FALSE)

write_demo_lines <- function(path, lines) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
}

write_demo_table <- function(x, path, sep = " ") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(
    x, file = path, sep = sep, quote = FALSE, row.names = FALSE,
    col.names = TRUE, na = "NA", eol = "\n"
  )
}

fmt <- function(x) {
  ifelse(is.na(x), "", sprintf("%.12g", as.numeric(x)))
}

pk_concentration <- function(time, cl, v, ka, dose = 100) {
  ke <- cl / v
  ifelse(time <= 0, 0,
         dose * ka / (v * (ka - ke)) * (exp(-ke * time) - exp(-ka * time)))
}

# Shared source observations for run101/run102. Stable row keys encode subject
# and time, so they are independent of display order.
subject <- data.frame(
  ID = 1:24,
  DOSEGRP = ifelse(1:24 <= 12, 1, 2),
  SEX = ifelse((1:24) %% 2 == 0, 1, 0),
  WT = round(52 + 1.35 * (1:24) + rnorm(24, 0, 3), 1),
  ETA1 = rnorm(24, 0, 0.18),
  ETA2 = rnorm(24, 0, 0.20)
)
subject$CL <- 5.3 * exp(subject$ETA1)
subject$V <- 50 * exp(subject$ETA2)
times <- c(0.5, 1, 2, 4, 8, 12, 24, 36)

rows <- vector("list", nrow(subject) * (length(times) + 1L))
k <- 1L
for (i in seq_len(nrow(subject))) {
  s <- subject[i, ]
  rows[[k]] <- data.frame(
    ROW = s$ID * 1000, ID = s$ID, TIME = 0, DV = 0, AMT = 100,
    EVID = 1, MDV = 1, DOSEGRP = s$DOSEGRP, SEX = s$SEX, WT = s$WT,
    OUTMAG = 0, ETA1 = s$ETA1, ETA2 = s$ETA2
  )
  k <- k + 1L
  for (time in times) {
    base_pred <- pk_concentration(time, 5.45, 49.5, 1.2)
    individual_mean <- pk_concentration(time, s$CL, s$V, 1.2)
    outmag <- if (s$ID == 4 && time == 4) 1 else if (
      s$ID == 16 && time == 24
    ) -1 else if (s$ID == 20 && time == 12) 1 else 0
    dv <- individual_mean * exp(rnorm(1, mean = -0.5 * 0.12^2, sd = 0.12)) +
      rnorm(1, mean = 0, sd = 0.015)
    # These three marked points are deliberately shifted so the synthetic
    # run102 residual panel shows a visible, documented improvement.
    dv <- pmax(dv + base_pred * 0.9 * outmag, 0.01)
    rows[[k]] <- data.frame(
      ROW = s$ID * 1000 + round(time * 10), ID = s$ID, TIME = time,
      DV = dv, AMT = 0, EVID = 0, MDV = 0, DOSEGRP = s$DOSEGRP,
      SEX = s$SEX, WT = s$WT, OUTMAG = outmag, ETA1 = s$ETA1, ETA2 = s$ETA2
    )
    k <- k + 1L
  }
}
source_101_102 <- do.call(rbind, rows)
source_101_102 <- source_101_102[order(source_101_102$ID, source_101_102$TIME), ]
row.names(source_101_102) <- NULL

# run103 retains the same observation count but replaces one observation per
# subject (TIME=2) with a different row key and a newly generated TIME=3 value.
source_103 <- source_101_102
replace_at <- which(source_103$MDV == 0 & source_103$TIME == 2)
set.seed(20261108)
for (index in replace_at) {
  id <- source_103$ID[index]
  s <- subject[subject$ID == id, ]
  time <- 3
  source_103$TIME[index] <- time
  source_103$ROW[index] <- id * 1000 + round(time * 10)
  source_103$DV[index] <- pk_concentration(time, s$CL, s$V, 1.2) *
    exp(rnorm(1, mean = -0.5 * 0.12^2, sd = 0.12)) + rnorm(1, 0, 0.015)
}
source_103 <- source_103[order(source_103$ID, source_103$TIME), ]
row.names(source_103) <- NULL

model_specs <- list(
  run101 = list(
    id = 101, cl = 5.45, v = 49.5, ka = 1.20, ofv = -210.423,
    method = "First Order Conditional Estimation with Interaction",
    theta = c(5.45, 49.5, 1.20, 0.15),
    se = c(0.42, 4.10, 0.18, 0.015), fixed = c(0, 0, 0, 0),
    omega = 0.0324, omega_se = 0.010, sigma = 1, sigma_se = 1e10,
    n_theta = 4, residual_theta = 4, uses_outlier_term = FALSE
  ),
  run102 = list(
    id = 102, cl = 5.30, v = 50.0, ka = 1.20, ofv = -231.876,
    method = "First Order Conditional Estimation with Interaction",
    theta = c(5.30, 50.0, 1.20, 0.90, 0.15),
    se = c(0.38, 3.90, 0.17, 0.08, 0.015), fixed = c(0, 0, 0, 0, 0),
    omega = 0.0324, omega_se = 0.010, sigma = 1, sigma_se = 1e10,
    n_theta = 5, residual_theta = 5, uses_outlier_term = TRUE
  ),
  run103 = list(
    id = 103, cl = 5.55, v = 48.0, ka = 1.15, ofv = -202.114,
    method = "First Order Conditional Estimation with Interaction",
    theta = c(5.55, 48.0, 1.15, 0.15),
    se = c(0.45, 4.20, 0.20, 0.016), fixed = c(0, 0, 0, 0),
    omega = 0.0361, omega_se = 0.011, sigma = 1, sigma_se = 1e10,
    n_theta = 4, residual_theta = 4, uses_outlier_term = FALSE
  )
)

make_run_table <- function(source, spec) {
  pred <- pk_concentration(source$TIME, spec$cl, spec$v, spec$ka)
  individual_pred <- pk_concentration(
    source$TIME,
    spec$cl * exp(source$ETA1),
    spec$v * exp(source$ETA2),
    spec$ka
  )
  if (spec$uses_outlier_term) {
    pred <- pred * (1 + 0.9 * source$OUTMAG)
    individual_pred <- individual_pred * (1 + 0.9 * source$OUTMAG)
  }
  ipred <- individual_pred
  scale <- pmax(0.15 * pmax(ipred, 0.01), 0.02)
  cwres <- (source$DV - ipred) / scale
  event <- source$EVID != 0 | source$MDV != 0
  pred[event] <- NA_real_
  ipred[event] <- NA_real_
  cwres[event] <- NA_real_
  data.frame(
    ROW = source$ROW, ID = source$ID, TIME = source$TIME, DV = source$DV,
    PRED = pred, IPRED = ipred, CWRES = cwres,
    EVID = source$EVID, MDV = source$MDV, AMT = source$AMT, RATE = 0,
    CMT = ifelse(source$EVID == 1, 1, 2), IWRES = cwres * 0.96,
    WRES = cwres * 1.04, OUTMAG = source$OUTMAG, DOSEGRP = source$DOSEGRP,
    SEX = source$SEX, WT = source$WT, ETA1 = source$ETA1
  )
}

format_ext_row <- function(iteration, estimates, objective) {
  # Sentinel IDs differ by one; keep this token in plain decimal so they do
  # not collapse when parsed from limited-precision scientific notation.
  iteration_text <- sprintf("%.0f", as.numeric(iteration))
  values <- c(iteration_text, sprintf("% .8E", estimates),
              sprintf("% .8E", objective))
  paste(values, collapse = "  ")
}

write_mod <- function(path, spec) {
  theta_lines <- if (spec$id == 102) {
    c("$THETA (0,5.30) ; CL (L/h)",
      "$THETA (0,50.0) ; V (L)",
      "$THETA (0,1.20) ; KA (1/h)",
      "$THETA (0,0.90) ; synthetic OUTMAG correction (demo only)",
      "$THETA (0,0.15) ; proportional residual error")
  } else {
    c(sprintf("$THETA (0,%.4f) ; CL (L/h)", spec$cl),
      sprintf("$THETA (0,%.4f) ; V (L)", spec$v),
      sprintf("$THETA (0,%.4f) ; KA (1/h)", spec$ka),
      "$THETA (0,0.15) ; proportional residual error")
  }
  error_lines <- if (spec$uses_outlier_term) {
    c("IPRED = PRED*(1+THETA(4)*OUTMAG)",
      "W = THETA(5)*PRED", "Y = IPRED + W*EPS(1)")
  } else {
    c("IPRED = PRED", "W = THETA(4)*PRED", "Y = IPRED + W*EPS(1)")
  }
  lines <- c(
    "; SYNTHETIC DEMO ONLY: this control stream was not executed by NONMEM.",
    sprintf("$PROBLEM Synthetic oral PK review fixture %d", spec$id),
    "$INPUT ROW ID TIME DV AMT EVID MDV DOSEGRP SEX WT OUTMAG",
    "$DATA synthetic_data.csv IGNORE=@",
    "$SUBROUTINES ADVAN2 TRANS2",
    "$PK",
    "CL = THETA(1)*EXP(ETA(1))",
    "V = THETA(2)*EXP(ETA(2))",
    "KA = THETA(3)",
    "$ERROR", error_lines,
    theta_lines,
    "$OMEGA 0.0324",
    "$OMEGA 0.0400",
    "$SIGMA 1 FIX",
    "$ESTIMATION METHOD=1 INTERACTION MAXEVAL=9999 PRINT=5 NOABORT",
    "$COVARIANCE PRINT=E",
    sprintf(paste0("$TABLE ROW ID TIME DV PRED IPRED CWRES EVID MDV AMT RATE CMT ",
                   "IWRES WRES OUTMAG DOSEGRP SEX WT ETA(1) ONEHEADER NOPRINT ",
                   "FILE=sdtab%d"), spec$id)
  )
  write_demo_lines(path, lines)
}

write_listing <- function(path, spec, table, source_label) {
  n_obs <- sum(table$MDV == 0 & table$EVID == 0)
  n_subjects <- length(unique(table$ID))
  lines <- c(
    sprintf("SYNTHETIC DEMO LISTING -- NOT GENERATED BY NONMEM (%s)", source_label),
    "NONMEM-style output format for reader tests; no software execution occurred.",
    "PROBLEM NO.: 1",
    sprintf("NO. OF DATA RECS IN DATA SET: %d", nrow(table)),
    sprintf("TOT. NO. OF OBS RECS: %d", n_obs),
    sprintf("TOT. NO. OF INDIVIDUALS: %d", n_subjects),
    "#PROB: 1",
    "#TBLN: 1",
    paste0("#METH: ", spec$method),
    "#TERM:",
    "0MINIMIZATION SUCCESSFUL",
    "NO. OF FUNCTION EVALUATIONS USED: 118",
    "NO. OF SIG. DIGITS IN FINAL EST.: 4.2",
    sprintf("#OBJV:******************************************** % .6f **************************************************", spec$ofv),
    "COVARIANCE STEP SUCCESSFUL",
    "SYNTHETIC FIXTURE VALUES; NOT A REAL MODEL FIT."
  )
  write_demo_lines(path, lines)
}

write_ext <- function(path, spec) {
  theta_names <- paste0("THETA", seq_len(spec$n_theta))
  parameter_names <- c(theta_names, "OMEGA(1,1)", "OMEGA(2,2)", "SIGMA(1,1)", "OBJ")
  estimates <- c(spec$theta, spec$omega, 0.04, spec$sigma)
  ses <- c(spec$se, spec$omega_se, 0.012, spec$sigma_se)
  fixed <- c(spec$fixed, 0, 0, 1)
  lines <- c(
    "SYNTHETIC DEMO OUTPUT -- NOT GENERATED BY NONMEM",
    sprintf("TABLE NO.     1: %s: Goal Function=MINIMUM VALUE OF OBJECTIVE FUNCTION: Problem=1 Subproblem=0", spec$method),
    paste(c("ITERATION", parameter_names), collapse = "  "),
    format_ext_row(0, estimates, spec$ofv + 8),
    format_ext_row(1, estimates * c(rep(1.03, length(spec$theta)), 1, 1, 1), spec$ofv + 2),
    format_ext_row(-1000000000, estimates, spec$ofv),
    format_ext_row(-1000000001, ses, 0),
    format_ext_row(-1000000006, fixed, 0)
  )
  write_demo_lines(path, lines)
}

write_bootstrap <- function(run_dir, spec) {
  boot_dir <- file.path(run_dir, "bootstrap")
  dir.create(boot_dir, recursive = TRUE, showWarnings = FALSE)
  set.seed(20261107 + spec$id)
  n_rep <- 8L
  success <- c(1, 1, 0, 1, 1, 1, 1, 1)
  cov_success <- c(1, 1, 0, 0, 1, 1, 1, 1)
  boundary <- c(0, 0, 0, 0, 1, 0, 0, 0)
  cov_warnings <- c(0, 0, 0, 0, 0, 0, 1, 0)
  rounding <- c(0, 0, 0, 0, 0, 1, 0, 0)
  cl <- spec$cl * exp(rnorm(n_rep, 0, 0.055))
  v <- spec$v * exp(rnorm(n_rep, 0, 0.065))
  ka <- spec$ka * exp(rnorm(n_rep, 0, 0.06))
  omega <- spec$omega * exp(rnorm(n_rep, 0, 0.12))
  sigma <- rep(1, n_rep)
  base <- data.frame(
    model = 0, problem = 1, subproblem = 1, covariance_step_run = 1,
    minimization_successful = 1, covariance_step_successful = 1,
    covariance_step_warnings = 0, estimate_near_boundary = 0,
    rounding_errors = 0, zero_gradients = 0, final_zero_gradients = 0,
    hessian_reset = 0, s_matrix_singular = 0, significant_digits = 4.2,
    condition_number = 12.4, est_methods = "FOCEI",
    model_run_time = "00:00:02", subprob_est_time = 2.1,
    subprob_cov_time = 0.7, ofv = spec$ofv, CL = spec$cl, V = spec$v,
    KA = spec$ka, `OMEGA(1,1)` = spec$omega, `OMEGA(2,2)` = 0.04,
    `SIGMA(1,1)` = 1, seCL = spec$se[1], seV = spec$se[2],
    seKA = spec$se[3], seOMEGA11 = spec$omega_se, seSIGMA11 = 1e10,
    check.names = FALSE
  )
  reps <- data.frame(
    model = seq_len(n_rep), problem = 1, subproblem = 1,
    covariance_step_run = 1, minimization_successful = success,
    covariance_step_successful = cov_success, covariance_step_warnings = cov_warnings,
    estimate_near_boundary = boundary, rounding_errors = rounding,
    zero_gradients = 0, final_zero_gradients = 0, hessian_reset = 0,
    s_matrix_singular = 0, significant_digits = c(4.3, 4.1, NA, 4.0, 3.8, 4.2, 4.1, 4.4),
    condition_number = c(11.3, 15.8, NA, 22.1, 18.6, 13.2, 19.4, 12.7),
    est_methods = "FOCEI", model_run_time = "00:00:02",
    subprob_est_time = 2, subprob_cov_time = 0.7,
    ofv = spec$ofv + c(1.4, 3.1, NA, 2.8, 4.2, 2.0, 3.7, 1.9),
    CL = cl, V = v, KA = ka, `OMEGA(1,1)` = omega,
    `OMEGA(2,2)` = 0.04 * exp(rnorm(n_rep, 0, 0.08)), `SIGMA(1,1)` = sigma,
    seCL = cl * 0.08, seV = v * 0.08, seKA = ka * 0.1,
    seOMEGA11 = omega * 0.2, seSIGMA11 = 1e10,
    check.names = FALSE
  )
  raw_results <- rbind(base, reps)
  utils::write.csv(
    raw_results,
    file = file.path(boot_dir, sprintf("raw_results_run%d.csv", spec$id)),
    row.names = FALSE, na = "NA", quote = TRUE
  )

  options_text <- c(
    "tool: bootstrap",
    sprintf("model: run%d", spec$id),
    "tool_version: synthetic fixture (no PsN execution)",
    "tool_options:",
    "  samples: 8",
    "  seed: 20261107",
    "  threads: 1",
    "  skip_minimization_terminated: 1",
    "  skip_covariance_step_terminated: 1",
    "  skip_estimate_near_boundary: 1",
    "  skip_with_covstep_warnings: 0"
  )
  write_demo_lines(file.path(boot_dir, "meta.yaml"), options_text)
  write_demo_lines(
    file.path(boot_dir, "command.txt"),
    sprintf(paste0("bootstrap run%d.mod -samples=8 -seed=20261107 -threads=1 ",
                   "-skip_minimization_terminated -skip_covariance_step_terminated ",
                   "-skip_estimate_near_boundary -no-skip_with_covstep_warnings"), spec$id)
  )
  write_demo_lines(
    file.path(boot_dir, "version_and_option_info.txt"),
    c("SYNTHETIC DEMO METADATA -- NO PsN EXECUTION",
      "samples: 8", "seed: 20261107", "threads: 1",
      "skip_minimization_terminated: 1",
      "skip_covariance_step_terminated: 1",
      "skip_estimate_near_boundary: 1",
      "skip_with_covstep_warnings: 0")
  )
}

write_vpc <- function(vpc_dir, run_name, table, prediction_corrected, seed) {
  dir.create(vpc_dir, recursive = TRUE, showWarnings = FALSE)
  obs <- table[table$MDV == 0 & table$EVID == 0, ]
  strata <- sort(unique(obs$DOSEGRP))
  bin_defs <- list(
    list(label = "Bin 1", low = min(obs$TIME), high = 4),
    list(label = "Bin 2", low = 4, high = 12),
    list(label = "Bin 3", low = 12, high = max(obs$TIME))
  )
  probs <- c(0.10, 0.50, 0.90)
  nsim <- 100L
  set.seed(seed)
  lines <- c(
    "VPC run info",
    '"","Date","observations","simulations","Modelfile","Simulation model","Independent variable","Dependent variable","PsN version","NONMEM version"',
    sprintf('"","2026-11-07","%d","%d","%s.mod","synthetic","TIME","DV","not run","not run"',
            nrow(obs), nsim, run_name)
  )
  if (prediction_corrected) {
    lines <- c(lines, "Additional feature", '"","Prediction correction"')
  }

  for (stratum in strata) {
    stratum_obs <- obs[obs$DOSEGRP == stratum, ]
    lines <- c(
      lines,
      sprintf("VPC results strata DOSEGRP = %s", fmt(stratum)),
      "Continuous data",
      sprintf("%d observations out of %d", nrow(stratum_obs), nrow(obs)),
      '"","< TIME","<=","median.idv","no. of obs","10% real","10% sim","95%CI for 10% from","95%CI for 10% to","50% real","50% sim","95%CI for 50% from","95%CI for 50% to","90% real","90% sim","95%CI for 90% from","95%CI for 90% to"'
    )
    for (bin_index in seq_along(bin_defs)) {
      bin <- bin_defs[[bin_index]]
      in_bin <- if (bin_index == 1L) {
        stratum_obs$TIME <= bin$high
      } else {
        stratum_obs$TIME > bin$low & stratum_obs$TIME <= bin$high
      }
      z <- stratum_obs[in_bin, ]
      if (!nrow(z)) next
      pred <- pmax(z$PRED, 0.01)
      sim <- matrix(NA_real_, nrow = nrow(z), ncol = nsim)
      for (j in seq_len(nsim)) {
        sim[, j] <- pred * exp(rnorm(nrow(z), mean = -0.5 * 0.18^2, sd = 0.18))
      }
      if (prediction_corrected) {
        reference_pred <- stats::median(pred)
        corrected_obs <- z$DV * reference_pred / pred
        corrected_sim <- sim * (reference_pred / pred)
      } else {
        corrected_obs <- z$DV
        corrected_sim <- sim
      }
      row_values <- c(
        if (bin_index == 1L) "first interval is closed" else "",
        fmt(bin$low), fmt(bin$high), fmt(stats::median(z$TIME)), as.character(nrow(z))
      )
      for (prob in probs) {
        obs_q <- stats::quantile(corrected_obs, probs = prob, type = 7, names = FALSE)
        sim_q <- apply(corrected_sim, 2, stats::quantile,
                       probs = prob, type = 7, names = FALSE)
        row_values <- c(
          row_values, fmt(obs_q),
          fmt(stats::quantile(sim_q, 0.5, type = 7, names = FALSE)),
          fmt(stats::quantile(sim_q, 0.025, type = 7, names = FALSE)),
          fmt(stats::quantile(sim_q, 0.975, type = 7, names = FALSE))
        )
      }
      lines <- c(lines, paste0('"', row_values[1], '",', paste(row_values[-1], collapse = ",")))
    }
  }
  write_demo_lines(file.path(vpc_dir, "vpc_results.csv"), lines)

  meta <- c(
    "tool: vpc",
    sprintf("model: %s", run_name),
    "tool_version: synthetic fixture (no PsN execution)",
    "tool_options:",
    sprintf("  predcorr: %d", as.integer(prediction_corrected)),
    "  idv: TIME",
    "  dv: DV",
    "  stratify_on: DOSEGRP",
    "  samples: 100",
    "  auto_bin: 3",
    "  confidence_interval: 95"
  )
  write_demo_lines(file.path(vpc_dir, "meta.yaml"), meta)
  command <- if (prediction_corrected) {
    sprintf("vpc %s.mod -samples=100 -predcorr -stratify_on=DOSEGRP -idv=TIME -auto_bin=3", run_name)
  } else {
    sprintf("vpc %s.mod -samples=100 -stratify_on=DOSEGRP -idv=TIME -auto_bin=3", run_name)
  }
  write_demo_lines(file.path(vpc_dir, "command.txt"), command)
}

for (run_name in names(model_specs)) {
  spec <- model_specs[[run_name]]
  run_dir <- file.path(demo_root, run_name)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  source <- if (run_name == "run103") source_103 else source_101_102
  run_table <- make_run_table(source, spec)
  write_mod(file.path(run_dir, paste0(run_name, ".mod")), spec)
  write_demo_table(
    source[, c("ROW", "ID", "TIME", "DV", "AMT", "EVID", "MDV",
               "DOSEGRP", "SEX", "WT", "OUTMAG")],
    file.path(run_dir, "synthetic_data.csv"), sep = ","
  )
  write_listing(file.path(run_dir, paste0(run_name, ".lst")), spec,
                run_table, run_name)
  write_ext(file.path(run_dir, paste0(run_name, ".ext")), spec)
  write_demo_table(run_table, file.path(run_dir, sprintf("sdtab%d", spec$id)))
  if (run_name %in% c("run101", "run102")) {
    write_bootstrap(run_dir, spec)
    write_vpc(file.path(run_dir, "pcVPC"), run_name, run_table,
              prediction_corrected = TRUE, seed = 20261207 + spec$id)
  } else {
    write_vpc(file.path(run_dir, "vpc"), run_name, run_table,
              prediction_corrected = FALSE, seed = 20261207 + spec$id)
  }
}

message("Generated deterministic synthetic fixtures under ", demo_root)
