# Synthetic demo project

**Every result file in this directory is deterministic, entirely synthetic, and not produced by NONMEM or PsN. No real patient data or third-party result files are included.** The NONMEM-style listings and tables exist only to exercise the local review application's readers.

Regenerate the files from the package root with:

```sh
Rscript tools/generate-demo.R
```

## Contents

- `run101` and `run102` use the same 24 synthetic subjects, 8 observations per subject, and row keys. They include one dose event per subject. `run102` has lower synthetic OFV and smaller conditional weighted residuals at three deliberately injected observations. Its extra `OUTFLAG` term exists only to make that visual change apparent; it is not a recommended model or a real analysis.
- `run103` has the same number of observations but uses a different observation time (`3` instead of `2`) for each subject, with distinct row keys. This lets the app show why equal row counts do not establish observation identity.
- `run101/bootstrap` and `run102/bootstrap` contain compact PsN-style raw result summaries. Each has the original model row (`model=0`) and eight synthetic bootstrap replicates with known success, covariance, boundary, and warning flags. The YAML and command files record the skip options used for the displayed kept/omitted counts.
- `run101/pcVPC` and `run102/pcVPC` contain compact, stratified prediction-corrected VPC summaries. Each has two `DOSEGRP` strata, three bins per stratum, and 10th, 50th, and 90th percentile summaries. Values and 95% simulation confidence limits are computed with R quantiles (`type = 7`) from deterministic in-memory synthetic values; raw simulation rows are not shipped.
- `run103/vpc` is an ordinary VPC with `predcorr: 0` and no `Prediction correction` marker. It is a negative example for classification.

The fixture generator uses base R and the fixed seed `20261107`. It can be run without NONMEM, PsN, Shiny, or network access.
