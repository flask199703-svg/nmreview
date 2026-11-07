# Verification record

Verification date is 2026-11-07. The local environment is Windows 11 x64 with R 4.6.0. Browser checks use Microsoft Edge in a fresh automated session. All repository fixtures are synthetic.

## Version 0.2.0

- R CMD check with the optional legacy and pinned upstream acceptance inputs returned **Status OK**. All six test scripts passed. The synthetic pair gives AIC -198.423 at k=6 and -217.876 at k=7, with comparison minus reference equal to -19.453. The changed input in run103 withholds the delta.
- The Edge smoke test passed 16 scenarios. It exercised auxiliary and diagnostic table selection, Inspector axes and subjects, numeric and category filters, code differences and the model tree, AIC and shrinkage, VPC and bootstrap, sensitivity runs, archive save/download/reopen, and the 1024 px layout. A new browser session restored Inspector filtering and the selected subject. The run observed no JavaScript errors or external HTTP requests.
- Two SQLite files created with the installed 0.1.0 package before the upgrade reopened successfully. The full archive retained 216 diagnostic rows for run101 and the summary archive retained its bootstrap and VPC results. The full archive also reopened through the browser. Fields introduced in 0.2 are unavailable in the legacy archive when the evidence was omitted.
- A public two-stage NMdata example provided an independent SHK check. Its ETA(2) SD shrinkages were 7.45914% for table 1 and 7.35284% for table 2. The reviewer distinguished the completed stochastic stage from the expectation-only stage and kept the corresponding condition numbers 108.384 and 45.2294 separate. [Pinned NMdata example](https://github.com/nmautoverse/NMdata/tree/180723b5c75d4ef57b72eba9cfcfa017895317d0/inst/examples/nonmem)
- The browser scan used a typed folder path. The native Browse helper retains the previously confirmed Windows selection path.

## Final acceptance

- `R CMD build .` and `R CMD check --no-manual nmreview_0.1.0.tar.gz`: **Status OK**, zero errors, warnings and notes. All five test files passed.
- The browser test completed all twelve recorded scenarios without JavaScript errors or external HTTP requests. The resulting 217,088-byte SQLite archive was opened in a fresh browser session and retained its review data.
- The optional pinned-upstream acceptance passed after the final code changes: one xpose fit with 74 subjects / 476 observations / four residual candidates; PsN bootstrap with 20 long rows and three included replicate fits; VPC with 18 summary rows, 14 observations and 20 simulations.
- The screenshots in `docs/screenshots/` were captured from the running app with synthetic data. Full machine-readable browser results and local logs are kept in the excluded `verification-local/` and `nmreview.Rcheck/` folders.

Documentation and screenshot packaging followed these checks without changing application code. GitHub Actions has been configured but has not been run on GitHub.

## Numerical and archive checks

- Three synthetic models each have 24 subjects, 192 observations and 24 dose events. Reference and comparison OFVs are -210.423 and -231.876, giving a difference of -21.453. The third model has the same observation count but different time keys and is withheld from OFV comparison.
- Each synthetic bootstrap has an original-model row and eight replicate rows for six parameters. Five replicates meet its recorded inclusion rules. Raw statuses and exclusion reasons are preserved.
- Each synthetic VPC has two strata, three bins and three percentiles, producing 18 summary rows. The two corrected analyses and the ordinary VPC are distinguished by recorded correction metadata.
- EXT tests distinguish the exact final-estimate, SE and fixed-parameter sentinels. Missing final estimates and ambiguous estimation stages are withheld.
- Archives are independently reopened, including tests with deliberately unavailable source references. Numerical values, non-finite values, replicate data, correction flags and settings survive round trips. Altered row counts and payload checksums are rejected. An existing target file and a locked target are protected.
- A summary-only archive retains bootstrap and VPC results while omitting observation tables.

The offline tests are `tests/test-backend.R`, `tests/test-demo.R`, `tests/test-archive.R` and `tests/test-app.R`. The app test exercises the normal server entry points, an observation selection, the comparison gate and archive saving.

`tests/test-psn.R` also covers independently written PsN-layout regressions. Fixed upstream fixtures are downloaded into a temporary directory for an optional acceptance run, outside the repository. The xpose fixture at commit `a2e988966cc898ce86798df6732cd71414702732` gives one fitted model, 74 subjects, 476 observations, OFV -1403.9051863364650 and four CWRES candidates. Its most extreme candidate has ID 703 and source line 348. [Upstream xpose example](https://github.com/UUPharmacometrics/xpose/tree/a2e988966cc898ce86798df6732cd71414702732/inst/extdata)

The PsN fixture at commit `b897860f97d7f220d2c6f6f3e3e553698de37577` gives five bootstrap model parameters across four raw rows, including the original model. Its three successful replicate rows remain included when covariance was unperformed. The ordinary VPC gives two bins and nine percentile levels, 14 observations and 20 simulations. In the first bin, the observed median is 18.1 and the simulated median is 17.0925, with confidence limits 14.611 and 27.726. Counts repeat for each percentile and are counted once per bin. [Upstream PsN examples](https://github.com/UUPharmacometrics/PsN/tree/b897860f97d7f220d2c6f6f3e3e553698de37577/test/test_files/SO/parser_psn)

The upstream bootstrap fixture's stored aggregate and raw recomputation disagree. The viewer preserves the stored CL median 0.00438462 and unavailable percentile confidence limits, while separately retaining the raw replicate values and inclusion flags. Source summaries with unknown denominators retain an unknown `n`.

## Browser checks

The optional `tools/browser-smoke.cjs` script checks folder-path loading, All runs assignments, independent reference and comparison selectors, reversed OFV sign, linked individual diagnostics, parameter review, bootstrap and pcVPC charts, local archive save, download and import into a fresh browser session. It also checks a tablet viewport, JavaScript errors and external web requests.

Run this developer test with Playwright available to Node and a running local app. `NMREVIEW_PLAYWRIGHT` can identify the installed Playwright module, and `NMREVIEW_BROWSER` can point to an existing browser executable. Screenshots and machine-readable results are written to the excluded `verification-local` folder.

## Scope of confirmation

The Windows folder-dialog helper passes syntax validation. On 2026-11-07, the user confirmed that **Browse…** opened the native Windows dialog and allowed folder selection on their desktop. The manual check covered selection. Cancel behavior remains unverified. The browser test covers the typed-path **Scan folder** workflow.

The app reads completed outputs. NONMEM estimation, PsN resampling and NONMEM-based simulations were not executed for this verification. General compatibility with every NONMEM/PsN release and customized output layout requires corresponding fixtures. Multiple unresolved estimation stages are explicitly withheld. GitHub Actions configuration is included for Windows/Linux checks when the repository is published.
