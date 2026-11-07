# Input formats and archive behavior

## NONMEM runs

Select a project folder containing model control streams (`.mod` or `.ctl`), report files (`.lst`, `.res` or `.out`), raw estimation results and diagnostic tables. Relative table names are read from `$TABLE FILE=...` declarations. The reader uses the actual table headers and retains the source line of each diagnostic row.

The numerical reader recognizes `ITERATION`, parameter columns and the raw-output sentinels for final estimates, standard errors and fixed parameters. Missing or unresolved information is shown as unavailable. Simulation-only problems are excluded from the model-comparison list. When multiple estimation tables are identified, select the table in Compare. A table without a reliable problem or stage assignment remains unavailable. Keep each control stream and its corresponding report together. File layouts that cannot be resolved safely produce import notes.

Ordinary observation plots exclude dose/event rows, missing observations and rows flagged as BLQ/BQL. Censored observations can still contribute to the original fitted likelihood, so review the original model's treatment of those observations separately. The viewer does not calculate new NONMEM residuals.

## Observation correspondence

The comparison verifies a unique record identifier or a suitable combination of subject, time and endpoint/occasion identifiers. Matching also checks shared observation values. Reordered records can match. Equal subject counts or equal numbers of rows alone do not establish correspondence. Duplicate or incompatible keys withhold linked comparison. The OFV difference has the sign **comparison minus reference**. The application provides review evidence and does not rank models or calculate likelihood-ratio p-values.

## Estimation diagnostics and AIC

EXT and sidecar tables are associated by identified estimation table and problem. Listing and SHK values retain their source and reported definition. Source disagreements are shown. The SHK reader supports single-subpopulation layouts with ETA columns and EPS positions established from SIGMA. For correlation matrices, a diagonal containing standard errors is recognized by agreement with the matched EXT or covariance output before it is normalized to one. A condition number reported by NONMEM is displayed separately from the eigenvalue ratio computed for an identified matrix.

AIC on the NONMEM objective scale is OFV + 2k. The count uses the final EXT fixed flags and supported diagonal, block and SAME structures. Unsupported counts can be supplied with a recorded basis. The app withholds AIC for objectives or estimation states whose likelihood basis is unresolved, including evaluation-only and penalized runs. Delta AIC requires compatible methods, fitted input evidence, response scale and likelihood convention. Control comments of the form "; NMREVIEW LIKELIHOOD=..." and "; NMREVIEW DV_SCALE=..." can record project declarations. The comparison settings can record an explicit assumption when evidence is incomplete. A known mismatch keeps the delta unavailable.

## Inspector and model relationships

The Inspector reads declared whitespace tables lazily and preserves their sections and source row identifiers. Choose a source table, and optionally join a second single-section table by verified ROW and subject keys or by a single record per subject. STUDY and ID identify a subject when both are present. Numeric identifiers are read as text to retain their written form. Display filters affect the plot and displayed rows. TAD presets use a supplied TAD column. Point selection is available for a single untransformed numeric scatter or profile. Use the displayed-row table for selection in other plots.

The Tree view reads a "; NMREVIEW PARENT=..." control comment or a parent assigned in the app. It preserves unresolved references and rejects cycles. Code differences compare the two imported problem blocks, using a bounded line comparison with context.

## PsN bootstrap

The expected inputs are `raw_results_<model>.csv`, optional `bootstrap_results.csv`, and metadata such as `meta.yaml` or `version_and_option_info.txt`. The raw file may use PsN's wide parameter columns. Replicate statuses, original-model rows and inclusion rules are retained. Missing information remains explicit.

Stored source summaries are preferred when available. Recomputed intervals use `stats::quantile(..., type = 7)` with the displayed confidence level. Their method is recorded and can differ from the source tool's interval procedure. BCa or other adjusted intervals require their corresponding source results and auxiliary information.

## PsN VPC and pcVPC

The expected summary is `vpc_results.csv`, with its run information, continuous-data sections, strata and bins. Prediction correction is identified from the output marker and available metadata. A `PRED` column alone does not establish that correction was performed. The reader preserves corrected values without applying a second correction.

For integrations, the reader also accepts a compact CSV with these columns:

```text
stratum,lower,upper,x,percentile,observed,simulated,ci_lower,ci_upper,n
```

Each row describes one stratum, bin and percentile. `lower` and `upper` are independent-variable bin boundaries. `x` is the plotting position. `ci_lower` and `ci_upper` describe uncertainty in the simulated percentile, rather than a prediction interval for individual observations. Preserve the percentile level and confidence level in the source settings.

The shipped positive pcVPC examples are independently generated synthetic summaries with known numerical expectations. The external PsN example used for verification is an ordinary VPC. Broad compatibility with every historical PsN release has not been established. Check import notices and bin values when first using a different PsN release or customized output layout.

## Compact archives

An NM Review archive is a versioned SQLite file containing typed JSON payloads, source inventory, payload checksums and row-count validation. Version 0.2 reads schema 1 and writes schema 2. Schema 2 keeps review settings, parsed estimation diagnostics, control blocks and the auxiliary tables used in the saved Inspector view. The source data option governs both primary and auxiliary observation rows. Old archives expose the results they contain, with newly introduced fields shown as unavailable when evidence is missing. A temporary file is written and independently read back before completion. The loader validates the archive structure and supported schema. Saves use a new filename and a per-target lock. Choose another filename if an archive or an interrupted save already occupies that name.

Optional observation rows retain linked diagnostic functionality. With these rows omitted, the archive still retains parameter, bootstrap and VPC summaries. The archive excludes bootstrap execution trees and raw VPC simulation matrices. Rebinning or applying a new correction requires the corresponding underlying data.

Archives can contain confidential individual-level data if that option is selected. Save project data and archives outside the code repository. The supplied `.gitignore` excludes the standard archive extensions and local verification outputs. Source files are never deleted by the application.

## Windows folder selection

The local application invokes the bundled Windows PowerShell and Windows Forms folder dialog in STA mode. It passes paths through temporary JSON files and does not evaluate their contents as commands. A destination can also be typed into the path field. Linux/macOS sessions use Tcl/Tk when available. A remote R server's dialog and filesystem belong to that server.
