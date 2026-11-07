library(nmreview)

# Exercise the actual server entry points, selection, diagnostics and local save.
# The native dialog itself is checked separately on Windows.
shiny::testServer(nmreview_app(), {
  session$setInputs(demo = 1)
  session$setInputs(
    reference = "run101:p1", candidate = "run102:p1",
    diagnostic = "residual_time", endpoint = "All", threshold = 3,
    bootstrap_analysis = "1", bootstrap_parameter = "CL",
    bootstrap_confidence = "source", vpc_reference = "1",
    vpc_candidate = "2", vpc_stratum = as.character(demo_project()$vpcs[[1]]$bins$stratum[1]), vpc_percentiles = c("10", "50", "90"), include_observations = TRUE
  )
  session$flushReact()
  stopifnot(grepl("3 runs", output$project_status$html, fixed = TRUE))
  stopifnot(grepl("-21.453", output$comparison_status$html, fixed = TRUE))
  stopifnot(grepl("Prediction corrected", output$vpc_status$html, fixed = TRUE))
  session$setInputs(outliers_rows_selected = 1)
  session$flushReact()
  stopifnot(grepl("CWRES", output$point_detail$html, fixed = TRUE))
  session$setInputs(candidate = "run103:p1")
  session$flushReact()
  stopifnot(grepl("withheld", output$comparison_status$html, fixed = TRUE))
  session$setInputs(threshold = NA_real_)
  session$flushReact()
  stopifnot(length(output$outliers) > 0L)
  archive_dir <- tempfile("nmreview-ui-")
  dir.create(archive_dir)
  on.exit(unlink(archive_dir, recursive = TRUE), add = TRUE)
  session$setInputs(archive_directory = archive_dir, save_archive = 1)
  session$flushReact()
  stopifnot(grepl("Saved and reopened successfully", output$archive_result$html, fixed = TRUE))
  archive_files <- list.files(archive_dir, pattern = "[.]sqlite$", full.names = TRUE)
  stopifnot(length(archive_files) == 1L)
  reloaded <- load_archive(archive_files[[1]])
  stopifnot(
    length(reloaded$models) == 3L, length(reloaded$bootstraps) == 2L,
    length(reloaded$vpcs) == 3L
  )
})
