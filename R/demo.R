#' Import the package's synthetic NONMEM review project
#'
#' The bundled project is generated locally from the deterministic script in
#' `tools/generate-demo.R`. It contains no real patient data and does not
#' require NONMEM to run.
#'
#' @return The project object returned by [import_project()].
#' @export
demo_project <- function() {
  # Prefer the current source checkout so regenerated fixtures are visible
  # without reinstalling the package.
  source_dir <- file.path("inst", "extdata", "demo")
  if (dir.exists(source_dir)) {
    return(import_project(source_dir))
  }

  # Installed packages keep bundled examples below extdata.
  demo_dir <- system.file("extdata", "demo", package = "nmreview")
  if (!dir.exists(demo_dir)) {
    stop("Synthetic demo files were not found. Run tools/generate-demo.R.",
      call. = FALSE
    )
  }

  import_project(demo_dir)
}
