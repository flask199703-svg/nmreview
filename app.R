# Run from a source checkout with shiny::runApp("path/to/nmreview").
for (file in list.files("R", pattern = "[.]R$", full.names = TRUE)) {
  source(file, local = environment(), encoding = "UTF-8")
}
nmreview_app()
