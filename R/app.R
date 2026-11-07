`%or%` <- function(x, value) if (is.null(x) || !length(x)) value else x

resource_file <- function(...) {
  local <- file.path("inst", ...)
  if (file.exists(local)) {
    return(local)
  }
  installed <- system.file(..., package = "nmreview")
  if (nzchar(installed)) {
    return(installed)
  }
  local
}

native_directory <- function(default = "", caption = "Select NONMEM output folder") {
  if (.Platform$OS.type == "windows") {
    config <- tempfile(fileext = ".json")
    result <- tempfile(fileext = ".json")
    errors <- tempfile(fileext = ".txt")
    on.exit(unlink(c(config, result, errors)), add = TRUE)
    jsonlite::write_json(list(default = default, caption = caption), config, auto_unbox = TRUE)
    powershell <- file.path(Sys.getenv("SystemRoot", "C:/Windows"), "System32", "WindowsPowerShell", "v1.0", "powershell.exe")
    script <- normalizePath(resource_file("app", "choose-folder.ps1"), mustWork = TRUE)
    status <- system2(powershell, c(
      "-NoProfile", "-STA", "-File", shQuote(script),
      "-ConfigPath", shQuote(config), "-ResultPath", shQuote(result)
    ), stdout = FALSE, stderr = errors, invisible = TRUE)
    if (!identical(status, 0L) || !file.exists(result)) stop("The Windows folder dialog could not open. Enter the folder path and select Scan folder.")
    path <- jsonlite::read_json(result)
    if (is.null(path)) {
      return(NA_character_)
    }
    return(as.character(path))
  }
  if (capabilities("tcltk")) {
    return(as.character(tcltk::tk_choose.dir(default, caption)))
  }
  stop("A native folder dialog is unavailable. Enter the folder path and select Scan folder.")
}

display_number <- function(x, digits = 4) {
  if (!length(x) || is.na(x[1]) || !is.finite(x[1])) {
    return("Unavailable")
  }
  format(signif(x[1], digits), trim = TRUE, scientific = FALSE)
}

source_label <- function(source) {
  if (is.list(source)) {
    return(as.character(source$relative %or% "Archive"))
  }
  as.character(source %or% "Archive")
}

covariance_label <- function(covariance) {
  if (is.list(covariance)) {
    return(as.character(covariance$status %or% "unknown"))
  }
  as.character(covariance %or% "unknown")
}

plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 12, base_family = "sans") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", colour = "#173a46"),
      axis.title = ggplot2::element_text(colour = "#47606b"),
      legend.position = "bottom", plot.margin = ggplot2::margin(12, 12, 12, 12)
    )
}

observation_rows <- function(model) {
  d <- model$data %or% data.frame()
  if (!nrow(d)) {
    return(d)
  }
  d$.model_row <- seq_len(nrow(d))
  keep <- rep(TRUE, nrow(d))
  for (name in intersect(c("EVID", "MDV", "BLQ", "BQL"), names(d))) {
    keep <- keep & !is.na(d[[name]]) & d[[name]] == 0
  }
  if ("DV" %in% names(d)) keep <- keep & is.finite(d$DV)
  d[keep, , drop = FALSE]
}

model_rows <- function(project, reference = NULL, candidate = NULL, detailed = FALSE) {
  if (!length(project$models)) {
    return(data.frame())
  }
  rows <- lapply(project$models, function(m) {
    role <- if (identical(m$id, reference)) "Reference" else if (identical(m$id, candidate)) "Comparison" else ""
    data.frame(
      Role = role, Run = m$id, Description = m$label %or% m$id,
      Problem = m$problem %or% NA, Stage = m$table_number %or% NA,
      Method = m$method %or% "Unknown", Subjects = m$nind %or% NA,
      Observations = m$nobs %or% NA, Termination = m$status %or% "Unknown",
      Covariance = covariance_label(m$covariance), OFV = m$ofv %or% NA,
      AIC = nm_model_aic(m)$value, Shrinkage = nm_shrinkage_label(m),
      Diagnostics = paste(intersect(c("DV", "PRED", "IPRED", "CWRES", "IWRES", "WRES"), names(m$data)), collapse = ", "),
      stringsAsFactors = FALSE
    )
  })
  d <- do.call(rbind, rows)
  if (isTRUE(detailed)) d else d[c("Role", "Run", "Description", "OFV", "AIC", "Termination", "Covariance", "Shrinkage")]
}

review_datatable <- function(data, rows = 8, select = "single") {
  if (!ncol(data)) data <- data.frame(Message = character())
  widget <- DT::datatable(data,
    rownames = FALSE, selection = select, escape = TRUE,
    options = list(
      pageLength = rows, lengthMenu = c(8, 20, 50, 100), scrollX = TRUE,
      autoWidth = TRUE, language = list(emptyTable = "No results available")
    ),
    class = "compact stripe hover"
  )
  numeric <- names(data)[vapply(data, is.numeric, logical(1))]
  numeric <- setdiff(numeric, c("ID", "ROW", "DVID", "OCC", "EVID", "MDV", "Problem", "Stage", "Subjects", "Observations", "iteration", "n", ".source_line", "reference_row", "candidate_row"))
  if (length(numeric)) widget <- DT::formatSignif(widget, columns = numeric, digits = 6)
  widget
}

message_plot <- function(text) {
  ggplot2::ggplot() +
    ggplot2::annotate("text",
      x = 0, y = 0, label = text,
      size = 4, colour = "#526976"
    ) +
    ggplot2::theme_void()
}

#' Start the local NONMEM review application
#' @export
run_app <- function(host = "127.0.0.1", port = NULL, launch.browser = interactive(), demo = FALSE) {
  shiny::runApp(nmreview_app(demo = demo), host = host, port = port, launch.browser = launch.browser)
}

#' Build the application without starting a server
#' @export
nmreview_app <- function(demo = FALSE) {
  options(shiny.maxRequestSize = max(getOption("shiny.maxRequestSize", 0), 512 * 1024^2))
  shiny::addResourcePath("nmreview-assets", normalizePath(resource_file("app", "www"), mustWork = TRUE))
  card <- function(title, ..., class = "") {
    shiny::div(
      class = paste("review-card", class),
      shiny::div(class = "card-heading", shiny::h3(title)), ...
    )
  }
  ui <- shiny::fluidPage(
    theme = bslib::bs_theme(
      version = 5, primary = "#006c67", bg = "#eff4f6", fg = "#163743",
      base_font = "system-ui", heading_font = "system-ui"
    ),
    shiny::tags$head(shiny::tags$link(rel = "stylesheet", href = "nmreview-assets/review.css")),
    shiny::div(
      class = "app-header",
      shiny::div(shiny::h1("NM Review"), shiny::span("NONMEM model comparison", class = "app-subtitle")),
      shiny::div(
        class = "header-actions", shiny::actionButton("demo", "Load synthetic demo"),
        shiny::fileInput("archive_file", label = NULL, buttonLabel = "Open archive", placeholder = "Portable .sqlite", accept = ".sqlite"),
        shiny::actionButton("go_archive", "Compact archive"), shiny::actionButton("go_help", "Help")
      )
    ),
    shiny::div(
      class = "source-toolbar",
      shiny::textInput("source_path", "NONMEM output folder", placeholder = "Choose a folder containing your model runs"),
      shiny::actionButton("browse", "Browse...", class = "btn-primary"),
      shiny::actionButton("scan", "Scan folder", onclick = "Shiny.setInputValue('source_path', document.getElementById('source_path').value, {priority: 'event'});"), shiny::uiOutput("project_status")
    ),
    shiny::tabsetPanel(
      id = "page", type = "tabs",
      shiny::tabPanel("All runs",
        value = "runs",
        card(
          "Runs in this folder", shiny::div(
            class = "action-row",
            shiny::span("Select a row to assign a model. Search and sort the complete run list.", class = "muted"),
            shiny::actionButton("set_reference", "Set as reference"),
            shiny::actionButton("set_candidate", "Set as comparison"),
            shiny::actionButton("go_compare", "Open comparison", class = "btn-primary")
          ),
          shiny::div(class = "action-row",
            shiny::radioButtons("runs_view", "View", c("List", "Tree"), inline = TRUE),
            shiny::conditionalPanel("input.runs_view === 'List'", shiny::checkboxInput("runs_details", "More columns")),
            shiny::conditionalPanel("input.runs_view === 'Tree'", shiny::actionButton("tree_edit", "Set model parent"))),
          shiny::conditionalPanel("input.runs_view === 'List'", DT::DTOutput("runs")),
          shiny::conditionalPanel("input.runs_view === 'Tree'", shiny::uiOutput("run_tree"))
        ), shiny::uiOutput("project_notices")
      ),
      shiny::tabPanel("Compare",
        value = "compare",
        shiny::div(
          class = "comparison-toolbar",
          shiny::selectInput("reference", "Reference model", choices = character()),
          shiny::selectInput("candidate", "Comparison model", choices = character()),
          shiny::selectInput("diagnostic", "Diagnostic", choices = c("CWRES vs time" = "residual_time", "CWRES vs prediction" = "residual_pred", "DV vs PRED" = "pred", "DV vs IPRED" = "ipred")),
          shiny::selectInput("endpoint", "Endpoint (DVID)", choices = "All"),
          shiny::numericInput("threshold", "Residual candidate |value| >", 3, min = 0.1, step = 0.5)
        ),
        shiny::uiOutput("stage_controls"),
        shiny::uiOutput("comparison_status"),
        shiny::uiOutput("review_summary"),
        shiny::tags$details(class = "review-card", shiny::tags$summary("Parameters & checks"),
          shiny::div(class = "plot-pair",
            card("Reference estimates", DT::DTOutput("reference_parameters")),
            card("Comparison estimates", DT::DTOutput("candidate_parameters"))),
          shiny::div(class = "plot-pair",
            card("Reference checks", shiny::uiOutput("reference_checks")),
            card("Comparison checks", shiny::uiOutput("candidate_checks")))),
        shiny::tags$details(class = "review-card", shiny::tags$summary("Code differences"), shiny::uiOutput("code_diff")),
        shiny::div(
          class = "comparison-layout",
          card(
            "Linked diagnostics", shiny::div(
              class = "plot-pair",
              shiny::plotOutput("reference_plot", height = "330px", click = "reference_click"),
              shiny::plotOutput("candidate_plot", height = "330px", click = "candidate_click")
            ),
            shiny::div(class = "plot-note", "Click a point or select an observation below to inspect its individual profile.")
          ),
          card(
            "Selected observation", shiny::uiOutput("point_detail"),
            shiny::plotOutput("individual_plot", height = "270px")
          )
        ),
        card("Residual candidates", shiny::div(class = "muted", "Candidate flags support review. Change the threshold to explore observations."), DT::DTOutput("outliers"))
      ),
      shiny::tabPanel("Inspector", value = "inspector", nm_inspector_ui(card)),
      shiny::tabPanel("Evaluation", value = "evaluation",
        shiny::tabsetPanel(id = "evaluation_page", type = "pills", selected = "vpc",
      shiny::tabPanel("Bootstrap",
        value = "bootstrap",
        shiny::div(
          class = "comparison-toolbar", shiny::selectInput("bootstrap_analysis", "Bootstrap analysis", choices = character()),
          shiny::selectInput("bootstrap_parameter", "Parameter", choices = character()),
          shiny::tags$details(shiny::tags$summary("Interval options"),
            shiny::selectInput("bootstrap_confidence", "Interval summary", c("Stored / default summary" = "source", "Recalculate 90% (R type 7)" = "0.9", "Recalculate 95% (R type 7)" = "0.95", "Recalculate 99% (R type 7)" = "0.99"), selected = "source"))
        ),
        shiny::uiOutput("bootstrap_status"), shiny::div(
          class = "plot-pair",
          card("Replicate estimates", shiny::plotOutput("bootstrap_plot", height = "360px")),
          card("Intervals across imported analyses", shiny::plotOutput("bootstrap_intervals", height = "360px"))
        ),
        card("Bootstrap summary", DT::DTOutput("bootstrap_summary")),
        shiny::tags$details(shiny::tags$summary("Replicate inclusion and source"), card("Replicates", DT::DTOutput("bootstrap_replicates")))
      ),
      shiny::tabPanel("VPC / pcVPC",
        value = "vpc",
        shiny::div(
          class = "comparison-toolbar", shiny::selectInput("vpc_reference", "Reference VPC analysis", choices = character()),
          shiny::selectInput("vpc_candidate", "Comparison VPC analysis", choices = character()),
          shiny::selectInput("vpc_stratum", "Stratum", choices = character())
        ),
        shiny::tags$details(shiny::tags$summary("Percentile options"),
          shiny::div(class = "vpc-percentiles", shiny::checkboxGroupInput("vpc_percentiles", "Percentiles to display", choices = character(), inline = TRUE))),
        shiny::uiOutput("vpc_status"),
        card("Observed percentiles and confidence bands for simulated percentiles", shiny::div(
          class = "plot-pair",
          shiny::plotOutput("vpc_reference_plot", height = "400px"), shiny::plotOutput("vpc_candidate_plot", height = "400px")
        )),
        shiny::tags$details(shiny::tags$summary("Stored bin values"), card("Bins", DT::DTOutput("vpc_bins")))
      ),
      shiny::tabPanel("Sensitivity", value = "sensitivity",
        card("Compare completed sensitivity runs",
          shiny::selectizeInput("sensitivity_runs", "Runs to compare with the reference", choices = NULL, multiple = TRUE),
          DT::DTOutput("sensitivity_table"),
          shiny::tags$details(shiny::tags$summary("Change notes and estimates"),
            shiny::div(class = "action-row", shiny::selectInput("sensitivity_note_run", "Run", character()),
              shiny::textInput("sensitivity_note", "Change from reference"),
              shiny::actionButton("save_sensitivity_note", "Save note")),
            DT::DTOutput("sensitivity_parameters")))
      ))
      ),
      shiny::tabPanel("Compact archive",
        value = "archive",
        card(
          "Keep the results you need", shiny::p("Store model summaries, bootstrap replicate estimates and VPC bin summaries in a portable SQLite file. Reopen it after disconnecting the original output folder."),
          shiny::checkboxInput("include_observations", "Include model observation and event rows for linked individual diagnostics", TRUE),
          shiny::p(class = "muted", "VPC bin summaries preserve the existing binning, strata and correction. Changing those choices requires the underlying simulation rows. Full bootstrap run directories and simulation arrays are omitted."),
          DT::DTOutput("archive_plan"),
          shiny::div(
            class = "archive-controls", shiny::downloadButton("download_archive", "Download compact archive"),
            shiny::textInput("archive_directory", "Or save to a local folder", placeholder = "Choose destination"),
            shiny::actionButton("browse_archive", "Browse..."), shiny::actionButton("save_archive", "Save and verify", class = "btn-primary", onclick = "Shiny.setInputValue('archive_directory', document.getElementById('archive_directory').value, {priority: 'event'});")
          ),
          shiny::uiOutput("archive_result")
        ),
        card("Source inventory", shiny::p(class = "muted", "Review relative source names, fingerprints and included data to plan retention."), DT::DTOutput("source_manifest"))
      ),
      shiny::tabPanel("Help", value = "help", card(
        "Review workflow",
        shiny::tags$ol(
          shiny::tags$li("Browse to a NONMEM project folder, or load the synthetic demo."),
          shiny::tags$li("In All runs, select any run and assign it as reference or comparison."),
          shiny::tags$li("Inspect shared-axis diagnostics and residual candidates. Click a point for its subject profile."),
          shiny::tags$li("Check parameter uncertainty, termination, bootstrap inclusion and VPC settings."),
          shiny::tags$li("Save a compact archive, then reopen it to verify independent access.")
        ),
        shiny::p("The initial scope is completed continuous-response NONMEM 7 runs and PsN bootstrap/VPC summaries. Missing columns and uncertain correspondence are shown explicitly."),
        shiny::p("Every shipped example is synthetic. A low OFV or an extreme residual alone does not determine model suitability."),
        shiny::p("Folder dialogs select folders on the computer running R. Run this app locally to use your Windows Explorer folders.")
      ))
    ),
    shiny::div(class = "app-footer", "NM Review 0.2.0", shiny::span("Local analysis | portable results", class = "muted"))
  )

  server <- function(input, output, session) {
    project <- shiny::reactiveVal(NULL)
    project_origin <- shiny::reactiveVal("")
    point <- shiny::reactiveVal(NULL)
    archive_result <- shiny::reactiveVal(NULL)
    threshold_value <- shiny::reactive({
      x <- input$threshold
      if (length(x) != 1L || !is.finite(x) || x <= 0) {
        return(3)
      }
      x
    })
    model_choices <- function(p) {
      stats::setNames(
        vapply(p$models, function(m) m$id, ""),
        vapply(p$models, function(m) paste(m$id, "-", m$label %or% ""), "")
      )
    }
    analysis_choices <- function(x) stats::setNames(seq_along(x), vapply(x, function(a) paste(a$model %or% "", a$id %or% basename(source_label(a$source))), ""))
    set_project <- function(p, origin = "Loaded project") {
      project(p)
      project_origin(origin)
      point(NULL)
      archive_result(NULL)
      choices <- model_choices(p)
      shiny::updateSelectInput(session, "reference", choices = choices, selected = if (isTRUE(p$view$reference %in% choices)) p$view$reference else unname(choices[1]))
      shiny::updateSelectInput(session, "candidate", choices = choices, selected = if (isTRUE(p$view$candidate %in% choices)) p$view$candidate else unname(choices[min(2, length(choices))]))
      shiny::updateSelectInput(session, "diagnostic", selected = p$view$diagnostic %or% "residual_time")
      shiny::updateNumericInput(session, "threshold", value = p$view$threshold %or% 3)
      shiny::updateSelectInput(session, "bootstrap_analysis", choices = analysis_choices(p$bootstraps))
      vpc_choices <- c("No analysis selected" = "", analysis_choices(p$vpcs))
      shiny::updateSelectInput(session, "vpc_reference", choices = vpc_choices, selected = if (length(p$vpcs)) "1" else "")
      shiny::updateSelectInput(session, "vpc_candidate", choices = vpc_choices, selected = if (length(p$vpcs) > 1) "2" else "")
    }
    safe_action <- function(expr) {
      tryCatch(force(expr), error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error", duration = 12)
      })
    }
    scan_folder <- function(path) {
      shiny::withProgress(message = "Reading NONMEM and PsN results", value = 0.2, {
        p <- import_project(path)
        set_project(p, basename(normalizePath(path, winslash = "/")))
        shiny::incProgress(0.8)
      })
    }
    shiny::observeEvent(input$demo, safe_action({
      set_project(demo_project(), "Synthetic demo")
      shiny::updateTextInput(session, "source_path", value = "")
    }))
    shiny::observeEvent(input$scan, safe_action(scan_folder(input$source_path)))
    shiny::observeEvent(input$browse, safe_action({
      path <- native_directory(input$source_path)
      if (length(path) && !is.na(path) && nzchar(path)) {
        shiny::updateTextInput(session, "source_path", value = path)
        scan_folder(path)
      }
    }))
    shiny::observeEvent(input$archive_file, safe_action({
      p <- load_archive(input$archive_file$datapath)
      set_project(p, paste("Archive", input$archive_file$name))
      shiny::updateTextInput(session, "source_path", value = "")
      shiny::showNotification("Archive opened independently of the source folder.", type = "message")
    }))
    if (demo) shiny::observeEvent(TRUE, set_project(demo_project(), "Synthetic demo"), once = TRUE)
    selected_model <- function(id) {
      p <- project()
      if (is.null(p) || !length(p$models) || is.null(id)) {
        return(NULL)
      }
      found <- which(vapply(p$models, function(m) identical(m$id, id), logical(1)))
      if (!length(found)) {
        return(NULL)
      }
      p$models[[found[1]]]
    }
    reference_model <- shiny::reactive(selected_model(input$reference))
    candidate_model <- shiny::reactive(selected_model(input$candidate))
    stage_token <- function(model, stage) paste0(model$id, "#", stage$number)
    active_stage <- function(model, token) {
      for (stage in model$stages) if (identical(token, stage_token(model, stage))) return(nm_select_stage(model, as.character(stage$number)))
      model
    }
    reference <- shiny::reactive(active_stage(reference_model(), input$reference_stage))
    candidate <- shiny::reactive(active_stage(candidate_model(), input$candidate_stage))
    output$stage_controls <- shiny::renderUI({
      stage_input <- function(model, role, label) {
        if (!length(model$stages) || (!is.null(model$review) && length(model$stages) == 1L)) return(NULL)
        choices <- stats::setNames(vapply(model$stages, function(s) stage_token(model, s), ""),
          vapply(model$stages, function(s) paste("Table", s$number, s$method), ""))
        input_id <- paste0(role, "_stage")
        saved <- shiny::isolate(input[[input_id]]) %or% project()$view[[input_id]]
        if (!isTRUE(saved %in% choices)) saved <- project()$view[[input_id]]
        shiny::selectInput(input_id, paste(label, "analysis"), c("Select estimation table" = "", choices), selected = saved %or% "")
      }
      shiny::div(class = "plot-pair", stage_input(reference_model(), "reference", "Reference"),
        stage_input(candidate_model(), "candidate", "Comparison"))
    })
    save_inspector_view <- nm_inspector_server(input, output, session, project, reference, candidate, safe_action)
    save_review_view <- nm_review_panel_server(input, output, session, project, reference, candidate, safe_action)
    archive_project <- function() {
      p <- save_review_view(save_inspector_view(project(), isTRUE(input$include_observations)))
      for (id in c("reference", "candidate", "reference_stage", "candidate_stage", "diagnostic", "threshold")) p$view[[id]] <- input[[id]]
      p
    }
    comparison <- shiny::reactive({
      shiny::req(reference(), candidate())
      compare_models(reference(), candidate())
    })
    output$project_status <- shiny::renderUI({
      p <- project()
      if (is.null(p)) {
        return(shiny::span("Choose a folder or load the demo", class = "muted"))
      }
      shiny::span(class = "project-status", paste(length(p$models), "runs |", length(p$bootstraps), "bootstrap |", length(p$vpcs), "VPC analyses"), shiny::tags$small(project_origin()))
    })
    output$project_notices <- shiny::renderUI({
      p <- project()
      if (is.null(p)) {
        return(NULL)
      }
      notes <- unique(p$notices %or% character())
      if (!length(notes)) {
        return(NULL)
      }
      shiny::div(class = "notice", shiny::tags$strong("Import notes"), shiny::tags$ul(lapply(notes, shiny::tags$li)))
    })
    output$runs <- DT::renderDT({
      shiny::req(project())
      review_datatable(model_rows(project(), input$reference, input$candidate, input$runs_details), rows = 20)
    })
    assign_run <- function(input_id) {
      shiny::req(project())
      if (identical(input$runs_view, "Tree")) {
        id <- input$tree_run
        shiny::req(id %in% vapply(project()$models, function(m) m$id, ""))
      } else {
        shiny::req(input$runs_rows_selected)
        id <- project()$models[[input$runs_rows_selected[1]]]$id
      }
      shiny::updateSelectInput(session, input_id, selected = id)
    }
    shiny::observeEvent(input$set_reference, assign_run("reference"))
    shiny::observeEvent(input$set_candidate, assign_run("candidate"))
    shiny::observeEvent(input$go_compare, shiny::updateTabsetPanel(session, "page", selected = "compare"))
    shiny::observeEvent(input$go_archive, shiny::updateTabsetPanel(session, "page", selected = "archive"))
    shiny::observeEvent(input$go_help, shiny::updateTabsetPanel(session, "page", selected = "help"))
    shiny::observeEvent(list(input$reference, input$candidate), {
      point(NULL)
      dvid <- unique(c(reference()$data$DVID, candidate()$data$DVID))
      shiny::updateSelectInput(session, "endpoint", choices = c("All", as.character(sort(dvid))), selected = "All")
      p <- project()
      if (is.null(p)) {
        return()
      }
      analysis_index <- function(analyses, model) {
        if (is.null(model) || !length(analyses)) {
          return("")
        }
        name <- tools::file_path_sans_ext(basename(source_label(model$source)))
        model_names <- vapply(p$models, function(m) tools::file_path_sans_ext(basename(source_label(m$source))), "")
        if (sum(model_names == name) > 1) {
          return("")
        }
        matches <- which(vapply(analyses, function(a) identical(as.character(a$model), name), logical(1)))
        if (length(matches)) as.character(matches[1]) else ""
      }
      shiny::updateSelectInput(session, "vpc_reference", selected = analysis_index(p$vpcs, reference()))
      shiny::updateSelectInput(session, "vpc_candidate", selected = analysis_index(p$vpcs, candidate()))
      bootstrap_index <- analysis_index(p$bootstraps, candidate())
      if (!nzchar(bootstrap_index)) bootstrap_index <- analysis_index(p$bootstraps, reference())
      if (nzchar(bootstrap_index)) shiny::updateSelectInput(session, "bootstrap_analysis", selected = bootstrap_index)
    })
    shiny::observeEvent(input$endpoint, point(NULL), ignoreInit = TRUE)
    output$comparison_status <- shiny::renderUI({
      c <- comparison()
      shiny::div(
        class = paste("comparison-state", if (isTRUE(c$comparable)) "ready" else "caution"),
        shiny::strong(paste("Delta OFV (comparison - reference)", if (isTRUE(c$comparable)) display_number(c$delta, 6) else "withheld")),
        shiny::span(paste(c$reason %or% "", collapse = " | ")),
        if (nzchar(c$warning %or% "")) shiny::tags$small(c$warning),
        shiny::tags$small("Interpret the difference together with the likelihood definition, response scale and estimation settings.")
      )
    })
    filtered <- function(m) {
      d <- observation_rows(m)
      if (nrow(d) && "DVID" %in% names(d) && !is.null(input$endpoint) && input$endpoint != "All") d <- d[as.character(d$DVID) == input$endpoint, , drop = FALSE]
      d
    }
    diagnostic_data <- function(m) {
      d <- filtered(m)
      if (!nrow(d)) {
        return(data.frame())
      }
      diag <- input$diagnostic %or% "residual_time"
      columns <- switch(diag,
        residual_time = c("TIME", "CWRES"),
        residual_pred = c("PRED", "CWRES"),
        pred = c("PRED", "DV"),
        ipred = c("IPRED", "DV")
      )
      if (!all(columns %in% names(d))) {
        return(data.frame())
      }
      d$.x <- d[[columns[1]]]
      d$.y <- d[[columns[2]]]
      attr(d, "axes") <- columns
      d[is.finite(d$.x) & is.finite(d$.y), , drop = FALSE]
    }
    chart_limits <- shiny::reactive({
      a <- diagnostic_data(reference())
      b <- diagnostic_data(candidate())
      x <- c(a$.x, b$.x)
      y <- c(a$.y, b$.y)
      if (!length(x) || !length(y)) {
        return(NULL)
      }
      list(x = range(x, finite = TRUE), y = range(y, finite = TRUE))
    })
    linked_row <- function(m) {
      selected <- point()
      if (is.null(selected)) {
        return(NA_integer_)
      }
      if (identical(selected$id, m$id)) {
        return(selected$row)
      }
      mapping <- comparison()$matched
      if (is.null(mapping) || !nrow(mapping)) {
        return(NA_integer_)
      }
      if (identical(selected$id, reference()$id)) {
        at <- match(selected$row, mapping$reference_row)
        return(mapping$candidate_row[at] %or% NA_integer_)
      }
      at <- match(selected$row, mapping$candidate_row)
      mapping$reference_row[at] %or% NA_integer_
    }
    diagnostic_plot <- function(m) {
      if (is.null(m)) {
        return(message_plot("Select a model in All runs"))
      }
      d <- diagnostic_data(m)
      if (!nrow(d)) {
        return(message_plot(paste(m$id, "\nRequired columns unavailable")))
      }
      axes <- attr(d, "axes")
      lim <- chart_limits()
      d$.flag <- if ("CWRES" %in% names(d)) abs(d$CWRES) > threshold_value() else FALSE
      plot <- ggplot2::ggplot(d, ggplot2::aes(x = .x, y = .y)) +
        ggplot2::geom_point(ggplot2::aes(colour = .flag), size = 1.8, alpha = 0.8) +
        ggplot2::scale_colour_manual(values = c("FALSE" = "#5b92a5", "TRUE" = "#b94f23"), guide = "none") +
        ggplot2::coord_cartesian(xlim = lim$x, ylim = lim$y) +
        plot_theme() +
        ggplot2::labs(title = m$id, x = axes[1], y = axes[2])
      if (axes[2] == "CWRES") {
        plot <- plot + ggplot2::geom_hline(yintercept = 0, colour = "#8b9ba2") +
          ggplot2::geom_hline(yintercept = c(-1, 1) * threshold_value(), linetype = "dashed", colour = "#c9997c")
      } else {
        plot <- plot + ggplot2::geom_abline(intercept = 0, slope = 1, colour = "#8b9ba2")
      }
      selected <- linked_row(m)
      if (length(selected) && !is.na(selected)) plot <- plot + ggplot2::geom_point(data = d[d$.model_row == selected, , drop = FALSE], shape = 21, fill = "#fff2bd", colour = "#633b09", size = 4, stroke = 1.2)
      plot
    }
    output$reference_plot <- shiny::renderPlot(diagnostic_plot(reference()), res = 110, alt = function() paste("Diagnostic for reference model", input$reference, "with the selected axes. Select a point to inspect the observation."))
    output$candidate_plot <- shiny::renderPlot(diagnostic_plot(candidate()), res = 110, alt = function() paste("Diagnostic for comparison model", input$candidate, "on the same axes as the reference."))
    select_click <- function(m, click) {
      d <- diagnostic_data(m)
      if (!nrow(d)) {
        return()
      }
      found <- shiny::nearPoints(d, click, xvar = ".x", yvar = ".y", maxpoints = 1, threshold = 15)
      if (nrow(found)) point(list(id = m$id, row = found$.model_row[1]))
    }
    shiny::observeEvent(input$reference_click, select_click(reference(), input$reference_click))
    shiny::observeEvent(input$candidate_click, select_click(candidate(), input$candidate_click))
    outliers <- shiny::reactive({
      shiny::req(reference(), candidate())
      models <- if (identical(reference()$id, candidate()$id)) list(reference()) else list(reference(), candidate())
      rows <- lapply(models, function(m) {
        d <- filtered(m)
        if (!nrow(d) || !"CWRES" %in% names(d)) {
          return(NULL)
        }
        d <- d[is.finite(d$CWRES) & abs(d$CWRES) > threshold_value(), , drop = FALSE]
        if (!nrow(d)) {
          return(NULL)
        }
        cols <- intersect(c("ID", "ROW", "TIME", "DVID", "OCC", "DV", "PRED", "IPRED", "CWRES", ".source_line"), names(d))
        out <- d[, cols, drop = FALSE]
        out$Run <- m$id
        out$.model_row <- d$.model_row
        out
      })
      rows <- Filter(Negate(is.null), rows)
      if (!length(rows)) {
        return(data.frame())
      }
      columns <- unique(unlist(lapply(rows, names)))
      rows <- lapply(rows, function(r) {
        for (name in setdiff(columns, names(r))) r[[name]] <- NA
        r[, columns, drop = FALSE]
      })
      result <- do.call(rbind, rows)
      result[order(-abs(result$CWRES)), , drop = FALSE]
    })
    output$outliers <- DT::renderDT({
      d <- outliers()
      review_datatable(d[, setdiff(names(d), ".model_row"), drop = FALSE])
    })
    shiny::observeEvent(input$outliers_rows_selected, {
      at <- input$outliers_rows_selected
      if (!length(at) || !nrow(outliers())) {
        return()
      }
      d <- outliers()[at[1], , drop = FALSE]
      point(list(id = d$Run, row = d$.model_row))
    })
    output$point_detail <- shiny::renderUI({
      s <- point()
      if (is.null(s)) {
        return(shiny::p(class = "muted", "Select a point or residual candidate."))
      }
      m <- selected_model(s$id)
      row <- m$data[s$row, , drop = FALSE]
      evidence <- if (".source_file" %in% names(row)) basename(as.character(row$.source_file[1])) else source_label(m$source)
      fields <- intersect(c("ID", "ROW", "TIME", "TAD", "DVID", "OCC", "DV", "AMT", "EVID", "MDV", "CWRES", "IPRED", ".source_line"), names(row))
      shiny::tagList(
        shiny::div(class = "point-run", s$id), shiny::div(
          class = "point-fields",
          lapply(fields, function(name) {
            value <- row[[name]][1]
            if (name %in% c("DV", "TIME", "TAD", "AMT", "CWRES", "IPRED") && is.numeric(value)) value <- display_number(value, 7)
            shiny::div(shiny::span(name, class = "muted"), shiny::strong(as.character(value)))
          })
        ),
        shiny::p(class = "muted source-evidence", paste("Source", evidence, "| Problem", m$problem %or% "unknown", "| table", m$table_number %or% "unknown"))
      )
    })
    output$individual_plot <- shiny::renderPlot(
      {
        if (is.null(point())) {
          return(message_plot("Individual profile"))
        }
        s <- point()
        selected <- selected_model(s$id)
        selected_row <- selected$data[s$row, , drop = FALSE]
        if (!all(c("ID", "TIME", "DV") %in% names(selected_row))) {
          return(message_plot("ID, TIME and DV are required"))
        }
        if (is.na(selected_row$ID[1])) {
          return(message_plot("Subject identifier unavailable"))
        }
        models <- list(reference(), candidate())
        lines <- lapply(models, function(m) {
          d <- observation_rows(m)
          if (!all(c("ID", "TIME", "IPRED") %in% names(d))) {
            return(NULL)
          }
          keep <- d$ID == selected_row$ID
          for (col in intersect(c("DVID", "OCC"), names(selected_row))) if (col %in% names(d)) keep <- keep & d[[col]] == selected_row[[col]]
          d <- d[keep, , drop = FALSE]
          data.frame(TIME = d$TIME, IPRED = d$IPRED, Run = m$id)
        })
        lines <- Filter(Negate(is.null), lines)
        observed <- observation_rows(selected)
        keep <- observed$ID == selected_row$ID
        for (col in intersect(c("DVID", "OCC"), names(selected_row))) keep <- keep & observed[[col]] == selected_row[[col]]
        observed <- observed[keep, , drop = FALSE]
        p <- ggplot2::ggplot(observed, ggplot2::aes(x = TIME, y = DV)) +
          ggplot2::geom_point(colour = "#173a46", size = 2) +
          plot_theme() +
          ggplot2::labs(x = "TIME", y = "DV / IPRED", title = paste("Subject", selected_row$ID))
        events <- selected$data
        if (all(c("ID", "TIME", "EVID") %in% names(events))) {
          events <- events[events$ID == selected_row$ID & events$EVID %in% c(1, 4), , drop = FALSE]
          if ("OCC" %in% names(events) && "OCC" %in% names(selected_row)) events <- events[events$OCC == selected_row$OCC, , drop = FALSE]
          if (nrow(events)) p <- p + ggplot2::geom_rug(data = events, ggplot2::aes(x = TIME), inherit.aes = FALSE, sides = "b", colour = "#a77531", linewidth = 1) + ggplot2::labs(subtitle = "Dose events marked below")
        }
        if (length(lines)) p <- p + ggplot2::geom_line(data = do.call(rbind, lines), ggplot2::aes(x = TIME, y = IPRED, colour = Run, linetype = Run), linewidth = 0.9) + ggplot2::scale_colour_manual(values = c("#006c67", "#5271a0"))
        p + ggplot2::geom_point(data = selected_row, shape = 21, fill = "#fff2bd", colour = "#633b09", size = 4)
      },
      res = 110
    )
    output$reference_parameters <- DT::renderDT({
      shiny::req(reference())
      review_datatable(reference()$parameters, select = "none")
    })
    output$candidate_parameters <- DT::renderDT({
      shiny::req(candidate())
      review_datatable(candidate()$parameters, select = "none")
    })
    checks <- function(m) {
      if (is.null(m)) {
        return(NULL)
      }
      cov <- if (is.list(m$covariance)) m$covariance else list()
      notes <- c(paste("Termination", m$status), paste("Covariance", covariance_label(m$covariance)), m$notices, cov$notices)
      shiny::tagList(
        shiny::tags$ul(lapply(unique(notes), shiny::tags$li)),
        nm_review_details(m),
        shiny::tags$details(shiny::tags$summary("Termination messages"), shiny::tags$pre(paste(m$termination %or% character(), collapse = "\n"))),
        if (is.data.frame(m$shrinkage) && nrow(m$shrinkage)) shiny::tags$pre(paste(utils::capture.output(print(m$shrinkage)), collapse = "\n")),
        if (is.data.frame(m$correlation) && nrow(m$correlation)) shiny::tags$pre(paste(utils::capture.output(print(m$correlation)), collapse = "\n")),
        if (is.data.frame(cov$data) && nrow(cov$data)) shiny::tags$pre(paste(utils::capture.output(print(cov$data)), collapse = "\n")),
        lapply(names(m$diagnostic_text), function(name) {
          record <- m$diagnostic_text[[name]]
          shiny::tags$details(
            shiny::tags$summary(paste(name, record$source)),
            if (isTRUE(record$truncated)) shiny::p("Source preview truncated. Retain the original for the complete output."),
            shiny::tags$pre(paste(record$text, collapse = "\n"))
          )
        })
      )
    }
    output$reference_checks <- shiny::renderUI(checks(reference()))
    output$candidate_checks <- shiny::renderUI(checks(candidate()))
    analysis <- function(name, index) {
      p <- project()
      if (is.null(p) || !length(p[[name]]) || is.null(index) || !nzchar(index)) {
        return(NULL)
      }
      p[[name]][[as.integer(index)]]
    }
    bootstrap <- shiny::reactive(analysis("bootstraps", input$bootstrap_analysis))
    shiny::observeEvent(bootstrap(), {
      b <- bootstrap()
      params <- unique(c(b$parameters$parameter, b$summary$parameter))
      shiny::updateSelectInput(session, "bootstrap_parameter", choices = params)
    })
    bootstrap_summary <- function(b) {
      if (is.null(b)) {
        return(data.frame())
      }
      d <- b$parameters
      if ((input$bootstrap_confidence %or% "source") == "source" && is.data.frame(b$summary) && nrow(b$summary)) {
        return(b$summary)
      }
      if (is.null(d) || !nrow(d)) {
        return(b$summary %or% data.frame())
      }
      confidence <- if ((input$bootstrap_confidence %or% "source") == "source") 0.95 else as.numeric(input$bootstrap_confidence)
      params <- unique(d$parameter)
      rows <- lapply(params, function(name) {
        rows <- d[d$parameter == name, , drop = FALSE]
        values <- rows$estimate[rows$included %in% TRUE & rows$is_original %in% FALSE & is.finite(rows$estimate)]
        original <- rows$estimate[rows$is_original %in% TRUE & is.finite(rows$estimate)]
        bounds <- if (length(values)) stats::quantile(values, c((1 - confidence) / 2, 1 - (1 - confidence) / 2), type = 7, names = FALSE) else c(NA, NA)
        data.frame(parameter = name, original = if (length(original)) original[1] else NA_real_, median = if (length(values)) stats::median(values) else NA_real_, lower = bounds[1], upper = bounds[2], n = length(values))
      })
      do.call(rbind, rows)
    }
    output$bootstrap_status <- shiny::renderUI({
      b <- bootstrap()
      if (is.null(b)) {
        return(shiny::p(class = "notice", "No bootstrap results loaded. Scan a folder containing raw_results_<model>.csv or bootstrap_results.csv."))
      }
      d <- b$parameters
      planned <- nm_one_num(b$settings$samples %or% NA_real_)
      counts <- if (!is.null(d) && nrow(d)) paste("Planned", if (is.finite(planned)) planned else "unknown", "|", length(unique(d$iteration[!d$is_original])), "replicates recorded |", length(unique(d$iteration[d$included & !d$is_original])), "included") else "Stored summary only"
      shiny::div(
        class = "notice", shiny::strong(counts), shiny::p(paste(b$notices, collapse = " | ")),
        shiny::tags$details(shiny::tags$summary("Import settings and inclusion rules"), shiny::tags$pre(jsonlite::toJSON(b$settings %or% list(), auto_unbox = TRUE, pretty = TRUE)))
      )
    })
    output$bootstrap_summary <- DT::renderDT(review_datatable(bootstrap_summary(bootstrap()), select = "none"))
    output$bootstrap_replicates <- DT::renderDT({
      b <- bootstrap()
      review_datatable(b$parameters %or% data.frame(), rows = 8, select = "none")
    })
    output$bootstrap_plot <- shiny::renderPlot(
      {
        b <- bootstrap()
        d <- b$parameters
        if (is.null(d) || !nrow(d)) {
          return(message_plot("Replicate-level estimates unavailable"))
        }
        d <- d[d$parameter == input$bootstrap_parameter & !d$is_original & is.finite(d$estimate), , drop = FALSE]
        if (!nrow(d)) {
          return(message_plot("Select a parameter"))
        }
        p <- ggplot2::ggplot(d, ggplot2::aes(x = estimate, fill = included)) +
          ggplot2::geom_histogram(bins = 24, colour = "white", position = "stack") +
          plot_theme() +
          ggplot2::scale_fill_manual(values = c("FALSE" = "#bd7655", "TRUE" = "#4a9e92"), labels = c("Excluded", "Included"), name = "Source rule") +
          ggplot2::labs(x = input$bootstrap_parameter, y = "Replicates", subtitle = "Dashed line indicates the original estimate")
        original <- b$parameters$estimate[b$parameters$parameter == input$bootstrap_parameter & b$parameters$is_original]
        if (length(original)) p <- p + ggplot2::geom_vline(xintercept = original[1], colour = "#173a46", linetype = "dashed", linewidth = 0.8)
        p
      },
      res = 110
    )
    output$bootstrap_intervals <- shiny::renderPlot(
      {
        p <- project()
        if (is.null(p) || !length(p$bootstraps)) {
          return(message_plot("No bootstrap analyses"))
        }
        rows <- lapply(p$bootstraps, function(b) {
          d <- bootstrap_summary(b)
          d <- d[d$parameter == input$bootstrap_parameter, , drop = FALSE]
          if (!nrow(d)) {
            return(NULL)
          }
          d$Analysis <- paste(b$model, b$id)
          d
        })
        rows <- Filter(Negate(is.null), rows)
        if (!length(rows)) {
          return(message_plot("No corresponding parameter"))
        }
        d <- do.call(rbind, rows)
        ggplot2::ggplot(d, ggplot2::aes(x = median, y = Analysis)) +
          ggplot2::geom_segment(ggplot2::aes(x = lower, xend = upper, yend = Analysis), colour = "#4a9e92", linewidth = 2) +
          ggplot2::geom_point(size = 3, colour = "#006c67") +
          ggplot2::geom_point(ggplot2::aes(x = original), shape = 4, size = 3) +
          plot_theme() +
          ggplot2::labs(x = input$bootstrap_parameter, y = NULL, caption = "Median (dot) | interval (bar) | original (cross)")
      },
      res = 110
    )
    vpc_ref <- shiny::reactive(analysis("vpcs", input$vpc_reference))
    vpc_cand <- shiny::reactive(analysis("vpcs", input$vpc_candidate))
    shiny::observeEvent(list(vpc_ref(), vpc_cand()), {
      strata <- unique(c(as.character(vpc_ref()$bins$stratum), as.character(vpc_cand()$bins$stratum)))
      shiny::updateSelectInput(session, "vpc_stratum", choices = strata)
      percentiles <- sort(unique(c(vpc_ref()$bins$percentile, vpc_cand()$bins$percentile)))
      percentiles <- percentiles[is.finite(percentiles)]
      selected <- intersect(as.numeric(input$vpc_percentiles), percentiles)
      if (!length(selected) && length(percentiles)) {
        selected <- if (all(c(5, 50, 95) %in% percentiles)) c(5, 50, 95) else unique(c(min(percentiles), percentiles[which.min(abs(percentiles - 50))], max(percentiles)))
      }
      choices <- if (length(percentiles)) stats::setNames(as.character(percentiles), paste0(percentiles, "%")) else character()
      shiny::updateCheckboxGroupInput(session, "vpc_percentiles", choices = choices, selected = as.character(selected))
    })
    correction_label <- function(v) {
      if (is.null(v)) {
        return("No analysis selected")
      }
      paste(v$model, "|", if (isTRUE(v$corrected)) "Prediction corrected" else if (isFALSE(v$corrected)) "Ordinary VPC" else "Correction unknown")
    }
    output$vpc_status <- shiny::renderUI({
      pair_notes <- character()
      if (!is.null(vpc_ref()) && !is.null(vpc_cand())) {
        if (!identical(unname(vpc_ref()$corrected), unname(vpc_cand()$corrected))) pair_notes <- c(pair_notes, "Correction differs between the selected analyses.")
        boundaries <- function(v) unique(v$bins[, intersect(c("stratum", "lower", "upper", "percentile"), names(v$bins)), drop = FALSE])
        if (!identical(boundaries(vpc_ref()), boundaries(vpc_cand()))) pair_notes <- c(pair_notes, "Binning, strata or percentile levels differ. Each panel retains its original settings.")
      }
      shiny::div(
        class = "notice", shiny::strong(correction_label(vpc_ref())), shiny::p(correction_label(vpc_cand())),
        shiny::p(paste(pair_notes, collapse = " ")),
        shiny::p(paste(unique(c(vpc_ref()$notices, vpc_cand()$notices)), collapse = " | ")),
        shiny::tags$details(
          shiny::tags$summary("Correction, binning and source settings"),
          shiny::tags$pre(jsonlite::toJSON(list(reference = vpc_ref()$settings %or% list(), comparison = vpc_cand()$settings %or% list()), auto_unbox = TRUE, pretty = TRUE))
        )
      )
    })
    vpc_data <- function(v) {
      d <- v$bins %or% data.frame()
      if (!nrow(d)) {
        return(d)
      }
      d[as.character(d$stratum) == input$vpc_stratum & d$percentile %in% as.numeric(input$vpc_percentiles), , drop = FALSE]
    }
    vpc_plot <- function(v) {
      d <- vpc_data(v)
      if (!nrow(d)) {
        return(message_plot("No VPC bins available"))
      }
      both <- rbind(vpc_data(vpc_ref()), vpc_data(vpc_cand()))
      if (!any(is.finite(c(both$observed, both$simulated, both$ci_lower, both$ci_upper)))) {
        return(message_plot("Finite VPC summaries unavailable"))
      }
      yrange <- range(c(both$observed, both$simulated, both$ci_lower, both$ci_upper), finite = TRUE)
      xrange <- range(c(both$lower, both$upper, both$x), finite = TRUE)
      levels <- sort(unique(both$percentile))
      colours <- stats::setNames(grDevices::hcl.colors(length(levels), "Dark 3"), as.character(levels))
      ggplot2::ggplot(d, ggplot2::aes(x = x, colour = factor(percentile), group = factor(percentile))) +
        ggplot2::geom_rect(ggplot2::aes(xmin = lower, xmax = upper, ymin = ci_lower, ymax = ci_upper, fill = factor(percentile)), colour = NA, alpha = 0.14) +
        ggplot2::geom_line(ggplot2::aes(y = observed), linewidth = 0.85) +
        ggplot2::geom_line(ggplot2::aes(y = simulated), linetype = "dashed", linewidth = 0.6) +
        ggplot2::scale_colour_manual(values = colours) +
        ggplot2::scale_fill_manual(values = colours) +
        ggplot2::coord_cartesian(xlim = xrange, ylim = yrange) +
        plot_theme() +
        ggplot2::labs(title = v$model %or% v$id, subtitle = if (isTRUE(v$corrected)) "pcVPC | solid observed / dashed simulated" else "VPC | solid observed / dashed simulated", x = v$settings$idv %or% "Independent variable", y = if (isTRUE(v$corrected)) "Prediction-corrected response" else "Response", colour = "Percentile", fill = "Percentile")
    }
    output$vpc_reference_plot <- shiny::renderPlot(vpc_plot(vpc_ref()), res = 110, alt = function() paste(nrow(vpc_data(vpc_ref())), "stored percentile summaries. Solid observed curves, dashed simulated curves and simulated percentile confidence bands."))
    output$vpc_candidate_plot <- shiny::renderPlot(vpc_plot(vpc_cand()), res = 110, alt = function() paste(nrow(vpc_data(vpc_cand())), "stored percentile summaries for the comparison analysis on the same axes."))
    output$vpc_bins <- DT::renderDT({
      bins <- vpc_ref()$bins %or% data.frame()
      if (".source_file" %in% names(bins)) bins$.source_file <- basename(bins$.source_file)
      review_datatable(bins, select = "none")
    })
    output$archive_plan <- DT::renderDT({
      shiny::req(project())
      review_datatable(archive_summary(project()), select = "none")
    })
    output$source_manifest <- DT::renderDT({
      shiny::req(project())
      review_datatable(project()$manifest, select = "none")
    })
    shiny::observeEvent(input$browse_archive, safe_action({
      path <- native_directory(input$archive_directory, "Select compact archive destination")
      if (length(path) && !is.na(path) && nzchar(path)) shiny::updateTextInput(session, "archive_directory", value = path)
    }))
    output$download_archive <- shiny::downloadHandler(
      filename = function() paste0("nmreview-", format(Sys.Date(), "%Y-%m-%d"), ".sqlite"), contentType = "application/vnd.sqlite3",
      content = function(file) {
        shiny::req(project())
        temporary <- tempfile(fileext = ".sqlite")
        on.exit(unlink(temporary), add = TRUE)
        save_archive(archive_project(), temporary, include_observations = isTRUE(input$include_observations))
        if (!file.copy(temporary, file, overwrite = TRUE)) stop("Could not prepare the archive download.")
      }
    )
    shiny::observeEvent(input$save_archive, safe_action({
      shiny::req(project())
      if (!dir.exists(input$archive_directory)) stop("Select an existing destination folder.")
      path <- tempfile(pattern = paste0("nmreview-", format(Sys.time(), "%Y%m%d-%H%M%S"), "-"), tmpdir = input$archive_directory, fileext = ".sqlite")
      save_archive(archive_project(), path, include_observations = isTRUE(input$include_observations))
      reopened <- load_archive(path)
      archive_result(list(path = path, bytes = file.info(path)$size, runs = length(reopened$models)))
    }))
    output$archive_result <- shiny::renderUI({
      r <- archive_result()
      if (is.null(r)) {
        return(NULL)
      }
      shiny::div(
        class = "comparison-state ready", shiny::strong("Saved and reopened successfully"),
        shiny::p(paste(r$runs, "runs |", format(round(r$bytes / 1024, 1)), "KiB")), shiny::tags$code(r$path)
      )
    })
  }
  shiny::shinyApp(ui, server)
}

utils::globalVariables(c(".data", ".x", ".y", ".flag", "TIME", "DV", "IPRED", "Run", "estimate", "included", "median", "lower", "upper", "original", "Analysis", "x", "percentile", "ci_lower", "ci_upper", "observed", "simulated"))
