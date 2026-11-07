nm_shrinkage_label <- function(model) {
  d <- model$review$shrinkage
  if (!NROW(d)) return("Shrinkage not reported")
  if (isTRUE(model$review$conflict)) return("Shrinkage source conflict")
  eta <- which(d$Metric == "ETA" & d$Definition == "SD" & is.finite(d$Value))
  unused <- d$Index[d$Metric == "ETA" & d$Definition == "Subjects" & d$Value %in% 0]
  eta <- eta[!d$Index[eta] %in% unused]
  eps <- which(d$Metric == "EPS" & d$Definition == "SD" & is.finite(d$Value))
  eta <- if (length(eta)) eta[which.max(d$Value[eta])] else integer()
  parts <- c(if (length(eta)) paste0("ETA(", d$Index[eta], ") SD ", display_number(d$Value[eta]), "%"),
    if (length(eps)) paste0("EPS SD ", paste0(display_number(d$Value[eps[1L]]), "%"), if (length(eps) > 1L) " (+ more)" else ""))
  if (length(parts)) paste(parts, collapse = " \u00b7 ") else "Shrinkage details available"
}

nm_review_details <- function(model) {
  if (is.null(model)) return(NULL)
  tags <- shiny::tags
  r <- model$review
  d <- r$shrinkage
  matrices <- Filter(Negate(is.null), list(r$correlation, r$covariance_matrix))
  shiny::tagList(
    tags$details(tags$summary("Shrinkage and ETABAR"),
      if (!NROW(d)) shiny::p("No identified shrinkage output for this analysis.") else
        tags$div(class = "diagnostic-table", tags$table(class = "table table-sm",
          tags$thead(do.call(tags$tr, lapply(names(d), tags$th))),
          tags$tbody(lapply(seq_len(nrow(d)), function(i) {
            do.call(tags$tr, lapply(d[i, , drop = FALSE], function(x) tags$td(ifelse(is.na(x), "\u2014", as.character(x)))))
          }))
        )),
      shiny::p(class = "muted", "Inspect SD, variance and EBV separately. ETA indices refer to this model.")
    ),
    tags$details(tags$summary("Matrices and condition number"),
      shiny::p(paste("Reported condition number of estimate correlation", display_number(r$condition_reported))),
      lapply(matrices, function(m) shiny::tagList(
        shiny::p(paste(m$kind, m$status, "| smallest eigenvalue", display_number(m$minimum),
          "| condition (largest / smallest eigenvalue)", display_number(m$condition))),
        if (!is.null(m$values)) tags$pre(paste(utils::capture.output(print(m$values, digits = 5)), collapse = "\n")),
        if (!is.null(m$source)) shiny::p(class = "muted", basename(m$source))
      )),
      if (!length(matrices)) shiny::p("No matched covariance or correlation matrix.")
    ),
    if (NROW(r$trace)) tags$details(tags$summary("Objective history"),
      shiny::p(paste("Showing", nrow(r$trace), "of", r$iteration_rows, "EXT iteration rows.")),
      tags$pre(paste(utils::capture.output(print(r$trace, row.names = FALSE)), collapse = "\n"))
    )
  )
}

nm_code_diff <- function(x, y, context = 2L) {
  if (!length(x) || !length(y)) return("Both control streams must be available.")
  if (identical(x, y)) return("Control streams are identical.")
  n <- length(x)
  m <- length(y)
  if (n * as.double(m) > 6e5) return("Control streams exceed the line comparison limit. Open the Source view to inspect them.")
  score <- matrix(0L, n + 1L, m + 1L)
  for (i in rev(seq_len(n))) for (j in rev(seq_len(m))) {
    score[i, j] <- if (x[i] == y[j]) 1L + score[i + 1L, j + 1L] else max(score[i + 1L, j], score[i, j + 1L])
  }
  rows <- character(n + m)
  i <- j <- 1L
  k <- 0L
  while (i <= n || j <= m) {
    k <- k + 1L
    if (i <= n && j <= m && x[i] == y[j]) {
      rows[k] <- sprintf("  %4d %4d  %s", i, j, x[i])
      i <- i + 1L
      j <- j + 1L
    } else if (i <= n && (j > m || score[i + 1L, j] >= score[i, j + 1L])) {
      rows[k] <- sprintf("- %4d       %s", i, x[i])
      i <- i + 1L
    } else {
      rows[k] <- sprintf("+      %4d  %s", j, y[j])
      j <- j + 1L
    }
  }
  rows <- rows[seq_len(k)]
  hit <- which(substr(rows, 1L, 1L) %in% c("-", "+"))
  keep <- sort(unique(unlist(lapply(hit, function(i) seq.int(max(1L, i - context), min(k, i + context))))))
  out <- ifelse(c(FALSE, diff(keep) > 1L), paste0("  ...\n", rows[keep]), rows[keep])
  paste(c("    Ref Comp", out), collapse = "\n")
}

nm_parent_graph <- function(models, overrides = list()) {
  ids <- vapply(models, function(m) m$id, "")
  stem <- sub(":p[0-9]+$", "", basename(ids))
  parent <- stats::setNames(vapply(models, function(m) {
    explicit <- overrides[[m$id]]
    if (is.null(explicit)) explicit <- m$review_context$parent
    if (!length(explicit) || is.na(explicit) || !nzchar(explicit)) return("")
    hit <- which(ids == explicit)
    if (!length(hit)) hit <- which(stem == explicit)
    if (length(hit) == 1L) ids[[hit]] else paste0("?", explicit)
  }, ""), ids)
  cycles <- vapply(ids, function(id) {
    seen <- character()
    next_id <- id
    while (next_id %in% ids) {
      if (next_id %in% seen) return(TRUE)
      seen <- c(seen, next_id)
      next_id <- parent[[next_id]]
    }
    FALSE
  }, logical(1))
  list(parent = parent, cycles = ids[cycles])
}

nm_tree_output <- function(models, overrides) {
  graph <- nm_parent_graph(models, overrides)
  parent <- graph$parent
  valid <- setdiff(names(parent), graph$cycles)
  children <- function(id) valid[parent[valid] == id]
  node <- function(id) {
    shiny::tags$li(shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary",
      id, "data-run-id" = id, onclick = "Shiny.setInputValue('tree_run', this.dataset.runId, {priority: 'event'});"),
      if (startsWith(parent[[id]], "?")) shiny::span(class = "muted", paste(" Unresolved parent", substring(parent[[id]], 2))),
      if (length(children(id))) shiny::tags$ul(lapply(children(id), node)))
  }
  shiny::tagList(shiny::tags$ul(class = "model-tree", lapply(valid[!parent[valid] %in% valid], node)),
    if (length(graph$cycles)) shiny::p(paste("Cycles in parent declarations", paste(graph$cycles, collapse = ", "), "\u00b7 Runs remain available in List.")))
}

nm_review_panel_server <- function(input, output, session, project, reference, candidate, safe_action) {
  preferences <- shiny::reactiveVal(list())
  parents <- shiny::reactiveVal(list())
  sensitivity <- shiny::reactiveVal(list())
  shiny::observeEvent(project(), {
    saved <- project()$view
    preferences(saved$aic %||% list())
    parents(saved$parents %||% list())
    sensitivity(saved$sensitivity %||% list())
    shiny::updateSelectizeInput(session, "sensitivity_runs", choices = vapply(project()$models, function(m) m$id, ""), selected = names(saved$sensitivity))
  }, priority = 10)
  pair_key <- function() {
    paste(reference()$id, reference()$review$number, candidate()$id, candidate()$review$number, sep = "|", collapse = "|")
  }
  active <- shiny::reactive({
    x <- preferences()
    if (identical(x$pair, pair_key())) x else list()
  })
  shiny::observeEvent(input$edit_aic, {
    shiny::req(reference(), candidate())
    x <- active()
    shiny::showModal(shiny::modalDialog(title = "AIC comparison settings",
      shiny::p("Supply counts only when the estimated independent parameters cannot be established from the files."),
      shiny::numericInput("reference_k", "Reference free parameters", x$reference_k %||% NA_real_, min = 0, step = 1),
      shiny::numericInput("candidate_k", "Comparison free parameters", x$candidate_k %||% NA_real_, min = 0, step = 1),
      shiny::checkboxInput("confirm_aic", "Confirm matching fitted data, response scale and likelihood when evidence is incomplete", isTRUE(x$assume)),
      shiny::textInput("aic_note", "Basis for the count or comparison assumption", value = x$note %||% ""),
      footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton("apply_aic", "Apply settings", class = "btn-primary"))))
  })
  shiny::observeEvent(input$apply_aic, safe_action({
    counts <- c(input$reference_k, input$candidate_k)
    set <- counts[!is.na(counts)]
    if (any(!is.finite(set) | set < 0 | set != floor(set))) stop("Enter whole non-negative counts.")
    if ((length(set) || isTRUE(input$confirm_aic)) && !nzchar(trimws(input$aic_note %or% ""))) stop("Record the basis for the count or assumption.")
    preferences(list(pair = pair_key(), reference_k = input$reference_k, candidate_k = input$candidate_k, assume = input$confirm_aic, note = input$aic_note))
    shiny::removeModal()
  }))
  output$review_summary <- shiny::renderUI({
    shiny::req(reference(), candidate())
    pref <- active()
    a <- nm_model_aic(reference(), pref$reference_k)
    b <- nm_model_aic(candidate(), pref$candidate_k)
    d <- nm_aic_comparison(reference(), candidate(), a, b, pref$assume)
    summary <- function(model, metric, label) shiny::div(class = "run-summary",
      shiny::strong(label), shiny::span(paste(model$status %or% "Unknown", "\u00b7 Covariance", covariance_label(model$covariance))),
      shiny::span(paste("OFV", display_number(model$ofv, 6), "\u00b7 AIC", if (is.finite(metric$value)) paste0(display_number(metric$value, 6), " (k=", metric$k, if (metric$manual) ", supplied" else "", ")") else "unavailable")),
      shiny::span(nm_shrinkage_label(model)),
      if (nzchar(metric$reason)) shiny::tags$small(metric$reason))
    shiny::div(class = "review-card",
      shiny::div(class = "plot-pair", summary(reference(), a, "Reference"), summary(candidate(), b, "Comparison")),
      shiny::div(class = "action-row",
        shiny::strong(paste("Delta AIC (comparison - reference)", if (is.finite(d$delta)) display_number(d$delta, 6) else "withheld")),
        shiny::span(class = "muted", d$reason), shiny::actionButton("edit_aic", "AIC settings")),
      shiny::tags$details(shiny::tags$summary("AIC calculation and evidence"),
        shiny::p("AIC on the NONMEM objective scale is OFV + 2 \u00d7 the number of independent estimated population parameters."),
        lapply(list(Reference = list(reference(), a), Comparison = list(candidate(), b)), function(x) {
          m <- x[[1L]]
          v <- x[[2L]]
          shiny::p(paste(m$id, m$review$method %or% "", paste(names(v$counts), v$counts, collapse = ", "),
            if (length(m$review$source)) paste("| EXT", m$review$source, "line", m$review$final_line) else "",
            "| Fitted input", m$review_context$data_source %or% "Unknown", "| Likelihood", m$likelihood %or% "Unknown", "| Response scale", m$dv_scale %or% "Unknown"))
        }),
        if (length(pref$note)) shiny::p(paste("Recorded basis", pref$note))))
  })
  output$code_diff <- shiny::renderUI({
    shiny::req(reference(), candidate())
    shiny::tagList(shiny::p(class = "muted", paste(source_label(reference()$source), "\u2192", source_label(candidate()$source))),
      shiny::tags$pre(class = "code-diff", nm_code_diff(reference()$control_code, candidate()$control_code)),
      shiny::tags$details(shiny::tags$summary("Source control streams"), shiny::div(class = "plot-pair",
        shiny::tags$pre(paste(reference()$control_code, collapse = "\n")),
        shiny::tags$pre(paste(candidate()$control_code, collapse = "\n")))))
  })
  output$run_tree <- shiny::renderUI({
    shiny::req(project())
    ids <- vapply(project()$models, function(m) m$id, "")
    chosen <- input$tree_run %or% ""
    shiny::tagList(shiny::p(if (chosen %in% ids) paste("Selected", chosen) else "Select a run in the tree to assign it."),
      nm_tree_output(project()$models, parents()))
  })
  shiny::observeEvent(input$tree_edit, {
    shiny::req(project())
    ids <- vapply(project()$models, function(m) m$id, "")
    shiny::showModal(shiny::modalDialog(title = "Set model parent",
      shiny::selectInput("tree_child", "Child run", ids, selected = input$tree_run),
      shiny::selectInput("tree_parent", "Parent run", c("No parent" = "", ids)),
      footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton("tree_apply", "Save parent"))))
  })
  shiny::observeEvent(input$tree_apply, safe_action({
    p <- parents()
    p[[input$tree_child]] <- input$tree_parent
    graph <- nm_parent_graph(project()$models, p)
    if (input$tree_child %in% graph$cycles) stop("This parent would create a cycle.")
    parents(p)
    shiny::removeModal()
  }))
  shiny::observeEvent(input$save_sensitivity_note, {
    shiny::req(input$sensitivity_note_run)
    x <- sensitivity()
    x[[input$sensitivity_note_run]] <- input$sensitivity_note %or% ""
    sensitivity(x)
  })
  shiny::observeEvent(input$sensitivity_runs, {
    shiny::updateSelectInput(session, "sensitivity_note_run", choices = input$sensitivity_runs)
  }, ignoreNULL = FALSE)
  shiny::observeEvent(input$sensitivity_note_run, shiny::updateTextInput(session, "sensitivity_note", value = sensitivity()[[input$sensitivity_note_run]] %or% ""))
  output$sensitivity_table <- DT::renderDT({
    shiny::req(project(), reference())
    ids <- unique(c(reference()$id, input$sensitivity_runs))
    all <- project()$models
    lookup <- vapply(all, function(m) m$id, "")
    rows <- lapply(ids[ids %in% lookup], function(id) {
      m <- if (id == reference()$id) reference() else if (id == candidate()$id) candidate() else all[[match(id, lookup)]]
      a <- nm_model_aic(m)
      delta <- if (id == reference()$id) list(delta = NA_real_, reason = "Reference") else nm_aic_comparison(reference(), m, nm_model_aic(reference()), a)
      data.frame(Run = id, Change = sensitivity()[[id]] %or% "", Subjects = m$nind, Observations = m$nobs,
        Status = m$status, OFV = m$ofv, AIC = a$value, Delta.AIC = delta$delta, Comparison = delta$reason,
        Shrinkage = nm_shrinkage_label(m), stringsAsFactors = FALSE)
    })
    review_datatable(do.call(rbind, rows), select = "none")
  })
  output$sensitivity_parameters <- DT::renderDT({
    shiny::req(project(), reference())
    ids <- unique(c(reference()$id, input$sensitivity_runs))
    rows <- lapply(ids, function(id) {
      m <- if (id == reference()$id) reference() else if (id == candidate()$id) candidate() else {
        hit <- Filter(function(x) identical(x$id, id), project()$models)
        if (length(hit)) hit[[1L]] else NULL
      }
      d <- m$parameters
      if (!NROW(d)) return(NULL)
      data.frame(Run = id, Parameter = d$parameter, Estimate = d$estimate, RSE = d$rse, Fixed = d$fixed)
    })
    review_datatable(do.call(rbind, rows) %||% data.frame(), select = "none")
  })
  function(p) {
    notes <- sensitivity()
    for (id in input$sensitivity_runs) if (is.null(notes[[id]])) notes[[id]] <- ""
    p$view$aic <- preferences()
    p$view$parents <- parents()
    p$view$sensitivity <- notes
    p
  }
}
