# Read-only exploration of declared NONMEM tables.

nm_subject_key <- function(d) {
  id <- nm_pick_col(d, c("ID", "USUBJID", "SUBJID"))
  if (!length(id)) return(rep(NA_character_, nrow(d)))
  id <- as.character(id)
  study <- nm_pick_col(d, c("STUDY", "STUDYID"))
  if (!length(study)) return(id)
  study <- as.character(study)
  ifelse(is.na(id) | is.na(study), NA_character_, paste0(nchar(study), ":", study, id))
}

nm_subject_choices <- function(d) {
  key <- nm_subject_key(d)
  id <- nm_pick_col(d, c("ID", "USUBJID", "SUBJID"))
  study <- nm_pick_col(d, c("STUDY", "STUDYID"))
  label <- if (length(study)) paste(study, id, sep = " / ") else as.character(id)
  keep <- !is.na(key) & !duplicated(key)
  stats::setNames(key[keep], label[keep])
}

nm_inspector_filter <- function(d, ids = character(), filters = list()) {
  keep <- if (length(ids)) nm_subject_key(d) %in% ids else rep(TRUE, nrow(d))
  for (f in filters) {
    if (!f$column %in% names(d)) stop("Filter column unavailable in the selected table")
    v <- d[[f$column]]
    a <- f$value
    if (f$operator %in% c("range", "ge", "le") && (!is.numeric(v) || any(!is.finite(a)))) stop("A numeric filter requires finite numeric bounds")
    test <- switch(f$operator, missing = is.na(v), present = !is.na(v),
      eq = !is.na(v) & as.character(v) %in% as.character(a),
      ne = !is.na(v) & !as.character(v) %in% as.character(a),
      range = v >= a[1L] & v <= a[2L], ge = v >= a[1L], le = v <= a[1L],
      stop("Unsupported filter operator"))
    keep <- keep & !is.na(test) & test
  }
  d[keep, , drop = FALSE]
}

nm_inspector_join <- function(primary, other, mode) {
  a <- nm_subject_key(primary)
  b <- nm_subject_key(other)
  if (anyNA(a) || anyNA(b)) stop("A join requires non-missing subject keys")
  if (xor(any(c("STUDY", "STUDYID") %in% names(primary)), any(c("STUDY", "STUDYID") %in% names(other)))) stop("Study identifier is available in only one table")
  if (mode == "record") {
    if (!"ROW" %in% names(primary) || !"ROW" %in% names(other) || anyNA(primary$ROW) ||
        anyNA(other$ROW) || anyDuplicated(primary$ROW) || anyDuplicated(other$ROW)) stop("A record join requires unique, complete ROW keys")
    index <- match(as.character(primary$ROW), as.character(other$ROW))
    if (anyNA(index) || any(a != b[index])) stop("Record and subject identities do not match")
  } else {
    index <- match(a, b)
    if (anyDuplicated(b) || anyNA(index)) stop("The joined table must have one matching row per subject")
  }
  common <- setdiff(intersect(names(primary), names(other)), c(".source_line", ".source_file"))
  for (column in common) {
    x <- as.character(primary[[column]])
    y <- as.character(other[[column]][index])
    if (any(xor(is.na(x), is.na(y))) || any(x != y, na.rm = TRUE)) {
      label <- make.unique(c(names(primary), paste0(column, " (joined)")))[ncol(primary) + 1L]
      primary[[label]] <- other[[column]][index]
    }
  }
  for (column in setdiff(names(other), names(primary))) primary[[column]] <- other[[column]][index]
  primary$.joined_source <- other$.source_file[index]
  primary$.joined_line <- other$.source_line[index]
  primary
}

nm_inspector_sections <- function(model, path) {
  if (!nzchar(path)) return(list(model$data %||% data.frame()))
  if (!is.null(model$stored_tables[[path]])) return(model$stored_tables[[path]])
  if (!path %in% model$table_catalog || !length(model$live_root)) return(list())
  root <- normalizePath(model$live_root, winslash = "/", mustWork = FALSE)
  file <- normalizePath(file.path(root, path), winslash = "/", mustWork = FALSE)
  if (!startsWith(tolower(file), paste0(tolower(root), "/")) || !file.exists(file)) return(list())
  lapply(nm_read_table_file(file), function(d) {
    d$.source_file <- path
    d
  })
}

nm_inspector_eta <- function(d, columns) {
  key <- nm_subject_key(d)
  if (anyNA(key)) stop("One row per subject requires identified subjects")
  first <- match(key, key)
  for (column in columns) {
    x <- d[[column]]
    if (any(xor(is.na(x), is.na(x[first]))) || any(x != x[first], na.rm = TRUE))
      stop("Selected values vary within subject. Filter to one occasion or use record-level plots.")
  }
  d[!duplicated(key), , drop = FALSE]
}

nm_inspector_ui <- function(card) {
  tag <- shiny::tags
  shiny::tagList(
    card("Data Inspector", shiny::uiOutput("inspect_models"),
      shiny::div(class = "comparison-toolbar",
        shiny::selectInput("inspect_target", "Target", c("Comparison", "Reference", "Both")),
        shiny::selectInput("inspect_x", "X axis", character()),
        shiny::selectInput("inspect_y", "Y axis", character()),
        shiny::selectizeInput("inspect_ids", "Subjects", choices = NULL, multiple = TRUE, options = list(placeholder = "All subjects"))
      ),
      shiny::div(class = "action-row", shiny::actionButton("inspect_filter_open", "Add filter"),
        shiny::actionButton("inspect_reset", "Reset filters"), shiny::uiOutput("inspect_status", inline = TRUE)),
      tag$details(tag$summary("Plot options"),
        shiny::div(class = "comparison-toolbar",
          shiny::selectInput("inspect_preset", "Preset", c("Custom", "DV vs PRED", "DV vs IPRED", "CWRES vs TIME", "CWRES vs TAD", "CWRES vs PRED", "ETA distribution", "ETA Q-Q")),
          shiny::selectInput("inspect_type", "Plot", c("Scatter", "Profile", "Histogram", "Q-Q", "Boxplot")),
          shiny::selectInput("inspect_color", "Color by", character(), selectize = FALSE),
          shiny::selectInput("inspect_facet", "Stratify by", character(), selectize = FALSE)
        ),
        shiny::div(class = "action-row",
          shiny::checkboxInput("inspect_log_x", "Log X"), shiny::checkboxInput("inspect_log_y", "Log Y"),
          shiny::checkboxInput("inspect_obs", "Observation rows only"),
          shiny::checkboxInput("inspect_subject", "One row per subject"))
      ),
      tag$details(tag$summary("Source tables"),
        shiny::div(class = "comparison-toolbar",
          shiny::selectInput("inspect_table", "Table", character(), selectize = FALSE),
          shiny::selectInput("inspect_section", "Table section", "1"),
          shiny::selectInput("inspect_join", "Join table (single target)", character(), selectize = FALSE),
          shiny::selectInput("inspect_join_mode", "Join key", c("ROW + subject" = "record", "Subject (one row in joined table)" = "subject"))
        ),
        shiny::p(class = "muted", "STUDY + ID identifies subjects when STUDY is present. Table sections are separate from estimation stages.")
      ),
      shiny::plotOutput("inspect_plot", height = "380px", click = "inspect_click")
    ),
    card("Selected record", shiny::uiOutput("inspect_record"), shiny::plotOutput("inspect_profile", height = "230px")),
    tag$details(tag$summary("Data quality and displayed rows"), card("Source table quality", DT::DTOutput("inspect_quality")),
      card("Displayed rows", DT::DTOutput("inspect_rows")))
  )
}

nm_inspector_server <- function(input, output, session, project, reference, candidate, safe_action) {
  filters <- shiny::reactiveVal(list())
  record <- shiny::reactiveVal(NULL)
  restore <- shiny::reactiveVal(NULL)
  restored_axes <- shiny::reactiveVal(FALSE)
  cache <- new.env(parent = emptyenv())
  models <- shiny::reactive(switch(input$inspect_target %or% "Comparison",
    Reference = list(Reference = reference()), Both = list(Reference = reference(), Comparison = candidate()),
    list(Comparison = candidate())))
  sections <- function(model, path) {
    if (!nzchar(path)) return(nm_inspector_sections(model, path))
    file <- file.path(model$live_root %||% "", path)
    info <- file.info(file)
    key <- paste(model$id, path, info$size, as.numeric(info$mtime), sep = "|")
    if (!exists(key, cache, inherits = FALSE)) {
      if (length(ls(cache)) >= 8L) rm(list = ls(cache), envir = cache)
      assign(key, nm_inspector_sections(model, path), envir = cache)
    }
    get(key, cache, inherits = FALSE)
  }
  shiny::observeEvent(project(), {
    rm(list = ls(cache), envir = cache)
    record(NULL)
    saved <- project()$view$inspector
    restore(saved)
    restored_axes(FALSE)
    filters(saved$filters %||% list())
    shiny::updateSelectInput(session, "inspect_target", selected = saved$target %||% "Comparison")
  }, priority = 30)
  shiny::observeEvent(models(), {
    m <- models()
    catalog <- if (length(m) == 1L) m[[1L]]$table_catalog else character()
    paths <- stats::setNames(catalog, catalog)
    saved <- restore()
    shiny::updateSelectInput(session, "inspect_table", choices = c("Diagnostic table" = "", paths), selected = saved$table %||% "")
    shiny::updateSelectInput(session, "inspect_join", choices = c("None" = "", paths), selected = saved$join %||% "")
    shiny::updateSelectInput(session, "inspect_join_mode", selected = saved$join_mode %||% "record")
  }, priority = 20)
  paths <- shiny::reactive({
    ms <- models()
    if (length(ms) != 1L) return(c(table = "", join = ""))
    allowed <- function(x) if (length(x) && x %in% ms[[1L]]$table_catalog) x else ""
    c(table = allowed(input$inspect_table), join = allowed(input$inspect_join))
  })
  primary <- shiny::reactive(lapply(models(), sections, path = paths()[["table"]]))
  shiny::observeEvent(primary(), {
    sets <- primary()
    n <- if (length(sets) == 1L) length(sets[[1L]]) else 1L
    shiny::updateSelectInput(session, "inspect_section", choices = seq_len(n), selected = restore()$section %||% "1")
  }, priority = 10)
  data <- shiny::reactive({
    sets <- primary()
    index <- suppressWarnings(as.integer(input$inspect_section %or% "1"))
    if (length(sets) > 1L || is.na(index)) index <- 1L
    d <- lapply(sets, function(s) if (index %in% seq_along(s)) s[[index]] else data.frame())
    p <- paths()
    if (length(d) == 1L && nzchar(p[["join"]]) && p[["join"]] != p[["table"]]) {
      other <- sections(models()[[1L]], p[["join"]])
      shiny::validate(shiny::need(length(other) == 1L, "A join requires one identified auxiliary table section."))
      d[[1L]] <- tryCatch(nm_inspector_join(d[[1L]], other[[1L]], input$inspect_join_mode %or% "record"),
        error = function(e) shiny::validate(shiny::need(FALSE, conditionMessage(e))))
    }
    d
  })
  cols <- shiny::reactive(Reduce(intersect, lapply(data(), function(d) names(d)[!startsWith(names(d), ".")])) %||% character())
  shiny::observeEvent(data(), {
    d <- data()
    n <- cols()
    saved <- restore()
    use_saved <- length(saved) && length(n) && any(vapply(d, nrow, integer(1)) > 0L) && !restored_axes() &&
      identical(input$inspect_target %or% "Comparison", saved$target %or% "Comparison") &&
      identical(unname(paths()[["table"]]), saved$table %or% "") &&
      identical(unname(paths()[["join"]]), saved$join %or% "") &&
      identical(input$inspect_section %or% "1", saved$section %or% "1")
    if (!use_saved) saved <- NULL
    pick <- function(current, defaults) {
      hit <- intersect(c(current, defaults, n), n)
      if (length(hit)) hit[[1L]] else ""
    }
    shiny::updateSelectInput(session, "inspect_x", choices = n, selected = pick(saved$x %||% input$inspect_x, c("PRED", "TIME")))
    shiny::updateSelectInput(session, "inspect_y", choices = n, selected = pick(saved$y %||% input$inspect_y, c("DV", "CWRES")))
    subjects <- lapply(d, nm_subject_choices)
    choices <- subjects[[1L]]
    if (length(subjects) > 1L) choices <- choices[choices %in% subjects[[2L]]]
    shiny::updateSelectizeInput(session, "inspect_ids", choices = choices, selected = intersect(saved$ids %||% input$inspect_ids, choices), server = TRUE)
    options <- c("None" = "", stats::setNames(n, n))
    for (k in c("color", "facet")) shiny::updateSelectInput(session, paste0("inspect_", k), choices = options, selected = saved[[k]] %||% input[[paste0("inspect_", k)]] %||% "")
    if (use_saved) {
      shiny::updateSelectInput(session, "inspect_type", selected = saved$type %||% "Scatter")
      for (k in c("log_x", "log_y", "obs", "subject")) shiny::updateCheckboxInput(session, paste0("inspect_", k), value = isTRUE(saved[[k]]))
      filters(saved$filters %||% list())
      restored_axes(TRUE)
      restore(NULL)
    }
    if (!length(restore())) filters(Filter(function(f) f$column %in% n, filters()))
    record(NULL)
  })
  shiny::observeEvent(input$inspect_preset, {
    p <- input$inspect_preset
    if (p == "Custom") return()
    eta <- startsWith(p, "ETA")
    axes <- if (eta) grep("^ETA\\(?[0-9]+\\)?$", cols(), ignore.case = TRUE, value = TRUE)[1L] else strsplit(p, " vs ", fixed = TRUE)[[1L]]
    if (anyNA(axes) || !all(axes %in% cols())) return(shiny::showNotification("Preset columns are unavailable in this table.", type = "warning"))
    shiny::updateSelectInput(session, "inspect_x", selected = if (eta) axes[1L] else axes[2L])
    if (!eta) shiny::updateSelectInput(session, "inspect_y", selected = axes[1L])
    shiny::updateSelectInput(session, "inspect_type", selected = if (!eta) "Scatter" else if (p == "ETA Q-Q") "Q-Q" else "Histogram")
    shiny::updateCheckboxInput(session, "inspect_subject", value = eta)
    shiny::updateCheckboxInput(session, "inspect_obs", value = !eta)
  })
  operators <- c("Equals / in list" = "eq", "Not in list" = "ne", "Between" = "range",
    "At least" = "ge", "At most" = "le", "Missing" = "missing", "Present" = "present")
  shiny::observeEvent(input$inspect_filter_open, {
    shiny::req(length(cols()))
    shiny::showModal(shiny::modalDialog(title = "Filter displayed rows", fade = FALSE,
      shiny::selectInput("inspect_filter_col", "Column", cols(), selectize = FALSE),
      shiny::selectInput("inspect_filter_op", "Operator", operators, selectize = FALSE),
      shiny::uiOutput("inspect_filter_values"),
      footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton("inspect_apply", "Apply filter", class = "btn-primary"))))
  })
  output$inspect_filter_values <- shiny::renderUI({
    shiny::req(input$inspect_filter_col, input$inspect_filter_op)
    op <- input$inspect_filter_op
    if (op %in% c("missing", "present")) return(NULL)
    if (op %in% c("range", "ge", "le")) return(shiny::tagList(
      shiny::numericInput("inspect_value1", if (op == "range") "Lower bound" else "Value", NA),
      if (op == "range") shiny::numericInput("inspect_value2", "Upper bound", NA)))
    shiny::selectizeInput("inspect_values", "Values", choices = NULL, multiple = TRUE)
  })
  shiny::observeEvent(list(input$inspect_filter_col, input$inspect_filter_op), {
    shiny::req(input$inspect_filter_col, input$inspect_filter_op %in% c("eq", "ne"))
    v <- unique(unlist(lapply(data(), function(d) as.character(d[[input$inspect_filter_col]])), use.names = FALSE))
    shiny::updateSelectizeInput(session, "inspect_values", choices = sort(v[!is.na(v)]), server = TRUE, selected = character())
  })
  shiny::observeEvent(input$inspect_apply, safe_action({
    col <- input$inspect_filter_col
    op <- input$inspect_filter_op
    shiny::req(col %in% cols(), op %in% operators)
    value <- switch(op, range = c(input$inspect_value1, input$inspect_value2),
      ge = input$inspect_value1, le = input$inspect_value1, input$inspect_values)
    if (op %in% c("range", "ge", "le") && (length(value) != if (op == "range") 2L else 1L)) stop("Enter the filter bounds.")
    if (op %in% c("eq", "ne") && !length(value)) stop("Select at least one value.")
    if (op == "range" && all(is.finite(value)) && value[1L] > value[2L]) stop("Lower bound exceeds upper bound.")
    f <- list(column = col, operator = op, value = value)
    invisible(lapply(data(), nm_inspector_filter, filters = list(f)))
    filters(c(filters(), list(f)))
    record(NULL)
    shiny::removeModal()
  }))
  shiny::observeEvent(input$inspect_reset, {
    filters(list())
    record(NULL)
    shiny::updateSelectizeInput(session, "inspect_ids", selected = character())
  })
  shiny::observeEvent(list(input$inspect_ids, input$inspect_obs), record(NULL))
  filtered <- shiny::reactive(lapply(data(), function(d) {
    if (isTRUE(input$inspect_obs)) d <- observation_data(list(data = d))
    nm_inspector_filter(d, input$inspect_ids %||% character(), filters())
  }))
  output$inspect_models <- shiny::renderUI(shiny::p(class = "muted", paste(vapply(models(), function(m) m$id %||% "No model selected", ""), collapse = " / ")))
  output$inspect_status <- shiny::renderUI({
    f <- filtered()
    n <- vapply(f, nrow, integer(1))
    msg <- if (!sum(vapply(data(), nrow, integer(1)))) "Observation data unavailable for this model or archive." else paste(paste0(names(n), " ", n, " rows"), collapse = " \u00b7 ")
    chip <- lapply(filters(), function(x) shiny::span(class = "filter-chip",
      paste(x$column, names(operators)[match(x$operator, operators)], paste(x$value, collapse = ", "))))
    show <- any(grepl("^(ETA|IPRED|IWRES)", c(input$inspect_x, input$inspect_y), ignore.case = TRUE))
    shiny::tagList(shiny::span(msg, class = "muted"), chip,
      if (show) shiny::tags$small(paste(vapply(models(), nm_shrinkage_label, ""), collapse = " \u00b7 ")))
  })
  points <- shiny::reactive({
    x <- input$inspect_x
    y <- input$inspect_y
    type <- input$inspect_type %or% "Scatter"
    one <- type %in% c("Histogram", "Q-Q")
    shiny::validate(shiny::need(length(x) && x %in% cols() && (one || (length(y) && y %in% cols())), "Choose available axes."))
    sets <- filtered()
    scales <- vapply(models(), function(m) m$dv_scale %||% NA_character_, "")
    response_axis <- any(c(x, y) %in% c("DV", "PRED", "IPRED", "CPRED"))
    shiny::validate(shiny::need(!response_axis || length(unique(scales[!is.na(scales)])) <= 1L, "Response scales differ. Select a single model."))
    numeric_axis <- function(col) vapply(sets, function(d) is.numeric(d[[col]]), logical(1))
    shiny::validate(shiny::need(length(unique(numeric_axis(x))) == 1L && (one || length(unique(numeric_axis(y))) == 1L), "Axis types differ between the selected tables."))
    frames <- lapply(names(sets), function(role) {
      d <- sets[[role]]
      if (!nrow(d)) return(NULL)
      color <- input$inspect_color %or% ""
      facet <- input$inspect_facet %or% ""
      subject_summary <- isTRUE(input$inspect_subject) || (one && grepl("^ETA\\(?[0-9]+", x, ignore.case = TRUE))
      if (subject_summary) d <- tryCatch(nm_inspector_eta(d, unique(intersect(c(x, if (!one) y, color, facet), names(d)))),
        error = function(e) shiny::validate(shiny::need(FALSE, conditionMessage(e))))
      key <- nm_subject_key(d)
      key[is.na(key)] <- paste0("unidentified-row-", which(is.na(key)))
      data.frame(.x = d[[x]], .y = if (one) NA_real_ else d[[y]], .Run = role,
        .group = paste(key, d$DVID %||% "", d$OCC %||% ""),
        .color = if (color %in% names(d)) as.character(d[[color]]) else "All",
        .stratum = if (facet %in% names(d)) as.character(d[[facet]]) else "All",
        .row = d$.source_line %||% seq_len(nrow(d)), .file = d$.source_file %||% "", stringsAsFactors = FALSE)
    })
    d <- do.call(rbind, frames)
    shiny::validate(shiny::need(NROW(d) > 0L, "No rows match these filters."),
      shiny::need(length(unique(d$.stratum)) <= 12L, "Choose a stratum with 12 or fewer levels."),
      shiny::need(length(unique(d$.color)) <= 30L, "Choose a color variable with 30 or fewer levels."))
    px <- isTRUE(input$inspect_log_x)
    py <- !one && isTRUE(input$inspect_log_y)
    shiny::validate(shiny::need(!px || is.numeric(d$.x), "Log X requires a numeric axis."),
      shiny::need(!py || is.numeric(d$.y), "Log Y requires a numeric axis."),
      shiny::need(!one || is.numeric(d$.x), "This plot requires a numeric X axis."),
      shiny::need(type != "Boxplot" || is.numeric(d$.y), "Boxplot requires a numeric Y axis."),
      shiny::need(type != "Profile" || (is.numeric(d$.x) && is.numeric(d$.y)), "Profile requires numeric axes."))
    valid <- function(v) if (is.numeric(v)) is.finite(v) else !is.na(v)
    keep <- valid(d$.x)
    if (!one) keep <- keep & valid(d$.y)
    if (px) keep <- keep & d$.x > 0
    if (py) keep <- keep & d$.y > 0
    shown <- d[keep, , drop = FALSE]
    if (px && one) shown$.x <- log10(shown$.x)
    attr(shown, "invalid") <- sum(!keep)
    shown
  })
  output$inspect_plot <- shiny::renderPlot({
    d <- points()
    shiny::validate(shiny::need(nrow(d) > 0L, "No finite values are available for these axes."))
    type <- input$inspect_type %or% "Scatter"
    x <- input$inspect_x
    y <- input$inspect_y
    if (type %in% c("Histogram", "Q-Q") && isTRUE(input$inspect_log_x)) x <- paste("log10", x)
    if (type == "Histogram") {
      g <- ggplot2::ggplot(d, ggplot2::aes(x = .data$.x)) + ggplot2::geom_histogram(bins = 25, fill = "#006c67")
      y <- "Count"
    } else if (type == "Q-Q") {
      g <- ggplot2::ggplot(d, ggplot2::aes(sample = .data$.x)) + ggplot2::stat_qq() + ggplot2::stat_qq_line()
      y <- x
      x <- "Normal quantile"
    } else {
      g <- ggplot2::ggplot(d, ggplot2::aes(x = .data$.x, y = .data$.y, colour = .data$.color))
      if (type == "Boxplot") g <- g + ggplot2::geom_boxplot(ggplot2::aes(group = .data$.x))
      else {
        g <- g + ggplot2::geom_point(alpha = 0.55, size = 1.3)
        if (type == "Profile") g <- g + ggplot2::geom_line(ggplot2::aes(group = .data$.group), alpha = 0.35)
      }
      if (isTRUE(input$inspect_log_x)) g <- g + ggplot2::scale_x_log10()
      if (isTRUE(input$inspect_log_y)) g <- g + ggplot2::scale_y_log10()
    }
    if (length(unique(d$.Run)) > 1L || length(unique(d$.stratum)) > 1L) g <- g + ggplot2::facet_grid(rows = ggplot2::vars(.data$.stratum), cols = ggplot2::vars(.data$.Run))
    if (length(unique(d$.color)) == 1L) g <- g + ggplot2::guides(colour = "none")
    g + ggplot2::labs(x = x, y = y, colour = input$inspect_color,
      caption = paste(attr(d, "invalid"), "rows excluded by missing, non-finite or log-axis values.",
        if (length(models()) > 1L) "Shared axes use the selected column names. Check units in each source." else "")) + plot_theme()
  }, res = 105, alt = "Inspector plot for the selected axes, subjects and row filters.")
  output$inspect_quality <- DT::renderDT({
    rows <- lapply(names(data()), function(role) {
      d <- data()[[role]]
      m <- models()[[role]]
      key <- nm_subject_key(d)
      row <- nm_pick_col(d, "ROW")
      blq <- nm_pick_col(d, c("BLQ", "BQL"))
      counts <- c(nrow(d), length(unique(stats::na.omit(key))), sum(is.na(key)),
        if (length(row)) sum(duplicated(stats::na.omit(row))) else NA_integer_,
        if ("DV" %in% names(d)) sum(is.na(d$DV)) else NA_integer_,
        if ("EVID" %in% names(d)) sum(nm_num(d$EVID) != 0, na.rm = TRUE) else NA_integer_,
        if (length(blq)) sum(tolower(as.character(blq)) %in% c("1", "yes", "true", "blq", "bql")) else NA_integer_,
        m$reported_nind %||% NA_integer_, m$reported_nobs %||% NA_integer_)
      labels <- c("Table rows", "Table subjects", "Missing subject", "Duplicate ROW", "Missing DV", "Events", "BLQ flagged", "Listing subjects", "Listing observations")
      data.frame(Target = role, as.list(stats::setNames(counts, labels)), check.names = FALSE)
    })
    review_datatable(do.call(rbind, rows), select = "none")
  })
  displayed <- shiny::reactive({
    ds <- filtered()
    common <- Reduce(intersect, lapply(ds, names))
    do.call(rbind, lapply(names(ds), function(role) {
      d <- ds[[role]][common]
      d$.Target <- rep(role, nrow(d))
      d[c(".Target", common)]
    }))
  })
  output$inspect_rows <- DT::renderDT(review_datatable(displayed(), rows = 10))
  shiny::observeEvent(input$inspect_rows_rows_selected, {
    i <- input$inspect_rows_rows_selected[1L]
    if (i <= nrow(displayed())) record(displayed()[i, , drop = FALSE])
  })
  shiny::observeEvent(input$inspect_click, {
    if (!input$inspect_type %in% c("Scatter", "Profile") || length(models()) != 1L ||
        nzchar(input$inspect_facet %or% "") || isTRUE(input$inspect_log_x) || isTRUE(input$inspect_log_y)) return()
    d <- points()
    if (!is.numeric(d$.x) || !is.numeric(d$.y)) return()
    hit <- shiny::nearPoints(d, input$inspect_click, xvar = ".x", yvar = ".y", maxpoints = 1)
    if (nrow(hit)) {
      rows <- displayed()
      i <- which(rows$.source_line == hit$.row[[1L]] & rows$.source_file == hit$.file[[1L]])
      if (length(i) == 1L) record(rows[i, , drop = FALSE])
    }
  })
  output$inspect_record <- shiny::renderUI({
    r <- record()
    if (is.null(r)) {
      if (length(input$inspect_ids) == 1L) return(shiny::p("Individual profile for the selected subject."))
      return(shiny::p("Select one subject, a numeric scatter point, or a displayed row."))
    }
    shiny::p(paste(r$.Target, "|", paste(names(nm_subject_choices(r)), collapse = ""), "|",
      r$.source_file %||% "", "line", r$.source_line %||% "", "| DV", r$DV %||% "Unavailable"))
  })
  output$inspect_profile <- shiny::renderPlot({
    key <- if (!is.null(record())) nm_subject_key(record()) else input$inspect_ids
    shiny::validate(shiny::need(length(key) == 1L && !is.na(key), "Select one subject to see their profile."))
    d <- do.call(rbind, lapply(names(models()), function(role) {
      a <- observation_data(models()[[role]])
      a <- a[nm_subject_key(a) %in% key, , drop = FALSE]
      if (!nrow(a) || !all(c("TIME", "DV") %in% names(a))) return(NULL)
      data.frame(TIME = nm_num(a$TIME), DV = nm_num(a$DV), IPRED = if ("IPRED" %in% names(a)) nm_num(a$IPRED) else NA_real_,
        Role = role, Series = paste(a$DVID %||% "", a$OCC %||% ""))
    }))
    shiny::validate(shiny::need(NROW(d) > 0L, "No TIME/DV profile is available in the diagnostic table."))
    ggplot2::ggplot(d, ggplot2::aes(x = .data$TIME, y = .data$DV)) +
      ggplot2::geom_point() + ggplot2::geom_line(ggplot2::aes(y = .data$IPRED, group = .data$Series), colour = "#006c67", na.rm = TRUE) +
      ggplot2::facet_wrap(ggplot2::vars(.data$Role)) + ggplot2::labs(y = "DV (points), IPRED (line)") + plot_theme()
  }, res = 105, alt = "Selected subject observations and individual predictions.")
  function(p, include_observations) {
    p$view$inspector <- list(target = input$inspect_target, table = input$inspect_table, section = input$inspect_section,
      join = input$inspect_join, join_mode = input$inspect_join_mode, x = input$inspect_x, y = input$inspect_y,
      ids = if (include_observations) input$inspect_ids else NULL,
      filters = if (include_observations) filters() else list(), color = input$inspect_color, facet = input$inspect_facet,
      type = input$inspect_type, log_x = input$inspect_log_x, log_y = input$inspect_log_y,
      obs = input$inspect_obs, subject = input$inspect_subject)
    if (include_observations && length(models()) == 1L) {
      m <- models()[[1L]]
      i <- which(vapply(p$models, function(z) identical(z$id, m$id), logical(1)))
      selected <- unique(unname(paths()))
      if (length(i) == 1L) for (path in selected[nzchar(selected)]) p$models[[i]]$stored_tables[[path]] <- sections(m, path)
    }
    p
  }
}
