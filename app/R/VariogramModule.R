VariogramUI <- function(id) {
  ns <- NS(id)

  left_panel <- card(
    card_header(strong("Tools"), DataUI(ns("data"))),

    tagList(
      h5(strong("Data Transformations")),
      layout_columns(
        selectInput(ns("response"), "Response Variable", choices = NULL),
        selectInput(ns("resp_apply"), "Apply Func", choices = resp_apply_funcs),
        col_widths = c(7, 5)
      )
    ),
    hr(),

    tagList(
      h5(strong("Fit Variogram Model")),
      selectInput(ns("model"), NULL, choices = vgm_models),
      conditionalPanel(
        condition = sprintf("input['%s'] != 'none'", ns("model")),
        layout_columns(
          numericInput(ns("sill"), "Sill", value = NA, min = 0),
          numericInput(ns("range"), "Range", value = NA, min = 0),
          numericInput(ns("nugget"), "Nugget", value = 0, min = 0)
        )
      )
    ),
    hr(),

    tagList(
      h5(strong("Binning Method")),
      actionLink(
        ns("about_binning"),
        tagList(bs_icon("book"), " About the binning methods")
      ),
      checkboxGroupInput(ns("methods"), NULL, binning_methods, "even"),
      layout_columns(
        numericInput(ns("cutoff"), "Cutoff", value = 0.5, min = 0),
        numericInput(ns("nbins"), "Bins", value = 15, min = 5, step = 1)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'].includes('ward')", ns("methods")),
        selectInput(ns("aggr"), "Aggregation Func", choices = c("mean", "median"))
      )
    )
  )

  right_panel <- card(layout_columns(
    VariogramPlotUI(ns("plot")),
    layout_columns(
      card(card_header(strong("Method Comparison")), DTOutput(ns("summary_table"))),
      card(card_header(strong("Summary")), verbatimTextOutput(ns("summary_text"))),
      col_widths = c(6, 6)
    ),
    col_widths = c(12, 12),
    row_heights = c(8, 4)
  ))

  layout_columns(left_panel, right_panel, col_widths = c(3, 9))
}

VariogramServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session[["ns"]]

    loaded_data <- DataServer("data")
    formula <- reformulate("1", "resp_transformed")

    observeEvent(loaded_data(), {
      num_cols <- NumericCols(loaded_data())
      updateSelectInput(session, "response", choices = num_cols)
    })

    transformed_data <- reactive({
      req(loaded_data(), input[["response"]], input[["resp_apply"]])

      df <- loaded_data()

      validate(need(input[["response"]] %in% names(df), "Select a response variable."))

      raw <- df[[input[["response"]]]]

      resp <- switch(
        input[["resp_apply"]],
        identity = raw,
        log = log(raw),
        log1p = log1p(raw),
        sqrt = sqrt(raw)
      )

      validate(need(
        all(is.finite(resp)),
        "Selected transform produced non-finite values. Try a different transform or variable."
      ))

      df[["resp_transformed"]] <- resp
      return(df)
    })

    v.bin_edges <- reactive({
      req(transformed_data(), input[["methods"]])

      cutoff <- if (is.na(input[["cutoff"]])) 0.5 else input[["cutoff"]]
      vcloud <- variogram(
        formula,
        data = transformed_data(),
        cloud = TRUE,
        cutoff = cutoff
      )

      distances <- vcloud[["dist"]]
      agg <- if (is.null(input[["aggr"]])) "mean" else input[["aggr"]]

      setNames(
        lapply(input[["methods"]], function(m) {
          tryCatch(
            MakeBoundaries(distances, method = m, nbins = input[["nbins"]], agg = agg),
            error = function(e) {
              showNotification(e[["message"]])
              return(NULL)
            }
          )
        }),
        input[["methods"]]
      )
    })

    v.variogram <- reactive({
      req(transformed_data(), v.bin_edges())

      edges <- v.bin_edges()

      out <- lapply(names(edges), function(m) {
        e <- edges[[m]]
        if (is.null(e) || length(e) < 3) {
          return(NULL)
        }

        v <- as.data.table(variogram(formula, data = transformed_data(), boundaries = e))

        v[, method := m]
        v
      })

      out <- out[!vapply(out, is.null, logical(1))]

      validate(need(
        length(out) > 0,
        "No method produced usable bins. Lower the bin count or pick different methods."
      ))

      rbindlist(out)
    })

    v.model_line <- reactive({
      req(v.variogram(), input[["model"]])

      model <- {
        if (input[["model"]] == "none") {
          return(NULL)
        }

        req(input[["sill"]], input[["range"]])
        vgm(
          psill = input[["sill"]],
          model = input[["model"]],
          range = input[["range"]],
          nugget = if (is.na(input[["nugget"]])) 0 else input[["nugget"]]
        )
      }

      ev <- v.variogram()
      if (is.null(ev) || nrow(ev) == 0) {
        return(NULL)
      }

      as.data.table(variogramLine(model, maxdist = max(ev[["dist"]], na.rm = TRUE)))
    })

    VariogramPlotServer("plot", v.variogram, v.model_line)

    method_summary <- reactive({
      req(v.variogram())
      v <- copy(v.variogram())

      summary_dt <- v[,
        .(n_bins = .N, avg_pairs = round(mean(np), 1), min_pairs = min(np)),
        by = method
      ]

      ln <- v.model_line()
      if (!is.null(ln)) {
        v[,
          gamma_hat := approx(ln[["dist"]], ln[["gamma"]], xout = dist, rule = 2)[["y"]]
        ]
        fit <- v[,
          .(fit_error = round(sum(np * (gamma - gamma_hat)^2, na.rm = TRUE), 1)),
          by = method
        ]
        summary_dt <- merge(summary_dt, fit, by = "method")
        summary_dt[, best := fit_error == min(fit_error)]
      } else {
        summary_dt[, best := FALSE]
      }

      summary_dt
    })

    output[["summary_table"]] <- renderDT({
      summary_dt <- copy(method_summary())
      summary_dt[, method := names(binning_methods)[match(method, binning_methods)]]

      if ("fit_error" %in% names(summary_dt)) {
        setorder(summary_dt, fit_error)
      }

      col_names <- c("Method", "Bins", "Avg Pairs/Bin", "Min Pairs/Bin")
      if ("fit_error" %in% names(summary_dt)) {
        col_names <- c(col_names, "Fit Error")
      }
      col_names <- c(col_names, "")

      best_col_index <- ncol(summary_dt) - 1

      dt_out <- datatable(
        summary_dt,
        rownames = FALSE,
        colnames = col_names,
        options = list(
          dom = "t",
          paging = FALSE,
          columnDefs = list(list(visible = FALSE, targets = best_col_index))
        )
      )

      formatStyle(
        dt_out,
        "method",
        target = "row",
        fontWeight = styleEqual(c(TRUE, FALSE), c("bold", "normal")),
        valueColumns = "best"
      )
    })

    output[["summary_text"]] <- renderPrint({
      summary_dt <- method_summary()
      display_names <- names(binning_methods)[match(
        summary_dt[["method"]],
        binning_methods
      )]
      has_fit <- "fit_error" %in% names(summary_dt)

      order_idx <- if (has_fit) {
        order(summary_dt[["fit_error"]])
      } else {
        seq_len(nrow(summary_dt))
      }

      for (i in order_idx) {
        cat(display_names[i], "\n")
        cat(sprintf(
          "  Bins: %d   Avg pairs/bin: %.1f   Min pairs/bin: %d\n",
          summary_dt[["n_bins"]][i],
          summary_dt[["avg_pairs"]][i],
          summary_dt[["min_pairs"]][i]
        ))
        if (has_fit) {
          tag <- if (isTRUE(summary_dt[["best"]][i])) "  (best fit)" else ""
          cat(sprintf("  Fit error: %.1f%s\n", summary_dt[["fit_error"]][i], tag))
        }
        cat("\n")
      }

      if (has_fit) {
        best_method <- display_names[which(summary_dt[["best"]])]
        cat(sprintf("Best performing method: %s\n", paste(best_method, collapse = ", ")))
      } else {
        cat("Choose a variogram model to compare fit error across binning methods.\n")
      }
    })

    observeEvent(input[["about_binning"]], {
      showModal(
        modalDialog(
          title = "About the Binning Methods",
          size = "l",
          easyClose = TRUE,

          tagList(
            div("References"),
            tags$ul(
              tags$li(tags$a(
                "scikit-gstat: Binning Methods",
                href = "https://scikit-gstat.readthedocs.io/en/latest/reference/binning.html",
                target = "_blank",
                rel = "noopener noreferrer"
              )),
              tags$li(tags$a(
                "numpy.histogram_bin_edges",
                href = "https://numpy.org/doc/stable/reference/generated/numpy.histogram_bin_edges.html#numpy.histogram_bin_edges",
                target = "_blank",
                rel = "noopener noreferrer"
              ))
            )
          ),

          footer = modalButton("Close")
        ),
        session
      )
    })
  })
}
