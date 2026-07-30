VariogramPlotUI <- function(id) {
  ns <- NS(id)
  card(card_header(strong("Empirical Variogram")), plotOutput(ns("graph")))
}

VariogramPlotServer <- function(id, v.variogram, v.model_line) {
  moduleServer(id, function(input, output, session) {
    ns <- session[["ns"]]

    output[["graph"]] <- renderPlot({
      v <- copy(v.variogram())
      v[,
        method_label := factor(
          method,
          levels = binning_methods,
          labels = names(binning_methods)
        )
      ]
      p <- ggplot(v, aes(dist, gamma, color = method_label)) +
        geom_smooth(
          method = "loess",
          formula = y ~ x,
          se = FALSE,
          linewidth = 0.8,
          alpha = 0.3
        ) +
        geom_point(aes(size = np)) +
        labs(
          x = "Distance",
          y = "Semivariance",
          color = "Binning Method",
          size = "Pairs"
        ) +
        theme_minimal(base_size = 16)


      ln <- v.model_line()
      if (!is.null(ln)) {
        p <- p +
          geom_line(
            data = ln,
            aes(dist, gamma),
            inherit.aes = FALSE,
            linetype = "dashed",
            linewidth = 0.9,
            color = "black"
          )
      }
      p
    })
  })
}