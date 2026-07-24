VariogramUI <- function(id) {
  ns <- NS(id)

  main_panel <- card(plotOutput(ns("graph")))

  sidebar <- sidebar(
    tagList(
      h5("Select Model"),
      selectInput(ns("response"), "Response Variable", choices = "None")
    ),
    tagList(
      h5("Binning Method"),
      selectInput(ns("method"), "Binning Method", choices = binning_methods),
      conditionalPanel(
        condition = sprintf("['even', 'uniform'].includes(input['%s'])", ns("method")),
        numericInput(ns("nbins"), "Number of Bins", min = 2, max = 30, value = 10)
      ),
      sliderInput(ns("cutoff"), "Cutoff", min = 0.1, max = 1, value = 0.5, step = 0.05)
    ),
    width = "30%"
  )

  layout_sidebar(main_panel, sidebar = sidebar)
}


VariogramServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session[["ns"]]

    observe({
      req(data())

      df <- data()
      cols <- NamesDropGeom(df)
      updateSelectInput(session, "response", choices = cols)
    })

    model <- reactive({
      req(input[["response"]])

      reformulate("1", input[["response"]])
    })

    boundaries <- reactive({
      req(data(), model(), input[["method"]], input[["nbins"]], input[["cutoff"]])

      vcloud <- variogram(model(), data = data(), cutoff = Inf, cloud = TRUE)
      distances <- vcloud[["dist"]]

      bounds <- tryCatch(
        {
          MakeBoundaries(
            distances,
            input[["method"]],
            input[["nbins"]],
            input[["cutoff"]]
          )
        },
        error = function(e) showNotification(e[["message"]])
      )

      return(bounds)
    })

    output[["graph"]] <- renderPlot({
      req(data(), model(), boundaries())

      fig <- variogram(model(), data = data(), boundaries = boundaries())
      plot(fig)
    })
  })
}
