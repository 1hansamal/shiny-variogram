DataUI <- function(id) {
  ns <- NS(id)
  toolbar(
    toolbar_input_button(ns("upload"), "Upload", bs_icon("upload"), show_label = TRUE),
    toolbar_divider(),
    toolbar_input_button(ns("preview"), "View Data", bs_icon("table"), show_label = TRUE)
  )
}

DataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session[["ns"]]

    uploaded_data <- reactiveVal(test_data)
    staged_data <- reactiveVal(NULL)

    # modal dialog for uploading data
    observeEvent(input[["upload"]], {
      staged_data(NULL)

      showModal(modalDialog(
        title = "Upload Your Data",
        size = "l",
        easyClose = TRUE,

        fileInput(ns("files"), "Choose a file", width = "100%"),
        uiOutput(ns("csv_opts")), # option to convert to sf when uploaded file is a csv
        card(card_header(strong("Preview of Data")), verbatimTextOutput(ns("preview"))),
        uiOutput(ns("new_col_ui")), # option to add new column using R code

        footer = tagList(
          actionButton(ns("submit"), "Upload", icon = bs_icon("upload")),
          modalButton("Dismiss")
        )
      ))
    })

    # read the file
    observeEvent(input[["files"]], {
      req(input[["files"]])

      file_path <- input[["files"]][["datapath"]]
      file_ext <- tolower(tools::file_ext(input[["files"]][["name"]]))

      loaded <- tryCatch(ReadFiles(file_path, file_ext), error = function(e) {
        showNotification(
          paste("Failed to read file:", conditionMessage(e)),
          type = "error"
        )
        NULL
      })

      staged_data(loaded)
    })

    # convert to sf
    output[["csv_opts"]] <- renderUI({
      req(staged_data(), input[["files"]])

      is_csv <- tolower(tools::file_ext(input[["files"]][["name"]])) == "csv"
      req(is_csv)

      cols <- names(staged_data())
      tagList(
        h5(strong("Convert to SF")),
        "You must convert CSV data to a proper sf object before uploading.",
        layout_columns(
          selectInput(ns("coord1"), "X / Longitude", choices = cols),
          selectInput(ns("coord2"), "Y / Latitude", choices = cols),
          textInput(ns("crs"), "CRS Code", placeholder = "eg. EPSG:4326"),
          actionButton(ns("conv_crs"), "Convert")
        )
      )
    })

    #### // TODO: this need to be validated 
    observeEvent(input[["conv_crs"]], {
      req(staged_data(), input[["coord1"]], input[["coord2"]], input[["crs"]])

      df <- staged_data()
      coords = c(input[["coord1"]], input[["coord2"]])

      converted <- tryCatch(
        {
          st_as_sf(df, coords = coords, crs = input[["crs"]])
        },
        error = function(e) {
          showNotification(e[["message"]], type = "error")
          NULL
        }
      )

      req(converted)
      staged_data(converted)
      showNotification("Data converted to a spatial (sf) object.", type = "message")
    })

    output[["preview"]] <- renderPrint({
      req(staged_data())

      print(head(staged_data(), n = 10))
    })

    # new column from R code
    output[["new_col_ui"]] <- renderUI({
      req(staged_data())
      card(
        card_header(strong("Add a Column (R code)")),
        "Reference the dataset as `data`",
        textAreaInput(
          ns("new_col_code"),
          NULL,
          placeholder = "eg. data$new_col <- data$col_a + data$col_b",
          rows = 2,
          width = "100%"
        ),
        actionButton(ns("run_new_col"), "Run Code", icon = bs_icon("code-slash"))
      )
    })

    # process R code 
    observeEvent(input[["run_new_col"]], {
      req(staged_data(), input[["new_col_code"]])

      validate_code <- trimws(input[["new_col_code"]])
      req(nchar(validate_code) > 0)

      env <- new.env(parent = baseenv())
      env[["data"]] <- staged_data()

      result <- tryCatch(
        {
          eval(parse(text = validate_code), envir = env)
          env[["data"]]
        },
        error = function(e) {
          showNotification(paste("Error in R code:", conditionMessage(e)), type = "error")
          NULL
        }
      )

      req(result)

      if (!is.data.frame(result)) {
        showNotification(
          "Your code must leave 'data' as a data frame/sf object.",
          type = "error"
        )
        return(invisible(NULL))
      }

      staged_data(result)
      updateTextAreaInput(session, "new_col_code", value = "")
      showNotification("Column(s) added successfully.", type = "message")
    })

    observeEvent(input[["submit"]], {
      req(staged_data())

      is_csv <- tolower(tools::file_ext(input[["files"]][["name"]])) == "csv"
      if (is_csv && !inherits(staged_data(), "sf")) {
        showNotification(
          "Please convert your CSV data to a spatial (sf) object before uploading.",
          type = "error"
        )
        return(invisible(NULL))
      }

      uploaded_data(staged_data())
      removeModal()
      showNotification("Data uploaded successfully.", type = "message")
    })

    # current data set preview
    observeEvent(input[["preview"]], {
      showModal(modalDialog(
        title = "Current Dataset",
        size = "xl",
        easyClose = TRUE,

        DTOutput(ns("current_preview")),

        footer = modalButton("Close")
      ))
    })

    output[["current_preview"]] <- renderDT({
      req(uploaded_data())

      datatable(
        DropGeom(uploaded_data()),
        options = list(scrollX = TRUE, pageLength = 20, dom = "tip"),
        rownames = FALSE
      )
    })

    return(uploaded_data)
  })
}
