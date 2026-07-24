FileUploadUI <- function(id) {
  ns <- NS(id)

  tooltip_msg <- "only csv files are allowd for now"
  sidebar <- sidebar(
    # file upload opts
    tagList(
      h5("Upload Your Data"),
      layout_columns(
        tooltip(fileInput(ns("files"), NULL), tooltip_msg, placement = "bottom"),
        actionButton(ns("upload"), "Upload", disabled = TRUE),
        col_widths = c(8, 4)
      ),
      hr()
    ),
    # transform crs opts
    tagList(uiOutput(ns("transform"))),
    width = "30%"
  )
  main_panel <- card(DTOutput(ns("data")))

  layout_sidebar(sidebar = sidebar, main_panel)
}

FileUploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session[["ns"]]

    data <- reactiveVal(test_data)

    # enable file upload button whenever file is uploaded
    observe({
      req(input[["files"]])

      updateActionButton(session, "upload", disabled = FALSE)
    })

    # load the data from file when upload button clicked
    loaded_data <- eventReactive(input[["upload"]], {
      req(input[["files"]])

      files <- input[["files"]]
      files_type <- tolower(tools::file_ext(files[["name"]]))

      tryCatch(
        {
          result <- ReadFile(files[["datapath"]], files_type)
          showNotification("File Uploaded Successfully!", type = "message")
          return(result)
        },
        error = function(e) {
          showNotification(e[["message"]], type = "error")
          return(NULL)
        }
      )
    })

    observe({
      if (!is.null(loaded_data())) data(loaded_data())
    })

    # options to transform crs ===========================================================
    output[["transform"]] <- renderUI({
      req(loaded_data(), input[["files"]])

      cols <- NamesDropGeom(loaded_data())
      file_type_csv <- tolower(tools::file_ext(input[["files"]][["name"]])) == "csv"

      if (file_type_csv) {
        tagList(
          "Convert To CRS",
          layout_columns(
            selectInput(ns("coord1"), "X / Longitude", choices = cols),
            selectInput(ns("coord2"), "Y / Latitude", choices = cols)
          ),
          layout_columns(
            textInput(ns("crs"), NULL, placeholder = "CRS Code"),
            actionButton(ns("convert_crs"), "Convert", disabled = TRUE)
          )
        )
      }
    })

    observe({
      req(input[["coord1"]], input[["coord2"]], input[["crs"]])

      updateActionButton(session, "convert_crs", disabled = FALSE)
    })

    observeEvent(input[["convert_crs"]], {
      req(loaded_data(), input[["coord1"]], input[["coord2"]], input[["crs"]])

      tryCatch(
        {
          df <- loaded_data()
          x_col <- input[["coord1"]]
          y_col <- input[["coord2"]]
          crs_code <- input[["crs"]]

          data_sf <- st_as_sf(df, coords = c(x_col, y_col), crs = crs_code)
          data(data_sf)

          showNotification("CRS Conversion Successful!", type = "message")
        },
        error = function(e) showNotification(e[["message"]], type = "error")
      )
    })

    # render data ========================================================================
    output[["data"]] <- renderDT({
      as.data.frame(data())
    })

    data
  })
}
