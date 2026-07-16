## module UI
## =========================================================================
FileHandlerUI <- function(id) {
  ns <- NS(id)

  onlyFiles <- c(".csv", ".zip", ".geojson")
  mimeTypes <- c("CSV" = "csv", "Shape File" = "shp", "GeoJSON" = "geojson")

  sidebar = sidebar(
    width = "25%",

    selectInput(ns("fileType"), "Select File Type", choices = mimeTypes),
    layout_columns(
      fileInput(ns("files"), NULL, accept = onlyFiles, multiple = FALSE),
      actionButton(ns("uploadBtn"), "Upload", disabled = TRUE),
      col_widths = c(8, 4)
    ),

    uiOutput(ns("fileOptions"))
  )

  layout_sidebar(sidebar = sidebar, card(DTOutput(ns("dataPreview"))))
}

## module Server
## =========================================================================

FileHandlerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session[["ns"]]

    # reactive containers to store the data
    loadedData <- reactiveVal(NULL)
    cleanedData <- reactiveVal(NULL)
    fileName <- reactiveVal(NULL)

    # enable upload btn when a file uploaded
    observe({
      req(input[["files"]])
      updateActionButton(session, "uploadBtn", disabled = FALSE)
    })

    # when upload button pressed, do this..
    observeEvent(input[["uploadBtn"]], {
      req(input[["files"]], input[["fileType"]])

      files <- input[["files"]]

      filePath <- files[["datapath"]]
      fileExt <- tolower(tools::file_ext(files[["name"]]))
      selectedType <- input[["fileType"]]
      validExt <- c(csv = "csv", shp = "zip", geojson = "geojson")

      tryCatch(
        {
          # check if file type matches, return if not
          if (fileExt != validExt[[selectedType]]) {
            stop("File does not match the selected type")
          }

          # read the file
          loadedFile <- ReadFile(filePath, selectedType)

          loadedData(loadedFile)
          showNotification("File processed successfully!", type = "message")
        },
        error = function(e) {
          showNotification(ui = e[["message"]], type = "error", session)
        }
      )
    })

    # add file options depending on file type
    renderUI({
      req(loadedData(), input[["fileType"]])

      df <- loadedData()
      if (inherits(df, "sf")) {
        cols <- names(st_drop_geometry(df))
      } else {
        cols <- names(df)
      }
      accordion(
        accordion_panel(
          "Select Columns to Keep",
          checkboxGroupInput(ns("selColumns"), NULL, choices = cols, selected = cols)
        ),
        #### // TODO: add option to convert to sf for csv files
        if (input[["fileType"]] == "csv") {
          accordion_panel("Convert to Sf Class")
        }
      )
    }) -> output[["fileOptions"]]

    # filter columns if needed
    observeEvent(input[["selColumns"]], {
      req(loadedData())

      df <- loadedData()
      if (inherits(df, "sf")) {
        cols <- names(st_drop_geometry(df))
      } else {
        cols <- names(df)
      }
      drop_cols <- setdiff(cols, input[["selColumns"]])

      if (length(drop_cols) > 0) {
        for (c in drop_cols) {
          df[[c]] <- NULL
        }
      }

      cleanedData(df)
    })

    # preview the data after cleanup
    renderDT({
      req(cleanedData())

      #### // TODO: customize this table output
      datatable(cleanedData())
    }) -> output[["dataPreview"]]

    return(reactive(cleanedData()))
  })
}
