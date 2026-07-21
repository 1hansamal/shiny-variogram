## module UI
## =========================================================================
FileHandlerUI <- function(id) {
  ns <- NS(id)

  onlyFiles <- c(".csv", ".zip", ".geojson")
  mimeTypes <- c("CSV" = "csv", "Shape File" = "shp", "GeoJSON" = "geojson")

  sidebar = sidebar(
    title = "File Tools",
    width = "25%",
    accordion(
      id = ns("accordion"),
      accordion_panel(
        "Upload Files",
        selectInput(ns("fileType"), "Select File Type", choices = mimeTypes),
        layout_columns(
          fileInput(ns("files"), NULL, accept = onlyFiles, multiple = FALSE),
          actionButton(ns("uploadBtn"), "Upload", disabled = TRUE),
          col_widths = c(8, 4)
        ),
      ),
      accordion_panel("Select Columns to Keep", value = "selcols"),
      accordion_panel("Convert to Sf Class", value = "convertsf")
    )
  )

  layout_sidebar(sidebar = sidebar, main = card(DTOutput(ns("dataPreview"))))
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

    # fallback dataset
    loadedData(testData)

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
          showNotification(
            "File processed successfully!",
            type = "message",
            session = session
          )
        },
        error = function(e) {
          showNotification(e[["message"]], type = "error", session = session)
        }
      )
    })

    # add file options depending on file type
    observe({
      req(loadedData())

      cols <- NamesDropGeom(loadedData())

      accordion_panel_update(
        id = "accordion",
        target = "selcols",
        session = session,

        checkboxGroupInput(ns("selColumns"), NULL, choices = cols, selected = cols)
      )
    })

    observe({
      req(cleanedData(), input[["fileType"]] == "csv", input[["files"]])

      cols <- names(cleanedData())

      accordion_panel_update(
        id = "accordion",
        target = "convertsf",
        session = session,

        layout_columns(
          selectInput(ns("coord1"), "X / Longitude Column", choices = cols),
          selectInput(ns("coord2"), "Y / Latitude Column", choices = cols)
        ),
        layout_columns(
          textInput(ns("crs"), NULL, placeholder = "CRS Code"),
          actionButton(ns("convertSF"), "Convert to SF", disabled = TRUE)
        )
      )
    })

    # filter columns if needed
    observeEvent(input[["selColumns"]], {
      req(loadedData())

      df <- loadedData()
      drop_cols <- setdiff(NamesDropGeom(df), input[["selColumns"]])

      if (length(drop_cols) > 0) {
        df[drop_cols] <- NULL
      }

      cleanedData(df)
    })

    # convert to sf
    observe({
      req(cleanedData(), input[["crs"]], input[["coord1"]], input[["coord2"]])

      updateActionButton(session, "convertSF", disabled = FALSE)
    })

    observeEvent(input[["convertSF"]], {
      req(
        !inherits(cleanedData(), "sf"),
        input[["crs"]],
        input[["coord1"]],
        input[["coord2"]]
      )

      df <- cleanedData()

      crs <- as.numeric(input[["crs"]])
      coords <- c(input[["coord1"]], input[["coord2"]])

      tryCatch(
        {
          df <- st_as_sf(df, coords = coords, crs = crs)

          cleanedData(df)
        },
        error = function(e) {
          showNotification(e[["message"]])
        }
      )
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
