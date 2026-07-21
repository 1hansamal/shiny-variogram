library(shiny)
library(shinyjs)

library(bslib)
library(bsicons)

library(DT)

library(data.table)

library(sf)
library(gstat)

# set size of file uploads to 10MB
options(shiny.maxRequestSize = 10 * 1024^2)

testData <- st_read(here::here("app/test/data/meuse.geojson"), quiet = TRUE)

myTheme <- bs_theme(version = 5, bootswatch = "flatly") #|>
# bs_add_variables(here::here("app/www/variables.scss")) |>
# bs_add_rules(here::here("app/www/rules.scss"))

##' read the given file and return it
ReadFile <- function(filePath, fileType) {
  if (fileType == "shp") {
    tempDir <- tempdir()
    utils::unzip(filePath, exdir = tempDir, overwrite = TRUE)

    # find the .shp file
    shpFiles <- list.files(tempDir, pattern = "\\.shp$", full.names = TRUE)
    if (length(shpFiles) == 0) {
      stop("No .shp file found in the ZIP archive")
    }

    # this only give the first found file in alphabetical order !!
    shpPath <- shpFiles[1]
    data <- st_read(shpPath, quiet = TRUE)
  } else if (fileType == "geojson") {
    data <- st_read(filePath, quiet = TRUE)
  } else if (fileType == "csv") {
    data <- read.csv(filePath)
  }

  return(data)
}

NamesDropGeom <- function(df) {
  if (inherits(df, "sf")) {
    cols <- names(st_drop_geometry(df))
  } else {
    cols <- names(df)
  }
  return(cols)
}

#' calculate bin boundaries based on equal distance method
EquidistantBins <- function(distances, nbins, cutoff = NULL) {
  d_min <- min(distances)
  d_max <- if (!is.null(cutoff)) {
    min(cutoff, max(distances))
  } else {
    max(distances)
  }

  boundaries <- seq(d_min, d_max, length.out = nbins + 1)

  return(boundaries)
}

ExplonentialBins <- function() {}
QuantilesBins <- function() {}
EquidistantBins <- function() {}