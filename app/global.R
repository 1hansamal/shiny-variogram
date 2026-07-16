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

