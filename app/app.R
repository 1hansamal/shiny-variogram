library(shiny)
library(shinyjs)
library(bslib)
library(bsicons)

# binning methods implemented refer to
# https://scikit-gstat.readthedocs.io/en/latest/reference/binning.html
#https://numpy.org/doc/stable/reference/generated/numpy.histogram_bin_edges.html#numpy.histogram_bin_edges

ui <- page_fillable(
  title = "Empirical Variogram Analysis Dashboard",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  card_body(VariogramUI("variogram"))
)

server <- function(input, output, session) {
  VariogramServer("variogram")
}


shinyApp(ui = ui, server = server)
