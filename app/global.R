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

# read the given file and return it
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


### Binning strategies for empirical variogram distance classes

MakeBoundaries <- function(
  distances,
  method = c(
    "even",
    "uniform",
    "fd",
    "scott",
    "rice",
    "sturges",
    "doane",
    "sqrt",
    "kmeans",
    "ward"
  ),
  nbins = NULL,
  cutoff = NULL
) {
  method <- match.arg(method)

  dmax <- max(distances)
  if (is.null(cutoff) || cutoff > dmax) {
    cutoff <- dmax
  }

  d <- distances[distances <= cutoff]

  if (is.null(nbins)) {
    nbins <- switch(
      method,
      fd = ceiling(cutoff / method.fd(d)),
      sturges = method.sturges(d),
      scott = ceiling(cutoff / method.scott(d)),
      rice = method.rice(d),
      sqrt = method.sqrt(d),
      doane = method.doane(d),
      kmeans = method.sqrt(d),
      ward = method.sqrt(d),
      even = method.sqrt(d),
      uniform = method.sqrt(d)
    )
  }

  nbins <- max(1, as.integer(nbins))

  boundaries <- switch(
    method,
    even = seq(0, cutoff, length.out = nbins + 1),
    uniform = method.uniform(d, nbins, cutoff),
    seq(0, cutoff, length.out = nbins + 1)
  )

  boundaries
}


method.uniform <- function(d, nbins) {
  probs <- seq(0, 1, length.out = nbins + 1)
  as.numeric(quantile(d, probs = probs, na.rm = TRUE))
}


method.fd <- function(d) {
  h <- 2 * IQR(d) / length(d)^(1 / 3)
}


method.sturges <- function(d) {
  ceiling(log2(length(d)) + 1)
}


method.scott <- function(d) {
  h <- sd(d) * (24 * sqrt(pi) / length(d))^(1 / 3)
}


method.rice <- function(d) {
  ceiling(2 * length(d)^(1 / 3))
}


method.sqrt <- function(d) {
  ceiling(sqrt(length(d)))
}


method.doane <- function(d) {
  n <- length(d)
  s <- sd(d)

  if (n < 3 || s == 0) {
    return(ceiling(log2(n) + 1))
  }

  g1 <- mean(((d - mean(d)) / s)^3)

  sg1 <- sqrt(6 * (n - 2) / ((n + 1) * (n + 3)))

  ceiling(1 + log2(n) + log2(1 + abs(g1) / sg1))
}


method.kmeans <- function(distances, nbins, cutoff) {
  if (nbins == 1) {
    return(c(0, cutoff))
  }

  km <- kmeans(x = matrix(distances, ncol = 1), centers = nbins)

  centers <- sort(drop(km$centers))

  bounds <- c(0, (centers[-1] + centers[-length(centers)]) / 2, cutoff)

  bounds
}


method.ward <- function(distances, nbins, cutoff, agg = c("mean", "median")) {
  agg <- match.arg(agg)

  if (nbins == 1) {
    return(c(0, cutoff))
  }

  d <- distances[is.finite(distances) & distances >= 0 & distances <= cutoff]

  if (length(d) < nbins) {
    stop("Number of bins exceeds number of distances.")
  }

  # Ward clustering on 1D observations
  hc <- hclust(dist(matrix(d, ncol = 1)), method = "ward.D2")

  clusters <- cutree(hc, k = nbins)

  centers <- sort(vapply(
    split(d, clusters),
    function(x) {
      if (agg == "median") {
        median(x)
      } else {
        mean(x)
      }
    },
    numeric(1)
  ))

  c(0, (centers[-1] + centers[-length(centers)]) / 2, cutoff)
}
