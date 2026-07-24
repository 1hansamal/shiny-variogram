library(shiny)

library(bslib)
library(bsicons)

library(DT)

library(data.table)

library(sf)
library(gstat)

# set size of file uploads to 10MB
options(shiny.maxRequestSize = 10 * 1024^2)

test_data <- st_read(here::here("app/test/data/meuse.geojson"), quiet = TRUE)

myTheme <- bs_theme(version = 5, bootswatch = "flatly") #|>
# bs_add_variables(here::here("app/www/variables.scss")) |>
# bs_add_rules(here::here("app/www/rules.scss"))

# read the given file and return it
ReadFile <- function(file_path, file_type) {
  data <- switch(
    file_type,

    zip = {
      temp_dir <- tempdir()
      utils::unzip(file_path, exdir = temp_dir, overwrite = TRUE)

      # Find the .shp file
      shp_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
      if (length(shp_files) == 0) {
        stop("No .shp file found in the ZIP archive")
      }

      # Uses the first .shp file found
      shp_path <- shp_files[1]
      st_read(shp_path, quiet = TRUE)
    },

    geojson = {
      st_read(file_path, quiet = TRUE)
    },

    csv = {
      read.csv(file_path)
    },

    stop(sprintf("Unsupported file type: %s", file_type))
  )

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
  cutoff = 0.5
) {
  method <- match.arg(method)
  
  dmax <- max(distances)
  cutoff <- if (cutoff <= 1) dmax * cutoff else min(cutoff, dmax)

  d <- distances[distances <= cutoff]

  # Methods that require user-specified nbins
  if (method %in% c("even", "uniform", "kmeans", "ward")) {

    nbins <- as.integer(nbins)
  } else {
    nbins <- switch(
      method,
      fd = ceiling(cutoff / method.fd(d)),
      scott = ceiling(cutoff / method.scott(d)),
      rice = method.rice(d),
      sturges = method.sturges(d),
      doane = method.doane(d),
      sqrt = method.sqrt(d)
    )

    nbins <- max(1L, nbins)
  }

  boundaries <- switch(
    method,
    even = seq(0, cutoff, length.out = nbins + 1),
    uniform = method.uniform(d, nbins),
    kmeans = method.kmeans(d, nbins),
    ward = method.ward(d, nbins),

    # Histogram-based methods compute nbins automatically
    fd = seq(0, cutoff, length.out = nbins + 1),
    scott = seq(0, cutoff, length.out = nbins + 1),
    rice = seq(0, cutoff, length.out = nbins + 1),
    sturges = seq(0, cutoff, length.out = nbins + 1),
    doane = seq(0, cutoff, length.out = nbins + 1),
    sqrt = seq(0, cutoff, length.out = nbins + 1)
  )

  boundaries
}


method.uniform <- function(d, nbins) {
  probs <- seq(0, 1, length.out = nbins + 1)
  bounds <- unique(as.numeric(quantile(d, probs, na.rm = TRUE)))

  if (length(bounds) < 2) {
    bounds <- c(min(d), max(d))
  }
  bounds
}

method.fd <- function(d) {
  2 * IQR(d) / length(d)^(1 / 3)
}
method.sturges <- function(d) {
  ceiling(log2(length(d)) + 1)
}
method.scott <- function(d) {
  sd(d) * (24 * sqrt(pi) / length(d))^(1 / 3)
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


binning_methods <- c(
  "Even Width" = "even",
  "Uniform Count (quantile)" = "uniform",
  "Freedman-Diaconis" = "fd",
  "Scott" = "scott",
  "Sturges" = "sturges",
  "Rice" = "rice",
  "Doane" = "doane",
  "Square Root" = "sqrt",
  "K-Means Clustering" = "kmeans",
  "Ward Clustering" = "ward"
)
