library(data.table)
library(DT)
library(sf)
library(gstat)
library(ggplot2)

test_data <- st_read(here::here("app/test/data/meuse.geojson"), quiet = TRUE)

ReadFiles <- function(file_path, file_ext) {
  loaded_data <- switch(
    file_ext,
    csv = {
      read.csv(file_path)
    },
    zip = {
      temp_dir <- tempdir()
      utils::unzip(file_path, exdir = temp_dir, overwrite = TRUE)

      shp_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
      if (length(shp_files) == 0) {
        stop("No .shp file found in the ZIP archive")
      }

      shp_path <- shp_files[1]
      st_read(shp_path, quiet = TRUE)
    },
    geojson = {
      st_read(file_path, quiet = TRUE)
    },
    stop(sprintf("Unsupported file type: %s", file_type))
  )

  return(loaded_data)
}

NamesDropGeom <- function(data) {
  if (inherits(data, "sf")) {
    cols <- names(st_drop_geometry(data))
  } else {
    cols <- names(data)
  }

  return(cols)
}

DropGeom <- function(data) {
  if (inherits(data, "sf")) st_drop_geometry(data) else data
}

NumericCols <- function(data) {
  df <- DropGeom(data)
  names(df)[vapply(df, is.numeric, logical(1))]
}


MakeBoundaries <- function(
  distances,
  method = c(
    "even",
    "uniform",
    "fd",
    "sturges",
    "scott",
    "rice",
    "sqrt",
    "doane",
    "kmeans",
    "ward"
  ),
  nbins = NULL,
  agg = c("mean", "median")
) {
  method <- match.arg(method)
  agg <- match.arg(agg)

  if (length(distances) == 0) {
    stop("Missing distances.")
  }

  distances <- sort(distances[is.finite(distances)])

  if (length(distances) == 0) {
    stop("No finite distances.")
  }

  dmin <- min(distances)
  dmax <- max(distances)

  boundaries <- switch(
    method,

    even = {
      seq(dmin, dmax, length.out = nbins + 1)
    },
    uniform = {
      .uniform_method(distances, nbins)
    },
    fd = {
      width <- .fd_method(distances)
      seq(dmin, dmax + width, by = width)
    },

    scott = {
      width <- .scott_method(distances)
      seq(dmin, dmax + width, by = width)
    },

    sturges = {
      k <- .sturges_method(distances)
      seq(dmin, dmax, length.out = k + 1)
    },

    rice = {
      k <- .rice_method(distances)
      seq(dmin, dmax, length.out = k + 1)
    },

    sqrt = {
      k <- .sqrt_method(distances)
      seq(dmin, dmax, length.out = k + 1)
    },

    doane = {
      k <- .doane_method(distances)
      seq(dmin, dmax, length.out = k + 1)
    },

    kmeans = {
      if (is.null(nbins)) {
        stop("'nbins' must be supplied for method = 'kmeans'.")
      }
      .kmeans_method(distances, nbins)
    },

    ward = {
      if (is.null(nbins)) {
        stop("'nbins' must be supplied for method = 'ward'.")
      }
      .ward_method(distances, nbins, agg)
    }
  )

  unique(boundaries)
}

.uniform_method <- function(dist, nbins) {
  probs <- seq(0, 1, length.out = nbins + 1)

  bounds <- unique(as.numeric(quantile(dist, probs, na.rm = TRUE)))

  if (length(bounds) < 2) {
    bounds <- c(min(dist), max(dist))
  }

  bounds
}

.kmeans_method <- function(dist, nbins) {
  if (nbins == 1) {
    return(c(min(dist), max(dist)))
  }

  if (length(dist) < nbins) {
    stop("Number of bins exceeds number of distances.")
  }

  km <- kmeans(x = matrix(dist, ncol = 1), centers = nbins, nstart = 25)

  centers <- sort(drop(km$centers))

  c(min(dist), (centers[-1] + centers[-length(centers)]) / 2, max(dist))
}

.ward_method <- function(dist, nbins, agg = c("mean", "median")) {
  agg <- match.arg(agg)

  if (nbins == 1) {
    return(c(min(dist), max(dist)))
  }

  if (length(dist) < nbins) {
    stop("Number of bins exceeds number of distances.")
  }

  hc <- hclust(dist(matrix(dist, ncol = 1)), method = "ward.D2")

  clusters <- cutree(hc, k = nbins)

  centers <- sort(vapply(
    split(dist, clusters),
    function(x) {
      if (agg == "median") median(x) else mean(x)
    },
    numeric(1)
  ))

  c(min(dist), (centers[-1] + centers[-length(centers)]) / 2, max(dist))
}

.fd_method <- function(dist) {
  2 * IQR(dist) / length(dist)^(1 / 3)
}

.scott_method <- function(dist) {
  3.5 * sd(dist) / length(dist)^(1 / 3)
}

.sturges_method <- function(dist) {
  ceiling(log2(length(dist)) + 1)
}

.rice_method <- function(dist) {
  ceiling(2 * length(dist)^(1 / 3))
}

.sqrt_method <- function(dist) {
  ceiling(sqrt(length(dist)))
}

.doane_method <- function(dist) {
  n <- length(dist)
  s <- sd(dist)

  if (n < 3 || s == 0) {
    return(ceiling(log2(n) + 1))
  }

  mu <- mean(dist)
  g1 <- mean(((dist - mu) / s)^3)
  sg1 <- sqrt(6 * (n - 2) / ((n + 1) * (n + 3)))

  ceiling(1 + log2(n) + log2(1 + abs(g1) / sg1))
}

# for selectInput
binning_methods <- c(
  "Even Width" = "even",
  "Uniform Count" = "uniform",
  "Freedman-Diaconis" = "fd",
  "Scott" = "scott",
  "Sturges" = "sturges",
  "Rice" = "rice",
  "Square Root" = "sqrt",
  "Doane" = "doane",
  "K-Means" = "kmeans",
  "Ward Clustering" = "ward"
)

method_descriptions <- c(
  even    = "Splits the observed distance range into equal-width bins.",
  uniform = "Chooses edges so each bin holds roughly the same number of point pairs.",
  fd      = "Freedman-Diaconis: bin width from the IQR, robust to outliers.",
  scott   = "Scott's rule: bin width from the standard deviation, assumes near-normal spacing.",
  sturges = "Sturges' rule: bin count from log2(n), suited to smaller, near-normal samples.",
  rice    = "Rice rule: bin count scales with the cube root of the sample size.",
  sqrt    = "Square-root choice: bin count equals the square root of the sample size.",
  doane   = "Doane's rule: a Sturges variant adjusted for skewness in the distances.",
  kmeans  = "Groups distances into k clusters via k-means; edges fall at cluster midpoints.",
  ward    = "Groups distances via Ward hierarchical clustering, then aggregates cluster centers."
)

resp_apply_funcs <- c(
  "None" = "identity",
  "Log" = "log",
  "Log1p" = "log1p",
  "Square Root" = "sqrt"
)

vgm_models <- c(
  "No Model" = "none",
  "Spherical" = "Sph",
  "Exponential" = "Exp",
  "Gaussian" = "Gau",
  "Matern" = "Mat",
  "Linear" = "Lin"
)