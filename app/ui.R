page_navbar(
  title = "Empirical Variogram Analysis Dashboard",
  window_title = "Interactive Spatial Correlation Visualizer",

  nav_panel(title = "DATA", FileUploadUI("upload_files")),
  nav_panel(title = "VARIOGRAM", VariogramUI("variogram")),
  nav_spacer(),
  nav_panel(title = "ABOUT"),

  theme = myTheme
)