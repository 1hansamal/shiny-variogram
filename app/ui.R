page_navbar(
  title = "Empirical Variogram Analysis Dashboard",
  window_title = "Interactive Spatial Correlation Visualizer",

  nav_panel(title = "Upload Data", FileHandlerUI("uploadFiles")),
  nav_panel(title = "Transform/Reshape Data"),
  nav_panel(title = "Variogram"),
  nav_spacer(),
  nav_panel(title = "About"),

  theme = myTheme,
  useShinyjs()
)