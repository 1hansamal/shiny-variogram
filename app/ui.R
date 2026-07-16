page_navbar(
  title = "Empirical Variogram Analysis Dashboard",
  window_title = "Interactive Spatial Correlation Visualizer",

  nav_panel(title = "Upload Data"),
  nav_panel(title = "Transform/Reshape Data"),
  nav_panel(title = "Variogram"),
  nav_spacer(),
  nav_panel(title = "About")
  
  gap = 0,
  padding = 5,
  
  useShinyjs()
)