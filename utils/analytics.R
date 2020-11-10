#' Utility functions to aid analysis and visualization of data

#colors
color.dark_gray = "#A6A6A6"
color.light_gray = "#eaeaea"

library(rcartocolor)

#ggplot theme for prettier charts and legends
theme_set (
  theme_minimal() +
    theme(
      #plot title
      plot.title = element_text(size = 14, margin = margin(t = 0, r = 0, b = 2, l = 0)),
      plot.subtitle = element_text(face = "bold", size = 10, color = color.dark_gray, margin = margin(t = 0, r = 0, b = 8, l = 0)),
      
      #axis titles and texts
      axis.title = element_text(face = "bold", size = 10),
      axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
      axis.text = element_text(family = "Courier New", face = "bold", size = 8, color = color.dark_gray),
      
      #grids
      panel.grid.major = element_line(colour = , size = 0.25),
      panel.grid.minor = element_blank(),
      
      #legend
      legend.margin = margin(t = 0, r = 10, b = 0, l = 0),
      legend.title = element_text(face = "bold", size = 9),
      legend.key.height = unit(0.8, "lines")
    )
)
