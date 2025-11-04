
theme_irpp <- function(base_size = 11, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      text = element_text(family = base_family,size = base_size),
      # Titles
      plot.title = element_text(face = "bold", size = base_size + 2, hjust = 0, 
                                color = "#003366"),
      plot.subtitle = element_text(size = base_size, hjust = 0),
      
      # Axis titles
      axis.title = element_text(size = base_size-1),
      #axis.title.x = element_text(margin = margin(t = 10)),
      #axis.title.y = element_text(margin = margin(r = 10)),
      axis.text.x = element_text(margin = margin(t = 3)),
      
      # Axis text
      axis.text = element_text(size = base_size - 1, color = "black"),
      
      # Grid lines
      panel.grid.major.y = element_line(color = "grey80"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      
      # Backgrounds
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      
      # Axis lines and ticks
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      
      # Legend
      legend.position = "bottom",
      legend.background = element_blank(),
      legend.key = element_blank(),
      
      # Margins
      plot.margin = margin(10, 15, 10, 10)
    )
}
