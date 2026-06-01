theme_paper <- function(base_size = BASE_SIZE) {
  
  theme_classic(base_size = base_size) +
    
    theme(
      
      axis.line = element_line(colour = "black", linewidth = 0.25),
      axis.ticks = element_line(colour = "black", linewidth = 0.25),
      #   axis.text = element_text(size = rel(1), color = "black"),
      #   axis.title = element_text(size = rel(1), face = "plain"),
      
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.size = unit(.11, "in"),
      legend.title = element_text(size = base_size),
      legend.text = element_text(size = base_size - 1, color = "grey30"),
      
      # --- Panel ---
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      # --- Facets ---
      #  strip.background = element_rect(fill = "gray90", color = "gray90", size = 0.5),
      strip.background = element_blank(),
      plot.title = element_text(size = base_size), 
      plot.subtitle = element_text(size = base_size)
      #,
      #  strip.text = element_text(size = rel(1), face = "bold", margin = margin(t = 4, b = 4))
    #  plot.margin = margin(0, 0, 0, 0)
    )
}