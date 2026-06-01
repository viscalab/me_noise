plot_suplementary_model <- function(data) {

  data <- data |> select(participant, pred) |> unnest(pred) 
  
  data_long <- data |> 
    pivot_longer(
      cols = c(mu, sigma), 
      names_to = "parameter", 
      values_to = "value"
    )
  
  dat_sum <- m_summ_bin |>
    pivot_longer(
      cols = c(m_norm, sta_dev_norm),
      names_to = "parameter",
      values_to = "sum_norm"
    ) |>
    mutate(parameter = case_match(parameter,
                                  "m_norm" ~ "mu",
                                  "sta_dev_norm" ~ "sigma",
                                  .default = parameter))
  math_labels <- c(
    "mu"    = "mu",
    "sigma" = "sigma"
  )
  
  p <- ggplot() +
    facet_grid(parameter ~ participant, 
               scales = "free", 
               axis.labels = "margins",
               axes = "all",
               switch = "y", 
               labeller = labeller(parameter = as_labeller(math_labels, default = label_parsed))) +
    
    geom_line(data = data_long, aes(x = x * 100, y = value, color = parameter), linewidth = .3) +
    geom_point(data = dat_sum, aes(x = c * 100, y = sum_norm, color = parameter), size = 0.5, shape = 16) +
    
    scale_color_manual(values = c("#AA7575", "navajowhite3"),
                       labels = c("mu" = expression(mu), "sigma" = expression(sigma))) +
    
    theme_paper(7) +
    theme(
      strip.text.x = element_blank(),
      strip.placement = "outside",
      strip.background = element_blank(),
      legend.position = "none"
    ) +
    scale_y_continuous(breaks = c(0,2,4), limits = c(NA, 5), expand=expansion(mult = c(0, 0.05))) +
    scale_x_continuous(limits=c(NA,15), expand=expansion(mult = c(0, 0.05))) +
    labs(x = "Contrast (%)", y = NULL) # Eliminamos el título global si ya no lo necesitas
  
  p
}


