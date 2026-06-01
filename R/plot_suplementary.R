

plots <- function(data, data2, model_pattern) {

  data <- data |> 
    filter(str_detect(model, model_pattern))
  
    data_long <- data |> 
    pivot_longer(
      cols = c(mu, sigma), 
      names_to = "parameter", 
      values_to = "value"
    )
  
  dat_sum <- data2 |>
    pivot_longer(
      cols = c(m_norm, sta_dev_norm),
      names_to = "parameter",
      values_to = "sum_norm"
    ) |>
    mutate(parameter = case_match(parameter,
                                  "m_norm" ~ "mu",
                                  "sta_dev_norm" ~ "sigma",
                                  .default = parameter))
  
  ggplot(data_long) +
    facet_grid(model + parameter ~ participant,   ## parameter is sigma and mu
               scales = "free_y",
               switch = "y",
               axes = "all",
               axis.labels="margins",
               labeller = labeller(parameter = label_parsed,
                                   model = function(x) "")) +
    geom_line(aes(x = x * 100, y = value, color = parameter)) +
    geom_point(data = dat_sum, aes(x = c * 100, y = sum_norm, color = parameter)) +
    scale_color_manual(values = c("#AA7575", "navajowhite3"),
                       labels = c("mu" = expression(mu), "sigma" = expression(sigma))) +
    theme_paper(7) +
    theme(
      panel.spacing = unit(0.5, "lines"),
      #strip.text = element_blank(),
      strip.text.y.left = element_text(size = 7, angle = 90),
      #strip.text.x = element_blank(),
      strip.placement = "outside",
      legend.position = "none",
      #legend.title = element_blank(),
    ) +
    labs(x = "Contrast (%)",
         y = NULL) +
  scale_y_continuous(n.breaks=4, expand = expansion(mult = c(0,.5))) +
  scale_x_continuous(expand=c(0,0))

}

