stats_comparison <- function(dat, m, d, x_label = "", y_label = "") {
  
  m_sym <- rlang::ensym(m)
  d_sym <- rlang::ensym(d)
  
  m <- dat[[rlang::as_string(m_sym)]]
  d <- dat[[rlang::as_string(d_sym)]]
  
  
  df <- tibble(m = m, d = d) |>
    drop_na()
  
  m <- df$m
  d <- df$d
  
## correlation
  cor_test <- cor.test(m, d, method = "pearson")
  print(cor_test)
  
  cor_p <- tidy(cor_test) |>
    pull(p.value)
  

  cor_v <- cor_test$estimate
## bf correlation
  bf_cor <- correlationBF(m, d, nullInterval = c(0, 1))
  print(bf_cor)
  
  bf_cor_value <- as.data.frame(bf_cor) |>
    slice(1) |>
    pull(bf)
  
## t test
  
  t_test <- t.test(d, m, paired = TRUE)
  print(t_test)
  
  t_p <- tidy(t_test) |>
    pull(p.value)

## bf t test

  bf_t <- ttestBF(d, m, paired = TRUE)
  print(bf_t)
  
  bf_t_value <- as.data.frame(bf_t) |>
    slice(1) |>
    pull(bf)
  
  
## plot
  plot_df <- dat |>
    select(participant, !!m_sym, !!d_sym) |>
    rename(d = !!d_sym, m = !!m_sym) |>
    pivot_longer(
      cols = c(m, d),
      names_to = "model",
      values_to = "contrast"
    ) |>
    drop_na() |>
    mutate(model = factor(model))
  
  #cols <- c("darkorchid4", "aquamarine4")
  
  summary_df <- plot_df |>
    group_by(model) |>
    summarise(calculate_t_ci(contrast), .groups = "drop")
  
  p_tt <- ggplot(summary_df, aes(x = model, y = estimate, color = model)) +
    geom_line(
      data = plot_df,
      aes(y = contrast, group = participant),
      color = "gray70",
      alpha = 0.7, linewidth=0.3
    ) +
    geom_point(
      data = plot_df,
      aes(y = contrast, group = participant),
      alpha = 0.75, size = .75, shape=16) +
    geom_crossbar(aes(y = estimate, ymin = conf.low, ymax = conf.high), linewidth = 0.25) +
    # labs(title = bquote("P = "*.(label_number(t_p))),
    #      subtitle = bquote( "BF"[10] * " = " *.(label_number(bf_t_value)))) +
    labs(title = bquote("P = "*.(label_number(t_p))*
                          ", BF"[10] * " = " *.(label_number(bf_t_value)))) +
    scale_color_manual(values = c("darkorchid4", "aquamarine4")) +
    scale_x_discrete(labels = c("m" = "ME", "d" = "Discr")) +
    ylab(x_label) +
    xlab("") +
    theme_paper(7) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3),
                limits = c(0, 500), expand = c(0,0)) +
    theme(legend.position = "none",
          #axis.text.x  = element_blank(),
          axis.ticks.x = element_blank(),
          plot.title = element_text(size = 5))
          #plot.subtitle = element_text(size = 5))

  
  lm <- lm(d ~ m)
  print(lm)
  
  p_cor <- 
    ggplot(df, aes(x = m, y = d)) +
    geom_point(color = "grey60", size=0.75, shape = 16, alpha = 0.75) +
    geom_abline(slope = 1, intercept = 0, linetype="dashed", linewidth=0.25) +
    geom_abline(slope = lm$coefficients[2], intercept = lm$coefficients[1], color = "grey60", linewidth=0.3) +
    labs(title = bquote(
      "R = " * .(label_number(cor_test$estimate)) * ", P = " * .(label_number(cor_p))),
      subtitle = bquote("BF"[+0] * " = " * .(label_number(bf_cor_value))
      )) +
    theme_paper(7) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          plot.title = element_text(size = 5),
          plot.subtitle = element_text(size = 5)) +
    ylab(bquote(.(x_label)[Discr])) +
    xlab(bquote(.(x_label)[ME]))
    
  list(
    ttest_p = t_p,
    cor_p = cor_p,
    cor_v = cor_v,
    bf_ttest = bf_t_value,
    bf_correlation = bf_cor_value,
    plot_cor = p_cor,
    plot_tt = p_tt
  )
}
