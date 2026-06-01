fit_m_ind_fun <- function(dat, 
                          m_summ_bin,
                          participant_id, 
                          mu, 
                          sigma, 
                          par_ini_lower, 
                          par_ini_upper) {
  
  dat_participant <- dat |> 
    filter(.data$participant == participant_id)
  
  m_summ_bin_participant <- m_summ_bin |> 
    filter(.data$participant == participant_id)
  
  fit <- dat_participant |>
    group_by(participant) |>
    nest() |>
    mutate(fit = map(data, function(d) {
      fit_mle_reg(
        d$c,
        d$r_norm,
        mu,
        sigma,
        .par_ini_lower = par_ini_lower,
        .par_ini_upper = par_ini_upper,
        method = "DEoptim",
        samples = 800,
        regularization = FALSE
      )
    })) |>
    unnest_wider(fit)
  
  m_pred <- fit |>
    group_by(participant) |>
    select(participant, pred)|>
    unnest(pred) 
  
  mu_raw <- ggplot() + 
    geom_point(data = dat_participant, aes(x = c, y = r_norm))+
    geom_line(data = m_pred, 
              aes(x = x, y = mu), color = "aquamarine4", linewidth = 1.2)
  
  mu <- ggplot() + 
    facet_wrap(vars(participant), scales = "free") +
    geom_point(data = m_summ_bin_participant, aes(x = c, y = m_norm)) +
    geom_line(data = m_summ_bin_participant, aes(x = c, y = m_norm)) +
    geom_line(data = m_pred, 
              aes(x = x, y = mu), color = "aquamarine4", linewidth = 1.2)
  
  sigma <- ggplot() + 
    geom_point(data = m_summ_bin_participant, aes(x = c, y = sta_dev_norm)) +
    geom_line(data = m_summ_bin_participant, aes(x = c, y = sta_dev_norm)) +
    geom_line(data = m_pred,
              aes(x = x, y = sigma), color = "navajowhite3", linewidth = 1.2) +
    ylim(0, NA) 
  
  p <- plot_grid(mu_raw, mu, sigma, ncol = 3)
  
  p_par <- fit |> 
    select(participant, par) |> 
    unnest(par) |> 
    ggplot(aes(x = parn, y = par)) +
    facet_wrap(~parn, scales = "free") +
    geom_point(aes(color = participant), 
               position = position_jitter(height = 0)) +
    geom_point(data = tibble(parn = paste0("p", seq_along(par_ini_upper)), 
                             par = par_ini_upper), color = "red", shape = 2) +
    geom_point(data = tibble(parn = paste0("p", seq_along(par_ini_lower)), 
                             par = par_ini_lower), color = "red", shape = 2) +
    theme()
  
  list(fit = fit, 
       pred = m_pred, 
       p = p, 
       p_par = p_par)
  
}