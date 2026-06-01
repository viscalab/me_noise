calculate_g <- function(y, cond, reverse = FALSE) {
  
  dat <- tibble(y = y, cond = cond)
  
  summary <- dat |>
    group_by(cond) |>
    summarise(m = mean(y), sd = sd(y), n = n(), .groups = "drop")
  
  g <- esc_mean_sd(
    grp1m = summary$m[1],
    grp1sd = summary$sd[1],
    grp1n = summary$n[1],
    grp2m = summary$m[2],
    grp2sd = summary$sd[2],
    grp2n = summary$n[2],
    es.type = "g"
  ) |> as_tibble()
  
  
  if (reverse) {
    g  <- g |>
      mutate(across(c(es, ci.lo, ci.hi), ~ -1 * .x)) |>
      rename(ci.hi = ci.lo, ci.lo = ci.hi) |>
      relocate(ci.hi, .after = ci.lo)
  }
  
  g |> 
    rename(
      estimate = es,
      conf.low = ci.lo,
      conf.high = ci.hi
    )
}
