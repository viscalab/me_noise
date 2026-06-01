fit_psy <- function(x, 
                    y, 
                    fun,
                    par_ini,
                    x_min = min(x), 
                    x_max = max(x),
                    full = TRUE) {
  
  nll <- create_nll_psy(x, y, fun)
  
  fit <-  optim(par_ini, nll)
  
  log_lik = -fit$value
  
  
  if (full) {
    
    prop <- tibble(x = x, y = y) |> 
      group_by(x) |> 
      summarise(k = sum(y), n = n(), prop = k /n)
    
    par <- tibble(par = fit$par) |> 
      mutate(parn = paste0("p", row_number())) |> 
      relocate(parn)
    
    x_lim <- tibble(x_min = x_min, 
                    x_max = x_max) 
    
    psy <- tibble(x = seq(x_lim$x_min -.01,
                          x_lim$x_max + 0.02,
                          length = 401)) |>
      mutate(prop = fun(x, par$par))
    
    n_par <- length(fit$par)
    
    aic <-  2 * n_par - 2 * log_lik
    
    list(prop = prop,
         par = par, 
         psy = psy, 
         log_lik = log_lik,
         n_par = n_par,
         aic = aic)
  }
  else {
    list(log_lik = log_lik)
  }
}