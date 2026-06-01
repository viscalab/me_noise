fit_psy_mu <- function(x_p,
                       x_c,
                       y, 
                       fun,
                       fun_mu, 
                       par_ini, 
                       par_ini_lower, 
                       par_ini_upper,
                       method, 
                       algorithm = "NLOPT_LN_COBYLA",
                       samples, 
                       regularization = FALSE,
                       lambda = NULL, 
                       reg_m = NULL, 
                       reg_sd = NULL,
                       x_min = min(x_p), 
                       x_max = max(x_c),
                       full = TRUE) {
  

  if (regularization) {
    nll <- create_nll_psy_mu_regularization(x_p, x_c, y, fun, fun_mu, reg_m, reg_sd, lambda)
  } else {
    nll <- create_nll_psy_mu(x_p, x_c, y, fun, fun_mu)
  }
  

  if (method == "nloptr") {
    
   
    
    fit <- nloptr(eval_f = nll,  
                   x0 = par_ini,                
                   lb = par_ini_lower,    
                   ub = par_ini_upper,      
                   opts = list(
                     algorithm = algorithm,
                     xtol_rel = 1e-6,
                     maxeval = samples)
                  )

     param <- fit$solution
     log_lik <- -fit$objective

  }
  
  if (method == "DEoptim") {
    
    fit <- DEoptim(nll, 
                   lower = par_ini_lower, upper = par_ini_upper,
                   DEoptim.control(trace = FALSE, itermax = samples))


    param <- fit$optim$bestmem
    log_lik <- -fit$optim$bestval
    
  }
  


  if (full) {

    prop <- tibble(x_p = x_p, x_c = x_c, y = y) |>
      group_by(x_p, x_c) |>
      summarise(k = sum(y), n = n(), prop = k /n, .groups = "keep")

    par <- tibble(par = param) |>
      mutate(parn = paste0("p", row_number())) |>
      relocate(parn)
    
    x_lim <- tibble(x_p = x_p, x_c = x_c) |>
      group_by(x_p) |>
      summarise(xx_min = min(x_c), xx_max = max(x_c)) |> 
      mutate(x_min = if_else(x_p > 0, xx_min, xx_max),
             x_max = if_else(x_p > 0, xx_max, xx_min)) 
    
    psy <- x_lim |>
      group_by(x_p) |>
      reframe(x_c = seq(x_p, x_max, length = 401)) |>
      mutate(prop = fun(x_p, x_c, fun_mu, par$par))
    
    mu <- tibble(x = seq(0.005, 0.14, length = 400)) |> #seq(min(x_c), max(x_c), length = 401)) |>
      mutate(y = fun_mu(x, par$par),
             slope = (y - lag(y)) / (x -lag(x)),
             delta = slope * sqrt(2)) |>
      drop_na()

    n_par <- length(param)

    aic <-  2 * n_par - 2 * log_lik

    list(prop = prop,
         par = par,
         psy = psy,
         mu = mu,
         log_lik = log_lik,
         n_par = n_par,
         aic = aic)
  }
  else {
    list(log_lik = log_lik)
  }
}
