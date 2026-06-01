
#try regularization with me

fit_mle_reg_fisher <- function(.x, 
                        .y, 
                        .funmu, 
                        .funsigma, 
                        .par_ini,
                        .par_ini_lower, 
                        .par_ini_upper,
                        method, 
                        #algorithm = "NLOPT_LN_BOBYQA", 
                        algorithm = "NLOPT_LN_COBYLA", 
                        samples, 
                        regularization = FALSE,
                        lambda = NULL, 
                        reg_m = NULL, 
                        reg_sd = NULL,
                        full = TRUE) {
  
  create_log_lik_fun_reg <- function(x, y, funmu, funsigma, reg_m, reg_sd, lambda){ 
    function(p) {
      n <- length(x)
      mu <- funmu(x, p)
      sigma <- funsigma(x, p)
      log_lik <- -0.5 * n * log(2 * pi) - sum(log(sigma)) - 0.5 * sum((y - mu)^2 / sigma^2) 
      
      z_p <- (p - reg_m) / reg_sd
      penalty_l2 <- lambda * sum(z_p^2) 
      
      log_lik - penalty_l2
    }
  }
  create_log_lik_fun <- function(x, y, funmu, funsigma){ 
    function(p) {
      n <- length(x)
      mu <- funmu(x, p)
      sigma <- funsigma(x, p)
      -0.5 * n * log(2 * pi) - sum(log(sigma)) - 0.5 * sum((y - mu)^2 / sigma^2) 
    }
  }
  
  if (regularization) {
    log_lik_fun <- create_log_lik_fun_reg(.x, .y, .funmu, .funsigma, reg_m, reg_sd, lambda)
  } else {
    log_lik_fun <- create_log_lik_fun(.x, .y, .funmu, .funsigma)
  }

  
  nll <- function(.x) -log_lik_fun(.x) 
  
  
  if (method == "nloptr") {
    
    fit <- nloptr(eval_f = nll,  
                  x0 = .par_ini,                
                  lb = .par_ini_lower,    
                  ub = .par_ini_upper,      
                  opts = list(
                    algorithm = algorithm,
                    xtol_rel = 1e-6,
                    maxeval = samples)
    )
    
    param <- fit$solution
    log_lik <- -fit$objective
    
  }

  if (method == "optim") {
    
    fit <- optim(par = .par_ini, 
                 fn = nll,
                 lower = .par_ini_lower,
                 upper = .par_ini_upper,
                 method = "L-BFGS-B",
                 control = list(maxit = samples))
    
    param <- fit$par
    log_lik <- -fit$value
    
  }
  
  if (method == "DEoptim") {
    
    fit <- DEoptim(nll, 
                   lower = .par_ini_lower, upper = .par_ini_upper,
                   DEoptim.control(trace = FALSE, itermax = samples))
    
    
    param <- fit$optim$bestmem
    log_lik <- -fit$optim$bestval
    
  }

  
  if (full) {
  
    par <- tibble(par = param) |>
      mutate(parn = paste0("p", row_number())) |>
      relocate(parn)

    pred <- tibble(x = seq(0.005, 0.14, length = 400)) |>#x = seq(min(.x), max(.x), length = 400)) |>
      mutate(mu = .funmu(x, par$par)) |> 
      mutate(sigma = .funsigma(x, par$par)) |> 
     mutate(slope = (mu - lag(mu)) / (x - lag(x)),
            delta_m = slope / sigma,
            delta_fisher = delta_m * sqrt(1+(2*(par$par[5]*par$par[7])^2)*(mu^(2*(par$par[7]-1))))) |> 
       mutate(x_log = log10(x),
              delta_m_log = log10(delta_m)) 
  
  
    #log_lik <- -fit$value
    k <- nrow(par)
    AIC <- -2 * log_lik + 2 * k
    
    BIC <- -2 * log_lik + log(length(.y)) * k

    list(par = par,
         pred = pred,
         log_lik = log_lik,
         AIC = AIC,
         BIC = BIC)
  } 
  else {
    list(log_lik = log_lik)
  }
}


