create_nll_psy_mu_regularization <- function(x_p, x_c, y, fun, fun_mu, reg_m, reg_sd, lambda) {
  function(p) {
    phi <- fun(x_p, x_c, fun_mu, p)
    nll <- -sum(if_else(y == 1, log(phi), log(1 - phi)))
    penalty_l2 <- lambda * sum(((p - reg_m) / reg_sd)^2) # Ridge 
    
    nll + penalty_l2
    
  }
}
