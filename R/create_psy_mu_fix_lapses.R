create_psy_mu_fix_lapses <- function(asymp_l = 0, asymp_r = 0) { 
  
  function(x_p, x_c, fun_mu, p) {
    phi <- pnorm(0,
                 mean = fun_mu(x_c, p) - fun_mu(x_p, p),
                 sd = 1, lower.tail = FALSE) 
    
    asymp_l + (1 - asymp_l - asymp_r) * phi
  }
}
