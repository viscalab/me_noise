create_logistic_fix_lapses <- function(asymp_l = .02, asymp_r = .02) {
  
  function(x, p) {
    asymp_l + (1 - asymp_l - asymp_r) * 1 / (1 + exp(-p[1] - p[2] * x))
  }
}