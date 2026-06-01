psy_logistic_fun <- function(x, p) {
  
  (0.02 + (1 - 2 * 0.02)) * 1 / (1 + exp(-p[1] - p[2] * x))
  
}