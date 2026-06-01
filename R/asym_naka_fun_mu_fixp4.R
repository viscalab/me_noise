asym_naka_fun_mu_fixp4 <- function(x, p) {
  p[1] * x ^ p[2] / (x ^ (p[2] + (-.5)) + p[3] ^ (p[2] + (-.5))) 
}