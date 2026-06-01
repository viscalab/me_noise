asym_naka_fun_mu <- function(x, p) {
  p[1] * x ^ p[2] / (x ^ (p[2] + p[4]) + p[3] ^ (p[2] + p[4])) 
}