d_naka_rushton_asym_no_p4 <- function(x, p) {
  mu <-  p[1] * abs(x) ^ p[2] / (abs(x) ^ (-.5 + p[2]) + p[3] ^ (-.5 + p[2]))
  if_else(x > 0, mu, -mu)
  
}