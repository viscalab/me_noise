d_naka_rushton_asym <- function(x, p) {
  mu <-  p[1] * abs(x) ^ p[2] / (abs(x) ^ (p[4] + p[2]) + p[3] ^ (p[4] +p[2]))
  if_else(x > 0, mu, -mu)
  
}