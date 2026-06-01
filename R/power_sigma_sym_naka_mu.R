
power_sigma_sym_naka_mu <- function(x, p) {
 p[5] + p[4] * (p[1] * x ^ p[2] / (x ^ p[2] + p[3] ^ p[2]))^p[6]
}
