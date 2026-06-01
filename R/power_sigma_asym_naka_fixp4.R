power_sigma_asym_naka_fixp4 <- function(x, p) {
 p[5] + p[4] * (p[1] * x ^ p[2] / (x ^ (p[2] + (-.5)) + p[3] ^ (p[2] + (-.5))))^p[6]
}

