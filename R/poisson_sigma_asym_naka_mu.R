poisson_sigma_asym_naka_mu <- function(x, p) {
 p[5] + sqrt(p[1] * x ^ p[2] / (x ^ (p[2] + p[4]) + p[3] ^ (p[2] + p[4]))) 
}