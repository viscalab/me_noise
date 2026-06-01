poisson_sigma_sym_naka_mu<- function(x, p) {
sqrt(p[1] * x ^ p[2] / (x ^ p[2] + p[3] ^ p[2])) +p[4]
}