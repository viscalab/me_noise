super_poisson_sigma_lineal_mu <- function(x, p) {
  sqrt((p[1] * x) + (((p[1] * x)*p[2])^2)) + p[3]
}