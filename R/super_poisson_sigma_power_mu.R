super_poisson_sigma_power_mu <- function(x, p) {
  sqrt((p[1]*x^p[2]) + ((p[1]*x^p[2])*p[3])^2) +p[4]
}