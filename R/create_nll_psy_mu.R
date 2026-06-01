create_nll_psy_mu <- function(x_p, x_c, y, fun, fun_mu) {
  function(p) {
    phi <- fun(x_p, x_c, fun_mu, p)
    
    eps <- 1e-12
    phi <- pmin(pmax(phi, eps), 1 - eps)
    
    nll <- -sum(if_else(y == 1, log(phi), log(1 - phi)))
    nll
  }
}

## usa la funcion psicometrica para calcular la log-likelihood
## se usa en la optimización