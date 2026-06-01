create_nll_psy <- function(x, y, fun) {
  function(p) {
    phi <- fun(x, p)
    -sum(if_else(y == 1, log(phi), log(1 - phi)))
  }
}