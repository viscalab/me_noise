calculate_threshold <- function(x, y, prop) {
  thre <- approx(y, x, xout = prop, ties = "ordered")$y
  
  tibble(prop, thre)
}