# label_number <- function(x) {
#   if (x == 0) return("0")
#   
#   # Reglas para números entre 0 y 1
#   if (x > 0 && x < 1) {
#     if (x < 0.001) {
#       sci_string <- formatC(x, digits = 2, format = "e") # Corregido para 3 cifras significativas
#       parts <- strsplit(sci_string, "e")[[1]]
#       mantissa <- as.numeric(parts[1])
#       exponent <- as.integer(parts[2])
#       return(bquote(.(mantissa) %*% 10^.(exponent)))
#     } else {
#       return(signif(x, digits = 3))
#     }
#   }
#   
#   # Reglas para números >= 1
#   if (x >= 1) {
#     if (x >= 1000) {
#       sci_string <- formatC(x, digits = 2, format = "e") # Corregido para 3 cifras significativas
#       parts <- strsplit(sci_string, "e")[[1]]
#       mantissa <- as.numeric(parts[1])
#       exponent <- as.integer(parts[2])
#       return(bquote(.(mantissa) %*% 10^.(exponent)))
#     } else {
#       return(signif(x, digits = 3))
#     }
#   }
# }

label_number <- function(x) {
  if (length(x) != 1) {
    return(sapply(x, label_number))
  }
  
  # Return NA for invalid inputs
  if (is.null(x) || is.na(x)) {
    return(NA)
  }
  
  # Handle zero separately
  if (x == 0) {
    return("0")
  }
  
  # Use the absolute value for logical checks
  num <- abs(x)
  
  if (num < 1) {
    # Rule for very small numbers (positive or negative)
    if (num < 0.001) {
      sci_string <- formatC(num, digits = 2, format = "e")
      parts <- strsplit(sci_string, "e")[[1]]
      mantissa <- as.numeric(parts[1])
      exponent <- as.integer(parts[2])
      
      # Add the negative sign back into the expression if needed
      if (x < 0) {
        return(bquote(-.(mantissa) %*% 10^.(exponent)))
      } else {
        return(bquote(.(mantissa) %*% 10^.(exponent)))
      }
    } else {
      # Rule for other small numbers
      return(signif(x, digits = 3))
    }
  }
  
  if (num >= 1) {
    # Rule for very large numbers (positive or negative)
    if (num >= 1000) {
      sci_string <- formatC(num, digits = 2, format = "e")
      parts <- strsplit(sci_string, "e")[[1]]
      mantissa <- as.numeric(parts[1])
      exponent <- as.integer(parts[2])
      
      # Add the negative sign back into the expression if needed
      if (x < 0) {
        return(bquote(-.(mantissa) %*% 10^.(exponent)))
      } else {
        return(bquote(.(mantissa) %*% 10^.(exponent)))
      }
    } else {
      # Rule for other large numbers
      return(signif(x, digits = 3))
    }
  }
}