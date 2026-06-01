super_poisson_sigma_asym_naka_mu<- function(x, p) {
  sqrt(p[1] * x ^ p[2] / (x ^ (p[2] + p[4]) + p[3] ^ (p[2] + p[4]))) 
       + (p[5] * (p[1] * x ^ p[2] / (x ^ (p[2] + p[4]) + p[3] ^ (p[2] + p[4]))))^2 + p[6]
}