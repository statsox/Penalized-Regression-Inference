
#' Simulate Sigma variance-covariance matrix with 1 on diagonal and off-diagonal
#' values set to some fixed value + noise. 
#' 
Sigma.k.cor <- function(k.cor.base, p.tmp){
  Sigma.mat.tmp <- matrix(k.cor.base, p.tmp, p.tmp) +  matrix(rnorm(p.tmp**2, sd = 0.03), p.tmp, p.tmp)
  range(Sigma.mat.tmp)
  diag(Sigma.mat.tmp) <- rep(1, p.tmp)
  Sigma.mat.tmp <- as.matrix(nearPD(Sigma.mat.tmp)$mat)
  Sigma.mat.tmp
}