
FVP.proj.mean <- function(beta.true, beta.sel.idx, Sigma, error.sd = 1){
  p <- length(beta.true)
  A.plus.idx <- c(beta.sel.idx, p+1)
  
  Sigma.aug.11 <- Sigma
  Sigma.aug.12 <- Sigma %*% beta.true
  Sigma.aug.21 <- t(beta.true) %*% Sigma
  Sigma.aug.22 <- t(beta.true) %*% Sigma %*% beta.true + error.sd^2
  
  Sigma.aug.1. <- cbind(Sigma.aug.11, Sigma.aug.12)
  Sigma.aug.2. <- cbind(Sigma.aug.21, Sigma.aug.22)
  Sigma.aug <- rbind(Sigma.aug.1., Sigma.aug.2.)
  
  Sigma.aug.A.plus <- Sigma.aug[A.plus.idx, A.plus.idx]
  Sigma.aug.A.plus.inv <- solve(Sigma.aug.A.plus)
  Sigma.aug.A.plus.inv.Y <- Sigma.aug.A.plus.inv[(length(A.plus.idx)),]
  return(Sigma.aug.A.plus.inv.Y)
}