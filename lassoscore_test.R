

rm(list=ls())
library(knitr)
library(MASS)
library(Matrix)
library(lassoscore)
library(glmnet)

# Simulate Sigma variance-covariance matrix 
Sigma.k.cor <- function(k.cor.base, p.tmp){
  Sigma.mat.tmp <- matrix(k.cor.base, p.tmp, p.tmp) +  matrix(rnorm(p.tmp**2, sd = 0.03), p.tmp, p.tmp)
  range(Sigma.mat.tmp)
  diag(Sigma.mat.tmp) <- rep(1, p.tmp)
  Sigma.mat.tmp <- as.matrix(nearPD(Sigma.mat.tmp)$mat)
  Sigma.mat.tmp
}

# Simulte data 
p.tmp <- 300
n.tmp <- 100
set.seed(1)
S <- Sigma.k.cor(0.2, p.tmp)
X <- scale(mvrnorm(n.tmp, mu = rep(0, p.tmp), Sigma = S))
beta.true <- rep(0, p.tmp)
beta.true[sample(1:p.tmp, 10, replace = FALSE)] <- 10
Y.true <- X %*% beta.true
Y.obs <- Y.true + matrix(rnorm(n.tmp))

# Function arguments 
y <- Y.obs
X = X
lambda = 10
family = "gaussian"
tol = .Machine$double.eps
maxit = 1000
resvar = NULL 
verbose = FALSE
subset = NULL

# ------------------------------------------------------------------------------



function (y, X, lambda = 0, family = c("gaussian", "binomial", "poisson"), 
          tol = .Machine$double.eps, maxit = 1000, resvar = NULL, 
          verbose = FALSE, subset = NULL){
  
  # family = match.arg(family)
  family.obj <- get(family, mode = "function", envir = parent.frame())()
  # 
  # if (!(family %in% c("gaussian", "binomial", "poisson"))) 
  #   stop("family not supported!")
  X <- scale(X) * sqrt(nrow(X)/(nrow(X) - 1))
  
  if (family == "gaussian") y <- scale(y, scale = FALSE)
  
  out0 <- glmnet(y = y, x = X, family = family, lambda = lambda, 
                 thresh = tol, maxit = maxit, standardize = FALSE)
  coef(out0)
  out0$r <- as.vector(family.obj$linkinv(as.vector(out0$a0 + X %*% out0$beta)) - y)
  out0$v <- family.obj$var(family.obj$linkinv(as.vector(out0$a0 + X %*% out0$beta)))
  
  if (family == "gaussian" & !is.numeric(resvar)) {
    resvar <- sum(out0$r^2)/(length(y) - sum(out0$beta != 0))
  }
  
  # else if (family != "gaussian") {
  #   resvar <- 1
  # }
  
  out0$n <- nrow(X)
  out0$beta <- as.vector(out0$beta)
  out0$beta
  wh <- as.vector(out0$beta != 0)
  
  if (is.null(subset)) {
    subset <- 1:ncol(X)
  }
  
  if (verbose) {
    cat("\nProgress:\n")
    pb <- txtProgressBar(min = 0, max = length(subset), style = 3)
    pb.i <- 0
  }
  
  scores <- scorevar.sand.cons <- scorevar.model.cons <- scorevar.sand <- scorevar.model <- numeric(ncol(X))
  scores[-subset] <- NA
  
  # Compute scores 
  for (i in subset) {
    
    if (out0$beta[i] != 0) {
      out <- glmnet(y = y, x = X[, -i], family = family, 
                    lambda = lambda, thresh = tol, maxit = maxit, 
                    standardize = FALSE)
      out$r <- as.vector(family.obj$linkinv(out$a0 + as.vector(X[, -i] %*% out$beta)) - y)
      out$v <- family.obj$var(family.obj$linkinv(out$a0 + as.vector(X[, -i] %*% out$beta)))
      out$n <- nrow(X)
      out$beta <- as.vector(out$beta)
      Xs <- X[, -i, drop = FALSE][, as.vector(out$beta != 0), drop = FALSE]
    }
    else {
      out <- out0
      Xs <- X[, as.vector(out$beta != 0), drop = FALSE]
    }
    
    xx <- X[, i, drop = FALSE]
    scores[i] <- sum(out$r * X[, i])/sqrt(out$n)
    scorevar.model.cons[i] <- with(out, resvar * (crossprod(xx * sqrt(v))/n))
    scorevar.sand.cons[i] <- with(out, var(xx * r) * (n - 1)/n)
    
    if (ncol(Xs) == 0) {
      scorevar.sand[i] <- scorevar.sand.cons[i]
      scorevar.model[i] <- scorevar.model.cons[i]
    }
    else if (nrow(Xs) > ncol(Xs) & ncol(Xs) > 0) {
      Ui <- with(out, solve(crossprod(Xs * sqrt(v))/n))
      V <- with(out, var(r * Xs) * (n - 1))
      Va <- with(out, cov(r * Xs, r * xx) * (n - 1))
      Ua <- with(out, crossprod(Xs, v * xx)/n)
      va <- with(out, var(xx * r) * (n - 1))
      scorevar.sand[i] <- (va + t(Ua) %*% (Ui) %*% (V %*%  Ui %*% Ua - 2 * Va))/with(out, n - sum(beta !=  0))
      scorevar.model[i] <- with(out, resvar * (crossprod(xx * sqrt(v))/n - t(Ua) %*% Ui %*% Ua))
    }
    
    if (verbose) {
      pb.i <- pb.i + 1
      setTxtProgressBar(pb, pb.i)
    }
  }
  
  re <- list(fit = out0, 
             scores = scores, 
             scorevar.model.cons = scorevar.model.cons, 
             scorevar.sand.cons = scorevar.model.cons, 
             scorevar.model = scorevar.model, 
             scorevar.sand = scorevar.sand, 
             p.model.cons = pchisq(scores^2/scorevar.model.cons, df = 1, lower.tail = FALSE), 
             p.sand.cons = pchisq(scores^2/scorevar.sand.cons, df = 1, lower.tail = FALSE), 
             p.model = pchisq(scores^2/scorevar.model, df = 1, lower.tail = FALSE), 
             p.sand = pchisq(scores^2/scorevar.sand,  df = 1, lower.tail = FALSE), 
             lambda = lambda)
                                                                                                                                                                                                               df = 1, lower.tail = FALSE), lambda = lambda)
  class(re) <- "lassoscore"
  if (verbose) 
    close(pb)
  return(re)
}