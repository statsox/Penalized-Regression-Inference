
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



#' Function to plot method "discoveries"
#' 
plot.discoveries <- function(beta, beta.is.selected){
  p <- length(beta)
  beta.selected <- rep(0, p)
  beta.selected[beta.is.selected] <- max(beta)
  plot.df <- data.frame(x = 1:p, beta.true = beta, beta.selected = beta.selected)
  plot.df$color <- sapply(1:p, function(i){
    beta.true.val <- plot.df$beta.true[i]
    beta.sel.val <- plot.df$beta.selected[i]
    if (beta.true.val != 0 & beta.sel.val == 0){
      return("FN")
    } else if (beta.true.val == 0 & beta.sel.val != 0){
      return("FP")
    } else if (beta.true.val == 0 & beta.sel.val == 0){
      return("TN")
    } else if (beta.true.val != 0 & beta.sel.val != 0){
      return("TP")
    }
  })
  
  plt <- 
    ggplot(data = plot.df, aes(x = x, y = beta.selected, color = color)) + 
    geom_point() + 
    labs(title = "", x = "", y = "", color = "") + 
    theme_bw(base_size = base_size.gg, base_family = "Helvetica") + 
    scale_color_manual(values=c("brown", "red", "green", "blue"), 
                       name="",
                       breaks=c("FN", "FP", "TN", "TP"))
  plot(plt)
}