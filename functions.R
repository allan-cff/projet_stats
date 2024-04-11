

# ----------------- Fonction calcul coefficient de corellation ----------------- #

Corr_Lin <-function(X, Y){
  CV <- cov(X, Y)
  sdX <-sd(X)
  sdY <-sd(Y)
  r <- CV/(sdX*sdY)
  return(r)
}
a <- 2

# ----------------- Fonction calcul coefficient de corellation ----------------- #

Parram_Stat <-function(X,Y){
  tmp <- sqrt( (1 - Corr_Lin(X,Y)^2) / (length(X)-2) )
  return(abs(Corr_Lin(X,Y))/tmp)
}