

# ----------------- Fonction calcul coefficient de correlation ----------------- #

Corr_Lin <-function(X, Y){
  CV <- cov(X, Y)
  sdX <-sd(X)
  sdY <-sd(Y)
  r <- CV/(sdX*sdY)
  return(r)
}


# ----------------- Fonction calcul paramètre statistique ----------------- #

Parram_Stat <-function(X,Y){
  tmp <- sqrt( (1 - Corr_Lin(X,Y)^2) / (length(X)-2) )
  return(abs(Corr_Lin(X,Y))/tmp)
}


# ----------------- Fonction de comparaison t théorique et t obtenu ----------------- #

testT <-function(t, t_th){
  if(t<t_th){
    return(0)
  }else{
    return(1)
  }
}