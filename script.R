library(tidyverse)
source("functions.R")


#   --------------- Initialisation des valeures, départements analysés et date des élections ---------------  #

departement <- c(44, 29, 61, 33, 13, 69, 75, 59, 37, 67) #departments
jour <- c("20020421", "20020505", "20070422", "20070506", "20120422", "20120506", "20170421", "20170507", "20220410", "20220424") # date election


#   --------------- variables globales  ---------------  #

matrice <- matrix(nrow = length(jour), ncol = length(departement))

D44_RR = c()
D29_RR = c()
D61_RR = c()
D33_RR = c()
D13_RR = c()
D69_RR = c()
D75_RR = c()
D59_RR = c()
D37_RR = c()
D67_RR = c()


#   --------------- Récupération des données météorologiques pour les départements et jours voulus  ---------------  #

for (i in seq_along(departement)) {
  
  downloadedCSV <- gzcon(url(paste("https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/QUOT/",
                                   "Q_", departement[i], "_previous-1950-2022_RR-T-Vent.csv.gz", sep="")))         # téléchargement fichier pluie pour un departement
  dat <- read.csv(textConnection(readLines(downloadedCSV)), sep=";")  # lecture du csv
  
  h=1
  for(j in jour){
    tmp <- dat %>% filter(AAAAMMJJ == j) %>% select(TX) %>% drop_na() %>% summarize(across(everything(), mean))   #récupère la moyenne pour un jour
    matrice[i, h] <- round(tmp[[1]], 2)     #remplissage matrice avec la valeur voulue
    h=h+1         #iteration moche
  }
  
}


#   --------------- Mise en forme matrice : ligne : départements / colone : date d'élection ---------------  #

colnames(matrice) <- c("[2002-1]", "[2002-2]", "[2007-1]", "[2007-2]", "[2012-1]", "[2012-2]", "[2017-1]", "[2017-2]", "[2022-1]", "[2022-2]")  # Noms des colonnes
rownames(matrice) <- c("[44]", "[29]", "[61]","[33]", "[13]", "[69]","[75]","[59]", "[37]", "[67]")  # Noms des lignes
print(matrice)


#   --------------- Analyse des données récupérés ---------------  #



for(i in 0:10){
  D44_RR = c(D44_RR, matrice[1,i])
}
for(i in 0:10){
  D29_RR = c(D29_RR, matrice[2,i])
}
for(i in 0:10){
  D61_RR = c(D61_RR, matrice[3,i])
}
for(i in 0:10){
  D33_RR = c(D33_RR, matrice[4,i])
}
for(i in 0:10){
  D13_RR = c(D13_RR, matrice[5,i])
}
for(i in 0:10){
  D69_RR = c(D69_RR, matrice[6,i])
}
for(i in 0:10){
  D75_RR = c(D75_RR, matrice[7,i])
}
for(i in 0:10){
  D59_RR = c(D59_RR, matrice[8,i])
}
for(i in 0:10){
  D37_RR = c(D37_RR, matrice[9,i])
}
for(i in 0:10){
  D67_RR = c(D67_RR, matrice[10,i])
}

Parram_Stat <-function(X,Y){
  tmp <- sqrt( (1 - Corr_Lin(X,Y)^2) / (length(X)-2) )
  return(abs(Corr_Lin(X,Y))/tmp)
}
print(Parram_Stat(D44_RR, D44_TX))

#   --------------- Moyenne, équart type et variance pour chaque départements ---------------  #

Moy_RR <- c(mean(D44_RR), mean(D29_RR), mean(D61_RR), mean(D33_RR), mean(D13_RR), mean(D69_RR), mean(D75_RR), mean(D59_RR), mean(D37_RR), mean(D67_RR))
SD_RR <- c(sd(D44_RR), sd(D29_RR), sd(D61_RR), sd(D33_RR), sd(D13_RR), sd(D69_RR), sd(D75_RR), sd(D59_RR), sd(D37_RR), sd(D67_RR))
VAR_RR <- c(var(D44_RR), var(D29_RR), var(D61_RR), var(D33_RR), var(D13_RR), var(D69_RR), var(D75_RR), var(D59_RR), var(D37_RR), var(D67_RR))
