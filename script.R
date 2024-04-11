library(tidyverse)




#   --------------- Initialisation des valeurs, départements analysés et date des élections ---------------  #

departement <- c(44, 29, 61, 33, 13, 69, 75, 59, 37, 67) #departments
jour <- c("20020421", "20020505", "20070422", "20070506", "20120422", "20120506", "20170421", "20170507", "20220410", "20220424") # date election



#   --------------- variables globales  ---------------  #

matriceRR <- matrix(nrow = length(jour), ncol = length(departement))

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
  
  for(j in seq_along(jour)){
    tmp <- dat %>% filter(AAAAMMJJ == jour[j]) %>% select(TX) %>% drop_na() %>% summarize(across(everything(), mean))   #récupère la moyenne pour un jour
    matrice[i, j] <- round(tmp[[1]], 2)     #remplissage matrice avec la valeur voulue
  }
  
}


#   --------------- Mise en forme matrice : ligne : départements / colone : date du scrutin ---------------  #

colnames(matriceRR) <- c("[2002-1]", "[2002-2]", "[2007-1]", "[2007-2]", "[2012-1]", "[2012-2]", "[2017-1]", "[2017-2]", "[2022-1]", "[2022-2]")  # Noms des colonnes
rownames(matriceRR) <- c("[44]", "[29]", "[61]","[33]", "[13]", "[69]","[75]","[59]", "[37]", "[67]")  # Noms des lignes
print(matriceRR)


#   --------------- Analyse des données récupérés ---------------  #



for(i in 0:10){
  D44_RR = c(D44_RR, matriceRR[1,i])
}
for(i in 0:10){
  D29_RR = c(D29_RR, matriceRR[2,i])
}
for(i in 0:10){
  D61_RR = c(D61_RR, matriceRR[3,i])
}
for(i in 0:10){
  D33_RR = c(D33_RR, matriceRR[4,i])
}
for(i in 0:10){
  D13_RR = c(D13_RR, matriceRR[5,i])
}
for(i in 0:10){
  D69_RR = c(D69_RR, matriceRR[6,i])
}
for(i in 0:10){
  D75_RR = c(D75_RR, matriceRR[7,i])
}
for(i in 0:10){
  D59_RR = c(D59_RR, matriceRR[8,i])
}
for(i in 0:10){
  D37_RR = c(D37_RR, matriceRR[9,i])
}
for(i in 0:10){
  D67_RR = c(D67_RR, matriceRR[10,i])
}


Parram_Stat <-function(X,Y){
  tmp <- sqrt( (1 - Corr_Lin(X,Y)^2) / (length(X)-2) )
  return(abs(Corr_Lin(X,Y))/tmp)
}
print(Parram_Stat(D44_RR, D44_TX))

#   --------------- Moyenne, équart type et variance pour chaque département ---------------  #

Moy_RR <- c(mean(D44_RR), mean(D29_RR), mean(D61_RR), mean(D33_RR), mean(D13_RR), mean(D69_RR), mean(D75_RR), mean(D59_RR), mean(D37_RR), mean(D67_RR))
SD_RR <- c(sd(D44_RR), sd(D29_RR), sd(D61_RR), sd(D33_RR), sd(D13_RR), sd(D69_RR), sd(D75_RR), sd(D59_RR), sd(D37_RR), sd(D67_RR))
VAR_RR <- c(var(D44_RR), var(D29_RR), var(D61_RR), var(D33_RR), var(D13_RR), var(D69_RR), var(D75_RR), var(D59_RR), var(D37_RR), var(D67_RR))


#   --------------- Exploitation des données pluie et abstention ---------------  #

source("functions.R")
r_RR = c(Corr_Lin(D44_RR, D44_Abs), Corr_Lin(D29_RR, D29_Abs), Corr_Lin(D61_RR, D61_Abs), Corr_Lin(D33_RR, D33_Abs), Corr_Lin(D13_RR, D13_Abs), Corr_Lin(D69_RR, D69_Abs), Corr_Lin(D75_RR, D75_Abs), Corr_Lin(D59_RR, D59_Abs), Corr_Lin(D37_RR, D37_Abs), Corr_Lin(D67_RR, D67_Abs))
print(r_RR)

t_RR = c(Parram_Stat(D44_RR, D44_Abs), Parram_Stat(D29_RR, D29_Abs), Parram_Stat(D61_RR, D61_Abs), Parram_Stat(D33_RR, D33_Abs), Parram_Stat(D13_RR, D13_Abs), Parram_Stat(D69_RR, D69_Abs), Parram_Stat(D75_RR, D75_Abs), Parram_Stat(D59_RR, D59_Abs), Parram_Stat(D37_RR, D37_Abs), Parram_Stat(D67_RR, D67_Abs))
print(t_RR)


#   --------------- Exploitation des données température et abstention ---------------  #

source("functions.R")
r_TX <- c(Corr_Lin(D44_TX, D44_Abs), Corr_Lin(D29_TX, D29_Abs), Corr_Lin(D61_TX, D61_Abs), Corr_Lin(D33_TX, D33_Abs), Corr_Lin(D13_TX, D13_Abs), Corr_Lin(D69_TX, D69_Abs), Corr_Lin(D75_TX, D75_Abs), Corr_Lin(D59_TX, D59_Abs), Corr_Lin(D37_TX, D37_Abs), Corr_Lin(D67_TX, D67_Abs))
print(r_TX)

t_TX <- c(Parram_Stat(D44_TX, D44_Abs), Parram_Stat(D29_TX, D29_Abs), Parram_Stat(D61_TX, D61_Abs), Parram_Stat(D33_TX, D33_Abs), Parram_Stat(D13_TX, D13_Abs), Parram_Stat(D69_TX, D69_Abs), Parram_Stat(D75_TX, D75_Abs), Parram_Stat(D59_TX, D59_Abs), Parram_Stat(D37_TX, D37_Abs), Parram_Stat(D67_TX, D67_Abs))
print(t_TX)


#   --------------- Affichages plots ---------------  #

plot(D44_RR, D44_Abs*100, pch = 0, col="#000000", xlab = "Pluie (mm)", ylab = "Abstention (%)")     # Plot abstention en fonction de pluie
points(D29_RR, D29_Abs*100, pch = 1, col="#4B0082")
points(D61_RR, D61_Abs*100, pch = 2, col="#666666")
points(D33_RR, D33_Abs*100, pch = 3, col="#8B4513")
points(D13_RR, D13_Abs*100, pch = 4, col="#00008B")
points(D69_RR, D69_Abs*100, pch = 5, col="#006400")
points(D75_RR, D75_Abs*100, pch = 6, col="#8B0000")
points(D59_RR, D59_Abs*100, pch = 7, col="#8B4513")
points(D37_RR, D37_Abs*100, pch = 8, col="#8A2BE2")
points(D67_RR, D67_Abs*100, pch = 9, col="#8B8B00")


plot(D44_TX, D44_Abs*100, pch = 0, col="#000000", xlab = "T°Max (°C)", ylab = "Abstention (%)")     # Plot abstention en fonction de T° max
points(D29_TX, D29_Abs*100, pch = 1, col="#4B0082")
points(D61_TX, D61_Abs*100, pch = 2, col="#666666")
points(D33_TX, D33_Abs*100, pch = 3, col="#8B4513")
points(D13_TX, D13_Abs*100, pch = 4, col="#00008B")
points(D69_TX, D69_Abs*100, pch = 5, col="#006400")
points(D75_TX, D75_Abs*100, pch = 6, col="#8B0000")
points(D59_TX, D59_Abs*100, pch = 7, col="#8B4513")
points(D37_TX, D37_Abs*100, pch = 8, col="#8A2BE2")
points(D67_TX, D67_Abs*100, pch = 9, col="#8B8B00")

