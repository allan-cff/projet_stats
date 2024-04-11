library(tidyverse)

#   --------------- Initialisation des valeurs, départements analysés et date des élections ---------------  #

departement <- c(44, 29, 61, 33, 13, 69, 75, 59, 37, 67) #departments
jour <- c("20020421", "20020505", "20070422", "20070506", "20120422", "20120506", "20170421", "20170507", "20220410", "20220424") # date election

#   --------------- variables globales  ---------------  #

matriceRR <- matrix(nrow = length(jour), ncol = length(departement))
mat_tmp <- matrix(nrow = length(jour), ncol = length(departement))

#   --------------- Récupération des données météorologiques pour les départements et jours voulus  ---------------  #

for (i in seq_along(departement)) {
  
  downloadedCSV <- gzcon(url(paste("https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/QUOT/",
                                   "Q_", departement[i], "_previous-1950-2022_RR-T-Vent.csv.gz", sep="")))         # téléchargement fichier pluie pour un departement
  dat <- read.csv(textConnection(readLines(downloadedCSV)), sep=";")  # lecture du csv
  
  for(j in seq_along(jour)){
    tmp <- dat %>% filter(AAAAMMJJ == jour[j]) %>% select(RR, TX) %>% drop_na() %>% summarize(across(everything(), mean))   #récupère la moyenne pour un jour
    matriceRR[i, j] <- round(tmp[[1]], 2)     #remplissage matrice avec la valeur voulue
    mat_tmp[i, j] <- round(tmp[[2]], 2)     #remplissage matrice avec la valeur voulue
  }
}

#   --------------- Mise en forme matrice : ligne : départements / colone : date du scrutin ---------------  #

colnames(matriceRR) <- c("[2002-1]", "[2002-2]", "[2007-1]", "[2007-2]", "[2012-1]", "[2012-2]", "[2017-1]", "[2017-2]", "[2022-1]", "[2022-2]")  # Noms des colonnes
rownames(matriceRR) <- c("[44]", "[29]", "[61]","[33]", "[13]", "[69]","[75]","[59]", "[37]", "[67]")  # Noms des lignes
print(matriceRR)

colnames(mat_tmp) <- c("[2002-1]", "[2002-2]", "[2007-1]", "[2007-2]", "[2012-1]", "[2012-2]", "[2017-1]", "[2017-2]", "[2022-1]", "[2022-2]")  # Noms des colonnes
rownames(mat_tmp) <- c("[44]", "[29]", "[61]","[33]", "[13]", "[69]","[75]","[59]", "[37]", "[67]")  # Noms des lignes
print(mat_tmp)

#   --------------- Analyse des données récupérés ---------------  #

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
D44_TX = c()
D29_TX = c()
D61_TX = c()
D33_TX = c()
D13_TX = c()
D69_TX = c()
D75_TX = c()
D59_TX = c()
D37_TX = c()
D67_TX = c()

for(i in 0:10){
  D44_RR = c(D44_RR, matriceRR[1,i])
  D44_TX = c(D44_TX, mat_tmp[1,i])
}
for(i in 0:10){
  D29_RR = c(D29_RR, matriceRR[2,i])
  D29_TX = c(D29_TX, mat_tmp[2,i])
}
for(i in 0:10){
  D61_RR = c(D61_RR, matriceRR[3,i])
  D61_TX = c(D61_TX, mat_tmp[3,i])
}
for(i in 0:10){
  D33_RR = c(D33_RR, matriceRR[4,i])
  D33_TX = c(D33_TX, mat_tmp[4,i])
}
for(i in 0:10){
  D13_RR = c(D13_RR, matriceRR[5,i])
  D13_TX = c(D13_TX, mat_tmp[5,i])
}
for(i in 0:10){
  D69_RR = c(D69_RR, matriceRR[6,i])
  D69_TX = c(D69_TX, mat_tmp[6,i])
}
for(i in 0:10){
  D75_RR = c(D75_RR, matriceRR[7,i])
  D75_TX = c(D75_TX, mat_tmp[7,i])
}
for(i in 0:10){
  D59_RR = c(D59_RR, matriceRR[8,i])
  D59_TX = c(D59_TX, mat_tmp[8,i])
}
for(i in 0:10){
  D37_RR = c(D37_RR, matriceRR[9,i])
  D37_TX = c(D37_TX, mat_tmp[9,i])
}
for(i in 0:10){
  D67_RR = c(D67_RR, matriceRR[10,i])
  D67_TX = c(D67_TX, mat_tmp[10,i])
}

#   --------------- Moyenne, équart type et variance pour chaque département ---------------  #

Moy_RR <- c(mean(D44_RR), mean(D29_RR), mean(D61_RR), mean(D33_RR), mean(D13_RR), mean(D69_RR), mean(D75_RR), mean(D59_RR), mean(D37_RR), mean(D67_RR))
SD_RR <- c(sd(D44_RR), sd(D29_RR), sd(D61_RR), sd(D33_RR), sd(D13_RR), sd(D69_RR), sd(D75_RR), sd(D59_RR), sd(D37_RR), sd(D67_RR))
VAR_RR <- c(var(D44_RR), var(D29_RR), var(D61_RR), var(D33_RR), var(D13_RR), var(D69_RR), var(D75_RR), var(D59_RR), var(D37_RR), var(D67_RR))

Moy_TX <- c(mean(D44_TX), mean(D29_TX), mean(D61_TX), mean(D33_TX), mean(D13_TX), mean(D69_TX), mean(D75_TX), mean(D59_TX), mean(D37_TX), mean(D67_TX))
SD_TX <- c(sd(D44_TX), sd(D29_TX), sd(D61_TX), sd(D33_TX), sd(D13_TX), sd(D69_TX), sd(D75_TX), sd(D59_TX), sd(D37_TX), sd(D67_TX))
VAR_TX <- c(var(D44_TX), var(D29_TX), var(D61_TX), var(D33_TX), var(D13_TX), var(D69_TX), var(D75_TX), var(D59_TX), var(D37_TX), var(D67_TX))

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

