library(tidyverse)
source("functions.R")

#   --------------- Initialisation des valeures, départements analysés et date des élections ---------------  #

departement <- c(44, 29, 61, 33, 13, 69, 75, 59, 37, 67) #departments
jour <- c("20020421", "20020505", "20070422", "20070506", "20120422", "20120506", "20170421", "20170507", "20220410", "20220424") # date election


#   --------------- variables globales ---------------  #

mat_tmp <- matrix(nrow = length(jour), ncol = length(departement))

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


#   --------------- Récupération des données météorologiques pour les départements et jours voulus  ---------------  #

for (i in seq_along(departement)) {
  
  downloadedCSV <- gzcon(url(paste("https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/QUOT/",
                                   "Q_", departement[i], "_previous-1950-2022_RR-T-Vent.csv.gz", sep="")))         # téléchargement fichier pluie pour un departement
  dat <- read.csv(textConnection(readLines(downloadedCSV)), sep=";")  # lecture du csv
  
  h=1
  for(j in jour){
    tmp <- dat %>% filter(AAAAMMJJ == j) %>% select(TX) %>% drop_na() %>% summarize(across(everything(), mean))   #récupère la moyenne pour un jour
    mat_tmp[i, h] <- round(tmp[[1]], 2)     #remplissage matrice avec la valeur voulue
    h=h+1         #iteration moche
  }
  
  
  
}


#   --------------- Mise en forme matrice : ligne : départements ||  colone : date d'élection ---------------  #

colnames(mat_tmp) <- c("[2002-1]", "[2002-2]", "[2007-1]", "[2007-2]", "[2012-1]", "[2012-2]", "[2017-1]", "[2017-2]", "[2022-1]", "[2022-2]")  # Noms des colonnes
rownames(mat_tmp) <- c("[44]", "[29]", "[61]","[33]", "[13]", "[69]","[75]","[59]", "[37]", "[67]")  # Noms des lignes
print(mat_tmp)


#   --------------- Analyse des données récupérés ---------------  #




for(i in 0:10){
  D44_TX = c(D44_TX, mat_tmp[1,i])
}
for(i in 0:10){
  D29_TX = c(D29_TX, mat_tmp[2,i])
}
for(i in 0:10){
  D61_TX = c(D61_TX, mat_tmp[3,i])
}
for(i in 0:10){
  D33_TX = c(D33_TX, mat_tmp[4,i])
}
for(i in 0:10){
  D13_TX = c(D13_TX, mat_tmp[5,i])
}
for(i in 0:10){
  D69_TX = c(D69_TX, mat_tmp[6,i])
}
for(i in 0:10){
  D75_TX = c(D75_TX, mat_tmp[7,i])
}
for(i in 0:10){
  D59_TX = c(D59_TX, mat_tmp[8,i])
}
for(i in 0:10){
  D37_TX = c(D37_TX, mat_tmp[9,i])
}
for(i in 0:10){
  D67_TX = c(D67_TX, mat_tmp[10,i])
}


#   --------------- Moyenne, équart type et variance pour chaque département ---------------  #

Moy_TX <- c(mean(D44_TX), mean(D29_TX), mean(D61_TX), mean(D33_TX), mean(D13_TX), mean(D69_TX), mean(D75_TX), mean(D59_TX), mean(D37_TX), mean(D67_TX))
SD_TX <- c(sd(D44_TX), sd(D29_TX), sd(D61_TX), sd(D33_TX), sd(D13_TX), sd(D69_TX), sd(D75_TX), sd(D59_TX), sd(D37_TX), sd(D67_TX))
VAR_TX <- c(var(D44_TX), var(D29_TX), var(D61_TX), var(D33_TX), var(D13_TX), var(D69_TX), var(D75_TX), var(D59_TX), var(D37_TX), var(D67_TX))

