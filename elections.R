library(tidyverse)

dataURL2002 <- "https://www.data.gouv.fr/fr/datasets/r/17fbaf03-33f2-48d6-95e8-aaa13809e432"
dataURL2007 <- "data/PR07_Bvot_T1T2.txt"
dataURL2012 <- "data/PR12_Bvot_T1T2.txt"
dataURL2017t1 <- "data/Presidentielle_2017_Resultats_Tour_1_c.csv"
dataURL2017t2 <- "data/Presidentielle_2017_Resultats_Tour_2_c.csv"
dataURL2022t1 <- "data/resultats-par-niveau-dpt-t1-france-entiere.csv"
dataURL2022t2 <- "data/resultats-par-niveau-dpt-t2-france-entiere.csv"

# On supprime les lignes de 0 à 16 et on remplace le header car le ministère de l'intérieur ne sait pas écrire de CSV
data2002 <- read.csv(textConnection(c("N_tour;code_dep;code_comm;nom_comm;N_bureau;Inscrits;Votants;Exprimes;N_cand;nom_cand;pren_cand;code_cand;voix_cand", readLines(url(dataURL2002))[-(0:17)])), sep=";") %>% filter(N_cand == 5)
data2007 <- read.csv(textConnection(c("N_tour;code_dep;code_comm;nom_comm;N_bureau;Inscrits;Votants;Exprimes;N_cand;nom_cand;pren_cand;code_cand;voix_cand", readLines(dataURL2007)[-(0:17)])), sep=";") %>% filter(N_cand == 12)
data2012 <- read.csv(textConnection(c("N_tour;code_dep;code_comm;nom_comm;JSP;IDK;N_bureau;Inscrits;Votants;Exprimes;N_cand;nom_cand;pren_cand;code_cand;voix_cand", readLines(dataURL2012))), sep=";") %>% filter(N_cand == 11)


election_data <- list()

election_data[[1]] <- data2002 %>% filter (N_tour == 1) %>% select(code_dep, Inscrits, Votants) %>% reframe(code_dep=code_dep, Inscrits=Inscrits, Abstentions=Inscrits-Votants) %>% group_by(code_dep) %>% summarize(Inscrits=sum(Inscrits), Abstentions=sum(Abstentions))
election_data[[2]] <- data2002 %>% filter (N_tour == 2) %>% select(code_dep, Inscrits, Votants) %>% reframe(code_dep=code_dep, Inscrits=Inscrits, Abstentions=Inscrits-Votants) %>% group_by(code_dep) %>% summarize(Inscrits=sum(Inscrits), Abstentions=sum(Abstentions))
election_data[[3]] <- data2007 %>% filter (N_tour == 1) %>% select(code_dep, Inscrits, Votants) %>% reframe(code_dep=code_dep, Inscrits=Inscrits, Abstentions=Inscrits-Votants) %>% group_by(code_dep) %>% summarize(Inscrits=sum(Inscrits), Abstentions=sum(Abstentions))
election_data[[4]] <- data2007 %>% filter (N_tour == 2) %>% select(code_dep, Inscrits, Votants) %>% reframe(code_dep=code_dep, Inscrits=Inscrits, Abstentions=Inscrits-Votants) %>% group_by(code_dep) %>% summarize(Inscrits=sum(Inscrits), Abstentions=sum(Abstentions))
election_data[[5]] <- data2012 %>% filter (N_tour == 1) %>% select(code_dep, Inscrits, Votants) %>% reframe(code_dep=code_dep, Inscrits=Inscrits, Abstentions=Inscrits-Votants) %>% group_by(code_dep) %>% summarize(Inscrits=sum(Inscrits), Abstentions=sum(Abstentions))
election_data[[6]] <- data2012 %>% filter (N_tour == 2) %>% select(code_dep, Inscrits, Votants) %>% reframe(code_dep=code_dep, Inscrits=Inscrits, Abstentions=Inscrits-Votants) %>% group_by(code_dep) %>% summarize(Inscrits=sum(Inscrits), Abstentions=sum(Abstentions))
election_data[[7]] <- read.csv(dataURL2017t1, sep=";") %>% reframe(code_dep=Code.du.département, Inscrits=Inscrits, Abstentions=Abstentions)
election_data[[8]] <- read.csv(dataURL2017t2, sep=";") %>% reframe(code_dep=Code.du.département, Inscrits=Inscrits, Abstentions=Abstentions)
election_data[[9]] <- read.csv(dataURL2022t1, sep=";") %>% reframe(code_dep=Code.du.département, Inscrits=Inscrits, Abstentions=Abstentions)
election_data[[10]] <- read.csv(dataURL2022t2, sep=";") %>% reframe(code_dep=Code.du.département, Inscrits=Inscrits, Abstentions=Abstentions)

#   --------------- Initialisation des valeures, départements analysés et date des élections ---------------  #

departement <- c(44, 29, 61, 33, 13, 69, 75, 59, 37, 67) #departments
jour <- c("20020421", "20020505", "20070422", "20070506", "20120422", "20120506", "20170421", "20170507", "20220410", "20220424") # date election

#   --------------- variables globales ---------------  #

matrice <- matrix(nrow = length(departement), ncol = length(departement))


#   --------------- Récupération des données météorologiques pour les départements et jours voulus  ---------------  #

for (i in seq_along(departement)) {
  h=1
  for(j in jour){
    tmp <-    #récupère l'abstention pour un département
    matrice[i, h] <- round(tmp[[1]], 2)     #remplissage matrice avec la valeur voulue
    h=h+1         #iteration moche
  }
}


#   --------------- Mise en forme matrice : ligne : départements / colone : date d'élection ---------------  #

colnames(matrice) <- c("[2002-1]", "[2002-2]", "[2007-1]", "[2007-2]", "[2012-1]", "[2012-2]", "[2017-1]", "[2017-2]", "[2022-1]", "[2022-2]")  # Noms des colonnes
rownames(matrice) <- c("[44]", "[29]", "[61]","[33]", "[13]", "[69]","[75]","[59]", "[37]", "[67]")  # Noms des lignes
print(matrice)


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


#   --------------- Moyenne, équart type et variance pour chaque département ---------------  #

Moy_RR <- c(mean(D44_RR), mean(D29_RR), mean(D61_RR), mean(D33_RR), mean(D13_RR), mean(D69_RR), mean(D75_RR), mean(D59_RR), mean(D37_RR), mean(D67_RR))
SD_RR <- c(sd(D44_RR), sd(D29_RR), sd(D61_RR), sd(D33_RR), sd(D13_RR), sd(D69_RR), sd(D75_RR), sd(D59_RR), sd(D37_RR), sd(D67_RR))
VAR_RR <- c(var(D44_RR), var(D29_RR), var(D61_RR), var(D33_RR), var(D13_RR), var(D69_RR), var(D75_RR), var(D59_RR), var(D37_RR), var(D67_RR))
