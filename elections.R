library(tidyverse)

dataURL2002 <- "data/PR02_Bvot_T1T2.txt"
dataURL2007 <- "data/PR07_Bvot_T1T2.txt"
dataURL2012 <- "data/PR12_Bvot_T1T2.txt"
dataURL2017t1 <- "data/Presidentielle_2017_Resultats_Tour_1_c.csv"
dataURL2017t2 <- "data/Presidentielle_2017_Resultats_Tour_2_c.csv"
dataURL2022t1 <- "data/resultats-par-niveau-dpt-t1-france-entiere.csv"
dataURL2022t2 <- "data/resultats-par-niveau-dpt-t2-france-entiere.csv"

# On supprime les lignes de 0 à 16 et on remplace le header car le ministère de l'intérieur ne sait pas écrire de CSV
data2002 <- read.csv(textConnection(c("N_tour;code_dep;code_comm;nom_comm;N_bureau;Inscrits;Votants;Exprimes;N_cand;nom_cand;pren_cand;code_cand;voix_cand", readLines(dataURL2002)[-(0:17)])), sep=";") %>% filter(N_cand == 5)
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

#   --------------- Initialisation des valeurs, départements analysés et date des scrutins ---------------  #

departement <- c(44, 29, 61, 33, 13, 69, 75, 59, 37, 67) #departments
jour <- c("20020421", "20020505", "20070422", "20070506", "20120422", "20120506", "20170421", "20170507", "20220410", "20220424") # date election

#   --------------- variables globales ---------------  #

matrice <- matrix(nrow = length(departement), ncol = length(departement))


#   --------------- Récupération des données d'abstention pour les départements et jours voulus  ---------------  #


for (dep in seq_along(departement)) {
  for(j in jour){
    tmp <- election_data[[j]] %>% filter(code_dep == department[dep]) %>% summarize(taux_abstention = Abstentions/Inscrits)  #récupère l'abstention pour un département
    matrice[dep, j] <- round(tmp[[1]], 2)     #remplissage matrice avec la valeur voulue
  }
}


#   --------------- Mise en forme matrice : ligne : départements / colonne : date du scrutin ---------------  #

colnames(matrice) <- c("[2002-1]", "[2002-2]", "[2007-1]", "[2007-2]", "[2012-1]", "[2012-2]", "[2017-1]", "[2017-2]", "[2022-1]", "[2022-2]")  # Noms des colonnes
rownames(matrice) <- c("[44]", "[29]", "[61]","[33]", "[13]", "[69]","[75]","[59]", "[37]", "[67]")  # Noms des lignes
print(matrice)


#   --------------- Analyse des données récupérées ---------------  #
D44_Abs = c()
D29_Abs = c()
D61_Abs = c()
D33_Abs = c()
D13_Abs = c()
D69_Abs = c()
D75_Abs = c()
D59_Abs = c()
D37_Abs = c()
D67_Abs = c()


for(i in 0:10){
  D44_Abs = c(D44_Abs, matrice[1,i])
}
for(i in 0:10){
  D29_Abs = c(D29_Abs, matrice[2,i])
}
for(i in 0:10){
  D61_Abs = c(D61_Abs, matrice[3,i])
}
for(i in 0:10){
  D33_Abs = c(D33_Abs, matrice[4,i])
}
for(i in 0:10){
  D13_Abs = c(D13_Abs, matrice[5,i])
}
for(i in 0:10){
  D69_Abs = c(D69_Abs, matrice[6,i])
}
for(i in 0:10){
  D75_Abs = c(D75_Abs, matrice[7,i])
}
for(i in 0:10){
  D59_Abs = c(D59_Abs, matrice[8,i])
}
for(i in 0:10){
  D37_Abs = c(D37_Abs, matrice[9,i])
}
for(i in 0:10){
  D67_Abs = c(D67_Abs, matrice[10,i])
}


#   --------------- Moyenne, équart type et variance pour chaque département ---------------  #

Moy_Abs <- c(mean(D44_Abs), mean(D29_Abs), mean(D61_Abs), mean(D33_Abs), mean(D13_Abs), mean(D69_Abs), mean(D75_Abs), mean(D59_Abs), mean(D37_Abs), mean(D67_Abs))
SD_Abs <- c(sd(D44_Abs), sd(D29_Abs), sd(D61_Abs), sd(D33_Abs), sd(D13_Abs), sd(D69_Abs), sd(D75_Abs), sd(D59_Abs), sd(D37_Abs), sd(D67_Abs))
VAR_Abs <- c(var(D44_Abs), var(D29_Abs), var(D61_Abs), var(D33_Abs), var(D13_Abs), var(D69_Abs), var(D75_Abs), var(D59_Abs), var(D37_Abs), var(D67_Abs))
