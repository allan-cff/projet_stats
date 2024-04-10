library(tidyverse)

departement <- c(44, 29, 61, 33, 13, 69, 75, 59, 37, 67)
jour <- c("20020421", "20020505", "20070422", "20070506", "20120422", "20120506", "20170421", "20170507", "20220410", "20220424")
matrice <- matrix(nrow = length(departement), ncol = length(departement))

for (i in seq_along(departement)) {
  
  downloadedCSV <- gzcon(url(paste("https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/QUOT/",
                                   "Q_", departement[i], "_previous-1950-2022_RR-T-Vent.csv.gz", sep="")))
  dat <- read.csv(textConnection(readLines(downloadedCSV)), sep=";")
  
  h=1
  for(j in jour){
    tmp <- dat %>% filter(AAAAMMJJ == j) %>% select(RR) %>% drop_na() %>% summarize(across(everything(), mean))
    matrice[i, h] <- tmp[[1]]
    h=h+1
  }
  
  
  
}

colnames(matrice) <- c("[2002-1]", "[2002-2]", "[2007-1]", "[2007-2]", "[2012-1]", "[2012-2]", "[2017-1]", "[2017-2]", "[2022-1]", "[2022-2]")  # Noms des colonnes
rownames(matrice) <- c("[44]", "[29]", "[61]","[33]", "[13]", "[69]","[75]","[59]", "[37]", "[67]")  # Noms des lignes
print(matrice)