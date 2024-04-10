library(tidyverse)

departement <- c(44, 33, 22)
jour <- c("20170423", "20180423")
matrice <- matrix(nrow = length(departement), ncol = length(departement)-1)

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

colnames(matrice) <- c("[2017]", "[2018]")  # Noms des colonnes
rownames(matrice) <- c("[44]", "[33]", "[22]")  # Noms des lignes
print(matrice)