library(tidyverse)

downloadedCSV <- gzcon(url("https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/QUOT/Q_44_previous-1950-2022_RR-T-Vent.csv.gz"))
dat <- read.csv(textConnection(readLines(downloadedCSV)), sep=";")
dat %>% select(AAAAMMJJ, RR)