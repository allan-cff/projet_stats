library(tidyverse)

downloadedCSV <- gzcon(url("https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/QUOT/Q_44_previous-1950-2022_RR-T-Vent.csv.gz"))
dat <- read.csv(textConnection(readLines(downloadedCSV)), sep=";")
result = dat %>% filter(AAAAMMJJ == "20170423") %>% select(RR)  %>% drop_na() %>% summarize(across(everything(), mean))
value = result[[1]]