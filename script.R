downloadedCSV <- gzcon(url(paste("https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/QUOT/",
                       "Q_44_previous-1950-2022_RR-T-Vent.csv.gz", sep="")))
txt <- readLines(downloadedCSV)
dat <- read.csv(textConnection(txt))