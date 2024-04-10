library(tidyverse)

dataURL2002 <- "https://www.data.gouv.fr/fr/datasets/r/17fbaf03-33f2-48d6-95e8-aaa13809e432"
dataURL2007 <- "https://www.data.gouv.fr/fr/datasets/r/83e6c566-e313-4dbb-a504-8467c0952697"
dataURL2012 <- "https://www.data.gouv.fr/fr/datasets/r/206b1668-2b31-46da-b957-9857f94fe85c"
dataURL2017t1 <- "https://www.data.gouv.fr/fr/datasets/r/8fdb0926-ea9d-4fb4-a136-7767cd97e30b"
dataURL2017t2 <- "https://www.data.gouv.fr/fr/datasets/r/0e50ba6a-8175-4455-9e4a-09a7783dc547"
dataURL2022t1 <- "https://www.data.gouv.fr/fr/datasets/r/79b5cac4-4957-486b-bbda-322d80868224"
dataURL2022t2 <- "https://www.data.gouv.fr/fr/datasets/r/4dfd05a9-094e-4043-8a19-43b6b6bbe086"

# On supprime les lignes de 0 à 16 car le ministère de l'intérieur ne sait pas écrire de CSV
dat2002 <- read.csv(textConnection(c("N_tour;code_dep;code_comm;nom_comm;N_bureau;Inscrits;Votants;Exprimes;N_cand;nom_cand;pren_cand;code_cand;voix_cand", readLines(url(dataURL2002))[-(0:17)])), sep=";") %>% filter(code_dep == "44") %>% filter(N_cand == "1") %>% select(Inscrits, Votants)  %>% drop_na() %>% summarize(across(everything(), sum))
dat2007 <- read.csv(textConnection(c("N_tour;code_dep;code_comm;nom_comm;N_bureau;Inscrits;Votants;Exprimes;N_cand;nom_cand;pren_cand;code_cand;voix_cand", readLines(url(dataURL2007))[-(0:17)])), sep=";") %>% filter(code_dep == "44") %>% filter(N_cand == "1") %>% select(Inscrits, Votants)  %>% drop_na() %>% summarize(across(everything(), sum))
dat2012 <- read.csv(textConnection(c("N_tour;code_dep;code_comm;nom_comm;JSP;IDK;N_bureau;Inscrits;Votants;Exprimes;N_cand;nom_cand;pren_cand;code_cand;voix_cand", readLines(url(dataURL2012)))), sep=";") %>% filter(code_dep == "44") %>% filter(N_cand == "2") %>% select(Inscrits, Votants)  %>% drop_na() %>% summarize(across(everything(), sum))