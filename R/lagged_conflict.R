library(here)
library(tidyverse)
here()
rawconflict <- read.csv(here("original", "conflictdata.csv"), header = TRUE)

newconflict1 <- rawconflict %>% group_by(year, ISO) %>% summarize(totaldeath= sum(best))
newconflict1$totaldeath[is.na(newconflict1$totaldeath)]=0
newconflict2 <- newconflict1 %>% mutate(armconf1 = ifelse(totaldeath >= 25,1,0))
newconflict2$year <- newconflict2$year + 1

