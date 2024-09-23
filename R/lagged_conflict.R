library(here)
library(tidyverse)
here()
rawconflict <- read.csv(here("original", "conflictdata.csv"), header = TRUE)

newconflict1 <- rawconflict %>% group_by(year, ISO) %>% summarize(conflict= sum(best))
newconflict2 <- newconflict1 %>% mutate(indicator = ifelse(conflict >= 25,1,0))
newconflict2$year <- newconflict2$year + 1
