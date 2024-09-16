library(here)
library(tidyverse)
here()
rawdisaster<- read.csv(here("original", "disaster.csv"), header = TRUE)
rawdisaster1 <- rawdisaster %>% filter(Year >= 2000 & Year <= 2019, Disaster.Type %in% c("Earthquake", "Drought")) 
rawdisaster2 <- rawdisaster1 %>% select(Year, ISO, Disaster.Type)
rawdisaster3 <- rawdisaster2 %>% mutate(drought = ifelse(Disaster.Type == "Drought",1,0),earthquake = ifelse(Disaster.Type == "Earthquake", 1, 0))

newdisdata <- rawdisaster3 %>% group_by(Year,ISO) %>% summarize(drought = max(drought), earthquake= max(earthquake))


head(rawdisaster2)
