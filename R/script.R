library(here)
library(tidyverse)
here()
rawdata<- read.csv(here("original", "maternalmortality.csv"), header = TRUE)

newdata <- rawdata %>% select(Country.Name, X2000:X2019)
newdata1 <- newdata %>% pivot_longer(cols=X2000:X2019, names_to = "Year", names_prefix = "X", values_to = "MatMor")
newdata2 <- newdata1 %>% mutate(Year = as.numeric(Year)) 

