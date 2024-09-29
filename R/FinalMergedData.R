library(here)
library(tidyverse)
source("R/mergedall_ISO_script.R")
source("R/lagged_conflict.R")
source("R/disaster_script.R")
covariates <- read.csv(here("original", "covariates.csv"), header = TRUE)
allfinal <- list(final_allmortality, newconflict2, covariates,newdisdata)
merged_all_con <- reduce(allfinal, full_join, by = c("year","ISO"))
names(merged_all_con)
merged_all_con <- merged_all_con %>% select(country_name, ISO, region, year, gdp1000, OECD, OECD2023, popdens, urban, agedep, male_edu, 
                                            temp, rainfall1000, totaldeath, armconf1, MaternalMortalityRate, InfantMortalityRate, 
                                            NeonatalMortalityRate, Under5MortalityRate, drought, earthquake)


#save as CSV
write.csv(merged_all_con, "merged_all_con.csv", row.names = FALSE)


                         