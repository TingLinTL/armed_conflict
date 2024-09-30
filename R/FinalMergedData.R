library(here)
library(tidyverse)
source("R/mergedall_ISO_script.R")
source("R/lagged_conflict.R")
source("R/disaster_script.R")
covariates <- read.csv(here("original", "covariates.csv"), header = TRUE)
sub_allfinal <- list(final_allmortality, newconflict2, newdisdata)
sub_merged_all_con <- reduce(sub_allfinal, full_join, by = c("year","ISO"))
allfinal <- left_join(covariates, sub_merged_all_con, by = c("year","ISO"))


names(allfinal)
allfinal <- allfinal %>% select(country_name, ISO, region, year, gdp1000, OECD, OECD2023, popdens, urban, agedep, male_edu, 
                                            temp, rainfall1000, totaldeath, armconf1, MaternalMortalityRate, InfantMortalityRate, 
                                            NeonatalMortalityRate, Under5MortalityRate, drought, earthquake)
allfinal$earthquake[is.na(allfinal$earthquake)]=0
allfinal$drought[is.na(allfinal$drought)]=0


# save as CSV
# write.csv(allfinal, "merged_all_con.csv", row.names = FALSE)


                         