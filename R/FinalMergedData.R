source("R/mergedall_ISO_script.R")
source("R/lagged_conflict.R")
source("R/disaster_script.R")
library(here)
library(tidyverse)
covariates <- read.csv(here("original", "covariates.csv"), header = TRUE)
allfinal <- list(final_allmortality, newconflict2, covariates,newdisdata)
merged_all_con <- reduce(allfinal, full_join, by = c("year","ISO"))



                         