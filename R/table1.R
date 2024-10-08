# Table1
# how many countries in total (our observations)
# 2 levels for conflit (binary, yes/no)
# count how many years the country suffering armec conflict
# min-max, or quantile
# columns for outcomes, mortality rate?
# main rows, factors.
# other rows; confounding variables

#assignment 2 from 2000-2017, since maternal mortality rates are 2018 and 2019 missing
library(tidyverse)
library(table1)
library(here)
data <- read.csv(here("merged_all_con.csv"), header = TRUE)
data$armconf1 <- ifelse(is.na(data$armconf1), 0, data$armconf1)
base <- data %>% filter(year == 2000)

base$armconf1new <- factor(base$armconf1, levels = c(0,1), labels = c("No armed conflict in 2000", "Armed conflict in 2000"))
base$droughtnew <- factor(base$drought, levels = c(0,1), labels = c("No", "Yes"))
base$earthquakenew <- factor(base$earthquake, levels = c(0,1), labels = c("No", "Yes"))
base$OECDnew <- factor(base$OECD, levels = c(0,1), labels = c("No", "Yes"))

label(base$gdp1000)       <- "GDP per capita (USD)"
label(base$OECDnew)       <- "OECD member"
label(base$popdens)       <- "Population density"
label(base$urban)         <- "Urban residence"
label(base$agedep)        <- "Age dependency ratio"
label(base$male_edu)      <- "Male education"
label(base$temp)          <- "Mean annual temperature"
label(base$rainfall1000)  <- "Mean annual rain fall"
label(base$earthquakenew)   <- "Earthquake"
label(base$droughtnew)      <- "Drought"
label(base$armconf1new)     <- "Armed conflict"



table1(~ gdp1000 + OECDnew + popdens + urban + agedep + male_edu + temp + rainfall1000 + earthquakenew + droughtnew| armconf1new, data = base,
       render.continuous = c(.="Median [Min, Max]"),
       overall=c(left="Total"))

