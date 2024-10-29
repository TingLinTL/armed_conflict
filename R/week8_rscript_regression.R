library(tidyverse)
library(table1)
library(here)
library(plm)
library(texreg)
data <- read.csv(here("merged_all_con.csv"), header = TRUE)
data$armconf1 <- ifelse(is.na(data$armconf1), 0, data$armconf1)
names(data)

lmmod1 <- lm(MaternalMortalityRate ~ -1 + armconf1 + gdp1000 + OECD + popdens + urban +
              agedep + male_edu + temp + rainfall1000 + earthquake + drought +
              ISO, 
            data = data)
summary(lmmod1)

plmmod1 <- plm(MaternalMortalityRate ~ armconf1 + gdp1000 + OECD + popdens + urban +
                agedep + male_edu + temp + rainfall1000 + earthquake + drought,
              index = c("ISO"), model = "within", data = data)
summary(plmmod1)
screenreg(list(lmmod1, plmmod1))

######

lmmod2 <- lm(MaternalMortalityRate ~ -1 + armconf1 + gdp1000 + OECD + popdens + urban +
              agedep + male_edu + temp + rainfall1000 + earthquake + drought +
              ISO + as.factor(year), 
            data = data)
summary(lmmod2)

plmmod2 <- plm(MaternalMortalityRate ~ armconf1 + gdp1000 + OECD + popdens + urban + 
                agedep + male_edu + temp + rainfall1000 + earthquake + drought,
              index = c("ISO", "year"),
              effect = "twoways",
              model = "within",
              data = data)
summary(plmmod2)

screenreg(list(lmmod2, plmmod2))

## Log transform GDP and use the transformed GDP as one of the predictors
data$loggdp1000 <- log(data$gdp1000)
## script that fits the four mortality models using plm()
preds <- as.formula(" ~ armconf1 + loggdp1000 + OECD + popdens + urban + 
                  agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                  ISO + as.factor(year)")

matmormod <- plm(update.formula(preds, MaternalMortalityRate ~ .), data = data)
un5mormod <- plm(update.formula(preds, Under5MortalityRate ~ .), data = data)
infmormod <- plm(update.formula(preds, InfantMortalityRate ~ .), data = data)
neomormod <- plm(update.formula(preds, NeonatalMortalityRate ~ .), data = data)

summary(matmormod)
summary(un5mormod)
summary(infmormod)
summary(neomormod)

screenreg(list(matmormod,un5mormod,infmormod,neomormod))
