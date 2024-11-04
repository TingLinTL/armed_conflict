library(here)
library(naniar)
library(dplyr)
library(mice)
library(plm)
finaldata <- read.csv(here("FinalDataset.csv"), header = TRUE)

#Visualize missing data
finaldata |> arrange(year, ISO) |>
  dplyr::select(-country_name, -ISO, -region, -year, -gdp1000) |>
  vis_miss()

#Regression models of interest
preds <- as.formula(" ~ armconf1 + loggdp1000 + OECD + popdens + urban + 
                  agedep + male_edu + temp + rainfall1000 + earthquake + drought ")

matmormod <- plm(update.formula(preds, MaternalMortalityRate ~ .), index = c("ISO", "year"), effect = "twoways",
                 model = "within", data = finaldata)
un5mormod <- plm(update.formula(preds, Under5MortalityRate ~ .), index = c("ISO", "year"), effect = "twoways",
                 model = "within", data = finaldata)
infmormod <- plm(update.formula(preds, InfantMortalityRate ~ .), index = c("ISO", "year"), effect = "twoways",
                 model = "within", data = finaldata)
neomormod <- plm(update.formula(preds, NeonatalMortalityRate ~ .), index = c("ISO", "year"), effect = "twoways",
                 model = "within", data = finaldata)

#convert ISO to numeric
midata <- finaldata |>
  mutate(ISOnum = as.numeric(as.factor(finaldata$ISO))) |>
  select(-country_name, -ISO)
#get meth and pred
mice0  <- mice(midata, seed = 100, m = 5, maxit = 0, print = F)

meth <- mice0$method
meth[c("urban", "male_edu", "temp", "rainfall1000", "MaternalMortalityRate", "InfantMortalityRate", "NeonatalMortalityRate", "Under5MortalityRate", "loggdp1000", "popdens")] <- "2l.lmer"

pred <- mice0$predictorMatrix
pred[c("urban", "male_edu", "temp", "rainfall1000", "MaternalMortalityRate", "InfantMortalityRate", "NeonatalMortalityRate", "Under5MortalityRate", "loggdp1000", 
       "popdens"), "ISOnum"] <- -2

mice.multi.out  <- mice(midata, seed = 100, m = 10, maxit = 20,
                        method = meth,
                        predictorMatrix = pred)

plot(mice.multi.out)

complete.data.multi <- complete(mice.multi.out, "all")
head(complete.data.multi$"1",n=20)

fit.multi.MMR <- with(mice.multi.out,
                      model.MMR <- lm(MaternalMortalityRate ~ -1 + armconf1 + loggdp1000 + OECD + popdens + urban + 
                                                      agedep + male_edu + temp + rainfall1000 + earthquake + drought+ as.factor(ISOnum) + as.factor(year)))
fit.multi.IMR <- with(mice.multi.out,
                      model.IMR <- lm(InfantMortalityRate ~ -1 + armconf1 + loggdp1000 + OECD + popdens + urban + 
                                        agedep + male_edu + temp + rainfall1000 + earthquake + drought+ as.factor(ISOnum) + as.factor(year)))
fit.multi.NMR <- with(mice.multi.out,
                      model.NMR <- lm(NeonatalMortalityRate ~ -1 + armconf1 + loggdp1000 + OECD + popdens + urban + 
                                        agedep + male_edu + temp + rainfall1000 + earthquake + drought + as.factor(ISOnum) + as.factor(year)))
fit.multi.U5MR <- with(mice.multi.out,
                      model.U5MR <- lm(Under5MortalityRate ~ -1 +armconf1 + loggdp1000 + OECD + popdens + urban + 
                                        agedep + male_edu + temp + rainfall1000 + earthquake + drought + as.factor(ISOnum) + as.factor(year)))

summary(pool(fit.multi.MMR))
summary(pool(fit.multi.IMR))
summary(pool(fit.multi.NMR))
summary(pool(fit.multi.U5MR))

library(tidyverse)
library(table1)
library(here)
library(plm)
library(texreg)

# Week 8 in-class
predsw8 <- as.formula(" ~ armconf1 + loggdp1000 + OECD + popdens + urban + 
                  agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                  ISO + as.factor(year)")

matmormodw8 <- plm(update.formula(predsw8, MaternalMortalityRate ~ .), data = data)
un5mormod <- plm(update.formula(predsw8, Under5MortalityRate ~ .), data = data)
infmormod <- plm(update.formula(preds, InfantMortalityRate ~ .), data = data)
neomormod <- plm(update.formula(preds, NeonatalMortalityRate ~ .), data = data)

htmlreg(list(matmormod, un5mormod, infmormod, neomormod),
        custom.model.names = c("Maternal Mortality", 
                               "Under 5 Mortality", 
                               "Infant Mortality", 
                               "Neonatal Mortality"),
        caption = "Table 1: Regression Results for Mortality Rates")