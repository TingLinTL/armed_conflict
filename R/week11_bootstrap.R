library(here)
library(dplyr)
library(boot)
here()
set.seed(2024)

finaldata <- read.csv(here("FinalDataset.csv"), header = TRUE)
matmor.arm1 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(MaternalMortalityRate) & armconf1 == 1) |>
  dplyr::select(ISO, MaternalMortalityRate)

matmor.arm0 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(MaternalMortalityRate) & armconf1 == 0) |>
  dplyr::select(ISO, MaternalMortalityRate)

set.seed(2024)
B <- 1000
med.diff <- rep(NA, B)
for(b in 1:B){
  resamp.arm1 <- matmor.arm1[sample(nrow(matmor.arm1), size = nrow(matmor.arm1), replace = TRUE),]
  resamp.arm0 <- matmor.arm0[sample(nrow(matmor.arm0), size = nrow(matmor.arm0), replace = TRUE),]
  med.diff[b] <- median(resamp.arm1$MaternalMortalityRate) - median(resamp.arm0$MaternalMortalityRate)
}
head(resamp.arm1, 12)

hist(med.diff, main = "Distribution of bootstrap statistic")


####
getmeddiffmatmor <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$MaternalMortalityRate, sample_data$armconf1, FUN = function(x) median(x,na.rm=TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

datamatmor <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(MaternalMortalityRate)) |>
  dplyr::select(ISO, MaternalMortalityRate, armconf1)


bootout <- boot(datamatmor, statistic = getmeddiffmatmor, strata = datamatmor$armconf1, R = 1000)
bootout
boot.ci(boot.out = bootout, conf = 0.95, type = c("basic", "perc", "bca"))

####

getmeddiffinfmor <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$InfantMortalityRate, sample_data$armconf1, FUN = function(x) median(x,na.rm=TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

datainfmor <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(InfantMortalityRate)) |>
  dplyr::select(ISO, InfantMortalityRate, armconf1)


bootout <- boot(datainfmor, statistic = getmeddiffinfmor, strata = datainfmor$armconf1, R = 1000)
bootout
boot.ci(boot.out = bootout, conf = 0.95, type = c("basic", "perc", "bca"))
####

getmeddiffunder5mor <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$Under5MortalityRate, sample_data$armconf1, FUN = function(x) median(x,na.rm=TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

dataunder5mor <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(Under5MortalityRate)) |>
  dplyr::select(ISO, Under5MortalityRate, armconf1)


bootout <- boot(dataunder5mor , statistic = getmeddiffunder5mor, strata = dataunder5mor$armconf1, R = 1000)
bootout
boot.ci(boot.out = bootout, conf = 0.95, type = c("basic", "perc", "bca"))

####
getmeddiffneomor <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$NeonatalMortalityRate, sample_data$armconf1, FUN = function(x) median(x,na.rm=TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

dataneomor <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(NeonatalMortalityRate)) |>
  dplyr::select(ISO, NeonatalMortalityRate, armconf1)


bootout <- boot(dataneomor , statistic = getmeddiffneomor, strata = dataneomor$armconf1, R = 1000)
bootout
boot.ci(boot.out = bootout, conf = 0.95, type = c("basic", "perc", "bca"))

