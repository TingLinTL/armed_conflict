library(here)
library(dplyr)
data <- read.csv(here("merged_all_con.csv"), header = TRUE)
data$armconf1 <- ifelse(is.na(data$armconf1), 0, data$armconf1)
data$totaldeath <- ifelse(is.na(data$totaldeath), 0, data$totaldeath)
data$popdens = data$popdens /100
data$loggdp1000 <- log(data$gdp1000)

# save as CSV
write.csv(data, "FinalDataset.csv", row.names = FALSE)



