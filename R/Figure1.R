# Figure: Maternal Mortality Rate Trend from 2000 to 2017

# (We don't consider 2018 and 2019 because we have missing maternal mortality rate for both years)

library(tidyverse)
library(table1)
library(here)
library(dplyr)
data <- read.csv(here("merged_all_con.csv"), header = TRUE)
figure<- data |> select(country_name, ISO, year, MaternalMortalityRate) |>
  filter(year < 2018) |>
  arrange(ISO, year) |>
  group_by(ISO) |>
  mutate(diffmatmor = MaternalMortalityRate- MaternalMortalityRate[1L]) |>
  arrange(ISO, desc(year)) |>
  mutate(incmatmor = ifelse(diffmatmor[1L] > 0 , 1, 0)) |>
  arrange(ISO, year) |>
  ungroup() |>
  filter(incmatmor == 1)

length(unique(figure$ISO))

fig1 <- figure |>
  ggplot(aes(x = year, y = MaternalMortalityRate, group = ISO)) +
  geom_line(aes(color = country_name), alpha = 1, linewidth = 1) +
  xlim(c(2000,2017)) +
  # use log 10 sclae for y axis
  scale_y_continuous(trans='log10') + 
  labs(y = "Maternal mortality (log 210 scale)", x = "Year", color = "Country", title = "Trend in maternal mortality for countries that had an increase from 2000 to 2017") + 
  # use black and white theme and increase the size of labels
  theme_bw(base_size = 12)

# save the gplot as a png file
ggsave(fig1, file = here("figure", "fig1.png"), width = 8, height = 5)