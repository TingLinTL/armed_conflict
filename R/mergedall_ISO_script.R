# clean data
cleandata <- function(file_path){
  x <- read.csv(file_path, header = TRUE)
  x %>% 
    select(Country.Name, X2000:X2019) %>% 
    pivot_longer(cols = X2000:X2019, names_to = "Year", names_prefix = "X", values_to = "MatMor") %>% 
    mutate(Year = as.numeric(Year)) 
}

# four "new" data sets
newdata_maternalmortality <- cleandata(here("original", "maternalmortality.csv"))
newdata_infantmortality <- cleandata(here("original", "infantmortality.csv"))
newdata_neonatalmortality <- cleandata(here("original", "neonatalmortality.csv"))
newdata_under5mortality <- cleandata(here("original", "under5mortality.csv"))

# merge four data sets
allmortality  <- list(newdata_maternalmortality, newdata_infantmortality, newdata_neonatalmortality, newdata_under5mortality)
merged_allmortality <- reduce(allmortality, full_join, by = c("Country.Name", "Year"))

rename_merged_allmortality <- merged_allmortality %>% 
  rename(
    CountryName = Country.Name,
    Year = Year,
    MaternalMortalityRate = MatMor.x,
    InfantMortalityRate = MatMor.y,
    NeonatalMortalityRate = MatMor.x.x,
    Under5MortalityRate = MatMor.y.y
  )

#add ISO-3c countrycode
library(countrycode)
rename_merged_allmortality$ISO <- countrycode(rename_merged_allmortality$CountryName,
                                              origin = "country.name",
                                              destination = "iso3c")


# Remove the CountryName variable
final_allmortality <- rename_merged_allmortality %>%
  select(-CountryName)
final_allmortality <- final_allmortality %>% rename(year=Year)

head(final_allmortality)
