#### if needed to add 3 digit country codes to a list of ASF affected countries, run this #####
filename = "ASF affected countries 2009 to 2020 McKee et al 2023.csv"

setwd(here::here('input','data','asf affected countries by year'))
df <- read.csv(filename)

df$Country_name = countrycode::countrycode(df$Country, destination = "country.name", origin = "iso3c")

write.csv(df, filename)
