files_geo <- here::here("input","data","place names and geolocation",'geonames') |> list.files(full.names = T) |> file.info()
geonames <- read.delim(base::rownames(files_geo)[which.max(files_geo$mtime)], sep = "\t", header = FALSE)

head(geonames)
head(geonames|>filter(V9 == "US")) #This has country

geonames |> filter(V9 == "US") |> count(V11) # 50 states + DC; No territories
geonames |> filter(grepl("Puerto Rico|Guam|Virgin Islands|American Samoa|Northern Mariana Islands", V18))
#V18 finds 'Guam' and v9 == 'GU' is also guam
geonames |> filter(grepl("PR", V9)) #PR = V9 but also v18 == 'Puerto_Rico', so probably just a _ in the spaces

geonames |> filter(grepl("Puerto_Rico|Guam|Virgin_Islands|Virgin|American_Samoa|Northern_Mariana_Islands", V18)) |> count(V18)

geonames |> filter(grepl("PR", V9)) #PR = V9 but also v18 == 'Puerto_Rico', so probably just a _ in the spaces
#looking at the lat longs, I can't see VI

#Just search for VI using the lat longs

geonames |> filter(V5 > 17 & V5 < 19 & V6 > -65 & V6 < -64.4) #This gets the Virgin Islands

geonames_us <- geonames |> filter(grepl("US|PR|GU|VI|AS|MP", V9)) # OK so V9 is country and territory codes
