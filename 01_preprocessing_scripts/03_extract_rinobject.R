library(dplyr)
library(lubridate)
library(tidyverse)
library(haven)
#Loads the file - make sure to filter by DIAPER ID 
cat(format(now()), "Reading DIAPER ID file.\n")
diaper_ids <- read_rds("processed_data/diaper_complete.rds")

NMAX <- Inf

#File paths
source("01_preprocessing_scripts/00_file_paths.R")

#Function to extract a year interval 
extract_years <- function(year_start, year_end) {
  seq(year(year_start), year(year_end))
}

addresses <- read_sav(file = PATHS$ADDRESS, n_max = NMAX) |> 
  filter(RINPERSOON %in% diaper_ids$RINPERSOON) |>
  mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "values"))
 

#Creates a date variable out of the character variables
addresses <- addresses |> 
  mutate(year_start = ymd(GBADATUMAANVANGADRESHOUDING)) |> 
  mutate(year_end   = ymd(GBADATUMEINDEADRESHOUDING))

#Apply the function
addresses <- addresses |> 
  rowwise() |> 
  mutate(year = list(extract_years(year_start, year_end)))

# Unnesting the years
addresses <- addresses |> 
  unnest(cols = c(year))


#Keeping only the record with a later end date (for the years in which there are multiple rinobject IDs)
addresses_filtered <- addresses |> 
  group_by(RINPERSOON, RINPERSOONS) |> 
  arrange(year, desc(year_end)) |> 
  filter(!duplicated(year)) |> 
  select(-year_start, -year_end, -GBADATUMAANVANGADRESHOUDING, -GBADATUMEINDEADRESHOUDING) 

rm(addresses)

#Join with IDs
diaper_ids_ext <- left_join(diaper_ids, addresses_filtered, by = join_by(RINPERSOON, RINPERSOONS, year))

#Check for missing residential history
sum(is.na(diaper_ids_ext$RINOBJECTNUMMER))
diaper_ids_ext <- diaper_ids_ext |> 
  filter(!is.na(RINOBJECTNUMMER))

write_rds(diaper_ids_ext, 'processed_data/diaper_complete.rds')
