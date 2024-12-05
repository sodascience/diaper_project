library(dplyr)
library(lubridate)
library(tidyverse)
library(haven)
#Loads the file - make sure to filter by DIAPER ID 

diaper_ids <- read_rds("01_processed_data/diaper_cbs.rds")

NMAX <- Inf

#File paths
source("preprocessing_scripts/00_file_paths.R")

#Function to extract a year interval 
extract_years <- function(year_start, year_end) {
  seq(year(year_start), year(year_end))
}

#List of relevant variables
#cols_secm <- c("RINPERSOON", "RINPERSOONS", "AANVSECM", "EINDSECM", "SECM")
# Household info ----------------------------------------------------------
#gbah <- read_sav(file = PATHS$GBAH, n_max = NMAX) |> 
#  filter(RINPERSOON %in% diaper_ids$RINPERSOON) |>
#  mutate(RINPERSOONS    = as_factor(RINPERSOONS, levels = "values"))

#gbah <- gbah |> 
#  mutate(year_start = ymd(DATUMAANVANGHH)) |> 
#  mutate(year_end = ymd(DATUMEINDEHH))


#gbah <- gbah |> 
#  rowwise() |> 
#  mutate(year = list(extract_years(year_start, year_end)))

# Unnesting the years
#gbah <- gbah |> 
#  unnest(cols = c(year))

#Keeping the record with a later date
#gbah_filtered <- gbah |> 
#  group_by(RINPERSOON, RINPERSOONS) |> 
#  arrange(year, desc(year_end)) |> 
#  filter(duplicated(year) == FALSE) |> 
#  select(-year_start, -year_end, -DATUMAANVANGHH, -DATUMEINDEHH, -PLHH, -REFPERSOONHH, -GEBJAARJONGSTEKINDHH, -GEBMAANDJONGSTEKINDHH,
#         -GEBJAAROUDSTEKINDHH, -GEBJAAROUDSTEKINDHH, -GEBMAANDOUDSTEKINDHH, -AANTALOVHH, -IMPUTATIECODEHH)
#rm(gbah)

#diaper_ids <- left_join(diaper_ids, gbah_filtered, join_by(RINPERSOON, RINPERSOONS, year))

#rm(gbah_filtered)



# Marital status ----------------------------------------------------------
#TODO: run gbaburg - threw an error
#gbaburg <- read_sav(file = PATHS$GABURG, n_max = NMAX) |> 
#  filter(RINPERSOON %in% diaper_ids$RINPERSOON) |>
#  mutate(RINPERSOONS    = as_factor(RINPERSOONS, levels = "values"))

#write_rds(gbaburg, 'processed_data/gbaburg.rds') #Saves the dataset

#row 809731 --> the date is February 29th but 1989 is not a leap year. The script throws an error. The years are filtered only to include years after 2010.

#Creating a date variable and years interval
#gbaburg <- gbaburg |> 
#  filter(GBAEINDEBURGERLIJKESTAAT >= "20100101") |> 
#  mutate(year_start = ymd(GBAAANVANGBURGERLIJKESTAAT)) |> 
#  mutate(year_end = ymd(GBAEINDEBURGERLIJKESTAAT))
#

#gbaburg <- gbaburg |> 
#  rowwise() |> 
#  mutate(year = list(extract_years(year_start, year_end)))

# Unnesting the years
#gbaburg <- gbaburg |> 
#  unnest(cols = c(year))

#Keeping the record with a later date
#gbaburg_filtered <- gbaburg |> 
#  group_by(RINPERSOON, RINPERSOONS) |> 
#  arrange(year, desc(year_end)) |> 
#  filter(duplicated(year) == FALSE) |> 
#  select(-year_start, -year_end, -GBAAANVANGBURGERLIJKESTAAT, -GBAEINDEBURGERLIJKESTAAT)
#rm(gbaburg)

#diaper_ids <- left_join(diaper_ids, gbaburg_filtered, join_by(RINPERSOON, RINPERSOONS, year))

#rm(gbaburg_filtered)


# Socioeconomic category --------------------------------------------------
#secm <- read_sav(PATHS$SECM, n_max = NMAX, col_select = all_of(cols_secm)) |> 
#  filter(RINPERSOON %in% diaper_ids$RINPERSOON) |>
#  mutate(RINPERSOONS    = as_factor(RINPERSOONS, levels = "values"))

#Creating a date variable and years interval
#secm <- secm |> 
#  mutate(year_start = ymd(AANVSECM)) |> 
#  mutate(year_end = ymd(EINDSECM))

#secm <- secm |> 
#  rowwise() |> 
#  mutate(year = list(extract_years(year_start, year_end)))

# Unnesting the years
#secm <- secm |> 
#  unnest(cols = c(year))

#Keeping the record with a later date
#secm_filtered <- secm |> 
#  group_by(RINPERSOON, RINPERSOONS) |> 
#  arrange(year, desc(year_end)) |> 
#  filter(duplicated(year) == FALSE) |> 
#  select(-year_start, -year_end, -AANVSECM, -EINDSECM)

#rm(secm)

#diaper_ids <- left_join(diaper_ids, secm_filtered, join_by(RINPERSOON, RINPERSOONS, year))
#rm(secm_filtered)

write_rds(diaper_ids, 'processed_data/diaper_cbs.rds')

