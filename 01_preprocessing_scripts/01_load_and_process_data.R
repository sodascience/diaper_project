## Open csv file and save as rds
#TODO: check the factor arguments (can we assign labels?)
#TODO: Make a new script for transforming variables and assigning labels
#TODO: Filter for singletons based on omv variable 
set.seed(42) #For data split

#Loading libraries
library(haven)
library(vroom)
library(readr)
library(tidyverse)

#List with variables of interest to load
cols_names <- c("RINPERSOONS_Kind", "Rinpersoon_Kind", "RINPERSOONS_Moeder", "Rinpersoon_Moeder",
                "jaar", "alcoholgebruik", "apgar5", "congenafw", "drugsgebruik", "etnic", "fluxus", "geboortegew", "gesl", "gew_vrouw",
                "grav", "hoftiezer", "lengte", "lft", "nicuopname", "pc4","rookgedrag",
                "ruptuur", "ses_percentiel", "stedelijkheid", "sterfte")
#List with variable types (should correspoing to cols_names)
cols_list <- cols(
  RINPERSOONS_Kind = col_character(),
  Rinpersoon_Kind = col_character(),
  RINPERSOONS_Moeder = col_character(),
  Rinpersoon_Moeder = col_character(),
  jaar = col_double(),
  alcoholgebruik = col_double(),
  apgar5 = col_double(),
  congenafw = col_factor(),
  drugsgebruik = col_double(),
  etnic = col_character(),
  fluxus = col_factor(),
  geboortegew = col_double(),
  gesl = col_character(),
  gew_vrouw = col_number(),
  grav = col_double(),
  hoftiezer = col_double(),
  lengte = col_double(),
  lft = col_double(),
  nicuopname = col_double(),
  pc4 = col_double(),
  rookgedrag = col_character(),
  ruptuur = col_double(),
  ses_percentiel = col_double(),
  stedelijkheid = col_double(),
  sterfte = col_double(),
)

#Reading the csv
diaper_data <- vroom('raw_data/Dataset.csv', delim = ";",
                     col_select = cols_names,
                     col_types = cols_list) #Reading data from csv

#Renaming variables 
diaper_data <- diaper_data |> 
  rename(RINPERSOON    = Rinpersoon_Moeder) |> 
  rename(RINPERSOONS   = RINPERSOONS_Moeder) |> 
  rename(RINPERSOON_K  = Rinpersoon_Kind) |> 
  rename(RINPERSOONS_K = RINPERSOONS_Kind) |> 
  rename(year = jaar) |> 
  rename(birth_weight = geboortegew) |> 
  rename(child_sex = gesl) |> 
  rename(neonatal_mortality = sterfte) |> 
  rename(urbanity = stedelijkheid)

#Dropping rows with missing maternal ID
diaper_data <- diaper_data |> 
  filter(!is.na(RINPERSOON))

#Dropping rows with missing child birth weight
diaper_data <- diaper_data |> 
  filter(!is.na(birth_weight))

#Keeping only singleton births, replace with the omv variable later 
diaper_data <- diaper_data |> 
  group_by(RINPERSOON, year) |> 
  filter(n() == 1) |> 
  ungroup()

#Saving the dataset
write_rds(diaper_data, "raw_data/diaper.rds") #Saving the file as an R file

#Removing duplicates based on RINPERSOON (maternal ID)
diaper_unique <- diaper_data |> 
  arrange(RINPERSOON, year) |> 
  filter(duplicated(RINPERSOON) == FALSE)

#Sample 10% of the dataset based on RINPERSOON
diaper_10 <- diaper_unique[sample(nrow(diaper_unique), 
                                  0.10*nrow(diaper_unique), 
                                  replace = FALSE),]

write_rds(diaper_10, 'raw_data/diaper_10_unique.rds') #Saving the dataset

#Extracting IDs
diaper_ids <- diaper_data |> 
  select(c(RINPERSOON, 
           RINPERSOONS, 
           year)) #selects the two ID variables for maternal ID
#Save the file with IDs
write_rds(diaper_ids, "processed_data/diaper_ids.rds") #Saving a file with IDs only 

