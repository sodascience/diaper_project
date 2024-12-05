library(tidyverse)
library(haven)
library(dplyr)
#TODO: Delete unnecessary parts later

#Debug: set NMAX to not infinite
NMAX <- Inf

#File paths
source("01_preprocessing_scripts/00_file_paths.R")

cat(format(now()), "Reading DIAPER ID file.\n")
diaper_ids <- read_rds("processed_data/diaper_ids.rds") #Load IDs file
diaper <- read_rds("raw_data/diaper.rds")
#Defining the variables
koppel_cols  <- c("RINPERSOON", "RINPERSOONS", "RINPERSOONHKW", "RINPERSOONSHKW")

#defining the function 
read_koppel <- function(path) {
  cat(path, "\r")
  read_sav(path, n_max = NMAX, col_select = all_of(koppel_cols)) |> 
    filter(RINPERSOON %in% diaper_ids$RINPERSOON) |> 
    mutate(
      RINPERSOONS    = as_factor(RINPERSOONS, levels = "values"),
      RINPERSOONSHKW = as_factor(RINPERSOONSHKW, levels = "values")
    ) 
}
#Reading the table
cat(format(now()), "Reading koppel table.\n")
koppel <- 
  lapply(PATHS$KOPPELHH, read_koppel) |> 
  bind_rows(.id = "year") |> 
  mutate(year = parse_integer(year))

#Joining with ID file 
diaper_ids <- left_join(diaper_ids, koppel, by = join_by(RINPERSOON, RINPERSOONS, year))

#Add all IDs to larger variables file

diaper_complete <- left_join(diaper, diaper_ids, by = join_by(RINPERSOON, RINPERSOONS, year), relationship = "one-to-one") #go back and check missing maternal IDs

#Save file
write_rds(diaper_complete, 'processed_data/diaper_complete.rds')

