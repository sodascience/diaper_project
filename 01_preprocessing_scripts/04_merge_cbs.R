library(tidyverse)
library(haven)
library(dplyr)

#Debug: set NMAX to not infinite
NMAX <- 10000

#File paths
source("01_preprocessing_scripts/00_file_paths.R")

#Read data
cat(format(now()), "Reading DIAPER ID file.\n")
diaper_ids <- read_rds('processed_data/diaper_complete.rds')

#Define columns with relevant variables
inha_cols    <- c("RINPERSOONHKW", "RINPERSOONSHKW", "INHARMEUR")
veh_cols     <- c("RINPERSOONHKW", "RINPERSOONSHKW", "VEHP100HVERM") #Wealth 
med_cols     <- c("RINPERSOON", "RINPERSOONS", "ATC4") #Need to be processed after
diag_cols    <- c("RINPERSOON", "RINPERSOONS", 
                  "GGZDBCdiagnoseaantalHOOFDDIAG", "GGZDBCDiagnosedatumHOOFDDIAG", 
                  "GGZDBCnevendiagnoseDSMIVHOOFDDIAG",
                  "GGZDBCDiagnosevolgnummerHOOFDDIAG") #Needs to be processed after
opl_cols     <- c("RINPERSOON", "RINPERSOONS", "OPLNIVSOI2016AGG4HBMETNIRWO") 
opl_19       <- c("RINPERSOON", "RINPERSOONS", "OPLNIVSOI2016AGG4HGMETNIRWO")
opl_20       <- c("RINPERSOON", "RINPERSOONS", "OPLNIVSOI2021AGG4HGmetNIRWO")
#alternative variable names
#2019 = OPLNIVSOI2016AGG4HGMETNIRWO
#2020 = OPLNIVSOI2021AGG4HGmetNIRWO
zorgk_cols   <- c("RINPERSOON", "RINPERSOONS", "NOPZVWKKRAAMZORG")
nabzorg_cols <- c("RINOBJECTNUMMER", "SOORTOBJECTNUMMER", "VZAANTZIEKHINCLBP05KM", "VZAFSTANDZIEKHINCLBP")


# Functions to read tables ------------------------------------------------
# Functions can be adapted if a variable needs to be processed
#Inha
read_inha <- function(path) {
  cat(path, "\r")
  read_sav(path, n_max = NMAX, col_select = all_of(inha_cols)) |> 
    filter(RINPERSOONHKW %in% diaper_ids$RINPERSOONHKW) |> 
    mutate(
      RINPERSOONSHKW = as_factor(RINPERSOONSHKW, levels = "values")
    )
}

#Vehtab
read_veh <- function(path) {
  cat(path, "\r")
  read_sav(path, n_max = NMAX, col_select = all_of(veh_cols)) |> 
    filter(RINPERSOONHKW %in% diaper_ids$RINPERSOONHKW) |> 
    mutate(
      RINPERSOONSHKW = as_factor(RINPERSOONSHKW, levels = "values")
    )
}

#Medicijntab
read_med <- function(path) {
  cat(path, "\r")
  read_sav(path, n_max = NMAX, col_select = all_of(med_cols)) |> 
    filter(RINPERSOON %in% diaper_ids$RINPERSOON) |> 
    mutate(
      RINPERSOONS    = as_factor(RINPERSOONS, levels = "values"))
}

#DIAG
read_diag <- function(path) {
  cat(path, "\r")
  read_sav(path, n_max = NMAX, col_select = all_of(diag_cols)) |> 
    filter(RINPERSOON %in% diaper_ids$RINPERSOON) |> 
    mutate(
      RINPERSOONS    = as_factor(RINPERSOONS, levels = "values")
    )
}

#OPL
read_opl <- function(path) {
  cat(path, "\r")
  read_sav(path, n_max = NMAX, col_select = all_of(opl_cols)) |> 
    filter(RINPERSOON %in% diaper_ids$RINPERSOON) |> 
    mutate(
      RINPERSOONS    = as_factor(RINPERSOONS, levels = "values")
    ) |> 
    rename(education = OPLNIVSOI2016AGG4HBMETNIRWO)
}

#Zorgkosten
read_zorgk <- function(path) {
  cat(path, "\r")
  read_sav(path, n_max = NMAX, col_select = all_of(zorgk_cols)) |> 
    filter(RINPERSOON %in% diaper_ids$RINPERSOON) |> 
    mutate(
      RINPERSOONS    = as_factor(RINPERSOONS, levels = "values")
    )
}

#Nabzorg
read_nabzorg <- function(path) {
  cat(path, "\r")
  read_sav(path, n_max = NMAX, col_select = all_of(nabzorg_cols)) |> 
    filter(RINOBJECTNUMMER %in% diaper_ids$RINOBJECTNUMMER)
}


# Reading tables and merging years ----------------------------------------
#Read INHA
#DECISION: Some of the rows in the main file do not have household ID. They are skipped for now. 
cat(format(now()), "Reading INHA table.\n")
inha <- 
  lapply(PATHS$INHA, read_inha) |> 
  bind_rows(.id = "year") |> 
  mutate(year = parse_integer(year))


diaper_ids <- left_join(diaper_ids, inha, by = join_by(RINPERSOONHKW, RINPERSOONSHKW, year))
rm(inha)


#Read VEH
cat(format(now()), "Reading VEH table.\n")
veh <- 
  lapply(PATHS$VEH, read_veh) |> 
  bind_rows(.id = "year") |> 
  mutate(year = parse_integer(year))

diaper_ids <- left_join(diaper_ids, veh, by = join_by(RINPERSOONHKW, RINPERSOONSHKW, year))
rm(veh)


#Read medicijntab
cat(format(now()), "Reading MED table.\n")
med <- 
  lapply(PATHS$MED, read_med) |> 
  bind_rows(.id = "year") |> 
  mutate(year = parse_integer(year))
diaper_ids <- left_join(diaper_ids, med, join_by(RINPERSOON, RINPERSOONS, year))


diaper_ids <- diaper_ids |>
  group_by(RINPERSOON) |> 
  pivot_wider(
    names_from = ATC4,
    values_from = ATC4,
    names_prefix = "ATC4",
    names_repair = "unique"
  )

rm(med)

#Read diag
cat(format(now()), "Reading DIAG table.\n")
diag <- 
  lapply(PATHS$DIAG, read_diag) |> 
  bind_rows(.id = "year") |> 
  mutate(year = parse_integer(year))
diaper_ids <- left_join(diaper_ids, diag, join_by(RINPERSOON, RINPERSOONS, year))

diaper_ids <- diaper_ids |>
  group_by(RINPERSOON) |> 
  pivot_wider(
    names_from = GGZDBCnevendiagnoseDSMIVHOOFDDIAG,
    values_from = GGZDBCnevendiagnoseDSMIVHOOFDDIAG,
    names_prefix = "DIAG",
    names_repair = "unique"
  )

rm(diag)

#Read opl
cat(format(now()), "Reading OPL table.\n")
opl <- 
  lapply(PATHS$OPL, read_opl) |> 
  bind_rows(.id = "year") |> 
  mutate(year = parse_integer(year))

#2019 - Separately because the variable names differ
opl_2019 <- read_sav(PATHS$OPL_19, n_max = NMAX, col_select = all_of(opl_19)) |> 
  filter(RINPERSOON %in% diaper_ids$RINPERSOON) |> 
  mutate(
    RINPERSOONS    = as_factor(RINPERSOONS, levels = "values"),
    year           = as.integer(2019)
  ) |> 
  rename(education = OPLNIVSOI2016AGG4HGMETNIRWO)


#2020 - Separately because the variable names differ
opl_2020 <- read_sav(PATHS$OPL_20, n_max = NMAX, col_select = all_of(opl_20)) |> 
  filter(RINPERSOON %in% diaper_ids$RINPERSOON) |> 
  mutate(
    RINPERSOONS    = as_factor(RINPERSOONS, levels = "values"),
    year           = as.integer(2020)
  ) |> 
  rename(education = OPLNIVSOI2021AGG4HGmetNIRWO)

#Joining the datasets 
opl_combined <- bind_rows(opl, opl_2019, opl_2020)
#Join with diaper
diaper_ids <- left_join(diaper_ids, opl_combined, join_by(RINPERSOON, RINPERSOONS, year))
rm(opl)

#Read zorkosten
cat(format(now()), "Reading ZORGKOSTEN table.\n")
zorgk <- 
  lapply(PATHS$ZORGK, read_zorgk) |> 
  bind_rows(.id = "year") |> 
  mutate(year = parse_integer(year))

diaper_ids <- left_join(diaper_ids, zorgk, join_by(RINPERSOON, RINPERSOONS, year))
rm(zorgk)

#Read nabzorg
cat(format(now()), "Reading NABZORG table.\n")
nabzorg <- 
  lapply(PATHS$NABZORG, read_nabzorg  ) |> 
  bind_rows(.id = "year") |> 
  mutate(year = parse_integer(year))

nabzorg <- nabzorg |> 
  mutate(SOORTOBJECTNUMMER = as_factor(SOORTOBJECTNUMMER, levels = "values"))
diaper_ids <- diaper_ids |> 
  mutate(SOORTOBJECTNUMMER = as_factor(SOORTOBJECTNUMMER, levels = "values"))

diaper_ids <- left_join(diaper_ids, nabzorg, join_by(RINOBJECTNUMMER, SOORTOBJECTNUMMER, year))
rm(nabzorg)

write_rds(diaper_ids, 'processed_data/diaper_cbs.rds')
