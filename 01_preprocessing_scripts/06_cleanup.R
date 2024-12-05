library(tidyverse)
library(dplyr)
library(haven)
library(forcats)
library(labelled)

#TODO: Organize the data imputation scripts

#Education variable (OPLNIVSOI2016AGG4HBMETNIRWO) --> currently only available from 2013 onwards
#NB: The Gezondheidsmonitor datasets have their own education variable 
#Load the file with all the CBS variables
diaper <- read_rds('processed_data/diaper_cbs.rds')

# Drop unnecessary variables --------------------------------------------
#Retain only necessary columns
diaper <- diaper |> 
  select(RINPERSOONS_K, RINPERSOON_K,  RINPERSOONS, RINPERSOON, year, alcoholgebruik, apgar5, congenafw, 
         drugsgebruik, etnic, fluxus, birth_weight, child_sex, gew_vrouw, grav, hoftiezer, lengte, lft, nicuopname, pc4, rookgedrag,
         ruptuur, ses_percentiel, urbanity, neonatal_mortality, INHARMEUR, education, RINPERSOONSHKW, RINPERSOONHKW, SOORTOBJECTNUMMER, 
         RINOBJECTNUMMER, VEHP100HVERM, VZAANTZIEKHINCLBP05KM, NOPZVWKKRAAMZORG, ATC4A12A, ATC4A11C, ATC4A11D, ATC4A11E, ATC4N02A, 
         ATC4B03X, ATC4N06A, ATC4B03B, DIAG11, DIAG13, VZAFSTANDZIEKHINCLBP)

# Transform classes  -----------------------------------------------------
# Inspect variable classes and identify what needs to be transformed 
sum_var_classes <- function(data) {
  class_sum <- sapply(data, function(x) paste(class(x), collapse = ", "))
  class_table <- data.frame(
    variable = names(class_sum),
    class    = unlist(class_sum),
    row.names = NULL
  )
  return(class_table)
}

class_sum <- sum_var_classes(diaper)
print(class_sum)

#Recode diag and medication columns from haven_labelled to factors. Assign 1 to existing diagnosis and 0 to NAs. 
#NB: If the individual did not receive a diagnosis, they do not have a value in the diagnosis and medication tables, thus NAs are recoded as 0. 

transf_cols <- function(data, prefixes) {
  #Ensure prefixes is a character vector
  if (!is.character(prefixes)) {
    stop("'prefixes' must be a character vector of column name prefixes.")
  }
  sel_cols <- names(data)[sapply(names(data), function(col){
    any(startsWith(col, prefixes))
    })]
  if(length(sel_cols) == 0) {
    stop("No columns match the  specified prefixes.")
  }
  
  for (col in sel_cols) {
    if (!is.null(data[[col]]) && !all(is.na(data[[col]]))){
      data[[col]] <- factor(
        if_else(
          is.na(as.character(as_factor(diaper[[col]]))) | as.character(as_factor(diaper[[col]])) == "",
          0,
          1
        ) 
      )
    }
  }
  return(data)
}
#Apply to dataset with both prefixes
diaper <- transf_cols(diaper, prefixes = c("ATC4", "DIAG"))


#Assign NA to percentile values below 1 (coded as unknown in the CBS documentation). Currently takes a while to compute
diaper <- diaper |> 
  mutate(
    perc_income      = if_else(as.numeric(INHARMEUR) < 1, NA, as.numeric(INHARMEUR)),
    hh_wealth_perc   = if_else(as.numeric(VEHP100HVERM) < 1, NA, as.numeric(VEHP100HVERM)))


# Assign labels to factors ------------------------------------------------
#Assign labels to factor variables (0 = No, 1 = Yes)
factor_cols <- grep("^(ATC4|DIAG)", names(diaper), value = TRUE)

diaper[factor_cols] <- lapply(diaper[factor_cols], function(col) {
  levels(col) <- c("No", "Yes")
  return(col)
})

#Assign labels to the other variables
diaper <- diaper |> 
  mutate(
    drugsgebruik = factor(
      drugsgebruik,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  ) |> 
  mutate(
    etnic = factor(
      etnic,
      levels = c(12, 3, 4, 5, 13, 8, 14, 11, "UNK"),
      labels = c(
        "Caucasian",
        "North African",
        "Other African",
        "Turkish",
        "Hindustani", 
        "Other Asian", 
        "Latin American",
        "Other (including mixed)",
        "Unknown"
      )
    )
  ) |> 
  mutate(
    fluxus = factor(
      fluxus,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  ) |> 
  mutate(
    nicuopname = factor(
      nicuopname,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  ) |> 
  mutate(
    rookgedrag = factor(
      rookgedrag,
      levels = c(1, 2, 3, 4, 5, 6, "UNK"),
      labels = c(
        "Did not smoke",
        "1-10 per day",
        "11-20 per day",
        "More than 20 per day",
        "Quit before pregnancy",
        "Quit during pregnancy",
        "Unknown"
      )
    )
  ) |> 
  mutate(
    ruptuur = factor(
      ruptuur,
      levels = c(0, 199916005, 199925004, 199930000, 199934009, 449807005, 449808000, 449809008),
      labels = c(
        "No rupture",
        "Grade 1 rupture",
        "Grade 2 rupture",
        "Grade 3 rupture",
        "Grade 4 rupture",
        "Grade 3a <50% external sphincter injury",
        "Grade 3b >=50% external sphincter injury",
        "Grade 3c external and internal sphincter injury"
      )
    )
  ) |> 
  mutate(
    urbanity = factor(
      urbanity,
      levels = c(1, 2, 3, 4, 5),
      labels = c(
        "Very urban (>= 2500 addresses per km2",
        "Urban (1500-2500 addresses per km2",
        "Modaretly urban (1000-1500 addresses per km2",
        "Less urban (500-1000 addresses per km2",
        "Not urban (<500 addresses per km2"
      )
    )
  ) |> 
  mutate(
    child_sex = factor(
      child_sex,
      levels = c(1, 2, 3, "UNK"),
      labels = c(
        "Male",
        "Female",
        "Inconclusive",
        "Unknown"
      )
    )
  ) |> 
  mutate(
    neonatal_mortality = factor(
      neonatal_mortality,
    levels = c(0, 1, 2, 3, 4, 5, 6),
    labels = c(
      "Not deceased",
      "Antepartum",
      "during labor",
      "Postpartum (0 - 7 days)",
      "Postpartum (8-28 days)",
      "Postpartum (>28 days)",
      "Unclear death phase"
    )
  )
)
# Compute variables -------------------------------------------------------
#Compute small (<10th percentile) and large (>90th percentile) for gestational age variables 
#Compute a variable for congenital diagnosis (all diagnoses grouped under 'yes')
diaper <- diaper |> 
  mutate(
    congen_diag = if_else(!is.na(congenafw), 1, 0),
    small_ga = if_else(hoftiezer >= 0 & hoftiezer <= 10, 1, 0),
    large_ga = if_else(hoftiezer >= 90 & hoftiezer <= 100, 1, 0)
  )

#Extremely low birth weight, cutoff is at 2500 grams, as that is the threshold set by the WHO  
diaper <- diaper |> 
  mutate(
    low_bw = if_else(birth_weight < 2500, 1, 0))


# Rename variables ------------------------------------------------------

diaper <- diaper |> 
  rename(
    num_hospitals       = VZAANTZIEKHINCLBP05KM,
    distance_hospitals  = VZAFSTANDZIEKHINCLBP, #distance in meters to the nearest hospital or clinic, calculated over the road
    cost_maternity_care = NOPZVWKKRAAMZORG, #maternity care costs
    calcium_sup         = ATC4A12A,
    vit_a_d             = ATC4A11C,
    vit_b1_b6_b12       = ATC4A11D,
    vit_b_complex       = ATC4A11E,
    vit_b12_folic       = ATC4B03B,
    opioids             = ATC4N02A,
    anemia_med          = ATC4B03X,
    antidepressants     = ATC4N06A,
    diag_depression     = DIAG11,
    diag_anxiety        = DIAG13
    ) 



# Compute remaining -------------------------------------------------------

#Education categories, according to CBS grouping - low (up to Mbo2/3), middle (up to vwo), high (up to doctor)

diaper <- diaper |> 
  mutate(education = as.character(education))

diaper <- diaper |> 
  mutate(
    education_cat = case_when(
      startsWith(education, "1") ~ "1",
      startsWith(education, "2") ~ "2",
      startsWith(education, "3") ~ "3",
      TRUE ~ NA_character_
    ),
    education_cat = factor(education_cat, 
                           levels = c("1", "2", "3"),
                           labels = c("Low", "Middle", "High"))
  )

#TODO: Fix 2019 values 

diaper <- diaper |> 
  mutate(inc_poverty = cut(perc_income,
                           breaks = c(0, 100, 165, Inf),
                           labels = c("Below poverty line", "Below median", "Above median"), #Where 100 is at or below poverty line, 2 is below the median income, 3 is above median 
                           right = TRUE))


# Labels ------------------------------------------------------------------
#TODO: Fix labels
#Troubleshoot
# set_label <- function(variable, label) {
#   attr(variable, "label") <- label
#   variable
# }
# diaper <- diaper |> 
#   mutate(
#     perc_income                 = set_label(perc_income, "Income as % of the at-risk-of-poverty line"))
  #   child_sex                 = set_variable_labels(child_sex, "Infant sex"),
  #   education_cat             = set_variable_labels(education_cat, "Maternal education"),
  #   hoftiezer                 = set_variable_labels(hoftiezer, "Birth weight % of the population growth curve"),
  #   etnic                     = set_variable_labels(etnic, "Ethnicity of the mother"),
  #   fluxus                    = set_variable_labels(fluxus, "Postpartum blood loss >1000mL"),
  #   ruptuur                   = set_variable_labels(ruptuur, "Rupture"),
  #   num_hospitals             = set_variable_labels(num_hospitals, "Rupture"),
  #   small_ga                  = set_variable_labels(small_ga, "Small for gestational age (SGA)"),
  #   large_ga                  = set_variable_labels(large_ga, "Large for gestational age (LGA)"),
  #   low_bw                    = set_variable_labels(low_bw, "Low birth weight WHO criterion (< 2500grams)"),
  #   cost_maternity_care       = set_variable_labels(cost_maternity_care, "Costs for maternity care in euros"),
  #   distance_hospitals        = set_variable_labels(distance_hospitals, "Distance in meters to the closest hospital"),
  #   num_hospitals             = set_variable_labels(num_hospitals, "Number of hospitals within 5km of residence"),
  #   diag_depression           = set_variable_labels(diag_depression, "DSM IV diagnosis of Major Depressive Disorder"),
  #   diag_anxiety              = set_variable_labels(diag_anxiety, "DSM IV diagnosis of Generalized Anxiety Disorder"),
  #   inc_poverty               = set_variable_labels(inc_poverty, "Income bracket relative to the at-risk-of-poverty line"),
  #   calcium_sup               = set_variable_labels(calcium_sup, "Calcium supplementation as recorded by GP"),
  #   vit_a_d                   = set_variable_labels(vit_a_d, "Vitamin A or D supplementation as recorded by GP"),
  #   vit_b1_b6_b12             = set_variable_labels(vit_b1_b6_b12, "Vitamin B1, B6 or B12 supplementation as recorded by GP"),
  #   vit_b_complex             = set_variable_labels(vit_b_complex, "Vitamins B complex supplementation as recorded by GP"),
  #   opioids                   = set_variable_labels(opioids, "Opioid use as recorded by GP"),
  #   anemia_med                = set_variable_labels(anemia_med, "Use of anaemia medication as recorded by GP")
  # )
# 
# #
# diaper <- diaper |> 
#   mutate(
#     perc_income               = set_variable_labels(perc_income, "Income as % of the at-risk-of-poverty line"),
#     child_sex                 = set_variable_labels(child_sex, "Infant sex"),
#     education_cat             = set_variable_labels(education_cat, "Maternal education"),
#     hoftiezer                 = set_variable_labels(hoftiezer, "Birth weight % of the population growth curve"),
#     etnic                     = set_variable_labels(etnic, "Ethnicity of the mother"),
#     fluxus                    = set_variable_labels(fluxus, "Postpartum blood loss >1000mL"),
#     ruptuur                   = set_variable_labels(ruptuur, "Rupture"),
#     num_hospitals             = set_variable_labels(num_hospitals, "Rupture"),
#     small_ga                  = set_variable_labels(small_ga, "Small for gestational age (SGA)"),
#     large_ga                  = set_variable_labels(large_ga, "Large for gestational age (LGA)"),
#     low_bw                    = set_variable_labels(low_bw, "Low birth weight WHO criterion (< 2500grams)"),
#     cost_maternity_care       = set_variable_labels(cost_maternity_care, "Costs for maternity care in euros"),
#     distance_hospitals        = set_variable_labels(distance_hospitals, "Distance in meters to the closest hospital"),
#     num_hospitals             = set_variable_labels(num_hospitals, "Number of hospitals within 5km of residence"),
#     diag_depression           = set_variable_labels(diag_depression, "DSM IV diagnosis of Major Depressive Disorder"),
#     diag_anxiety              = set_variable_labels(diag_anxiety, "DSM IV diagnosis of Generalized Anxiety Disorder"),
#     inc_poverty               = set_variable_labels(inc_poverty, "Income bracket relative to the at-risk-of-poverty line"),
#     calcium_sup               = set_variable_labels(calcium_sup, "Calcium supplementation as recorded by GP"),
#     vit_a_d                   = set_variable_labels(vit_a_d, "Vitamin A or D supplementation as recorded by GP"),
#     vit_b1_b6_b12             = set_variable_labels(vit_b1_b6_b12, "Vitamin B1, B6 or B12 supplementation as recorded by GP"),
#     vit_b_complex             = set_variable_labels(vit_b_complex, "Vitamins B complex supplementation as recorded by GP"),
#     opioids                   = set_variable_labels(opioids, "Opioid use as recorded by GP"),
#     anemia_med                = set_variable_labels(anemia_med, "Use of anaemia medication as recorded by GP")
#   )
# 

# Drop double variables ---------------------------------------------------

diaper <- diaper |> 
  select(-INHARMEUR, -education, -congenafw, -lengte)

#Save the dataset
write_rds(diaper, 'processed_data/diaper_clean.rds')
rm(list = ls())
