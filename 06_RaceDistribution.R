library(here)
library(tidyverse)
library(dplyr)

# Load Transitions Data
Transitions <- read.csv(here("transitions_merged.csv"))


#--- Transitions Cleaning ---

# Remove m0_ prefix
Transitions <- Transitions |>
  rename_with(~ sub("^m0_", "", .x), starts_with("m0_"))

# Select + rename
Transitions <- Transitions |>
  select(promis_global05_902706, promis_global09r_fbd679, isolation_cat, race) |>
  rename(
    social_relationships = promis_global05_902706,
    social_activities    = promis_global09r_fbd679
  ) |> 
  mutate(
    # --- Social variables (with Excellent as reference) ---
    social_relationships_nominal = factor(
      social_relationships,
      levels = c(5, 4, 3, 2, 1),
      labels = c("Excellent", "Very good", "Good", "Fair", "Poor"),
      ordered = FALSE
    ),
    social_relationships1_d = factor(
      case_when(
        social_relationships >= 3 ~ "High",  # Excellent, Very good, Good (5,4,3)
        social_relationships <= 2 ~ "Low",   # Fair, Poor (2,1)
        TRUE ~ NA_character_),
      levels = c("High", "Low")
    ),
    social_relationships2_d = factor(
      case_when(
        social_relationships >= 4 ~ "High",  # Excellent, Very good, Good (5,4,3)
        social_relationships <= 3 ~ "Low",   # Fair, Poor (2,1)
        TRUE ~ NA_character_),
      levels = c("High", "Low")
    ),
    
    social_activities_nominal = factor(
      social_activities,
      levels = c(5, 4, 3, 2, 1),
      labels = c("Excellent", "Very good", "Good", "Fair", "Poor"),
      ordered = FALSE
    ),
    social_activities1_d = factor(
      case_when(
        social_activities >= 3 ~ "High",
        social_activities <= 2 ~ "Low",
        TRUE ~ NA_character_),
      levels = c("High", "Low")
    ),
    social_activities2_d = factor(
      case_when(
        social_activities >= 4 ~ "High",
        social_activities <= 3 ~ "Low",
        TRUE ~ NA_character_),
      levels = c("High", "Low")
    ),
    
    # Isolation (with Not Isolated as reference)
    isolation_cat_nominal = factor(
      isolation_cat,
      levels = c(3, 2, 1, 0),
      labels = c("Not Isolated", "Somewhat Isolated", "Very Isolated", "Most Isolated"),
      ordered = FALSE
    ),
    isolation_cat1_d = case_when(
      isolation_cat == 3 ~ "Low",   # Not Isolated
      isolation_cat %in% c(0, 1, 2) ~ "High",  # All other categories
      TRUE ~ NA_character_
    ),
    isolation_cat2_d = case_when(
      isolation_cat %in% c(3, 2) ~ "Low",   # Not Isolated
      isolation_cat %in% c(0, 1) ~ "High",  # All other categories
      TRUE ~ NA_character_
    ),

    # Demographics
    race_f = factor(
      race,
      levels = c(0, 1, 2, 3, 5),
      labels = c("White", "Black", "Unknown", "Asian", "Other")
    ),
    race_dichotomized = if_else(race_f == "White", 0, 1),

    # Convert all _d variables to factors (Low as reference)
    across(ends_with("_d"), ~ factor(.x, levels = c("Low", "High")))
  )




#analysis_data <- readRDS(here("Aim3_Data_with_PCA.rds"))


#--- Social Relationships 1 (original dichotomization) ---
table(Transitions$social_relationships1_d,
      Transitions$race_dichotomized)

# Row Proportion
prop.table(
  table(Transitions$social_relationships1_d,
        Transitions$race_dichotomized), 1
)

# Column Proportion
prop.table(
  table(Transitions$social_relationships1_d,
        Transitions$race_dichotomized), 2
)


#--- Social Relationships 2 (new dichotomization) ---
table(Transitions$social_relationships2_d,
      Transitions$race_dichotomized)

# Row Proportion
prop.table(
  table(Transitions$social_relationships2_d,
        Transitions$race_dichotomized), 1
)

# Column Proportion
prop.table(
  table(Transitions$social_relationships2_d,
        Transitions$race_dichotomized), 2
)


#--- Social Activities 1 (original dichotomization) ---
table(Transitions$social_activities1_d,
      Transitions$race_dichotomized)

# Row Proportion
prop.table(
  table(Transitions$social_activities1_d,
        Transitions$race_dichotomized), 1
)

# Column Proportion
prop.table(
  table(Transitions$social_activities1_d,
        Transitions$race_dichotomized), 2
)

#--- Social Activities 2 (new dichotomization) ---
table(Transitions$social_activities2_d,
      Transitions$race_dichotomized)

# Row Proportion
prop.table(
  table(Transitions$social_activities2_d,
        Transitions$race_dichotomized), 1
)

# Column Proportion
prop.table(
  table(Transitions$social_activities2_d,
        Transitions$race_dichotomized), 2
)



#--- Social Isolation 1 (original dichotomization) ---
table(Transitions$isolation_cat1_d,
      Transitions$race_dichotomized)

# Row Proportion
prop.table(
  table(Transitions$isolation_cat1_d,
        Transitions$race_dichotomized), 1
)

# Column Proportion
prop.table(
  table(Transitions$isolation_cat1_d,
        Transitions$race_dichotomized), 2
)


#--- Social Isolation 2 (new dichotomization) ---
table(Transitions$isolation_cat2_d,
      Transitions$race_dichotomized)

# Row Proportion
prop.table(
  table(Transitions$isolation_cat2_d,
        Transitions$race_dichotomized), 1
)

# Column Proportion
prop.table(
  table(Transitions$isolation_cat2_d,
        Transitions$race_dichotomized), 2
)




