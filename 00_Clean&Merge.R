library(here)
library(dplyr)
library(ggplot2)
library(tableone)
library(openxlsx)
library(readr)

# Load Transitions Data
Transitions <- read.csv(here("transitions_merged.csv"))


#--- Transitions Cleaning ---

# Remove m0_ prefix
Transitions <- Transitions |>
  rename_with(~ sub("^m0_", "", .x), starts_with("m0_"))

# Select + rename
Transitions <- Transitions |>
  select(
    record_id, biospecimen_id,
    promis_global05_902706, promis_global09r_fbd679,
    isolation_score, isolation_cat, isolation_cat2,
    race, race_other, ethnicity, sex, gender_identity, age, age_cat, bmi,
    education, insurance_type, oth_insurance_provider, marriage, finance,
    home_ownership, home_ownership_oth, employ_status, employ_status_other,
    workfunction, lbp_vas_current, cdc_pain_freq, cdc_pain_interfere,
    nih_lbpfrequency, peg_enjoyment, peg_activity,
    physical_activity_days, physical_activity_minutes, physical_activity_score,
    char_total_score, psqi_global_score, days_since_lbp_start,
    stomach_pain, other_pain, headaches, widespread_pain,
    treatment_nsaid, treatment_nsaid_current,
    tretment_mr, treatment_mr_current,
    treatment_opioids, treatment_opioids_current,
    treatment_injections, treatment_exercise_therapy,
    treatment_smt, treatment_cbt,
    prior_lbp, prior_lbp_num, prior_lbp_days_since, prior_lbp_vas,
    startback_total_score, startback_subscore, startback_risk, startback_risk_label,
    promis_global05_902706, promis_global01_9f6d75,
    promis_global02_b59611, promis_global10r, promis_global03_a64ef7,
    promis_global06_711d3a, promis_global08r,
    zipcode, neighbor_ex03,
    substance_use1, substance_use2,
    smoking_status_100, smoking_status_daily,
    smoker_status_cat, smoker_status_cat2
  ) |>
  rename(
    social_relationships = promis_global05_902706,
    social_activities    = promis_global09r_fbd679,
    qol                  = promis_global02_b59611,
    general_health       = promis_global01_9f6d75,
    mental_health        = promis_global10r,
    physical_health      = promis_global03_a64ef7,
    perform_pa           = promis_global06_711d3a,
    fatigue              = promis_global08r,
    neighborhood_walk    = neighbor_ex03
  ) |> 
  mutate(
    # --- Social variables (with Excellent as reference) ---
    social_relationships_ordinal = factor(
      social_relationships,
      levels = c(5, 4, 3, 2, 1),
      labels = c("Excellent", "Very good", "Good", "Fair", "Poor"),
      ordered = TRUE
    ),
    social_relationships_nominal = factor(
      social_relationships,
      levels = c(5, 4, 3, 2, 1),
      labels = c("Excellent", "Very good", "Good", "Fair", "Poor"),
      ordered = FALSE
    ),
    social_relationships_d = case_when(
      social_relationships >= 3 ~ "High",  # Excellent, Very good, Good (5,4,3)
      social_relationships <= 2 ~ "Low",   # Fair, Poor (2,1)
      TRUE ~ NA_character_
    ),
    
    social_activities_ordinal = factor(
      social_activities,
      levels = c(5, 4, 3, 2, 1),
      labels = c("Excellent", "Very good", "Good", "Fair", "Poor"),
      ordered = TRUE
    ),
    social_activities_nominal = factor(
      social_activities,
      levels = c(5, 4, 3, 2, 1),
      labels = c("Excellent", "Very good", "Good", "Fair", "Poor"),
      ordered = FALSE
    ),
    social_activities_d = case_when(
      social_activities >= 3 ~ "High",
      social_activities <= 2 ~ "Low",
      TRUE ~ NA_character_
    ),
    
    # Isolation (with Not Isolated as reference)
    isolation_cat_ordinal = factor(
      isolation_cat,
      levels = c(3, 2, 1, 0),
      labels = c("Not Isolated", "Somewhat Isolated", "Very Isolated", "Most Isolated"),
      ordered = TRUE
    ),
    isolation_cat_nominal = factor(
      isolation_cat,
      levels = c(3, 2, 1, 0),
      labels = c("Not Isolated", "Somewhat Isolated", "Very Isolated", "Most Isolated"),
      ordered = FALSE
    ),
    isolation_cat_d = case_when(
      isolation_cat == 3 ~ "Low",   # Not Isolated
      isolation_cat %in% c(0, 1, 2) ~ "High",  # All other categories
      TRUE ~ NA_character_
    ),
    
    # Demographics
    race_f = factor(
      race,
      levels = c(0, 1, 2, 3, 5),
      labels = c("White", "Black", "Unknown", "Asian", "Other")
    ),
    race_dichotomized = if_else(race_f == "White", 0, 1),
    ethnicity_f = factor(
      ethnicity,
      levels = c(0, 1),
      labels = c("Not Hispanic", "Hispanic")
    ),
    sex_f = factor(
      sex,
      levels = c(0, 1),
      labels = c("Male", "Female")
    ),
    education_f = factor(
      education,
      levels = c(7, 6, 5, 4, 3, 2, 1, 0),
      labels = c(
        "Graduate degree", "Some graduate school", "Bachelors Degree",
        "Associates Degree", "Some College", "Trade School",
        "High School Graduate", "Less than High School"
      ),
      ordered = TRUE
    ),
    
    # Insurance, finances, home, employment
    insurance_type_f = factor(
      na_if(insurance_type, 999),
      levels = c(0, 1, 2, 3, 4, 9),
      labels = c(
        "None/Uninsured", "Medicaid", "Medicare",
        "Other Public Insurance", "Private Insurance", "Unsure"
      )
    ),
    marriage_f = factor(
      marriage,
      levels = c(0, 1, 2, 3, 4),
      labels = c(
        "Single Never Married", "Divorced", "Separated",
        "Widowed", "Married/Living with Partner"
      )
    ),
    finance_f = factor(
      na_if(finance, 999),
      levels = c(0, 1, 2, 3),
      labels = c(
        "Live Comfortably",
        "Meet Basic Expenses with Little Left Over",
        "Just Meet Basic Expenses",
        "Can't Meet Basic Expenses"
      ),
      ordered = FALSE
    ),
    home_ownership_f = factor(
      na_if(home_ownership, 999),
      levels = c(0, 1, 2, 3),
      labels = c("Rent", "Own", "Live with Family or Friends", "Other"),
      ordered = FALSE
    ),
    employ_status_f = factor(
      na_if(employ_status, 999),
      levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      labels = c(
        "Working Full Time", "Working Part Time",
        "Unemployed Looking for Work", "Sick Leave",
        "Disabled due to back pain", "Disabled Other",
        "Student", "Temporarily Laid Off", "Retired",
        "Keeping House", "Other"
      )
    ),
    
    # Work and pain measures
    workfunction_f = factor(
      workfunction,
      levels = c(0, 1, 2),
      labels = c(
        "Not Been off work due to LBP",
        "Been off work due to LBP",
        "Does Not Apply"
      )
    ),
    cdc_pain_freq_f = factor(
      cdc_pain_freq,
      levels = c(0, 1, 2, 3),
      labels = c("Never", "Some Days", "Most Days", "Every Day"),
      ordered = TRUE
    ),
    cdc_pain_interfere_f = factor(
      cdc_pain_interfere,
      levels = c(0, 1, 2, 3),
      labels = c("Never", "Some Days", "Most Days", "Every Day"),
      ordered = TRUE
    ),
    widespread_pain_f = factor(
      widespread_pain,
      levels = c(0, 1, 2),
      labels = c("Not bothered at all", "Bothered a little", "Bothered a lot"),
      ordered = TRUE
    ),
    startback_risk_label_f = factor(
      startback_risk_label,
      levels = c("Low Risk", "Medium Risk", "High Risk"),
      ordered = TRUE
    ),
    
    # Health and QoL
    general_health_f = factor(
      general_health,
      levels = c(5, 4, 3, 2, 1),
      labels = c("Excellent", "Very Good", "Good", "Fair", "Poor"),
      ordered = TRUE
    ),
    qol_f = factor(
      qol,
      levels = c(5, 4, 3, 2, 1),
      labels = c("Excellent", "Very Good", "Good", "Fair", "Poor"),
      ordered = TRUE
    ),
    mental_health_f = factor(
      mental_health,
      levels = c(5, 4, 3, 2, 1),
      labels = c("Excellent", "Very Good", "Good", "Fair", "Poor"),
      ordered = TRUE
    ),
    physical_health_f = factor(
      physical_health,
      levels = c(5, 4, 3, 2, 1),
      labels = c("Excellent", "Very Good", "Good", "Fair", "Poor"),
      ordered = TRUE
    ),
    
    # Convert all _d variables to factors (Low as reference)
    across(ends_with("_d"), ~ factor(.x, levels = c("Low", "High")))
  )