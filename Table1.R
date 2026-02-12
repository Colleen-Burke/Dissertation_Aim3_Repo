# Table 1
library(tidyverse)
library(here)
library(knitr)

table1_data <- readRDS(here("Aim3_Data.rds"))

table1_data <- table1_data |> 
  filter(record_id != 30)

table_one <- table1_data |> 
  summarise(
    N = n(),
    
    'Age, mean (SD)' = paste0(
      round(mean(age, na.rm = TRUE), 1),
      " (",
      round(sd(age, na.rm = TRUE), 1),
      ")"
    ),
    
    'Female, n (%)' = paste0(
      sum(sex_f == "Female", na.rm = TRUE),
      " (",
      round(mean(sex_f == "Female", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Hispanic, n (%)' = paste0(
      sum(ethnicity_f == "Hispanic", na.rm = TRUE),
      " (",
      round(mean(ethnicity_f == "Hispanic", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'White Race, n (%)' = paste0(
      sum(race_f == "White"),
      " (",
      round(mean(race_f == "White") * 100, 1),
      ")"
    ),
    
    'Black Race, n (%)' = paste0(
      sum(race_f == "Black"),
      " (",
      round(mean(race_f == "Black") * 100, 1),
      ")"
    ),
    
    'Asian Race, n (%)' = paste0(
      sum(race_f == "Asian"),
      " (",
      round(mean(race_f == "Asian") * 100, 1),
      ")"
    ),
    
    'Other Race, n (%)' = paste0(
      sum(race_f == "Other"),
      " (",
      round(mean(race_f == "Other") * 100, 1),
      ")"
    ),
    
    'Unknown Race, n (%)' = paste0(
      sum(race_f == "Unknown"),
      " (",
      round(mean(race_f == "Unknown") * 100, 1),
      ")"
    ),
    
    'College Education or Higher, n (%)' = paste0(
      sum(education_f %in% c("Graduate degree", 
                             "Some graduate school", 
                             "Bachelors Degree")),
      " (",
      round(mean(education_f %in% c("Graduate degree", 
                                    "Some graduate school", 
                                    "Bachelors Degree")) * 100, 1),
      ")"
    ),
    
    'Financial Stability, n (%)' = paste0(
      sum(finance_f == "Live Comfortably", na.rm = TRUE),
      " (",
      round(mean(finance_f == "Live Comfortably", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Uninsured, n (%)' = paste0(
      sum(insurance_type_f == "None/Uninsured", na.rm = TRUE),
      " (",
      round(mean(insurance_type_f == "None/Uninsured", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Medicaid or Other Public Insurance, n (%)' = paste0(
      sum(insurance_type_f %in% c("Medicaid", "Other Public Insurance"), na.rm = TRUE),
      " (",
      round(mean(insurance_type_f %in% c("Medicaid", "Other Public Insurance"), 
                 na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Medicare, n (%)' = paste0(
      sum(insurance_type_f == "Medicare", na.rm = TRUE),
      " (",
      round(mean(insurance_type_f == "Medicare", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Private Insurance, n (%)' = paste0(
      sum(insurance_type_f == "Private Insurance", na.rm = TRUE),
      " (",
      round(mean(insurance_type_f == "Private Insurance", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Unsure, n (%)' = paste0(
      sum(insurance_type_f == "Unsure", na.rm = TRUE),
      " (",
      round(mean(insurance_type_f == "Unsure", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Excellent General Health, n (%)' = paste0(
      sum(general_health_f == "Excellent", na.rm = TRUE),
      " (",
      round(mean(general_health_f == "Excellent", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Very Good General Health, n (%)' = paste0(
      sum(general_health_f == "Very Good", na.rm = TRUE),
      " (",
      round(mean(general_health_f == "Very Good", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Good General Health, n (%)' = paste0(
      sum(general_health_f == "Good", na.rm = TRUE),
      " (",
      round(mean(general_health_f == "Good", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Fair General Health, n (%)' = paste0(
      sum(general_health_f == "Fair", na.rm = TRUE),
      " (",
      round(mean(general_health_f == "Fair", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Poor General Health, n (%)' = paste0(
      sum(general_health_f == "Poor", na.rm = TRUE),
      " (",
      round(mean(general_health_f == "Poor", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Excellent Physical Health, n (%)' = paste0(
      sum(physical_health_f == "Excellent", na.rm = TRUE),
      " (",
      round(mean(physical_health_f == "Excellent", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Very Good Physical Health, n (%)' = paste0(
      sum(physical_health_f == "Very Good", na.rm = TRUE),
      " (",
      round(mean(physical_health_f == "Very Good", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Good Physical Health, n (%)' = paste0(
      sum(physical_health_f == "Good", na.rm = TRUE),
      " (",
      round(mean(physical_health_f == "Good", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Fair Physical Health, n (%)' = paste0(
      sum(physical_health_f == "Fair", na.rm = TRUE),
      " (",
      round(mean(physical_health_f == "Fair", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Poor Physical Health, n (%)' = paste0(
      sum(physical_health_f == "Poor", na.rm = TRUE),
      " (",
      round(mean(physical_health_f == "Poor", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Excellent Mental Health, n (%)' = paste0(
      sum(mental_health_f == "Excellent", na.rm = TRUE),
      " (",
      round(mean(mental_health_f == "Excellent", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Very Good Mental Health, n (%)' = paste0(
      sum(mental_health_f == "Very Good", na.rm = TRUE),
      " (",
      round(mean(mental_health_f == "Very Good", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Good Mental Health, n (%)' = paste0(
      sum(mental_health_f == "Good", na.rm = TRUE),
      " (",
      round(mean(mental_health_f == "Good", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Fair Mental Health, n (%)' = paste0(
      sum(mental_health_f == "Fair", na.rm = TRUE),
      " (",
      round(mean(mental_health_f == "Fair", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Poor Mental Health, n (%)' = paste0(
      sum(mental_health_f == "Poor", na.rm = TRUE),
      " (",
      round(mean(mental_health_f == "Poor", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Excellent QOL, n (%)' = paste0(
      sum(qol_f == "Excellent", na.rm = TRUE),
      " (",
      round(mean(qol_f == "Excellent", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Very Good QOL, n (%)' = paste0(
      sum(qol_f == "Very Good", na.rm = TRUE),
      " (",
      round(mean(qol_f == "Very Good", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Good QOL, n (%)' = paste0(
      sum(qol_f == "Good", na.rm = TRUE),
      " (",
      round(mean(qol_f == "Good", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Fair QOL, n (%)' = paste0(
      sum(qol_f == "Fair", na.rm = TRUE),
      " (",
      round(mean(qol_f == "Fair", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Poor QOL, n (%)' = paste0(
      sum(qol_f == "Poor", na.rm = TRUE),
      " (",
      round(mean(qol_f == "Poor", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'BMI, mean (SD)' = paste0(
      round(mean(bmi_measured, na.rm = TRUE), 1),
      " (",
      round(sd(bmi_measured, na.rm = TRUE), 1),
      ")"
    ),
    
    'High Startback Risk, n (%)' = paste0(
      sum(startback_risk_label_f == "High Risk", na.rm = TRUE),
      " (",
      round(mean(startback_risk_label_f == "High Risk", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Medium Startback Risk, n (%)' = paste0(
      sum(startback_risk_label_f == "Medium Risk", na.rm = TRUE),
      " (",
      round(mean(startback_risk_label_f == "Medium Risk", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Low Startback Risk, n (%)' = paste0(
      sum(startback_risk_label_f == "Low Risk", na.rm = TRUE),
      " (",
      round(mean(startback_risk_label_f == "Low Risk", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Pain Intensity, 0-100, mean (SD)' = paste0(
      round(mean(lbp_vas_current, na.rm = TRUE), 1),
      " (",
      round(sd(lbp_vas_current, na.rm = TRUE), 1),
      ")"
    ),

    'Physical Activity, min/day, mean (SD)' = paste0(
      round(mean(physical_activity_score, na.rm = TRUE) / 7, 1),
      " (",
      round(sd(physical_activity_score, na.rm = TRUE) / 7, 1),
      ")"
    ),

    'High Social Health 1, n (%)' = paste0(
      sum(social_health_d == "Good", na.rm = TRUE),
      " (",
      round(mean(social_health_d == "Good", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Low Social Isolation 1, n (%)' = paste0(
      sum(isolation_cat_d == "Low", na.rm = TRUE),
      " (",
      round(mean(isolation_cat_d == "Low", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'High Social Activities 1, n (%)' = paste0(
      sum(social_activities_d == "High", na.rm = TRUE),
      " (",
      round(mean(social_activities_d == "High", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'High Social Relationships 1, n (%)' = paste0(
      sum(social_relationships_d == "High", na.rm = TRUE),
      " (",
      round(mean(social_relationships_d == "High", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'High Social Health 2, n (%)' = paste0(
      sum(social_health2_d == "Good", na.rm = TRUE),
      " (",
      round(mean(social_health2_d == "Good", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'Low Social Isolation 2, n (%)' = paste0(
      sum(isolation_cat2_d == "Low", na.rm = TRUE),
      " (",
      round(mean(isolation_cat2_d == "Low", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'High Social Activities 2, n (%)' = paste0(
      sum(social_activities2_d == "High", na.rm = TRUE),
      " (",
      round(mean(social_activities2_d == "High", na.rm = TRUE) * 100, 1),
      ")"
    ),
    
    'High Social Relationships 2, n (%)' = paste0(
      sum(social_relationships2_d == "High", na.rm = TRUE),
      " (",
      round(mean(social_relationships2_d == "High", na.rm = TRUE) * 100, 1),
      ")"
    )
    
  ) 


table_one |> kable()

write.csv(table_one, here("Table_One.csv"))
