library(here)
library(dplyr)

analysis_df <- readRDS(here("Aim3_Data_full.rds"))


# ---------- Social Relationships Adjusted Models ----------
analysis_df <- analysis_df |>
  mutate(
    social_relationships2_d = factor(social_relationships2_d,
                                     levels = c("High", "Low"))
  )

lm_sr1_adj <- lm(PC1 ~ social_relationships2_d + 
                    bmi_measured + age + sex + race_dichotomized + 
                    lbp_vas_current + mental_health, 
                  data = analysis_df)
summary(lm_sr1_adj)
confint(lm_sr1_adj)

lm_sr2_adj <- lm(PC2 ~ social_relationships2_d + 
                    bmi_measured + age + sex + race_dichotomized + 
                    lbp_vas_current + mental_health, 
                  data = analysis_df)
summary(lm_sr2_adj)
confint(lm_sr2_adj)


# ---------- Social Activities Adjusted Models ----------
analysis_df <- analysis_df |>
  mutate(
    social_activities2_d = factor(social_activities2_d,
                                  levels = c("High", "Low"))
  )

lm_sa1_adj <- lm(PC1 ~ social_activities2_d + 
                   bmi_measured + age + sex + race_dichotomized + 
                   lbp_vas_current + mental_health, 
                 data = analysis_df)
summary(lm_sa1_adj)
confint(lm_sa1_adj)

lm_sa2_adj <- lm(PC2 ~ social_activities2_d + 
                   bmi_measured + age + sex + race_dichotomized + 
                   lbp_vas_current + mental_health, 
                 data = analysis_df)
summary(lm_sa2_adj)
confint(lm_sa2_adj)


# ---------- Social Isolation Adjusted Models ----------

lm_si1_adj <- lm(PC1 ~ isolation_cat_d + 
                   bmi_measured + age + sex + race_dichotomized + 
                   lbp_vas_current + mental_health,
                 data = analysis_df)
summary(lm_si1_adj)
confint(lm_si1_adj)

lm_si2_adj <- lm(PC2 ~ isolation_cat_d + 
                   bmi_measured + age + sex + race_dichotomized + 
                   lbp_vas_current + mental_health, 
                 data = analysis_df)
summary(lm_si2_adj)
confint(lm_si2_adj)


# ---------- Social Health Adjusted Models ----------
analysis_df <- analysis_df |> 
  mutate(
    social_health2_d = case_when(
      social_health_num %in% c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11) ~ "Poor",
      social_health_num %in% c(12, 13, 14) ~ "Good",
      TRUE ~ NA_character_
    ),
    # set reference (optional): "Good" as referent
    social_health2_d = factor(social_health2_d, levels = c("Good", "Poor"))
  )

lm_sh1_adj <- lm(PC1 ~ social_health2_d + 
                   bmi_measured + age + sex + race_dichotomized + 
                   lbp_vas_current + mental_health,
                 data = analysis_df)
summary(lm_sh1_adj)
confint(lm_sh1_adj)

lm_sh2_adj <- lm(PC2 ~ social_health2_d + 
                   bmi_measured + age + sex + race_dichotomized + 
                   lbp_vas_current + mental_health, 
                 data = analysis_df)
summary(lm_sh2_adj)
confint(lm_sh2_adj)
