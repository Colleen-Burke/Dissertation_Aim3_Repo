library(here)
library(dplyr)

analysis_df <- readRDS(here("Aim3_Data_with_PCA.rds"))

# ---------- Social Health Factor  Models ----------
lm_sh1 <- lm(PC1 ~ social_health_num, data = analysis_df)
summary(lm_sh1)
confint(lm_sh1)

lm_sh2 <- lm(PC2 ~ social_health_num, data = analysis_df)
summary(lm_sh2)
confint(lm_sh2)


# ---------- Social Health Factor Dichotomous Models ----------
analysis_df <- analysis_df |> 
  mutate(social_health_d = case_when(
    social_health_num %in% c(2, 3, 4, 5, 6, 7, 8, 9, 10) ~ "Poor",   
    social_health_num %in% c(11, 12, 13, 14) ~ "Good",
    TRUE ~ NA_character_
  ))

lm_sh1_d <- lm(PC1 ~ social_health_d, data = analysis_df)
summary(lm_sh1_d)
confint(lm_sh1_d)

lm_sh2_d <- lm(PC2 ~ social_health_d, data = analysis_df)
summary(lm_sh2_d)
confint(lm_sh2_d)



# ---------- Social Relationships Ordinal Models ----------
lm_sr1 <- lm(PC1 ~ social_relationships_ordinal, data = analysis_df)
summary(lm_sr1)
confint(lm_sr1)

lm_sr2 <- lm(PC2 ~ social_relationships_ordinal, data = analysis_df)
summary(lm_sr2)
confint(lm_sr2)


# ---------- Social Relationships Nominal Models ----------
lm_2sr1 <- lm(PC1 ~ social_relationships_nominal, data = analysis_df)
summary(lm_2sr1)
confint(lm_2sr1)

lm_2sr2 <- lm(PC2 ~ social_relationships_nominal, data = analysis_df)
summary(lm_2sr2)
confint(lm_2sr2)

# ---------- Social Activity Ordinal Models ----------
lm_sa1 <- lm(PC1 ~ social_activities_ordinal, data = analysis_df)
summary(lm_sa1)
confint(lm_sa1)

lm_sa2 <- lm(PC2 ~ social_activities_ordinal, data = analysis_df)
summary(lm_sa2)
confint(lm_sa2)


# ---------- Social Activity Nominal Models ----------
lm_2sa1 <- lm(PC1 ~ social_activities_nominal, data = analysis_df)
summary(lm_2sa1)
confint(lm_2sa1)

lm_2sa2 <- lm(PC2 ~ social_activities_nominal, data = analysis_df)
summary(lm_2sa2)
confint(lm_2sa2)



# ---------- Composite Social Function Models ----------
lm_socialfunc1 <- lm(PC1 ~ social_function_num, data = analysis_df)
summary(lm_socialfunc1)
confint(lm_socialfunc1)

lm_socialfunc2 <- lm(PC2 ~ social_function_num, data = analysis_df)
summary(lm_socialfunc2)
confint(lm_socialfunc2)


# ---------- Composite Social Function Dichotomized Models ----------
library(dplyr)
analysis_df <- analysis_df |> 
  mutate(social_function_d = case_when(
    social_function_num %in% c(2, 3, 4, 5, 6, 7) ~ "Poor",   
    social_function_num %in% c(8, 9, 10) ~ "Good",
    TRUE ~ NA_character_
  ))

lm_socialfunc1_d <- lm(PC1 ~ social_function_d, data = analysis_df)
summary(lm_socialfunc1_d)
confint(lm_socialfunc1_d)

lm_socialfunc2_d <- lm(PC2 ~ social_function_d, data = analysis_df)
summary(lm_socialfunc2_d)
confint(lm_socialfunc2_d)



# ---------- Social Isolation Score Models ----------
lm_iso1 <- lm(PC1 ~ isolation_score, data = analysis_df)
summary(lm_iso1)
confint(lm_iso1)

lm_iso2 <- lm(PC2 ~ isolation_score, data = analysis_df)
summary(lm_iso2)
confint(lm_iso2)







# ---------- Social Isolation Ordinal Models ----------
lm_si1 <- lm(PC1 ~ isolation_cat_ordinal, data = analysis_df)
summary(lm_si1)
confint(lm_si1)

lm_si2 <- lm(PC2 ~ isolation_cat_ordinal, data = analysis_df)
summary(lm_si2)
confint(lm_si2)


# ---------- Social Isolation Nominal Models ----------
lm_2si1 <- lm(PC1 ~ isolation_cat_nominal, data = analysis_df)
summary(lm_2si1)
confint(lm_2si1)


lm_2si2 <- lm(PC2 ~ isolation_cat_nominal, data = analysis_df)
summary(lm_2si2)
confint(lm_2si2)


# ---------- Social Isolation Item (Communicate) Models ----------
lm_siic1 <- lm(PC1 ~ isolation_item_communicate, data = analysis_df)
summary(lm_siic1)
confint(lm_siic1)

lm_siic2 <- lm(PC2 ~ isolation_item_communicate, data = analysis_df)
summary(lm_siic2)
confint(lm_siic2)


# ---------- Social Isolation Item (Visit) Models ----------
lm_siiv1 <- lm(PC1 ~ isolation_item_visit, data = analysis_df)
summary(lm_siiv1)
confint(lm_siiv1)

lm_siiv2 <- lm(PC2 ~ isolation_item_visit, data = analysis_df)
summary(lm_siiv2)
confint(lm_siiv2)



# ---------- Social Relationships Dichotomized Models ----------
analysis_df <- analysis_df |>
  mutate(
    social_relationships_d = factor(social_relationships_d,
                                 levels = c("High", "Low"))
  )

lm_srd1 <- lm(PC1 ~ social_relationships_d, data = analysis_df)
summary(lm_srd1)
confint(lm_srd1)

lm_srd2 <- lm(PC2 ~ social_relationships_d, data = analysis_df)
summary(lm_srd2)
confint(lm_srd2)


# ---------- Social Relationships New Dichotomized Models ----------
analysis_df <- analysis_df |>
  mutate(
    social_relationships2_d = factor(social_relationships2_d,
                                    levels = c("High", "Low"))
  )

lm_srd1_new <- lm(PC1 ~ social_relationships2_d, data = analysis_df)
summary(lm_srd1_new)
confint(lm_srd1_new)

lm_srd2_new <- lm(PC2 ~ social_relationships2_d, data = analysis_df)
summary(lm_srd2_new)
confint(lm_srd2_new)


# ---------- Social Activities Dichotomized Models ----------
analysis_df <- analysis_df |>
  mutate(
    social_activities_d = factor(social_activities_d,
                                 levels = c("High", "Low"))
  )


lm_sad1 <- lm(PC1 ~ social_activities_d, data = analysis_df)
summary(lm_sad1)
confint(lm_sad1)

lm_sad2 <- lm(PC2 ~ social_activities_d, data = analysis_df)
summary(lm_sad2)
confint(lm_sad2)


# ---------- Social Activities New Dichotomized Models ----------
analysis_df <- analysis_df |>
  mutate(
    social_activities2_d = factor(social_activities2_d,
                                 levels = c("High", "Low"))
  )


lm_sad1_new <- lm(PC1 ~ social_activities2_d, data = analysis_df)
summary(lm_sad1_new)
confint(lm_sad1_new)

lm_sad2_new <- lm(PC2 ~ social_activities2_d, data = analysis_df)
summary(lm_sad2_new)
confint(lm_sad2_new)



# ---------- Social Isolation Dichotomized Models ----------
lm_sid1 <- lm(PC1 ~ isolation_cat_d, data = analysis_df)
summary(lm_sid1)
confint(lm_sid1)

lm_sid2 <- lm(PC2 ~ isolation_cat_d, data = analysis_df)
summary(lm_sid2)
confint(lm_sid2)


# ---------- Social Isolation New Dichotomized Models ----------
lm_sid1_new <- lm(PC1 ~ isolation_cat2_d, data = analysis_df)
summary(lm_sid1_new)
confint(lm_sid1_new)

lm_sid2_new <- lm(PC2 ~ isolation_cat2_d, data = analysis_df)
summary(lm_sid2_new)
confint(lm_sid2_new)



# ---------- Social Isolation Item (Communicate) Dichotomized Models ----------
lm_siicd1 <- lm(PC1 ~ isolation_item_communicate_d, data = analysis_df)
summary(lm_siicd1)
confint(lm_siicd1)

lm_siicd2 <- lm(PC2 ~ isolation_item_communicate_d, data = analysis_df)
summary(lm_siicd2)
confint(lm_siicd2)


# ---------- Social Isolation Item (Visit) Dichotomized Models ----------
lm_siivd1 <- lm(PC1 ~ isolation_item_visit_d, data = analysis_df)
summary(lm_siivd1)
confint(lm_siivd1)

lm_siivd2 <- lm(PC2 ~ isolation_item_visit_d, data = analysis_df)
summary(lm_siivd2)
confint(lm_siivd2)



# ---------- Race models ----------
#lm_race1 <- lm(PC1 ~ race_f, data = analysis_df)
#summary(lm_race1)

#lm_race2 <- lm(PC2 ~ race_f, data = analysis_df)
#summary(lm_race2)


# ---------- Race Dichotomized -----------
#lm_raced1 <- lm(PC1 ~ race_dichotomized, data = analysis_df)
#summary(lm_raced1)

#lm_raced2 <- lm(PC2 ~ race_dichotomized, data = analysis_df)
#summary(lm_raced2)






saveRDS(analysis_df, "Aim3_Data_full.rds")





