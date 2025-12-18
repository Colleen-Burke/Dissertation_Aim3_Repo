library(here)

analysis_df <- readRDS(here("Aim3_Data_full.rds"))


#--- Data Cleaning -------------------------------------------------------------
# Split the data by race
analysis_df_race1 <- analysis_df |> filter(race_dichotomized == 0)
analysis_df_race2 <- analysis_df |> filter(race_dichotomized == 1)

# Make High Social Relationships (New) the Referent
analysis_df_race1 <- analysis_df_race1 |>
  mutate(
    social_relationships2_d = factor(social_relationships2_d,
                                     levels = c("High", "Low"))
  )

analysis_df_race2 <- analysis_df_race2 |>
  mutate(
    social_relationships2_d = factor(social_relationships2_d,
                                     levels = c("High", "Low"))
  )

# Make High Social Activities (New) the Referent
analysis_df_race1 <- analysis_df_race1 |>
  mutate(
    social_activities2_d = factor(social_activities2_d,
                                  levels = c("High", "Low"))
  )

analysis_df_race2 <- analysis_df_race2 |>
  mutate(
    social_activities2_d = factor(social_activities2_d,
                                  levels = c("High", "Low"))
  )

# Make New High Social Health the Referent
analysis_df_race1 <- analysis_df_race1 |>
  mutate(
    social_health2_d = factor(social_health2_d,
                              levels = c("High", "Low"))
  )

analysis_df_race2 <- analysis_df_race2 |>
  mutate(
    social_health2_d = factor(social_health2_d,
                              levels = c("High", "Low"))
  )



#-------- Social Relationships & PC1 Adjusted + Stratified ---------------------
lm_sr1_adj_race1 <- lm(
  PC1 ~ social_relationships2_d + age + sex +
    bmi_measured + lbp_vas_current + mental_health,
  data = analysis_df_race1
)
summary(lm_sr1_adj_race1)
confint(lm_sr1_adj_race1)

lm_sr1_adj_race2 <- lm(
  PC1 ~ social_relationships2_d + age + sex +
    bmi_measured + lbp_vas_current + mental_health,
  data = analysis_df_race2
)
summary(lm_sr1_adj_race2)
confint(lm_sr1_adj_race2)


#-------- Social Relationships & PC2 Adjusted + Stratified ---------------------
lm_sr2_adj_race1 <- lm(
  PC2 ~ social_relationships2_d + age + sex  +
    bmi_measured + lbp_vas_current + mental_health,
  data = analysis_df_race1
)
summary(lm_sr2_adj_race1)
confint(lm_sr2_adj_race1)

lm_sr2_adj_race2 <- lm(
  PC2 ~ social_relationships2_d + age + sex + 
    bmi_measured + lbp_vas_current + mental_health,
  data = analysis_df_race2
)
summary(lm_sr2_adj_race2)
confint(lm_sr2_adj_race2)


#-------- Social Activities & PC1 Adjusted + Stratified ------------------------
lm_sa1_adj_race1 <- lm(
  PC1 ~ social_activities2_d + age + sex + 
    bmi_measured + lbp_vas_current + mental_health,
  data = analysis_df_race1
)
summary(lm_sa1_adj_race1)
confint(lm_sa1_adj_race1)

lm_sa1_adj_race2 <- lm(
  PC1 ~ social_activities2_d + age + sex + 
    bmi_measured + lbp_vas_current + mental_health,
  data = analysis_df_race2
)
summary(lm_sa1_adj_race2)
confint(lm_sa1_adj_race2)


#-------- Social Activities & PC2 Adjusted + Stratified ------------------------
lm_sa2_adj_race1 <- lm(
  PC2 ~ social_activities2_d + age + sex + 
    bmi_measured + lbp_vas_current + mental_health,
  data = analysis_df_race1
)
summary(lm_sa2_adj_race1)
confint(lm_sa2_adj_race1)

lm_sa2_adj_race2 <- lm(
  PC2 ~ social_activities2_d + age + sex + 
    bmi_measured + lbp_vas_current + mental_health,
  data = analysis_df_race2
)
summary(lm_sa2_adj_race2)
confint(lm_sa2_adj_race2)


#-------- Social Isolation & PC1 Adjusted + Stratified -------------------------
lm_si1_adj_race1 <- lm(
  PC1 ~ isolation_cat_d + age + sex +
    bmi_measured + lbp_vas_current + mental_health,
  data = analysis_df_race1
)
summary(lm_si1_adj_race1)
confint(lm_si1_adj_race1)

lm_si1_adj_race2 <- lm(
  PC1 ~ isolation_cat_d + age + sex + 
    bmi_measured + lbp_vas_current + mental_health,
  data = analysis_df_race2
)
summary(lm_si1_adj_race2)
confint(lm_si1_adj_race2)


#-------- Social Isolation & PC2 Adjusted + Stratified -------------------------
lm_si2_adj_race1 <- lm(
  PC2 ~ isolation_cat_d + age + sex + 
    bmi_measured + lbp_vas_current + mental_health,
  data = analysis_df_race1
)
summary(lm_si2_adj_race1)
confint(lm_si2_adj_race1)

lm_si2_adj_race2 <- lm(
  PC2 ~ isolation_cat_d + age + sex + 
    bmi_measured + lbp_vas_current + mental_health,
  data = analysis_df_race2
)
summary(lm_si2_adj_race2)
confint(lm_si2_adj_race2)


#-------- Social Health & PC1 Adjusted + Stratified ----------------------------
lm_sh1_adj_race1 <- lm(
  PC1 ~ social_health2_d + age + sex + 
    bmi_measured + lbp_vas_current + mental_health,
  data = analysis_df_race1
)
summary(lm_sh1_adj_race1)
confint(lm_sh1_adj_race1)

lm_sh1_adj_race2 <- lm(
  PC1 ~ social_health2_d + age + sex + 
    bmi_measured + lbp_vas_current + mental_health,
  data = analysis_df_race2
)
summary(lm_sh1_adj_race2)
confint(lm_sh1_adj_race2)


#-------- Social Health & PC2 Adjusted + Stratified ----------------------------
lm_sh2_adj_race1 <- lm(
  PC2 ~ social_health2_d + age + sex + 
    bmi_measured + lbp_vas_current + mental_health,
  data = analysis_df_race1
)
summary(lm_sh2_adj_race1)
confint(lm_sh2_adj_race1)

lm_sh2_adj_race2 <- lm(
  PC2 ~ social_health2_d + age + sex + 
    bmi_measured + lbp_vas_current + mental_health,
  data = analysis_df_race2
)
summary(lm_sh2_adj_race2)
confint(lm_sh2_adj_race2)
