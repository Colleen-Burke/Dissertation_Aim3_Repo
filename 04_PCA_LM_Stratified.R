library(here)

analysis_df <- readRDS(here("Aim3_Data_full.rds"))


#--- Data Cleaning -------------------------------------------------------------
# Split the data by race
analysis_df_race1 <- analysis_df |> filter(race_dichotomized == 0)
analysis_df_race2 <- analysis_df |> filter(race_dichotomized == 1)

# Make High Social Relationships the Referent
analysis_df_race1 <- analysis_df_race1 |>
  mutate(
    social_relationships_d = factor(social_relationships_d,
                                     levels = c("High", "Low"))
  )

analysis_df_race2 <- analysis_df_race2 |>
  mutate(
    social_relationships_d = factor(social_relationships_d,
                                     levels = c("High", "Low"))
  )

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

# Make High Social Activities the Referent
analysis_df_race1 <- analysis_df_race1 |>
  mutate(
    social_activities_d = factor(social_activities_d,
                                     levels = c("High", "Low"))
  )

analysis_df_race2 <- analysis_df_race2 |>
  mutate(
    social_activities_d = factor(social_activities_d,
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



#--- Social Relationships (Dichot.) and PC1 Stratified by Race ---------------------------------------
lm_srd1_race1 <- lm(PC1 ~ social_relationships_d, data = analysis_df_race1)
lm_srd1_race2 <- lm(PC1 ~ social_relationships_d, data = analysis_df_race2)

summary(lm_srd1_race1)
confint(lm_srd1_race1)

summary(lm_srd1_race2)
confint(lm_srd1_race2)



#--- Social Relationships (Dichot.) and PC2 Stratified by Race ---------------------------------------
lm_srd2_race1 <- lm(PC2 ~ social_relationships_d, data = analysis_df_race1)
lm_srd2_race2 <- lm(PC2 ~ social_relationships_d, data = analysis_df_race2)

summary(lm_srd2_race1)
confint(lm_srd2_race1)

summary(lm_srd2_race2)
confint(lm_srd2_race2)




#--- Social Relationships (New Dichot.) and PC1 Stratified by Race ---------------------------------------
lm_srd1_new_race1 <- lm(PC1 ~ social_relationships2_d, data = analysis_df_race1)
lm_srd1_new_race2 <- lm(PC1 ~ social_relationships2_d, data = analysis_df_race2)

summary(lm_srd1_new_race1)
confint(lm_srd1_new_race1)

summary(lm_srd1_new_race2)
confint(lm_srd1_new_race2)



#--- Social Relationships (New Dichot.) and PC2 Stratified by Race ---------------------------------------
lm_srd2_new_race1 <- lm(PC2 ~ social_relationships2_d, data = analysis_df_race1)
lm_srd2_new_race2 <- lm(PC2 ~ social_relationships2_d, data = analysis_df_race2)

summary(lm_srd2_new_race1)
confint(lm_srd2_new_race1)

summary(lm_srd2_new_race2)
confint(lm_srd2_new_race2)



#--- Social Activities Dichotomized and PC1 Stratified by Race ---------------------------------------
lm_sad1_race1 <- lm(PC1 ~ social_activities_d, data = analysis_df_race1)
lm_sad1_race2 <- lm(PC1 ~ social_activities_d, data = analysis_df_race2)

summary(lm_sad1_race1)
confint(lm_sad1_race1)

summary(lm_sad1_race2)
confint(lm_sad1_race2)



#--- Social Activities Dichotomized and PC2 Stratified by Race ---------------------------------------
lm_sad2_race1 <- lm(PC2 ~ social_activities_d, data = analysis_df_race1)
lm_sad2_race2 <- lm(PC2 ~ social_activities_d, data = analysis_df_race2)

summary(lm_sad2_race1)
confint(lm_sad2_race1)

summary(lm_sad2_race2)
confint(lm_sad2_race2)



#--- Social Activities (New Dichotomized) and PC1 Stratified by Race ---------------------------------------
lm_sad1_new_race1 <- lm(PC1 ~ social_activities2_d, data = analysis_df_race1)
lm_sad1_new_race2 <- lm(PC1 ~ social_activities2_d, data = analysis_df_race2)

summary(lm_sad1_new_race1)
confint(lm_sad1_new_race1)

summary(lm_sad1_new_race2)
confint(lm_sad1_new_race2)



#--- Social Activities (New Dichotomized) and PC2 Stratified by Race ---------------------------------------
lm_sad2_new_race1 <- lm(PC2 ~ social_activities2_d, data = analysis_df_race1)
lm_sad2_new_race2 <- lm(PC2 ~ social_activities2_d, data = analysis_df_race2)

summary(lm_sad2_new_race1)
confint(lm_sad2_new_race1)

summary(lm_sad2_new_race2)
confint(lm_sad2_new_race2)




#--- Social Health with PC1 Stratified by race ---
lm_sf1_race1 <- lm(PC1 ~ social_function_num, data = analysis_df_race1)
lm_sf1_race2 <- lm(PC1 ~ social_function_num, data = analysis_df_race2)

summary(lm_sf1_race1)
confint(lm_sf1_race1)

summary(lm_sf1_race2)
confint(lm_sf1_race2)


#--- Social Health with PC2 Stratified by race ---
lm_sf2_race1 <- lm(PC2 ~ social_function_num, data = analysis_df_race1)
lm_sf2_race2 <- lm(PC2 ~ social_function_num, data = analysis_df_race2)

summary(lm_sf2_race1)
confint(lm_sf2_race1)

summary(lm_sf2_race2)
confint(lm_sf2_race2)




#--- Social Health Factor Dichotomous with PC1 Stratified by race ---
lm_sfd1_race1 <- lm(PC1 ~ social_function_d, data = analysis_df_race1)
lm_sfd1_race2 <- lm(PC1 ~ social_function_d, data = analysis_df_race2)

summary(lm_sfd1_race1)
confint(lm_sfd1_race1)

summary(lm_sfd1_race2)
confint(lm_sfd1_race2)



#--- Social Function Dichotomous with PC2 Stratified by race ---
lm_sfd2_race1 <- lm(PC2 ~ social_function_d, data = analysis_df_race1)
lm_sfd2_race2 <- lm(PC2 ~ social_function_d, data = analysis_df_race2)

summary(lm_sfd2_race1)
confint(lm_sfd2_race1)

summary(lm_sfd2_race2)
confint(lm_sfd2_race2)







#--- Social Isolation and PC1 Stratified by Race ---------------------------------------
lm_sid1_race1 <- lm(PC1 ~ isolation_cat_d, data = analysis_df_race1)
lm_sid1_race2 <- lm(PC1 ~ isolation_cat_d, data = analysis_df_race2)

summary(lm_sid1_race1)
confint(lm_sid1_race1)

summary(lm_sid1_race2)
confint(lm_sid1_race2)


#--- Social Isolation and PC2 Stratified by Race ---------------------------------------
lm_sid2_race1 <- lm(PC2 ~ isolation_cat_d, data = analysis_df_race1)
lm_sid2_race2 <- lm(PC2 ~ isolation_cat_d, data = analysis_df_race2)

summary(lm_sid2_race1)
confint(lm_sid2_race1)

summary(lm_sid2_race2)
confint(lm_sid2_race2)



#--- Social Isolation (New) and PC1 Stratified by Race ---------------------------------------
lm_sid1_new_race1 <- lm(PC1 ~ isolation_cat2_d, data = analysis_df_race1)
lm_sid1_new_race2 <- lm(PC1 ~ isolation_cat2_d, data = analysis_df_race2)

summary(lm_sid1_new_race1)
confint(lm_sid1_new_race1)

summary(lm_sid1_new_race2)
confint(lm_sid1_new_race2)


#--- Social Isolation (New) and PC2 Stratified by Race ---------------------------------------
lm_sid2_new_race1 <- lm(PC2 ~ isolation_cat2_d, data = analysis_df_race1)
lm_sid2_new_race2 <- lm(PC2 ~ isolation_cat2_d, data = analysis_df_race2)

summary(lm_sid2_new_race1)
confint(lm_sid2_new_race1)

summary(lm_sid2_new_race2)
confint(lm_sid2_new_race2)





#--- Social Health with PC1 Stratified by race ---
lm_sh1_race1 <- lm(PC1 ~ social_health_num, data = analysis_df_race1)
lm_sh1_race2 <- lm(PC1 ~ social_health_num, data = analysis_df_race2)

summary(lm_sh1_race1)
confint(lm_sh1_race1)

summary(lm_sh1_race2)
confint(lm_sh1_race2)


#--- Social Health with PC2 Stratified by race ---
lm_sh2_race1 <- lm(PC2 ~ social_health_num, data = analysis_df_race1)
lm_sh2_race2 <- lm(PC2 ~ social_health_num, data = analysis_df_race2)

summary(lm_sh2_race1)
confint(lm_sh2_race1)

summary(lm_sh2_race2)
confint(lm_sh2_race2)




#--- Social Health Factor Dichotomous with PC1 Stratified by race ---
lm_shd1_race1 <- lm(PC1 ~ social_health_d, data = analysis_df_race1)
lm_shd1_race2 <- lm(PC1 ~ social_health_d, data = analysis_df_race2)

summary(lm_shd1_race1)
confint(lm_shd1_race1)

summary(lm_shd1_race2)
confint(lm_shd1_race2)



#--- Social Health Factor Dichotomous with PC2 Stratified by race ---
lm_shd2_race1 <- lm(PC2 ~ social_health_d, data = analysis_df_race1)
lm_shd2_race2 <- lm(PC2 ~ social_health_d, data = analysis_df_race2)

summary(lm_shd2_race1)
confint(lm_shd2_race1)

summary(lm_shd2_race2)
confint(lm_shd2_race2)
