library(here)

analysis_df <- readRDS(here("Aim3_Data_full.rds"))

# Split the data by race
analysis_df_race1 <- analysis_df |> filter(race_dichotomized == 0)
analysis_df_race2 <- analysis_df |> filter(race_dichotomized == 1)


#--- Social Relationships (New Dichot.) and PC1 Stratified by Race ---------------------------------------
lm_srd1_race1 <- lm(PC1 ~ social_relationships2_d, data = analysis_df_race1)
lm_srd1_race2 <- lm(PC1 ~ social_relationships2_d, data = analysis_df_race2)

summary(lm_srd1_race1)
confint(lm_srd1_race1)

summary(lm_srd1_race2)
confint(lm_srd1_race2)



#--- Social Relationships (New Dichot.) and PC2 Stratified by Race ---------------------------------------
lm_srd2_race1 <- lm(PC2 ~ social_relationships2_d, data = analysis_df_race1)
lm_srd2_race2 <- lm(PC2 ~ social_relationships2_d, data = analysis_df_race2)

summary(lm_srd2_race1)
confint(lm_srd2_race1)

summary(lm_srd2_race2)
confint(lm_srd2_race2)



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







#--- Social Composite with PC1 Stratified by race ---
lm_sf1_race1 <- lm(PC1 ~ social_factor, data = analysis_df_race1)
lm_sf1_race2 <- lm(PC1 ~ social_factor, data = analysis_df_race2)

summary(lm_sf1_race1)
confint(lm_sf1_race1)

summary(lm_sf1_race2)
confint(lm_sf1_race2)


#--- Social Composite with PC2 Stratified by race ---
lm_sf2_race1 <- lm(PC2 ~ social_factor, data = analysis_df_race1)
lm_sf2_race2 <- lm(PC2 ~ social_factor, data = analysis_df_race2)

summary(lm_sf2_race1)
confint(lm_sf2_race1)

summary(lm_sf2_race2)
confint(lm_sf2_race2)




#--- Social Composite Dichotomous with PC1 Stratified by race ---
lm_sfd1_race1 <- lm(PC1 ~ social_factor_d, data = analysis_df_race1)
lm_sfd1_race2 <- lm(PC1 ~ social_factor_d, data = analysis_df_race2)

summary(lm_sfd1_race1)
confint(lm_sfd1_race1)

summary(lm_sfd1_race2)
confint(lm_sfd1_race2)



#--- Social Composite Dichotomous with PC2 Stratified by race ---
lm_sfd2_race1 <- lm(PC2 ~ social_factor_d, data = analysis_df_race1)
lm_sfd2_race2 <- lm(PC2 ~ social_factor_d, data = analysis_df_race2)

summary(lm_sfd2_race1)
confint(lm_sfd2_race1)

summary(lm_sfd2_race2)
confint(lm_sfd2_race2)
