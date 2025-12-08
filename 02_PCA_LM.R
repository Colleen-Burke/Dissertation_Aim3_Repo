library(here)

analysis_df <- readRDS(here("Aim3_Data_with_PCA.rds"))

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

lm_sa2 <- lm(PC2 ~ social_activities_ordinal, data = analysis_df)
summary(lm_sa2)


# ---------- Social Activity Nominal Models ----------
lm_2sa1 <- lm(PC1 ~ social_activities_nominal, data = analysis_df)
summary(lm_2sa1)

lm_2sa2 <- lm(PC2 ~ social_activities_nominal, data = analysis_df)
summary(lm_2sa2)


# ---------- Social Isolation Ordinal Models ----------
lm_si1 <- lm(PC1 ~ isolation_cat_ordinal, data = analysis_df)
summary(lm_si1)

lm_si2 <- lm(PC2 ~ isolation_cat_ordinal, data = analysis_df)
summary(lm_si2)


# ---------- Social Isolation Nominal Models ----------
lm_2si1 <- lm(PC1 ~ isolation_cat_nominal, data = analysis_df)
summary(lm_2si1)


lm_2si2 <- lm(PC2 ~ isolation_cat_nominal, data = analysis_df)
summary(lm_2si2)


# ---------- Social Isolation Item (Communicate) Models ----------
lm_siic1 <- lm(PC1 ~ isolation_item_communicate, data = analysis_df)
summary(lm_siic1)

lm_siic2 <- lm(PC2 ~ isolation_item_communicate, data = analysis_df)
summary(lm_siic2)


# ---------- Social Isolation Item (Visit) Models ----------
lm_siiv1 <- lm(PC1 ~ isolation_item_visit, data = analysis_df)
summary(lm_siiv1)

lm_siiv2 <- lm(PC2 ~ isolation_item_visit, data = analysis_df)
summary(lm_siiv2)



# ---------- Social Relationships Dichotomized Models ----------
analysis_df <- analysis_df |>
  mutate(
    social_relationships_d = factor(social_relationships_d,
                                 levels = c("High", "Low"))
  )

lm_srd1 <- lm(PC1 ~ social_relationships_d, data = analysis_df)
summary(lm_srd1)

lm_srd2 <- lm(PC2 ~ social_relationships_d, data = analysis_df)
summary(lm_srd2)

# ---------- Social Activities Dichotomized Models ----------
analysis_df <- analysis_df |>
  mutate(
    social_activities_d = factor(social_activities_d,
                                 levels = c("High", "Low"))
  )


lm_sad1 <- lm(PC1 ~ social_activities_d, data = analysis_df)
summary(lm_sad1)

lm_sad2 <- lm(PC2 ~ social_activities_d, data = analysis_df)
summary(lm_sad2)

# ---------- Social Isolation Dichotomized Models ----------
lm_sid1 <- lm(PC1 ~ isolation_cat_d, data = analysis_df)
summary(lm_sid1)
confint(lm_sid1)

lm_sid2 <- lm(PC2 ~ isolation_cat_d, data = analysis_df)
summary(lm_sid2)
confint(lm_sid2)

# ---------- Social Isolation Item (Communicate) Dichotomized Models ----------
lm_siicd1 <- lm(PC1 ~ isolation_item_communicate_d, data = analysis_df)
summary(lm_siicd1)

lm_siicd2 <- lm(PC2 ~ isolation_item_communicate_d, data = analysis_df)
summary(lm_siicd2)


# ---------- Social Isolation Item (Visit) Dichotomized Models ----------
lm_siivd1 <- lm(PC1 ~ isolation_item_visit_d, data = analysis_df)
summary(lm_siivd1)

lm_siivd2 <- lm(PC2 ~ isolation_item_visit_d, data = analysis_df)
summary(lm_siivd2)



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





#--- Social Isolation Stratified by Race ---
# Split the data by race
analysis_df_race1 <- analysis_df |> filter(race_dichotomized == 0)
analysis_df_race2 <- analysis_df |> filter(race_dichotomized == 1)

# Dichotomized models
lm_sid1_race1 <- lm(PC1 ~ isolation_cat_d, data = analysis_df_race1)
lm_sid1_race2 <- lm(PC1 ~ isolation_cat_d, data = analysis_df_race2)

summary(lm_sid1_race1)
summary(lm_sid1_race2)


#Ordinal models
lm_sif1_race1 <- lm(PC1 ~ isolation_cat_ordinal, data = analysis_df_race1)
lm_sif1_race2 <- lm(PC1 ~ isolation_cat_ordinal, data = analysis_df_race2) 

summary(lm_sif1_race1)
summary(lm_sif1_race2)

#Nominal models
lm_2sif1_race1 <- lm(PC1 ~ isolation_cat_nominal, data = analysis_df_race1)
lm_2sif1_race2 <- lm(PC1 ~ isolation_cat_nominal, data = analysis_df_race2) 

summary(lm_2sif1_race1)
summary(lm_2sif1_race2)

