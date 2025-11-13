# Load Data
analysis_df <- readRDS(here("Aim3_Data_with_PCA.rds"))

# ---------- Social Relationships Ordinal Models ----------
lm_sr1 <- lm(PC1 ~ social_relationships_ordinal, data = analysis_df)
summary(lm_sr1)

lm_sr2 <- lm(PC2 ~ social_relationships_ordinal, data = analysis_df)
summary(lm_sr2)


# ---------- Social Relationships Nominal Models ----------
lm_2sr1 <- lm(PC1 ~ social_relationships_nominal, data = analysis_df)
summary(lm_2sr1)

lm_2sr2 <- lm(PC2 ~ social_relationships_nominal, data = analysis_df)
summary(lm_2sr2)

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




# ---------- Social Relationships Dichotomized Models ----------
lm_srd1 <- lm(PC1 ~ social_relationships_d, data = analysis_df)
summary(lm_srd1)

lm_srd2 <- lm(PC2 ~ social_relationships_d, data = analysis_df)
summary(lm_srd2)

# ---------- Social Activities Dichotomized Models ----------
lm_sad1 <- lm(PC1 ~ social_activities_d, data = analysis_df)
summary(lm_sad1)

lm_sad2 <- lm(PC2 ~ social_activities_d, data = analysis_df)
summary(lm_sad2)

# ---------- Social Isolation Dichotomized Models ----------
lm_sid1 <- lm(PC1 ~ isolation_cat_d, data = analysis_df)
summary(lm_sid1)

lm_sid2 <- lm(PC2 ~ isolation_cat_d, data = analysis_df)
summary(lm_sid2)





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




