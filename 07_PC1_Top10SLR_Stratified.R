library(tidyverse)
library(broom)
library(here)
library(dplyr)

PC1top10_analysis_df <- readRDS(here("Aim3_Data_full.rds"))

#--- Define exposures & outcomes -----------------------------------------------
exposures <- c(
  "social_relationships_ordinal", "social_relationships_nominal", 
  "social_relationships_d", "social_relationships2_d", 
  "social_activities_ordinal", "social_activities_nominal", 
  "social_activities_d", "social_activities2_d", "isolation_cat_ordinal", 
  "isolation_cat_nominal", "isolation_cat_d", "isolation_cat2_d", 
  "isolation_item_communicate", "isolation_item_visit", 
  "isolation_item_communicate_d", "isolation_item_visit_d", "social_factor", 
  "social_factor_d", "social_composite_sum"
)

outcomes_PC1 <- c(
  "IL_10", "IFN_gamma", "IL_12p70", "IFN_alpha2", "CX3CL1",
  "IFN_lambda1", "IFN_lambda2_3", "sTREM_1", "sRAGE", "GM_CSF"
)

# Keep only variables that exist in the data
valid_exposures <- intersect(exposures, names(PC1top10_analysis_df))
valid_outcomes_PC1  <- intersect(outcomes_PC1,  names(PC1top10_analysis_df))

# Optional: see what was missing
print(setdiff(exposures, valid_exposures))
print(setdiff(outcomes_PC1,  valid_outcomes_PC1))

#--- Create all exposure–outcome pairs -----------------------------------------
PC1model_grid <- expand_grid(
  exposure = valid_exposures,
  outcome_PC1  = valid_outcomes_PC1
)

#--- Fit models stratified by race_dichotomized --------------------------------
PC1model_results_strat <- PC1model_grid %>%
  mutate(
    PC1results = map2(exposure, outcome_PC1, \(x, y) {
      # Keep only needed variables and drop missing
      df <- PC1top10_analysis_df %>%
        select(race_dichotomized, all_of(x), all_of(y)) %>%
        drop_na()
      
      # If nothing left, return empty tibble
      if (nrow(df) == 0) {
        return(tibble())
      }
      
      # Group by race_dichotomized and fit separate models
      df %>%
        group_by(race_dichotomized) %>%
        group_modify(\(.data, .key) {
          fit <- lm(reformulate(x, response = y), data = .data)
          broom::tidy(fit, conf.int = TRUE, conf.level = 0.95)
        }) %>%
        ungroup() %>%
        mutate(
          exposure = x,
          outcome_PC1  = y
        )
    })
  ) %>%
  select(PC1results) %>%
  unnest(PC1results)

#--- Keep exposure term only & tidy summary ------------------------------------
PC1top10_SLM_strat_summary <- PC1model_results_strat %>%
  filter(term != "(Intercept)") %>%
  select(
    race_dichotomized,
    exposure,
    outcome_PC1,
    term,
    estimate,
    std.error,
    statistic,
    conf.low,
    conf.high,
    p.value
  )

print(as.data.frame(PC1top10_SLM_strat_summary))

write.csv(
  PC1top10_SLM_strat_summary,
  here("PC1top10_SLM_stratified_SLM_summary.csv"),
  row.names = FALSE
)

# saveRDS(PC1top10_SLM_strat_summary, here("PC1top10_SLM_stratified_SLM_summary.rds"))
