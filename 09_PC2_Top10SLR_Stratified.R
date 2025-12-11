library(tidyverse)
library(broom)
library(here)
library(dplyr)

PC2top10_analysis_df <- readRDS(here("Aim3_Data_full.rds"))

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

outcomes_PC2 <- c(
  "TGF_beta1", "VEGF", "IL_18", "VILIP_1", "sRAGE", "sTREM_1", "CX3CL1", "IL_6",
  "IL_10", "IL_12p70"
)

# Keep only variables that exist in the data
valid_exposures <- intersect(exposures, names(PC2top10_analysis_df))
valid_outcomes_PC2  <- intersect(outcomes_PC2,  names(PC2top10_analysis_df))

# Optional: see what was missing
print(setdiff(exposures, valid_exposures))
print(setdiff(outcomes_PC2,  valid_outcomes_PC2))

#--- Create all exposure–outcome pairs -----------------------------------------
PC1model_grid <- expand_grid(
  exposure = valid_exposures,
  outcome_PC2  = valid_outcomes_PC2
)

#--- Fit models stratified by race_dichotomized --------------------------------
PC2model_results_strat <- PC1model_grid %>%
  mutate(
    PC2results = map2(exposure, outcome_PC2, function(x, y) {
      
      df <- PC2top10_analysis_df %>%
        select(race_dichotomized, all_of(x), all_of(y)) %>%
        drop_na()
      
      if (nrow(df) == 0) return(tibble())
      
      df %>%
        group_split(race_dichotomized, .keep = TRUE) %>%   # ✅ split first
        map_dfr(function(d) {
          
          # ✅ skip if exposure has only 1 level in this race
          if (n_distinct(d[[x]]) < 2) {
            return(tibble())
          }
          
          fit <- lm(
            as.formula(paste(y, "~", x)),
            data = d
          )
          
          broom::tidy(fit, conf.int = TRUE) %>%
            mutate(
              race_dichotomized = unique(d$race_dichotomized)
            )
        }) %>%
        mutate(
          exposure    = x,
          outcome_PC2 = y
        )
    })
  ) %>%
  select(PC2results) %>%
  unnest(PC2results)




#--- Keep exposure term only & tidy summary ------------------------------------
PC2top10_SLM_strat_summary <- PC2model_results_strat %>%
  filter(term != "(Intercept)") %>%
  select(
    race_dichotomized,
    exposure,
    outcome_PC2,
    term,
    estimate,
    std.error,
    statistic,
    conf.low,
    conf.high,
    p.value
  )

print(as.data.frame(PC2top10_SLM_strat_summary))

write.csv(
  PC2top10_SLM_strat_summary,
  here("PC2top10_SLM_stratified_SLM_summary.csv"),
  row.names = FALSE
)

# saveRDS(PC2top10_SLM_strat_summary, here("PC2top10_SLM_stratified_SLM_summary.rds"))
