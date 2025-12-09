library(tidyverse)
library(broom)
library(here)
library(dplyr)

top10_analysis_df <- readRDS(here("Aim3_Data_with_PCA.rds"))

#--- Define exposures & outcomes -----------------------------------------------
exposures <- c(
  "isolation_cat_ordinal", "isolation_cat_nominal", 
  "isolation_cat_d", "isolation_cat2_d", "isolation_item_visit", 
  "isolation_item_communicate", "isolation_item_visit_d", 
  "isolation_item_communicate_d"
)

outcomes <- c(
  "IL_10", "IFN_gamma", "IL_12p70", "IFN_alpha2", "CX3CL1",
  "IFN_lambda1", "IFN_lambda2_3", "sTREM_1", "sRAGE", "GM_CSF"
)

# Keep only variables that exist in the data
valid_exposures <- intersect(exposures, names(top10_analysis_df))
valid_outcomes  <- intersect(outcomes,  names(top10_analysis_df))

# Optional: see what was missing
print(setdiff(exposures, valid_exposures))
print(setdiff(outcomes,  valid_outcomes))

#--- Create all exposure–outcome pairs -----------------------------------------
model_grid <- expand_grid(
  exposure = valid_exposures,
  outcome  = valid_outcomes
)

#--- Fit models stratified by race_dichotomized --------------------------------
model_results_strat <- model_grid %>%
  mutate(
    results = map2(exposure, outcome, \(x, y) {
      # Keep only needed variables and drop missing
      df <- top10_analysis_df %>%
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
          outcome  = y
        )
    })
  ) %>%
  select(results) %>%
  unnest(results)

#--- Keep exposure term only & tidy summary ------------------------------------
top10_SLM_strat_summary <- model_results_strat %>%
  filter(term != "(Intercept)") %>%
  select(
    race_dichotomized,
    exposure,
    outcome,
    term,
    estimate,
    std.error,
    statistic,
    conf.low,
    conf.high,
    p.value
  )

print(as.data.frame(top10_SLM_strat_summary))

write.csv(
  top10_SLM_strat_summary,
  here("top10_SLM_stratified_SLM_summary.csv"),
  row.names = FALSE
)

# saveRDS(top10_SLM_strat_summary, here("top10_SLM_stratified_SLM_summary.rds"))
