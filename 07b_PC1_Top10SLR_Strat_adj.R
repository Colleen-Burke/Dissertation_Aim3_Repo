library(tidyverse)
library(broom)
library(here)
library(dplyr)

PC1top10_analysis_df <- readRDS(here("Aim3_Data_full.rds"))

PC1top10_analysis_df <- PC1top10_analysis_df |>
  mutate(
    social_activities2_d = factor(social_activities2_d,
                                  levels = c("High", "Low")),
    social_relationships2_d = factor(social_relationships2_d,
                                     levels = c("High", "Low"))
  )

#--- Define exposures, outcomes & covariates -----------------------------------
exposures <- c(
  "social_relationships2_d", "social_activities2_d", "isolation_cat_d", 
  "social_health2_d"
)

outcomes_PC1 <- c(
  "IL_10", "IFN_gamma", "IL_12p70", "IFN_alpha2", "CX3CL1",
  "IFN_lambda1", "IFN_lambda2_3", "sTREM_1", "sRAGE", "GM_CSF"
)

covariates <- c(
  "bmi_measured", "age", "sex", "lbp_vas_current", "mental_health"
)

# Keep only variables that exist in the data
valid_exposures <- intersect(exposures, names(PC1top10_analysis_df))
valid_outcomes_PC1 <- intersect(outcomes_PC1, names(PC1top10_analysis_df))
valid_covariates <- intersect(covariates, names(PC1top10_analysis_df))

# Optional: see what was missing
print(setdiff(exposures, valid_exposures))
print(setdiff(outcomes_PC1, valid_outcomes_PC1))
print(setdiff(covariates, valid_covariates))

#--- Create all exposure–outcome pairs -----------------------------------------
PC1model_grid <- expand_grid(
  exposure = valid_exposures,
  outcome_PC1 = valid_outcomes_PC1
)

#--- Fit ADJUSTED models stratified by race_dichotomized ----------------------
PC1model_results_strat <- PC1model_grid %>%
  mutate(
    PC1results = map2(exposure, outcome_PC1, \(x, y) {
      # Keep only needed variables and drop missing
      vars_needed <- c("race_dichotomized", x, y, valid_covariates)
      df <- PC1top10_analysis_df %>%
        select(all_of(vars_needed)) %>%
        drop_na()
      
      # If nothing left, return empty tibble
      if (nrow(df) == 0) {
        return(tibble())
      }
      
      # Group by race_dichotomized and fit separate ADJUSTED models
      df %>%
        group_by(race_dichotomized) %>%
        group_modify(\(.data, .key) {
          # Build formula: outcome ~ exposure + covariates
          formula_str <- paste(y, "~", paste(c(x, valid_covariates), collapse = " + "))
          fit <- lm(as.formula(formula_str), data = .data)
          broom::tidy(fit, conf.int = TRUE, conf.level = 0.95)
        }) %>%
        ungroup() %>%
        mutate(
          exposure = x,
          outcome_PC1 = y
        )
    })
  ) %>%
  select(PC1results) %>%
  unnest(PC1results)

#--- Keep exposure term only & tidy summary ------------------------------------
PC1top10_SLM_strat_summary <- PC1model_results_strat %>%
  filter(term != "(Intercept)") %>%
  # Keep only exposure terms, not covariate terms
  filter(term %in% valid_exposures | 
           str_starts(term, paste0(valid_exposures, collapse = "|"))) %>%
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
  here("PC1top10_SLM_stratified_adjusted_summary.csv"),
  row.names = FALSE
)

# saveRDS(PC1top10_SLM_strat_summary, here("PC1top10_SLM_stratified_adjusted_summary.rds"))