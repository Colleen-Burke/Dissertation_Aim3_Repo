library(tidyverse)
library(broom)
library(here)
library(dplyr)

PC2top10_analysis_df <- readRDS(here("Aim3_Data_full.rds"))

PC2top10_analysis_df <- PC2top10_analysis_df |>
  mutate(
    social_activities2_d = factor(social_activities2_d,
                                  levels = c("High", "Low")),
    social_relationships2_d = factor(social_relationships2_d,
                                     levels = c("High", "Low"))
  )

#--- Define exposures, outcomes & covariates -----------------------------------
exposures <- c("social_relationships2_d", "social_activities2_d", 
               "isolation_cat_d", "social_health2_d")

outcomes_PC2 <- c(
  "TGF_beta1", "VEGF", "IL_18", "VILIP_1", "IL_6"
)

covariates <- c(
  "age", "sex", "bmi_measured", "mental_health", "lbp_vas_current"
)

# Keep only variables that exist in the data
valid_exposures <- intersect(exposures, names(PC2top10_analysis_df))
valid_outcomes_PC2 <- intersect(outcomes_PC2, names(PC2top10_analysis_df))
valid_covariates <- intersect(covariates, names(PC2top10_analysis_df))

# Optional: see what was missing
print(setdiff(exposures, valid_exposures))
print(setdiff(outcomes_PC2, valid_outcomes_PC2))
print(setdiff(covariates, valid_covariates))

#--- Create all exposure–outcome pairs -----------------------------------------
PC1model_grid_adj <- expand_grid(
  exposure = valid_exposures,
  outcome_PC2 = valid_outcomes_PC2
)

#--- Fit ADJUSTED models stratified by race_dichotomized ----------------------
PC2model_results_strat_adj <- PC1model_grid_adj %>%
  mutate(
    PC2results = map2(exposure, outcome_PC2, function(x, y) {
      
      # Include covariates in the data selection
      vars_needed <- c("race_dichotomized", x, y, valid_covariates)
      df <- PC2top10_analysis_df %>%
        select(all_of(vars_needed)) %>%
        drop_na()
      
      if (nrow(df) == 0) return(tibble())
      
      df %>%
        group_split(race_dichotomized, .keep = TRUE) %>%
        map_dfr(function(d) {
          
          # ✅ skip if exposure has only 1 level in this race
          if (n_distinct(d[[x]]) < 2) {
            return(tibble())
          }
          
          # Build adjusted formula: outcome ~ exposure + covariates
          formula_str <- paste(y, "~", paste(c(x, valid_covariates), collapse = " + "))
          
          fit <- lm(
            as.formula(formula_str),
            data = d
          )
          
          broom::tidy(fit, conf.int = TRUE) %>%
            mutate(
              race_dichotomized = unique(d$race_dichotomized)
            )
        }) %>%
        mutate(
          exposure = x,
          outcome_PC2 = y
        )
    })
  ) %>%
  select(PC2results) %>%
  unnest(PC2results)

#--- Keep exposure term only & tidy summary ------------------------------------
PC2top10_SLM_strat_summary_adj <- PC2model_results_strat_adj %>%
  filter(term != "(Intercept)") %>%
  # Keep only exposure terms, not covariate terms
  filter(term %in% valid_exposures | 
           str_starts(term, paste0(valid_exposures, collapse = "|"))) %>%
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

print(as.data.frame(PC2top10_SLM_strat_summary_adj))

write.csv(
  PC2top10_SLM_strat_summary_adj,
  here("PC2top10_SLM_stratified_adjusted_summary.csv"),
  row.names = FALSE
)

# saveRDS(PC2top10_SLM_strat_summary, here("PC2top10_SLM_stratified_adjusted_summary.rds"))