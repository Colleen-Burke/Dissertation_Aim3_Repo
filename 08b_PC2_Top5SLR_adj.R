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
  "age", "sex", 
  "bmi_measured", "mental_health", "lbp_vas_current"
)

# Keep only the ones that actually exist in the data
valid_exposures <- intersect(exposures, names(PC2top10_analysis_df))
valid_PC2outcomes <- intersect(outcomes_PC2, names(PC2top10_analysis_df))
valid_covariates <- intersect(covariates, names(PC2top10_analysis_df))

# (optional) tell yourself what was missing
print(setdiff(exposures, valid_exposures))
print(setdiff(outcomes_PC2, valid_PC2outcomes))
print(setdiff(covariates, valid_covariates))

#--- Create model grid ---------------------------------------------------------
PC2model_grid_adj <- expand_grid(
  exposure = valid_exposures,
  outcome_PC2 = valid_PC2outcomes
) |>
  mutate(
    data = map2(exposure, outcome_PC2, ~ {
      vars_needed <- c(.x, .y, valid_covariates)
      PC2top10_analysis_df |>
        select(all_of(vars_needed)) |>
        drop_na()
    })
  )

view(PC2model_grid_adj)

#--- Run each ADJUSTED linear model --------------------------------------------
PC2model_results_adj <- PC2model_grid_adj %>%
  mutate(
    model = map2(
      exposure, outcome_PC2,
      ~ {
        # Build formula: outcome ~ exposure + covariates
        formula_str <- paste(.y, "~", paste(c(.x, valid_covariates), collapse = " + "))
        lm(as.formula(formula_str), data = PC2top10_analysis_df)
      }
    ),
    tidy = map(model, broom::tidy, conf.int = TRUE, conf.level = 0.95)
  ) %>%
  select(exposure, outcome_PC2, tidy) %>%
  unnest(tidy)

#--- View summary table of results ---------------------------------------------
PC2top10_SLM_summary_adj2 <- PC2model_results_adj %>%
  filter(term != "(Intercept)") %>%  # remove intercept
  # Keep only exposure terms, not covariate terms
  filter(term %in% valid_exposures | 
           str_starts(term, paste0(valid_exposures, collapse = "|"))) %>%
  select(exposure, outcome_PC2, term, 
         estimate, std.error, statistic, conf.low, conf.high, p.value)

print(as.data.frame(PC2top10_SLM_summary_adj2))

write.csv(PC2top10_SLM_summary_adj, here("PC2top10_SLM_adj_summary2.csv"), row.names = FALSE)
#saveRDS(PC2top10_SLM_summary, here("PC2top10_SLM_adjusted_summary.rds"))