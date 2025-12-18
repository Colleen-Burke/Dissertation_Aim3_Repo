library(tidyverse)
library(broom)
library(here)
library(dplyr)

PC1top10_analysis_adj_df <- readRDS(here("Aim3_Data_full.rds"))

PC1top10_analysis_adj_df <- PC1top10_analysis_adj_df |>
  mutate(
    social_activities2_d = factor(social_activities2_d,
                                  levels = c("High", "Low")),
    social_relationships2_d = factor(social_relationships2_d,
                                     levels = c("High", "Low")),
    social_health2_d = case_when(
    social_health_num %in% c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11) ~ "Poor",   
    social_health_num %in% c(12, 13, 14) ~ "Good",
    TRUE ~ NA_character_
  ))


# Exposures of interest
exposures <- c(
  "social_relationships2_d",
  "social_activities2_d",
  "isolation_cat_d",
  "social_health2_d"
)

outcomes_PC1 <- c(
  "IL_10", "IFN_gamma", "IL_12p70", "IFN_alpha2", "CX3CL1",
  "IFN_lambda1", "IFN_lambda2_3", "sTREM_1", "sRAGE", "GM_CSF"
)

# Covariates to adjust for
covariates <- c(
  "age", "sex", "race_dichotomized",
  "bmi_measured", "mental_health", "lbp_vas_current"
)

# Keep only variables that exist
valid_exposures <- intersect(exposures, names(PC1top10_analysis_adj_df))
valid_outcomes_PC1 <- intersect(outcomes_PC1, names(PC1top10_analysis_adj_df))
valid_covariates <- intersect(covariates, names(PC1top10_analysis_adj_df))

# Model grid
PC1model_grid_adj <- expand_grid(
  exposure = valid_exposures,
  outcome_PC1 = valid_outcomes_PC1
)

# Run each ADJUSTED linear model
PC1model_results_adj <- PC1model_grid_adj %>%
  mutate(
    model = map2(
      exposure, outcome_PC1,
      ~ lm(
        as.formula(
          paste(.y, "~", paste(c(.x, valid_covariates), collapse = " + "))
        ),
        data = PC1top10_analysis_adj_df
      )
    ),
    tidy = map(model, broom::tidy, conf.int = TRUE, conf.level = 0.95)
  ) %>%
  select(exposure, outcome_PC1, tidy) %>%
  unnest(tidy)

# Keep exposure term(s) only (handles factor contrasts)
PC1top10_SLM_summary_adj <- PC1model_results_adj %>%
  mutate(
    is_exposure_term = map2_lgl(
      term, exposure,
      ~ .x == .y || stringr::str_starts(.x, .y)
    )
  ) %>%
  filter(is_exposure_term) %>%
  select(
    exposure, outcome_PC1, term,
    estimate, std.error, statistic,
    conf.low, conf.high, p.value
  )

print(as.data.frame(PC1top10_SLM_summary_adj))

write.csv(
  PC1top10_SLM_summary_adj,
  here("PC1top10_SLM_summary_adj.csv"),
  row.names = FALSE
)

#saveRDS(PC1top10_SLM_summary, here("PC1top10_SLM_summary.rds"))
