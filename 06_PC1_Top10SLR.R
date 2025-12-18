library(tidyverse)
library(broom)
library(here)
library(dplyr)


PC1top10_analysis_df <- readRDS(here("Aim3_Data_full.rds"))
#PC1top10_analysis_df <- read_csv(here("Aim3_Data_full.rds"))

PC1top10_analysis_df <- PC1top10_analysis_df |>
  mutate(
    social_activities2_d = factor(social_activities2_d,
                                 levels = c("High", "Low")),
    social_relationships2_d = factor(social_relationships2_d,
                                     levels = c("High", "Low"))
  )


# what you wanted
exposures <- c(
  "social_relationships_ordinal", "social_relationships_nominal", 
  "social_relationships_d", "social_relationships2_d", 
  "social_activities_ordinal", "social_activities_nominal", 
  "social_activities_d", "social_activities2_d", "isolation_cat_ordinal", 
  "isolation_cat_nominal", "isolation_cat_d", "isolation_cat2_d", 
  "isolation_item_communicate", "isolation_item_visit", "isolation_score",
  "isolation_item_communicate_d", "isolation_item_visit_d", "social_health_num", 
  "social_health_d", "social_function_num", "social_function_d", 
  "social_health2_d", "social_function2_d")

outcomes_PC1 <- c(
  "IL_10", "IFN_gamma", "IL_12p70", "IFN_alpha2", "CX3CL1",
  "IFN_lambda1", "IFN_lambda2_3", "sTREM_1", "sRAGE", "GM_CSF"
)

# keep only the ones that actually exist in the data
valid_exposures <- intersect(exposures, names(PC1top10_analysis_df))
valid_outcomes_PC1  <- intersect(outcomes_PC1,  names(PC1top10_analysis_df))

# (optional) tell yourself what was missing
setdiff(exposures, valid_exposures)
setdiff(outcomes_PC1,  valid_outcomes_PC1)

PC1model_grid <- expand_grid(exposure = valid_exposures,
                          outcome_PC1  = valid_outcomes_PC1) |>
  mutate(
    data = map2(exposure, outcome_PC1, ~
                  PC1top10_analysis_df |>
                  select(all_of(c(.x, .y))) |>
                  drop_na()
    )
  )

view(PC1model_grid)

# Run each simple linear model
PC1model_results <- PC1model_grid %>%
  mutate(
    model = map2(
      exposure, outcome_PC1,
      ~ lm(as.formula(paste(.y, "~", .x)), data = PC1top10_analysis_df)
    ),
    tidy = map(model, broom::tidy, conf.int = TRUE, conf.level = 0.95)
  ) %>%
  select(exposure, outcome_PC1, tidy) %>%
  unnest(tidy)

# View summary table of results
PC1top10_SLM_summary <- PC1model_results %>%
  filter(term != "(Intercept)") %>%  # keep exposure term only
  select(exposure, outcome_PC1, term, 
        estimate, std.error, statistic, conf.low, conf.high, p.value)

print(as.data.frame(PC1top10_SLM_summary))

write.csv(PC1top10_SLM_summary, here("PC1top10_SLM_summary.csv"))

#saveRDS(PC1top10_SLM_summary, here("PC1top10_SLM_summary.rds"))
