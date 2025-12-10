library(tidyverse)
library(broom)
library(here)
library(dplyr)


PC2top10_analysis_df <- readRDS(here("Aim3_Data_full.rds"))
#top10_analysis_df <- read_csv(here("Aim3_Data_full.rds"))

# what you wanted
exposures <- c("social_relationships_ordinal", "social_relationships_nominal", 
               "social_relationships_d", "social_relationships2_d", 
               "social_activities_ordinal", "social_activities_nominal", 
               "social_activities_d", "social_activities2_d", "isolation_cat_ordinal", 
               "isolation_cat_nominal", "isolation_cat_d", "isolation_cat2_d", 
               "isolation_item_communicate", "isolation_item_visit", 
               "isolation_item_communicate_d", "isolation_item_visit_d", "race_f", 
               "race_dichotomized", "social_factor", "social_factor_d", 
               "social_composite_sum")
outcomes_PC2 <- c(
  "TGF_beta1", "VEGF", "IL_18", "VILIP_1", "sRAGE", "sTREM_1", "CX3CL1", "IL_6",
  "IL_10", "IL_12p70"
)

# keep only the ones that actually exist in the data
valid_exposures <- intersect(exposures, names(PC2top10_analysis_df))
valid_PC2outcomes  <- intersect(outcomes_PC2,  names(PC2top10_analysis_df))

# (optional) tell yourself what was missing
setdiff(exposures, valid_exposures)
setdiff(outcomes_PC2,  valid_PC2outcomes)

PC2model_grid <- expand_grid(exposure = valid_exposures,
                          outcome_PC2  = valid_PC2outcomes) |>
  mutate(
    data = map2(exposure, outcome_PC2, ~
                  PC2top10_analysis_df |>
                  select(all_of(c(.x, .y))) |>
                  drop_na()
    )
  )

view(PC2model_grid)

# Run each simple linear model
PC2model_results <- PC2model_grid %>%
  mutate(
    model = map2(
      exposure, outcome_PC2,
      ~ lm(as.formula(paste(.y, "~", .x)), data = PC2top10_analysis_df)
    ),
    tidy = map(model, broom::tidy, conf.int = TRUE, conf.level = 0.95)
  ) %>%
  select(exposure, outcome_PC2, tidy) %>%
  unnest(tidy)

# View summary table of results
PC2top10_SLM_summary <- PC2model_results %>%
  filter(term != "(Intercept)") %>%  # keep exposure term only
  select(exposure, outcome_PC2, term, 
         estimate, std.error, statistic, conf.low, conf.high, p.value)

print(as.data.frame(PC2top10_SLM_summary))

write.csv(PC2top10_SLM_summary, here("PC2top10_SLM_summary.csv"))

#saveRDS(PC2top10_SLM_summary, here("PC2top10_SLM_summary.rds"))
