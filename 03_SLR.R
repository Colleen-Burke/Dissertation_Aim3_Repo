library(tidyverse)
library(broom)
library(here)

# Define exposures and outcomes
exposures <- c(
  "social_relationships_ordinal", "social_relationships_nominal", "social_relationships_d", 
  "social_activities_ordinal", "social_activities_nominal", "social_activities_d", 
  "isolation_cat_ordinal", "isolation_cat_nominal", "isolation_cat_d", 
  "race_f", "race_dichotomized"
)

outcomes <- c(
  "IL_1beta", "IL_6", "TNF", "IP_10", "IFN_lambda1", "IL_8",
  "IL_12p70", "IFN_alpha2", "IFN_lambda2_3", "GM_CSF", "IFN_beta", "IL_10",
  "IFN_gamma", "VILIP_1", "MCP_1", "sTREM_2", "BDNF", "TGF_beta1", "VEGF",
  "sTREM_1", "beta_NGF", "IL_18", "sRAGE", "CX3CL1"
)

# Create all combinations
model_grid <- expand_grid(exposure = exposures, outcome = outcomes)

# Run each simple linear model (no adjustment)
model_results <- model_grid |> 
  mutate(
    model = map2(
      exposure, outcome,
      ~ lm(as.formula(paste(.y, "~", .x)), data = analysis_df)
    ),
    tidy = map(model, broom::tidy)
  ) |> 
  select(exposure, outcome, tidy) |> 
  unnest(tidy)

# View summary table of results
SLM_summary <- model_results |> 
  filter(term != "(Intercept)") |>  # keep exposure term only
  select(exposure, outcome, term, estimate, std.error, statistic, p.value) |> 
  arrange(p.value)

# Preview top results
print(SLM_summary)


# View the top 30 results by smallest p-value
SLM_summary |> 
  arrange(p.value) |> 
  slice_head(n = 100) |> 
  print(n = 100)


saveRDS(SLM_summary, here("SLM_summary.rds"))
