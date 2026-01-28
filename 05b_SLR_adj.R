library(tidyverse)
library(broom)
library(here)

analysis_df <- readRDS(here("Aim3_Data_full.rds"))

exposures <- c("social_relationships2_d", "social_activities2_d", "isolation_cat_d", "social_health2_d")

outcomes <- c(
  "IL_1beta", "IL_6", "TNF", "IP_10", "IFN_lambda1", "IL_8",
  "IL_12p70", "IFN_alpha2", "IFN_lambda2_3", "GM_CSF", "IFN_beta", "IL_10",
  "IFN_gamma", "VILIP_1", "MCP_1", "sTREM_2", "BDNF", "TGF_beta1", "VEGF",
  "sTREM_1", "beta_NGF", "IL_18", "sRAGE", "CX3CL1"
)

covariates <- c("age", "sex", "bmi_measured", "mental_health", "lbp_vas_current")

model_grid <- expand_grid(exposure = exposures, outcome = outcomes)

model_results_adj <- model_grid |>
  mutate(
    model = map2(
      exposure, outcome,
      ~ lm(
        as.formula(paste(.y, "~", paste(c(.x, covariates), collapse = " + "))),
        data = analysis_df
      )
    ),
    tidy = map(model, ~ broom::tidy(.x, conf.int = TRUE))
  ) |>
  select(exposure, outcome, tidy) |>
  unnest(tidy)

# Keep exposure coefficient(s):
# - continuous exposure: term == exposure
# - factor exposure: term starts with exposure name (e.g., "social_health2_dLow")
SLM_summary_adj2 <- model_results_adj |>
  mutate(is_exposure_term = map2_lgl(term, exposure, ~ .x == .y || stringr::str_starts(.x, .y))) |>
  filter(is_exposure_term) |>
  select(exposure, outcome, term, estimate, std.error, statistic, p.value, conf.low, conf.high) |>
  rename(ci_lower = conf.low, ci_upper = conf.high) |>
  arrange(p.value)

print(SLM_summary_adj2)

SLM_summary_adj2 |>
  slice_head(n = 150) |>
  print(n = 150)

write.csv(SLM_summary_adj2, here("SLM_summary_adj2.csv"), row.names = FALSE)
