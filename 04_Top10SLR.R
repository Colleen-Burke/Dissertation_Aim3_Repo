library(tidyverse)
library(broom)
library(here)
library(dplyr)


top10_analysis_df <- read_csv(here("analysis_df.csv"))

# what you wanted
exposures <- c("isolation_cat_ordinal", "isolation_cat_nominal", "isolation_cat_d")
outcomes <- c(
  "IL_10", "IFN_gamma", "IL_12p70", "IFN_alpha2", "CX3CL1",
  "IFN_lambda1", "IFN_lambda2_3", "sTREM_1", "sRAGE", "GM_CSF"
)

# keep only the ones that actually exist in the data
valid_exposures <- intersect(exposures, names(top10_analysis_df))
valid_outcomes  <- intersect(outcomes,  names(top10_analysis_df))

# (optional) tell yourself what was missing
setdiff(exposures, valid_exposures)
setdiff(outcomes,  valid_outcomes)

model_grid <- expand_grid(exposure = valid_exposures,
                          outcome  = valid_outcomes) |>
  mutate(
    data = map2(exposure, outcome, ~
                  top10_analysis_df |>
                  select(all_of(c(.x, .y))) |>
                  drop_na()
    )
  )

view(model_grid)

# Run each simple linear model
model_results <- model_grid %>%
  mutate(
    model = map2(
      exposure, outcome,
      ~ lm(as.formula(paste(.y, "~", .x)), data = top10_analysis_df)
    ),
    tidy = map(model, broom::tidy)
  ) %>%
  select(exposure, outcome, tidy) %>%
  unnest(tidy)

# View summary table of results
top10_SLM_summary <- model_results %>%
  filter(term != "(Intercept)") %>%  # keep exposure term only
  select(exposure, outcome, term, estimate, std.error, statistic, p.value)

print(top10_SLM_summary)



saveRDS(top10_SLM_summary, here("top10_SLM_summary.rds"))
