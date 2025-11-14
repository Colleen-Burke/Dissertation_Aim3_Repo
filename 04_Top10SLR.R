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

