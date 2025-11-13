library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

#Load Data
Aim3_Data <- readRDS(here("Aim3_Data.rds"))


# --- Inputs ---
biomarkers <- c(
  "IL_1beta","IL_6","TNF","IP_10","IFN_lambda1","IL_8",
  "IL_12p70","IFN_alpha2","IFN_lambda2_3","GM_CSF","IFN_beta","IL_10",
  "IFN_gamma","VILIP_1","MCP_1","sTREM_2","BDNF","TGF_beta1","VEGF",
  "sTREM_1","beta_NGF","IL_18","sRAGE","CX3CL1"
)

# --- Prepare data ---
base_df <- Aim3_Data |>
  filter(biospecimen_id != "KPB030")

pca_df <- base_df |>
  select(biospecimen_id, all_of(biomarkers)) |>
  drop_na()

# --- Run PCA ---
pca_fit <- prcomp(pca_df |> select(-biospecimen_id), center = TRUE, scale. = TRUE)

# --- Extract scores and merge back ---
pca_scores <- pca_df |>
  select(biospecimen_id) |>
  bind_cols(as.data.frame(pca_fit$x) |> 
              setNames(paste0("PC", seq_len(ncol(pca_fit$x)))))

analysis_df <- base_df |>
  left_join(pca_scores, by = "biospecimen_id")

# --- Variance explained & scree plot ---
var_explained <- (pca_fit$sdev)^2 / sum(pca_fit$sdev^2)

scree_df <- tibble(
  PC = seq_along(var_explained),
  Variance = var_explained,
  Cumulative = cumsum(var_explained)
)

ggplot(scree_df, aes(PC, Variance)) +
  geom_line() + geom_point() +
  labs(title = "Scree Plot", y = "Proportion of Variance Explained") +
  theme_minimal()

