library(tidyverse)
library(dplyr)
library(ggplot2)
library(here)

#------ PCA Models -------------
PCA_overall <- read.csv(here("PCA_overall.csv"))

ggplot(PCA_overall, aes(y = Exposure, x = Estimate)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.2) +
  geom_point(size = 2.5) +
  facet_wrap(~ Outcome, ncol = 1, scales = "free_y", strip.position = "top") +
  labs(x = "Estimate", y = "Social Health Factors") +
  theme_minimal() +
  theme(
    strip.text.x = element_text(face = "bold", size = 11),
    strip.background = element_blank(),
    panel.spacing.y = unit(1.0, "lines")
  )



#-------- PCA Stratified -------------
PCA_overall_strat <- read.csv(here("PCA_overall_strat.csv"))

ggplot_data <- PCA_overall_strat %>%
  mutate(
    Exposure = factor(Exposure,
                      levels = c("Social_Roles", 
                                 "Social_Activity", 
                                 "Social_Isolation", 
                                 "Social_Health"),
                      labels = c("Social Roles", 
                                 "Social Activity", 
                                 "Social Isolation", 
                                 "Social Health")),
    Social = factor(Social, levels = c(1, 0)),
    Race = factor(Race,
                  levels = c(0, 1, 2),
                  labels = c("Overall", "White", "Non-White")) %>%
      fct_rev()
  )

ggplot(ggplot_data, aes(y = Exposure, x = Estimate, color = Race)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_errorbarh(
    aes(xmin = Lower_CI, xmax = Upper_CI),
    height = 0.2,
    position = position_dodge(width = 0.6)
  ) +
  geom_point(
    size = 2.5,
    position = position_dodge(width = 0.6)
  ) +
  facet_wrap(~ Outcome, ncol = 1, scales = "free_y", strip.position = "top") +
  labs(x = "Estimate", y = "Social Factors", color = "Race") +
  theme_minimal() +
  theme(
    strip.text.x = element_text(face = "bold", size = 11),
    strip.background = element_blank(),
    panel.spacing.y = unit(1.0, "lines")
  )




#---------PC1 Top 10 Stratified -------------
PC1_biomarkers_strat <- read.csv(here("PC1_biomarkers_stratified.csv"))

ggplot_data_strat <- PC1_biomarkers_strat |>
  mutate(
    Social = factor(exposure,
                    levels = c("Social_Isolation", "Social_Health"),
                    labels = c("Social Isolation", "Social Health")),
    Race = factor(race,
                  levels = c("Overall", "White", "Non-White"),
                  labels = c("Overall", "White", "Non-White")) |>
      fct_rev(),
    biomarker = factor(biomarker,
                       levels = c("IL_10", "IFN_gamma", "IL_12p70", "IFN_alpha2",
                                  "CX3CL1", "IFN_lambda1", "IFN_lambda2_3",
                                  "sTREM_1", "sRAGE", "GM_CSF"),
                       labels = c("Interleukin-10 (IL-10)",
                                  "Interferon Gamma (IFN-\u03b3)",
                                  "Interleukin 12 (IL-12p70)",
                                  "Interferon Alpha-2 (IFN-\u03b12)",
                                  "Fractalkine (CX3CL1)",
                                  "Interferon Lambda-1 (IFN-\u03bb1)",
                                  "Interferon Lambda-2/3 (IFN-\u03bb2/3)",
                                  "CineTreak Stream 1 (sTREM-1)",
                                  "Soluble Receptor for Advanced Glycation End Products (sRAGE)",
                                  "Granulocyte-Macrophage Colony-Stimulating Factor (GM-CSF)"))
  )

ggplot(ggplot_data_strat, aes(y = Social, x = estimate, color = Race)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.2,
    position = position_dodge(width = 0.6)
  ) +
  geom_point(
    size = 2.5,
    position = position_dodge(width = 0.6)
  ) +
  facet_wrap(~ biomarker, ncol = 1, scales = "free_y", strip.position = "top") +
  labs(x = "Estimate", y = "Social Factors", color = "Race") +
  theme_minimal() +
  theme(
    strip.text.x = element_text(face = "bold", size = 11),
    strip.background = element_blank(),
    panel.spacing.y = unit(1.0, "lines")
  )

