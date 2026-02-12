library(tidyverse)
library(dplyr)
library(ggplot2)
library(here)

#------ PCA Models -------------
PCA_overall <- read.csv(here("PCA_overall.csv"))

PCA_overall %>%
  mutate(Social = factor(Social, levels = c(1, 0))) %>%  # Reorder so 1 is on top
  ggplot(aes(y = Exposure, x = Estimate, color = Outcome)) +
  facet_grid(rows = vars(Outcome, Social), scales = "free_y", space = "free_y",
             labeller = labeller(Social = function(x) "")) +  # Remove Social labels
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI),
                 height = 0.2,
                 position = position_dodge(width = 0.5)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "Estimate", y = "Social Health Factors", color = NULL) +
  theme_minimal() +
  guides(color = "none")


#------ PC1 Model Stratified -------------
PCA_overall_strat <- read.csv(here("PCA_overall_strat.csv"))

PCA_overall_strat %>%
  filter(Outcome != "PC2") %>%  # Drop PC2
  mutate(Social = factor(Social, levels = c(1, 0)),
         Race = factor(Race, 
                       levels = c(0, 1, 2), 
                       labels = c("Overall", "White", "Non-White")),
         Race = fct_rev(Race)) %>%
  ggplot(aes(y = Exposure, x = Estimate, color = Race)) +
  facet_grid(rows = vars(Outcome, Social), scales = "free_y", space = "free_y",
             labeller = labeller(Social = function(x) "")) +
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI),
                 height = 0.2,
                 position = position_dodge(width = 0.6, preserve = "single")) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6, preserve = "single")) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "Estimate", y = "Social Health Factors", color = "Race") +
  theme_minimal()



#---------PC1 Top 10 -------------
PC1_biomarkers <- read.csv(here("PC1_biomarkers.csv"))

ggplot_data <- PC1_biomarkers |> 
  filter(biomarker %in% c("IFN_lambda1", "IFN_lambda2_3", "sTREM_1"))

ggplot(data = ggplot_data) +
  facet_grid(rows = vars(exposure), scales = "free_y", space = "free_y") +
  xlab("PC1 Top Contributing Biomarkers") + ylab("Estimates (95% CI)") +
  geom_pointrange(aes(x = biomarker, y = estimate, ymin = conf.low, ymax = conf.high, color = exposure)) + 
  geom_hline(yintercept = 0, lty = 2) +  
  coord_flip() +  
  theme_minimal() + theme(legend.position="none") 





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
      fct_rev()
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

