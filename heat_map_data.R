library(tidyverse)
library(here)

heat_map_data <- analysis_df |> 
  select(
    record_id, biospecimen_id, social_activities2_d, social_relationships2_d,
    isolation_cat_d, social_health2_d, IL_1beta, IL_6, TNF, IP_10, IFN_lambda1, 
    IL_8, IL_12p70, IFN_alpha2, IFN_lambda2_3, GM_CSF, IFN_beta, IL_10, 
    IFN_gamma, VILIP_1, MCP_1, sTREM_2, BDNF, TGF_beta1, VEGF, sTREM_1, 
    beta_NGF, IL_18, sRAGE, CX3CL1
       ) |> 
  mutate(
    social_activities = recode_factor(social_activities2_d, 
                                         "Low" = "1", 
                                         "High" = "0"),
    social_relationships = recode_factor(social_relationships2_d,
                                            "Low" = "1",
                                            "High" = "0"),
    social_isolation = recode_factor(isolation_cat_d, 
                                     "High" = "1",
                                     "Low" = "0"),
    social_health = recode_factor(social_health2_d,
                                  "Poor" = "1",
                                  "Good" = "0")
  ) |> 
  select(record_id, biospecimen_id, social_activities, social_relationships,
         social_isolation, social_health, IL_1beta, IL_6, TNF, IP_10, 
         IFN_lambda1, IL_8, IL_12p70, IFN_alpha2, IFN_lambda2_3, GM_CSF, 
         IFN_beta, IL_10, IFN_gamma, VILIP_1, MCP_1, sTREM_2, BDNF, TGF_beta1, 
         VEGF, sTREM_1, beta_NGF, IL_18, sRAGE, CX3CL1)

write.csv(heat_map_data, here("heat_map_data.csv"))
  