library(tidyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(ggeffects)
library(marginaleffects)
library(xlsx)
library(MuMIn)

wave_ord <- c(1, 2, 4, "a1", 7, 10:20, 22, 23)
df <- readRDS("Data/full_tidied.rds")
df <- df %>% mutate(
  educ_imp = relevel(droplevels(educ_imp), ref = "Low"),
  wave_char = factor(wave_char, wave_ord)
)
df_rebase <- df %>% mutate(wave_char = relevel(wave_char, ref = "23"))

############################ Helper Functions###############
table_tidy <- function(table) {
  table <- table %>%
    as.data.frame() %>%
    tibble::rownames_to_column()
  colnames(table) <- c(
    "Variable", "Estimate", "Standard Error",
    "DF", "t-value", "P-value"
  )
  table <- table %>%
    mutate(
      Variable = gsub("wave_char(.*):party(.*)", "Wave * Party:   W\\1 * \\2", Variable),
      Variable = gsub(
        "wave_char(.*):(tax_and_soc_scale|econ_LR.*)",
        "Wave * (Economic L-R leaning:   W\\1 * Econ L-R leaning", Variable
      ),
      Variable = gsub(
        "wave_char(.*):(immigration_difficulty_scale|cult\\d*_LR.*)",
        "Wave * Cultural L-R leaning:   W\\1 * Cult L-R leaning", Variable
      ),
      Variable = gsub(
        "wave_char(.*):left_vs_right",
        "Wave * Self Assessed L-R leaning:   W\\1 * Self L-R leaning", Variable
      ),
      Variable = gsub("wave_char", "Wave:   W", Variable),
      Variable = gsub("wave", "Wave", Variable),
      Variable = gsub("age_group", "Year of Birth:   ", Variable),
      Variable = gsub("(gender)|(eastwest)", "", Variable),
      Variable = gsub("educ_imp", "Education:   ", Variable),
      Variable = gsub("afd_pref", "AfD Preference:   ", Variable),
      Variable = gsub("West", "West Germany", Variable),
      Variable = gsub("polit_int", "Political Interest", Variable),
      Variable = gsub("tax_and_soc_scale|econ_LR.*", "Economic L-R leaning", Variable),
      Variable = gsub("immigration_difficulty_scale|cult2_LR.*", "Cultural L-R leaning", Variable),
      Variable = gsub("left_vs_right", "Self Assessed L-R leaning", Variable),
      Variable = gsub("\\(|\\)", "", Variable),
      Estimate = round(Estimate, 2),
      `Standard Error` = round(`Standard Error`, 3),
      ` ` = case_when(
        `P-value` < 0.001 ~ "***",
        `P-value` < 0.01 ~ "**",
        `P-value` < 0.05 ~ "*",
        TRUE ~ ""
      )
    )
  vars <- table$Variable[grepl("\\:", table$Variable)]
  vars <- unique(gsub("(.*)\\:.*", "\\1", vars))
  for (i in 1:length(vars)) {
    index <- min(which(grepl(paste0(vars[i], ":"), table$Variable, fixed = TRUE)))
    table <- table %>% add_row(Variable = vars[i], .before = index)
  }
  table <- table %>%
    mutate(
      `Variable` = gsub(".*\\:", "", Variable),
      across(.cols = everything(), .fns = as.character),
      across(.cols = everything(), .fns = ~ if_else(is.na(.x), "", .x))
    ) %>%
    select(Variable, Estimate, ` `, `Standard Error`, everything())
  return(table)
}


######################## Explanatory Interaction Models ########################
################## Non-Weighted ######################
#### Separate #####
## Models ##
# Self-assigned leaning model
polit_int <- lmer(
  econ_vs_clim ~ age_group + gender + polit_int + eastwest + educ_imp +
    wave_char * left_vs_right + (1 | lfdn),
  data = df
)

# # re fit with wave 15 as baseline
# polit_int_reba <- lmer(
#   econ_vs_clim~age_group+gender+polit_int+eastwest+educ_imp+
#     wave_char*left_vs_right+(1|lfdn),data=df_rebase)
#
# 11*summary(polit_int)$coefficients["wave_char23:left_vs_right",1]
# 11*confint(polit_int,parm="wave_char23:left_vs_right")
# 11*summary(polit_int)$coefficients["left_vs_right",1]
# 11*confint(polit_int,parm="left_vs_right")
# 11*summary(polit_int_reba)$coefficients["left_vs_right",1]
# 11*confint(polit_int_reba,parm="left_vs_right")

# Economic Leaning model
polit_int2 <- lmer(
  econ_vs_clim ~ age_group + gender + polit_int + eastwest + educ_imp +
    wave_char * tax_and_soc_scale + (1 | lfdn),
  data = df
)

#
# polit_int_reba2 <- lmer(
#   econ_vs_clim~age_group+gender+polit_int+eastwest+educ_imp+
#     wave_char*tax_and_soc_scale+(1|lfdn),data=df_rebase)
#
# 7*summary(polit_int2)$coefficients["tax_and_soc_scale",1]
# 7*confint(polit_int2,parm="tax_and_soc_scale")
# 7*summary(polit_int_reba2)$coefficients["tax_and_soc_scale",1]
# 7*confint(polit_int_reba2,parm="tax_and_soc_scale")

# Cultural Leaning model
polit_int3 <- lmer(
  econ_vs_clim ~ age_group + gender + polit_int + eastwest + educ_imp +
    wave_char * immigration_difficulty_scale + (1 | lfdn),
  data = df
)

# polit_int_reba3 <- lmer(
#   econ_vs_clim~age_group+gender+polit_int+eastwest+educ_imp+
#     wave_char*immigration_difficulty_scale+(1|lfdn),data=df_rebase)
#
# 7*summary(polit_int3)$coefficients["immigration_difficulty_scale",1]
# 7*confint(polit_int3,parm="immigration_difficulty_scale")
# 7*summary(polit_int_reba3)$coefficients["immigration_difficulty_scale",1]
# 7*confint(polit_int_reba3,parm="immigration_difficulty_scale")

write.xlsx(summary(polit_int)$coefficients %>% table_tidy(),
  "Tables/polit_int_each_mod.xlsx",
  sheet = "self", row.names = FALSE
)
write.xlsx(summary(polit_int2)$coefficients %>% table_tidy(), "Tables/polit_int_each_mod.xlsx",
  sheet = "economic", append = TRUE, row.names = FALSE
)
write.xlsx(summary(polit_int3)$coefficients %>% table_tidy(), "Tables/polit_int_each_mod.xlsx",
  sheet = "cultural", append = TRUE, row.names = FALSE
)

## Marginal Effects ##
polit_int_eff <- ggeffect(
  polit_int,
  c("wave_char", "left_vs_right [-5,0,5]")
)

polit_int2_eff <- ggeffect(
  polit_int2,
  c("wave_char", "tax_and_soc_scale [-3,0,3]")
)

polit_int3_eff <- ggeffect(
  polit_int3,
  c("wave_char", "immigration_difficulty_scale [-3,0,3]")
)

## Save ##
saveRDS(list(
  self = polit_int_eff,
  econ = polit_int2_eff,
  cult = polit_int3_eff
), "ModelData/polit_int_each_eff.RData")

#### New L/R Measures ####
# Economic Leaning model
polit_int_econlr <- lmer(
  econ_vs_clim ~ age_group + gender + polit_int + eastwest + educ_imp +
    wave_char * econ_LR + (1 | lfdn),
  data = df
)


r.squaredGLMM(polit_int_econlr)

# Cultural Leaning model
polit_int_cultlr <- lmer(
  econ_vs_clim ~ age_group + gender + polit_int + eastwest + educ_imp +
    wave_char * cult2_LR + (1 | lfdn),
  data = df
)

r.squaredGLMM(polit_int_cultlr)

write.xlsx(summary(polit_int_econlr)$coefficients %>% table_tidy(), "Tables/polit_int_LRmeasures_mod.xlsx",
  sheet = "economic", row.names = FALSE
)
write.xlsx(summary(polit_int_cultlr)$coefficients %>% table_tidy(), "Tables/polit_int_LRmeasures_mod.xlsx",
  sheet = "cultural", append = TRUE, row.names = FALSE
)

## Marginal Effects ##
polit_int_econlr_eff <- ggeffect(
  polit_int_econlr,
  c("wave_char", "econ_LR [-3,0,3]")
)

polit_int_cultlr_eff <- ggeffect(
  polit_int_cultlr,
  c("wave_char", "cult2_LR [-3,0,3]")
)

## Save ##
saveRDS(list(
  econ = polit_int_econlr_eff,
  cult = polit_int_cultlr_eff
), "ModelData/polit_int_LRmeasures_eff.RData")

####  Joint  ######
polit_int_all <- lmer(econ_vs_clim ~
  age_group + gender + polit_int + eastwest + educ_imp +
  wave_char * (left_vs_right + tax_and_soc_scale + immigration_difficulty_scale) +
  (1 | lfdn), data = df)

11 * summary(polit_int_all)$coefficients["wave_char23:left_vs_right", 1]
11 * confint(polit_int_all, parm = "wave_char23:left_vs_right")

write.xlsx(summary(polit_int_all)$coefficients %>% table_tidy(),
  file = "Tables/polit_int_all_mod.xlsx", row.names = FALSE
)
## Marginal Effects ##
polit_int_eff_all <- ggeffect(
  polit_int_all,
  c("wave_char", "left_vs_right [-5,0,5]")
)

polit_int2_eff_all <- ggeffect(
  polit_int_all,
  c("wave_char", "tax_and_soc_scale [-3,0,3]")
)

polit_int3_eff_all <- ggeffect(
  polit_int_all,
  c("wave_char", "immigration_difficulty_scale [-3,0,3]")
)

## Save ##
saveRDS(list(
  self = polit_int_eff_all,
  econ = polit_int2_eff_all,
  cult = polit_int3_eff_all
), "ModelData/polit_int_all_eff.RData")
####  Econ/Cult  ######
polit_int_econcult <- lmer(econ_vs_clim ~
  age_group + gender + polit_int + eastwest + educ_imp +
  wave_char * (tax_and_soc_scale + immigration_difficulty_scale) +
  (1 | lfdn), data = df)

r.squaredGLMM(polit_int_econcult)
polit_int_econcult_reba <- lmer(econ_vs_clim ~
  age_group + gender + polit_int + eastwest + educ_imp +
  wave_char * (tax_and_soc_scale + immigration_difficulty_scale) +
  (1 | lfdn), data = df_rebase)

# 7*summary(polit_int_econcult)$coefficients["tax_and_soc_scale",1]
# 7*confint(polit_int_econcult,parm="tax_and_soc_scale")
# 7*summary(polit_int_econcult_reba)$coefficients["tax_and_soc_scale",1]
# 7*confint(polit_int_econcult_reba,parm="tax_and_soc_scale")#

7 * summary(polit_int_econcult)$coefficients["immigration_difficulty_scale", 1]
7 * confint(polit_int_econcult, parm = "immigration_difficulty_scale")
7 * summary(polit_int_econcult_reba)$coefficients["immigration_difficulty_scale", 1]
7 * confint(polit_int_econcult_reba, parm = "immigration_difficulty_scale")

write.xlsx(summary(polit_int_econcult)$coefficients %>% table_tidy(),
  file = "Tables/polit_int_econcult_mod.xlsx", row.names = FALSE
)
## Marginal Effects ##
polit_int2_eff_econcult <- ggeffect(
  polit_int_econcult,
  c("wave_char", "tax_and_soc_scale [-3,0,3]")
)

polit_int3_eff_econcult <- ggeffect(
  polit_int_econcult,
  c("wave_char", "immigration_difficulty_scale [-3,0,3]")
)

## Save ##
saveRDS(list(
  econ = polit_int2_eff_econcult,
  cult = polit_int3_eff_econcult
), "ModelData/polit_int_econcult_eff.RData")

####  Econ/Cult w Econ ####
polit_int_econcult_wextra <- lmer(econ_vs_clim ~
  age_group + gender + polit_int + eastwest + educ_imp +
  personal_econ + general_econ +
  wave_char * (tax_and_soc_scale + immigration_difficulty_scale) +
  (1 | lfdn), data = df)

write.xlsx(summary(polit_int_econcult_wextra)$coefficients %>% table_tidy(),
  file = "Tables/polit_int_econcult_wEconState_mod.xlsx", row.names = FALSE
)

polit_int2_eff_econcult_wextra <- ggeffect(
  polit_int_econcult_wextra,
  c("wave_char", "tax_and_soc_scale [-3,0,3]")
)

polit_int3_eff_econcult_wextra <- ggeffect(
  polit_int_econcult_wextra,
  c("wave_char", "immigration_difficulty_scale [-3,0,3]")
)

saveRDS(
  list(
    econ = polit_int2_eff_econcult_wextra,
    cult = polit_int3_eff_econcult_wextra
  ),
  "ModelData/polit_int_econcult_wEconState_eff.RData"
)

## with future as well
polit_int_econcult_wextra2 <- lmer(econ_vs_clim ~
  age_group + gender + polit_int + eastwest + educ_imp +
  personal_econ + personal_econ_future +
  wave_char * (tax_and_soc_scale + immigration_difficulty_scale) +
  (1 | lfdn), data = df)

polit_int2_eff_econcult_wextra2 <- ggeffect(
  polit_int_econcult_wextra2,
  c("wave_char", "tax_and_soc_scale [-3,0,3]")
)

polit_int3_eff_econcult_wextra2 <- ggeffect(
  polit_int_econcult_wextra2,
  c("wave_char", "immigration_difficulty_scale [-3,0,3]")
)

##### Econ/Cult wAfD Support #####
polit_int_econcult_wAfD <- lmer(econ_vs_clim ~
  age_group + gender + polit_int + eastwest + educ_imp +
  afd_pref +
  wave_char * (tax_and_soc_scale + immigration_difficulty_scale) +
  (1 | lfdn), data = df)

write.xlsx(summary(polit_int_econcult_wAfD)$coefficients %>% table_tidy(),
  file = "Tables/polit_int_econcult_wAfD_mod.xlsx", row.names = FALSE
)

polit_int2_eff_econcult_wAfD <- ggeffect(
  polit_int_econcult_wAfD,
  c("wave_char", "tax_and_soc_scale [-3,0,3]")
)

polit_int3_eff_econcult_wAfD <- ggeffect(
  polit_int_econcult_wAfD,
  c("wave_char", "immigration_difficulty_scale [-3,0,3]")
)

saveRDS(
  list(
    econ = polit_int2_eff_econcult_wAfD,
    cult = polit_int3_eff_econcult_wAfD
  ),
  "ModelData/polit_int_econcult_wAfD_eff.RData"
)


####  Econ/Cult LR Imp  ######
polit_int_econcult_lr_imp <- lmer(econ_vs_clim ~
  age_group + gender + polit_int + eastwest + educ_imp +
  wave_char * (econ_LR_imp + cult2_LR_imp) +
  (1 | lfdn), data = df)

r.squaredGLMM(polit_int_econcult_lr_imp)
polit_int_econcult_lr_imp_reba <- lmer(econ_vs_clim ~
  age_group + gender + polit_int + eastwest + educ_imp +
  wave_char * (econ_LR_imp + cult2_LR_imp) +
  (1 | lfdn), data = df_rebase)

# 7*summary(polit_int_econcult_lr_imp)$coefficients["econ_LR_imp",1]
# 7*confint(polit_int_econcult_lr_imp,parm="econ_LR_imp")
# 7*summary(polit_int_econcult_lr_imp_reba)$coefficients["econ_LR_imp",1]
# 7*confint(polit_int_econcult_lr_imp_reba,parm="econ_LR_imp")#

7 * summary(polit_int_econcult_lr_imp)$coefficients["cult2_LR_imp", 1]
7 * confint(polit_int_econcult_lr_imp, parm = "cult2_LR_imp")
7 * summary(polit_int_econcult_lr_imp_reba)$coefficients["cult2_LR_imp", 1]
7 * confint(polit_int_econcult_lr_imp_reba, parm = "cult2_LR_imp")

write.xlsx(summary(polit_int_econcult_lr_imp)$coefficients %>% table_tidy(),
  file = "Tables/polit_int_econcult_lrimp_mod.xlsx", row.names = FALSE
)
## Marginal Effects ##
polit_int2_eff_econcult_lr_imp <- ggeffect(
  polit_int_econcult_lr_imp,
  c("wave_char", "econ_LR_imp [-3,0,3]")
)

polit_int3_eff_econcult_lr_imp <- ggeffect(
  polit_int_econcult_lr_imp,
  c("wave_char", "cult2_LR_imp [-3,0,3]")
)

## Save ##
saveRDS(list(
  econ = polit_int2_eff_econcult_lr_imp,
  cult = polit_int3_eff_econcult_lr_imp
), "ModelData/polit_int_econcult_lr_imp_eff.RData")

##################   Weighted   ######################
#### Separate #####
##  Models ##
# Self-assigned leaning model
polit_int_w <- lmer(econ_vs_clim ~
  age_group + gender + polit_int + eastwest + educ_imp + wave_char * left_vs_right +
  (1 | lfdn), data = df, weights = wei5_on)

# Economic Leaning model
polit_int2_w <- lmer(econ_vs_clim ~
  age_group + gender + polit_int + eastwest + educ_imp + wave_char * tax_and_soc_scale +
  (1 | lfdn), data = df, weights = wei5_on)

# Cultural Leaning model
polit_int3_w <- lmer(econ_vs_clim ~
  age_group + gender + polit_int + eastwest + educ_imp + wave_char * immigration_difficulty_scale +
  (1 | lfdn), data = df, weights = wei5_on)

write.xlsx(summary(polit_int_w)$coefficients,
  "Tables/polit_int_each_mod_w.xlsx",
  sheet = "self"
)
write.xlsx(summary(polit_int2_w)$coefficients,
  "Tables/polit_int_each_mod_w.xlsx",
  sheet = "economic", append = TRUE
)
write.xlsx(summary(polit_int3_w)$coefficients,
  "Tables/polit_int_each_mod_w.xlsx",
  sheet = "cultural", append = TRUE
)

## Marginal Effects ##
polit_int_eff_w <- ggeffect(
  polit_int_w,
  c("wave_char", "left_vs_right [-5,0,5]")
)

polit_int2_eff_w <- ggeffect(
  polit_int2_w,
  c("wave_char", "tax_and_soc_scale [-3,0,3]")
)

polit_int3_eff_w <- ggeffect(
  polit_int3_w,
  c("wave_char", "cult2_LR [-3,0,3]")
)

saveRDS(list(
  self = polit_int_eff_w,
  econ = polit_int2_eff_w,
  cult = polit_int3_eff_w
), "ModelData/polit_int_each_eff_w.RData")
####  Joint  #####
##  Models  ##
polit_int_all_w <- lmer(econ_vs_clim ~
  age_group + gender + polit_int + eastwest + educ_imp +
  wave_char * (left_vs_right + tax_and_soc_scale + immigration_difficulty_scale) +
  (1 | lfdn), data = df, weights = wei5_on)

write.xlsx(summary(polit_int_all_w)$coefficients,
  file = "Tables/polit_int_all_mod_w.xlsx"
)
## Marginal Effects ##
polit_int_eff_all_w <- ggeffect(
  polit_int_all_w,
  c("wave_char", "left_vs_right [-5,0,5]")
)

polit_int2_eff_all_w <- ggeffect(
  polit_int_all_w,
  c("wave_char", "tax_and_soc_scale [-3,0,3]")
)

polit_int3_eff_all_w <- ggeffect(
  polit_int_all_w,
  c("wave_char", "immigration_difficulty_scale [-3,0,3]")
)

## Save ##
saveRDS(list(
  self = polit_int_eff_all_w,
  econ = polit_int2_eff_all_w,
  cult = polit_int3_eff_all_w
), "ModelData/polit_int_all_eff_w.RData")

####  Econ/cult  #####
##  Models  ##
polit_int_econcult_w <- lmer(
  econ_vs_clim ~
    age_group + gender + polit_int + eastwest + educ_imp +
    wave_char * (tax_and_soc_scale + immigration_difficulty_scale) +
    (1 | lfdn),
  data = df, weights = wei5_on
)

write.xlsx(summary(polit_int_econcult_w)$coefficients,
  file = "Tables/polit_int_econcult_mod_w.xlsx"
)
## Marginal Effects ##
polit_int2_eff_econcult_w <- ggeffect(
  polit_int_econcult_w,
  c("wave_char", "tax_and_soc_scale [-3,0,3]")
)

polit_int3_eff_econcult_w <- ggeffect(
  polit_int_econcult_w,
  c("wave_char", "immigration_difficulty_scale [-3,0,3]")
)

## Save ##
saveRDS(list(
  econ = polit_int2_eff_econcult_w,
  cult = polit_int3_eff_econcult_w
), "ModelData/polit_int_econcult_eff_w.RData")
################################ Basic Models ##################################
#### Separate #####
# Self-assigned leaning model
polit_base <- lmer(
  econ_vs_clim ~ wave_char + age_group +
    gender + polit_int + eastwest + educ_imp + left_vs_right +
    (1 | lfdn),
  data = df
)

# Economic Leaning model
polit_base2 <- lmer(
  econ_vs_clim ~ wave_char + age_group + gender + polit_int + eastwest +
    educ_imp + tax_and_soc_scale +
    (1 | lfdn),
  data = df
)

# Cultural Leaning model
polit_base3 <- lmer(
  econ_vs_clim ~ wave_char + age_group + gender + polit_int + eastwest +
    educ_imp + immigration_difficulty_scale +
    (1 | lfdn),
  data = df
)

# Test for statistical significance of wave interaction
anova(polit_base, polit_int)
anova(polit_base2, polit_int2)
anova(polit_base3, polit_int3)

write.xlsx(summary(polit_base)$coefficients,
  "Tables/polit_base_each_mod.xlsx",
  sheet = "self"
)
write.xlsx(summary(polit_base2)$coefficients, "Tables/polit_base_each_mod.xlsx",
  sheet = "economic", append = TRUE
)
write.xlsx(summary(polit_base3)$coefficients, "Tables/polit_base_each_mod.xlsx",
  sheet = "cultural", append = TRUE
)

## Marginal Effects ##
polit_base_eff <- ggeffect(polit_base, "left_vs_right [-5,0,5]")
polit_base2_eff <- ggeffect(polit_base2, "tax_and_soc_scale [-3,0,3]")
polit_base3_eff <- ggeffect(polit_base3, "immigration_difficulty_scale [-3,0,3]")

## Save ##
saveRDS(list(
  self = polit_base_eff,
  econ = polit_base2_eff,
  cult = polit_base3_eff
), "ModelData/polit_base_each_eff.RData")

####  Joint #######
## Models ##
polit_base_all <- lmer(econ_vs_clim ~ wave_char +
  age_group + gender + left_vs_right + eastwest + educ_imp +
  polit_int + tax_and_soc_scale + immigration_difficulty_scale +
  (1 | lfdn), data = df)

write.xlsx(summary(polit_base_all)$coefficients,
  file = "Tables/polit_base_all_mod.xlsx"
)
## Marginal Effects ##
polit_base_eff_all <- ggeffect(polit_base_all, "left_vs_right [-5,0,5]")
polit_base2_eff_all <- ggeffect(polit_base_all, "tax_and_soc_scale [-3,0,3]")
polit_base3_eff_all <- ggeffect(polit_base_all, "immigration_difficulty_scale [-3,0,3]")

## Save ##
saveRDS(
  list(
    self = polit_base_eff_all,
    econ = polit_base2_eff_all,
    cult = polit_base3_eff_all
  ),
  "ModelData/polit_base_all_eff.RData"
)

####  Econ/Cult #######
## Models ##
polit_base_econcult <- lmer(econ_vs_clim ~ wave_char +
  age_group + gender + eastwest + educ_imp +
  polit_int + tax_and_soc_scale + immigration_difficulty_scale +
  (1 | lfdn), data = df)

write.xlsx(summary(polit_base_econcult)$coefficients,
  file = "Tables/polit_base_econcult_mod.xlsx"
)
## Marginal Effects ##
polit_base2_eff_econcult <- ggeffect(polit_base_econcult, "tax_and_soc_scale [-3,0,3]")
polit_base3_eff_econcult <- ggeffect(polit_base_econcult, "immigration_difficulty_scale [-3,0,3]")

## Save ##
saveRDS(
  list(
    econ = polit_base2_eff_econcult,
    cult = polit_base3_eff_econcult
  ),
  "ModelData/polit_base_econcult_eff.RData"
)


################################ Full Models ###################################
#### Separate ####
## Models ##
# Self-assigned leaning model
polit_int <- lmer(econ_vs_clim ~ wave_char * (
  age_group + gender + polit_int + eastwest + educ_imp + left_vs_right) +
  (1 | lfdn), data = df)

# Economic Leaning model
polit_int2 <- lmer(econ_vs_clim ~ wave_char * (
  age_group + gender + polit_int + eastwest + educ_imp + tax_and_soc_scale) +
  (1 | lfdn), data = df)

# Cultural Leaning model
polit_int3 <- lmer(econ_vs_clim ~ wave_char * (
  age_group + gender + polit_int + eastwest + educ_imp + immigration_difficulty_scale) +
  (1 | lfdn), data = df)

write.xlsx(summary(polit_int)$coefficients,
  "Tables/polit_intall_each_mod.xlsx",
  sheet = "self"
)
write.xlsx(summary(polit_int2)$coefficients, "Tables/polit_intall_each_mod.xlsx",
  sheet = "economic", append = TRUE
)
write.xlsx(summary(polit_int3)$coefficients, "Tables/polit_intall_each_mod.xlsx",
  sheet = "cultural", append = TRUE
)

## Marginal Effects ##
polit_int_eff <- ggeffect(
  polit_int,
  c("wave_char", "left_vs_right [-5,0,5]")
)

polit_int2_eff <- ggeffect(
  polit_int2,
  c("wave_char", "tax_and_soc_scale [-3,0,3]")
)

polit_int3_eff <- ggeffect(
  polit_int3,
  c("wave_char", "immigration_difficulty_scale [-3,0,3]")
)

## Save ##
saveRDS(list(
  self = polit_int_eff,
  econ = polit_int2_eff,
  cult = polit_int3_eff
), "ModelData/polit_intall_each_eff.RData")
############################### Linear Wave Models #############################
#####################   Basic    ######################
#### Joint ####
polit_base_lin <- lmer(
  econ_vs_clim ~ wave + age_group +
    gender + polit_int + eastwest + educ_imp +
    left_vs_right + tax_and_soc_scale + immigration_difficulty_scale +
    (1 | lfdn),
  data = df
)



write.xlsx(summary(polit_base_lin)$coefficients,
  "Tables/polit_base_mod_lin.xlsx",
  sheet = "self"
)
#####################   Interaction   #################
#### Separate ######
polit_int_lin <- lmer(
  econ_vs_clim ~
    wave_char + age_group + gender + polit_int + eastwest + educ_imp + left_vs_right +
    wave:left_vs_right +
    (1 | lfdn),
  data = df
)

# summary(polit_int_lin)$coefficients["left_vs_right:wave",1]
# confint(polit_int_lin,parm="left_vs_right:wave")

# Economic Leaning model
polit_int2_lin <- lmer(
  econ_vs_clim ~ wave_char +
    age_group + gender + polit_int + eastwest + educ_imp + tax_and_soc_scale +
    wave:tax_and_soc_scale +
    (1 | lfdn),
  data = df
)

# summary(polit_int2_lin)$coefficients["tax_and_soc_scale:wave",1]
# confint(polit_int2_lin,parm="tax_and_soc_scale:wave")

# Cultural Leaning model
polit_int3_lin <- lmer(
  econ_vs_clim ~
    wave_char + age_group + gender + polit_int + eastwest + educ_imp + immigration_difficulty_scale +
    wave:immigration_difficulty_scale +
    (1 | lfdn),
  data = df
)

# summary(polit_int3_lin)$coefficients["immigration_difficulty_scale:wave",1]
# confint(polit_int3_lin,parm="immigration_difficulty_scale:wave")

write.xlsx(summary(polit_int_lin)$coefficients,
  "Tables/polit_int_each_mod_lin.xlsx",
  sheet = "self"
)
write.xlsx(summary(polit_int2_lin)$coefficients, "Tables/polit_int_each_mod_lin.xlsx",
  sheet = "economic", append = TRUE
)
write.xlsx(summary(polit_int3_lin)$coefficients, "Tables/polit_int_each_mod_lin.xlsx",
  sheet = "cultural", append = TRUE
)

#### Joint ########
polit_int_all_lin <- lmer(
  econ_vs_clim ~
    wave_char + age_group + gender + polit_int + eastwest + educ_imp +
    left_vs_right + tax_and_soc_scale + immigration_difficulty_scale +
    wave:(
      left_vs_right + tax_and_soc_scale + immigration_difficulty_scale) +
    (1 | lfdn),
  data = df
)

write.xlsx(summary(polit_int_all_lin)$coefficients,
  file = "Tables/polit_int_all_mod_lin.xlsx"
)
