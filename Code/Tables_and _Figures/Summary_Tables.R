library(dplyr)
library(tidyr)
library(xlsx)
library(SuppDists)

df <- readRDS("Data/full_tidied.rds")

wave_ord <- c(1, 2, 4, "a1", 7, 8, 10:23)
# First filter out to get the data we use in our Final analysis
df_filt <- df %>% filter(if_all(
  c(
    econ_vs_clim, wave_char, age_group,
    gender, polit_int, eastwest, educ_imp, lfdn
  ),
  ~ !is.na(.x)
))

df_filt <- df_filt %>% mutate(
  wave = factor(wave, levels = sort(unique(df_filt$wave))),
  wave_char = factor(wave_char, levels = wave_ord)
)
length(unique(df_filt$lfdn))

#################### Summary Functions #########################################
summarise_disc_simple <- function(data, variable) {
  variable <- enquo(variable)
  var_string <- quo_name(variable)
  b <- data %>%
    group_by(lfdn) %>%
    arrange(wave) %>%
    summarise(!!variable := first(!!variable))
  out <- bind_rows(Paticipants = b, Responses = df_filt, .id = "Type")

  out_dat <- out %>%
    group_by(Type, !!variable) %>%
    summarise(count = n()) %>%
    rename("variable" = var_string) %>%
    mutate(
      total = sum(count), prop = count / total,
      dispplot = paste0(count, "\n(", round(100 * prop, 1), "%)"),
      disp = paste0(count, "(", round(100 * prop, 1), "%)")
    )
  table <- out_dat %>%
    select(variable, disp, Type) %>%
    pivot_wider(id_cols = "variable", names_from = "Type", values_from = "disp")
  return(table)
}

summarise_cont_simple <- function(data, sum_var, labs = NULL, weight_var = NULL) {
  sum_var <- enquo(sum_var)
  var_string <- quo_name(sum_var)
  df <- data %>%
    summarise(mean = mean(!!sum_var), sd = sd(!!sum_var))
  if (!missing(weight_var)) {
    weight_var <- enquo(weight_var)
    df_w <- data %>%
      filter(!is.na(!!weight_var)) %>%
      srvyr::as_survey(!!weight_var) %>%
      srvyr::summarise(
        mean = srvyr::survey_mean(!!sum_var),
        sd = srvyr::survey_sd(!!sum_var)
      ) %>%
      select(mean, sd)
    df <- bind_rows(
      "Non-Weighted" = df,
      "Weighted" = df_w, .id = "Type"
    )
  }
  df %>%
    mutate(
      disp = paste0(round(mean, 2), " (", round(sd, 2), ")"),
      variable = quo_name(sum_var)
    ) %>%
    select(variable, Type, disp) %>%
    pivot_wider(names_from = "Type", values_from = "disp")
}

######################## Tables ################################################
############### Discrete Variables ##############
var_list <- quos(gender, age_group, educ_imp, eastwest)
var_names <- c("Gender", "Age Group", "Education Level", "East or West Germany")
dat_list <- list()
for (i in 1:4) {
  dat_list[[i]] <- summarise_disc_simple(df_filt, variable = !!var_list[[i]]) %>%
    mutate(Type = var_names[i]) %>%
    select(Type, variable, everything())
}
out <- bind_rows(dat_list)
write.xlsx(out, file = "Tables/disc_summary.xlsx", row.names = FALSE)

out <- df %>%
  mutate(wave = as.factor(wave)) %>%
  group_by(wave) %>%
  summarise(All_Samples = n()) %>%
  left_join(
    df_filt %>%
      group_by(wave) %>%
      summarise(Model_Samples = n())
  ) %>%
  mutate(Model_Samples = if_else(is.na(Model_Samples), 0L, Model_Samples))
write.xlsx(out, file = "Tables/nsamples.xlsx", row.names = FALSE)



############### Continuous Variables ################
var_list <- quos(
  polit_int, left_vs_right, tax_and_soc_scale,
  immigration_difficulty_scale, econ_vs_clim
)
dat_list <- list()
for (i in 1:5) {
  dat_list[[i]] <- summarise_cont_simple(df_filt, !!var_list[[i]],
    weight_var = wei5_on
  )
}
out <- bind_rows(dat_list)
write.xlsx(out, file = "Tables/cont_summary.xlsx", row.names = FALSE)

#### Correlations ####
polit_lean_vars <- df %>%
  select(
    econ_vs_clim,
    left_vs_right,
    tax_and_soc_scale,
    immigration_difficulty_scale
  ) %>%
  drop_na() %>%
  as.matrix()

colnames(polit_lean_vars) <- c(
  "Economic Growth vs Climate Change",
  "Self-Assigned Political Leaning",
  "Economic Leaning",
  "Cultural Leaning"
)

cor_test <- list()
r <- 1
P_val_mat <- matrix(NA, nrow = 4, ncol = 4)
for (i in 1:4) {
  for (j in 1:4) {
    cor_test[[r]] <- cor.test(polit_lean_vars[, i], polit_lean_vars[, j])
    if (i == j) {
      P_val_mat[i, j] <- 0
    } else {
      P_val_mat[i, j] <- (cor_test[[r]]$p.value)
    }
    r <- r + 1
  }
}

cor_tab <- round(cor(polit_lean_vars), 2)
P_val_mat
write.xlsx(cor_tab, file = "Tables/leaning_var_corr.xlsx")

############ Party Breakdown #############
df %>%
  group_by(party) %>%
  summarise(across(
    c(left_vs_right, tax_and_soc_scale, immigration_difficulty_scale),
    ~ mean(.x, na.rm = TRUE)
  ))

############ Responses #############
df_exp <- df_filt %>%
  tidyr::expand(lfdn, wave_char) %>%
  left_join(df_filt, by = c("lfdn", "wave_char")) %>%
  mutate(indicator = !is.na(econ_vs_clim)) %>%
  select(lfdn, wave_char, indicator, wave_start, wave_end) %>%
  arrange(lfdn, wave_char) %>%
  group_by(lfdn) %>%
  mutate(
    current_wave = cumsum(indicator),
    nwaves = sum(indicator)
  ) %>%
  ungroup() %>%
  mutate(
    newvar = case_when(
      indicator == TRUE ~ "Response",
      indicator == FALSE & current_wave == 0 ~ "Joining Analysis",
      indicator == FALSE & nwaves == current_wave ~ "Left Analysis",
      indicator == FALSE & current_wave > 0 & current_wave < nwaves ~ "No Response"
    ),
    newvar = factor(newvar, levels = c("Joining Analysis", "Response", "No Response", "Left Analysis")),
    first_response = (wave_char == "1" | lag(newvar) == "Joining Analysis") & newvar == "Response",
    last_response = (wave_char == "23" | lead(newvar) == "Left Analysis") & newvar == "Response"
  )

out <- df_exp %>%
  filter(newvar == "Response") %>%
  group_by(wave_char) %>%
  summarise(
    wave_date = paste0(min(wave_start, na.rm = TRUE), " – ", max(wave_end, na.rm = TRUE)),
    n_response = n(),
    across(
      c(first_response, last_response),
      ~ paste0(sum(.x), " (", round(100 * sum(.x) / n(), 1), "%)")
    )
  ) %>%
  left_join(df %>% group_by(wave_char) %>% summarise(n = n()), by = "wave_char") %>%
  rename(c(
    "Wave" = 1, "Date of Collection" = 2, "N included in our analyses" = 3,
    "N included for first time in our analyses (%)" = 4,
    "N included for last time in our analyses (%)" = 5, "N Total" = n
  )) %>%
  select(1, 2, 6, everything()) %>%
  as.data.frame()

write.xlsx(out, file = "Tables/response_summary.xlsx", row.names = FALSE)
