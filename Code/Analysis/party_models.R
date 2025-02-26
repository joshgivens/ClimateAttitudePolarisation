library(tidyr)
library(dplyr)
library(lme4)
library(knitr)
library(ggplot2)
library(lmerTest)
library(ggeffects)
library(marginaleffects)
library(gridExtra)
library(xlsx)
library(r2mlm)
library(MuMIn)

wave_start_vec <- as.Date(
  c(
    "2016/10/06", "2017/02/16", "2017/05/11", "2017/07/06",
    "2017/07/20",
    "2017/08/17",
    "2017/09/04", "2017/09/18", "2017/09/27", "2018/03/15", "2018/11/06",
    "2019/05/28", "2019/11/05", "2020/04/21", "2020/11/03", "2021/02/25",
    "2021/05/06", "2021/07/07", "2021/08/11", "2021/09/15", "2021/09/29",
    "2021/12/09", "2022/05/18", "2022/10/12"
  ),
  format = "%Y/%m/%d"
)

replace_list <- function(x, values, replacements) {
  c(replacements)[base::match(x, c(values))]
}

plot_func_cont <- function(effectdat, waves) {
  gg <- effectdat %>% ggplot(aes(
    x = x, y = predicted, ymin = conf.low, ymax = conf.high,
    group = group, color = group, fill = group
  )) +
    geom_line() +
    geom_ribbon(alpha = 0.3) +
    labs(x = "Wave", y = "Economic Growth vs Climate Change", colour = "Party", fill = "Party") +
    scale_y_continuous(breaks = -3:3, limits = c(-3, 3)) +
    scale_linetype_discrete(name = "Party") +
    scale_colour_manual(
      breaks = names(party_colours), labels = names(party_colours),
      values = party_colours, aesthetics = c("colour", "fill")
    ) +
    scale_x_continuous(breaks = waves, labels = as.character(waves), minor_breaks = 1:23) +
    theme_bw()
  return(gg)
}

plot_func_date_cont <- function(effectdat, waves) {
  waves <- sort(waves)
  wave_start <- replace_list(waves, c(1:4, "a1", 5:23), wave_start_vec)
  wave_start_disp <- paste0(waves, " (", format(wave_start, format = "%m/%y"), ")")
  gg <- effectdat %>% ggplot(aes(
    x = x, y = predicted, ymin = conf.low, ymax = conf.high,
    group = group, color = group, fill = group
  )) +
    geom_line() +
    geom_ribbon(alpha = 0.3) +
    labs(x = "Wave", y = "Economic Growth vs Climate Change", colour = "Party", fill = "Party") +
    scale_y_continuous(limits = c(-3, 3), breaks = -3:3) +
    scale_linetype_discrete(name = "Party") +
    scale_colour_manual(
      breaks = names(party_colours), labels = names(party_colours),
      values = party_colours, aesthetics = c("colour", "fill")
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    scale_x_continuous(breaks = 1:23, labels = wave_start_disp)
}


plot_func <- function(effectdat) {
  effectdat %>% ggplot(aes(
    x = x, y = predicted, ymin = conf.low, ymax = conf.high,
    linetype = group, group = group
  )) +
    geom_line() +
    geom_ribbon(alpha = 0.3) +
    labs(x = "Wave", y = "Economic Growth vs Climate Change") +
    scale_y_continuous(breaks = -3:3, limits = c(-3, 3)) +
    theme_bw()
}

plot_func_date <- function(effectdat, even_space = TRUE) {
  temp_dat <- effectdat %>%
    as.data.frame() %>%
    mutate(
      wave_start = replace_list(x, c(1:4, "a1", 5:23), wave_start_vec),
      wave_start_disp = paste0(x, " (", format(wave_start, format = "%m/%y"), ")"),
      wave_start_fact = factor(wave_start_disp, levels = unique(wave_start_disp))
    )
  if (even_space) {
    gg <- ggplot(temp_dat, aes(
      x = wave_start_fact, y = predicted, ymin = conf.low, ymax = conf.high,
      fill = group, color = group, group = group
    ))
  } else {
    gg <- temp_dat %>% ggplot(aes(
      x = wave_start, y = predicted, ymin = conf.low, ymax = conf.high,
      fill = group, color = group, group = group
    ))
  }
  gg <- gg + geom_line() + geom_ribbon(alpha = 0.3) +
    labs(y = "Economic Growth vs Climate Change", x = "Wave (Start Date)") +
    scale_y_continuous(limits = c(-3, 3), breaks = -3:3) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  if (!even_space) {
    gg <- gg + scale_x_date(
      breaks = unique(temp_dat$wave_start),
      minor_breaks = NULL,
      labels = levels(temp_dat$wave_start_fact)
    )
  }
  return(gg)
}

wave_ord <- c(1, 2, 4, "a1", 7, 8, 10:20, 22, 23)
df <- readRDS("Data/full_tidied.rds")
df <- df %>% mutate(
  educ_imp = relevel(droplevels(educ_imp), ref = "Low"),
  wave_char = factor(wave_char, wave_ord)
)

# Not sure whether or not to keep this
df <- df %>% filter(party != "None")

df_rebase <- df %>% mutate(wave_char = relevel(wave_char, ref = "15"))
newlevels <- c("GRUNE", "DIE LINKE", "SPD", "Other", "CDU/CSU", "FDP", "AfD")
df_rebase2 <- df %>% mutate(party = factor(party, levels = newlevels))

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
      # Variable=gsub(":"," - ",Variable,fixed = TRUE),
      Variable = gsub("wave_char(.*):party(.*)", "Wave * Party:   W\\1 * \\2", Variable),
      Variable = gsub("wave:party(.*)", "Wave * Party: Wave * \\1", Variable),
      Variable = gsub("wave_char", "Wave:   W", Variable),
      Variable = gsub("wave", "Wave", Variable),
      Variable = gsub("age_group", "Year of Birth:   ", Variable),
      Variable = gsub("(gender)|(eastwest)", "", Variable),
      Variable = gsub("educ_imp", "Education:   ", Variable),
      Variable = gsub("party_split1", "Party Split:  ", Variable),
      Variable = gsub("party", "Party:   ", Variable),
      Variable = gsub("polit_int", "Political Interest", Variable),
      Variable = gsub("tax_and_soc_scale", "Economic Leaning", Variable),
      Variable = gsub("immigration_difficulty_scale", "Cultural Leaning", Variable),
      Variable = gsub("left_vs_right", "Self Assessed Political Leaning", Variable),
      Variable = gsub("\\(|\\)", "", Variable),
      Estimate = round(Estimate, 2),
      ` ` = case_when(
        `P-value` < 0.001 ~ "***",
        `P-value` < 0.01 ~ "**",
        `P-value` < 0.05 ~ "*",
        TRUE ~ ""
      ),
      `Standard Error` = round(`Standard Error`, 3)
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

my.ggsave <- function(filename = default_name(plot), plot = last_plot(), height = 3,
                      width = 6, dpi = 300, device = "png", ...) {
  ggsave(
    filename = filename, plot = plot, height = height, width = width, dpi = dpi,
    device = device, ...
  )
}

party_colours <- c(
  "#A6006B", "#000000", "#1AA037", "#E3000F", "#FFEF00",
  "#0489DB", "#eb7405"
)
names(party_colours) <- c(
  "SPD", "CDU/CSU", "GRUNE", "DIE LINKE", "FDP",
  "AfD", "Other"
)

################################################################################
######################### Continuous Wave party Models #########################
################################################################################
partysplit_int_cont <- lmer(
  econ_vs_clim ~ age_group + gender + polit_int + eastwest + educ_imp +
    wave * (party_split1) + (1 | lfdn),
  data = df
)

write.xlsx(summary(partysplit_int_cont)$coefficients %>% table_tidy(),
  file = "Tables/partysplit_int_cont_mod.xlsx", row.names = FALSE
)

partysplit_int_cont_eff <- ggeffect(
  partysplit_int_cont, c("wave [1,23]", "party_split1")
)


party_int_cont <- lmer(
  econ_vs_clim ~ age_group + gender + polit_int + eastwest + educ_imp +
    wave * (party) + (1 | lfdn),
  data = df
)

r.squaredGLMM(party_int_cont)
write.xlsx(summary(party_int_cont)$coefficients %>% table_tidy(),
  file = "Tables/party_int_cont_mod.xlsx", row.names = FALSE
)

party_int_cont_forplot <- lmer(
  econ_vs_clim ~ age_group + gender + polit_int + eastwest + educ_imp +
    wave * (party) + (1 | lfdn),
  data = df_rebase2
)
#### Effects plot ####
party_int_cont_eff <- ggeffect(
  party_int_cont_forplot, c("wave [1,23]", "party")
)

gg <- plot_func_cont(party_int_cont_eff, waves = 1:23) +
  coord_cartesian(ylim = c(-1, 1.7))

gg_date <- plot_func_date_cont(party_int_cont_eff, waves = 1:23) +
  coord_cartesian(ylim = c(-1, 1.7))

my.ggsave("Figures/ModelFigures/party_int_cont_test.png", gg, height = 3.25, width = 6.5)
my.ggsave("Figures/ModelFigures/party_int_cont_date.png", gg_date, height = 3.75, width = 6.5)

################################################################################
######################### Discrete Wave party Models ###########################
################################################################################
party_int_disc <- lmer(
  econ_vs_clim ~ age_group + gender + polit_int + eastwest + educ_imp +
    wave_char * (party) + (1 | lfdn),
  data = df
)

write.xlsx(summary(party_int_disc)$coefficients %>% table_tidy(),
  file = "Tables/party_int_disc_mod.xlsx", row.names = FALSE
)
r.squaredGLMM(party_int_disc)

party_int_disc_forplot <- lmer(
  econ_vs_clim ~ age_group + gender + polit_int + eastwest + educ_imp +
    wave_char * (party) + (1 | lfdn),
  data = df_rebase2
)

# Give confidence intervals
# change in climate support in AfD between waves 1 and 23
summary(party_int_disc)$coefficients["wave_char23", 1]
confint(party_int_disc, parm = "wave_char23")
# Change in climate support for Grune between waves 1 and 23
summary(party_int_disc_forplot)$coefficients["wave_char23", 1]
confint(party_int_disc_forplot, parm = "wave_char23")


#### Effects plot ####
party_int_disc_eff <- ggeffect(party_int_disc_forplot, c("wave_char", "party"))

gg <- plot_func(party_int_disc_eff) +
  scale_linetype_discrete(name = "Party") +
  scale_colour_manual(
    breaks = names(party_colours), labels = names(party_colours),
    values = party_colours, aesthetics = c("colour", "fill")
  ) +
  coord_cartesian(ylim = c(-1, 1.7))

gg_date <- plot_func_date(party_int_disc_eff) +
  scale_colour_manual(
    breaks = names(party_colours), labels = names(party_colours),
    values = party_colours, aesthetics = c("colour", "fill"), name = "Party"
  ) +
  coord_cartesian(ylim = c(-1, 1.7))

gg_date_uneven <- plot_func_date(party_int_disc_eff, even_space = FALSE) +
  scale_linetype_discrete(name = "Party") +
  scale_colour_manual(
    breaks = names(party_colours), labels = names(party_colours),
    values = party_colours, aesthetics = c("colour", "fill")
  ) +
  coord_cartesian(ylim = c(-1, 1.7))

my.ggsave("Figures/ModelFigures/party_int_disc.png", gg, height = 3.25, width = 6.5)
my.ggsave("Figures/ModelFigures/party_int_disc_date.png", gg_date, height = 3.75, width = 6.5)
my.ggsave("Figures/ModelFigures/party_int_disc_date_uneven.png", gg_date_uneven, height = 3.75, width = 6.5)

################################################################################
###################### Party vs Left-Right Models ##############################
################################################################################

party_vs_econ_lean <- lmer(
  tax_and_soc_scale ~ -1 + party + (1 | lfdn),
  data = df
)

party_vs_cult_lean <- lmer(
  immigration_difficulty_scale ~ -1 + party + (1 | lfdn),
  data = df
)

base_party_vs_cult_lean <- lmer(immigration_difficulty_scale ~ 1 + (1 | lfdn), data = df)
base_party_vs_econ_lean <- lmer(tax_and_soc_scale ~ 1 + (1 | lfdn), data = df)

sum_tab <- df %>%
  group_by(party) %>%
  summarise(
    econ = mean(tax_and_soc_scale, na.rm = TRUE),
    cult = mean(immigration_difficulty_scale, na.rm = TRUE)
  ) %>%
  as.data.frame()

summary(lm(tax_and_soc_scale ~ -1 + party, data = df))$coefficients
summary(lm(immigration_difficulty_scale ~ -1 + party, data = df))$coefficients

anova(party_vs_cult_lean, base_party_vs_cult_lean)

anova(party_vs_econ_lean, base_party_vs_econ_lean)
joined_table <- left_join(summary(party_vs_cult_lean)$coefficients %>% table_tidy(),
  summary(party_vs_econ_lean)$coefficients %>% table_tidy(),
  by = "Variable",
  suffix = c(" Cultural", " Economic")
) %>%
  select(Variable, contains("Estimate")) %>%
  mutate(across(contains("Estimate"), ~ gsub("\\*", "", .x)))

write.xlsx(summary(party_vs_cult_lean)$coefficients %>% table_tidy(),
  file = "Tables/party_vs_idealog_lean.xlsx", row.names = FALSE, sheetName = "Cultural Leaning",
  append = FALSE
)

write.xlsx(summary(party_vs_econ_lean)$coefficients %>% table_tidy(),
  file = "Tables/party_vs_idealog_lean.xlsx", row.names = FALSE, sheetName = "Economic Leaning",
  append = TRUE
)

write.xlsx(joined_table,
  file = "Tables/party_vs_idealog_lean.xlsx",
  row.names = FALSE, sheetName = "Both", append = TRUE
)

write.xlsx(sum_tab,
  file = "Tables/party_vs_idealog_lean.xlsx",
  row.names = FALSE, sheetName = "Both Basic", append = TRUE
)

# fa483c
# 272cab
