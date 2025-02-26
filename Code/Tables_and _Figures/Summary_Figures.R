library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(RColorBrewer)
library(tidyr)
library(ggsankey)
library(agrmt)

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

wave_disp <- format(wave_start_vec, format = "%m/%d")

df <- readRDS("Data/full_tidied.rds")
# First filter out to get the data we use in our Final analysis
df_filt <- df %>% filter(if_all(
  c(
    econ_vs_clim, wave_char, age_group,
    gender, left_vs_right, polit_int, eastwest, educ_imp, tax_and_soc_scale,
    immigration_difficulty_scale,
    lfdn
  ),
  ~ !is.na(.x)
))

df_filt2 <- df %>% filter(if_all(
  c(
    econ_vs_clim, wave_char, age_group,
    gender, polit_int, eastwest, educ_imp, party, lfdn
  ),
  ~ !is.na(.x)
))

ord2_ind <- c(1, 2, 4, 5, 8, 9, 11:21, 23:24)
wave_ord <- c(1, 2, 4, "a1", 7, 10:16, 19, 22:23)
wave_ord2 <- c(1, 2, 4, "a1", 7, 8, 10:20, 22:23)
wave_date2 <- wave_start_vec[ord2_ind]
df_filt <- df_filt %>% mutate(wave_char = factor(wave_char, levels = wave_ord))
df_filt2 <- df_filt2 %>% mutate(wave_char = factor(wave_char, levels = wave_ord2))

########################### Functions ##########################################
disc_plot <- function(data, sum_var) {
  sum_var <- enquo(sum_var)
  df_temp <- data %>%
    group_by(wave_char, !!sum_var) %>%
    summarise(count = n()) %>%
    mutate(
      prop = count / sum(count), disp = paste0(count, " ", round(100 * prop), "%"),
      dispplot = gsub("\\s", "\n", disp), wavedisp = paste0(wave_char, " (N=", sum(count), ")")
    )

  table <- df_temp %>%
    pivot_wider(id_cols = wavedisp, names_from = !!sum_var, values_from = disp)
  table <- table %>%
    kable("latex", col.names = c("Wave", colnames(table)[-1]))

  gg <- ggplot(df_temp, aes(x = wave_char, y = prop, fill = !!sum_var)) +
    geom_bar(stat = "identity", position = position_stack()) +
    geom_text(aes(label = dispplot), position = position_stack(vjust = 0.5), size = 3, lineheight = 0.75) +
    theme_bw() +
    labs(y = "Proportion", x = "Wave")

  return(list(data = df_temp, table = table, plot = gg))
}

cont_plot <- function(data, sum_var, labs = NULL, weight_var = NULL, date = FALSE, even_date = TRUE) {
  sum_var <- enquo(sum_var)
  var_string <- quo_name(sum_var)
  ylim <- c(min(data[[var_string]]), max(data[[var_string]]))
  y_breaks <- sort(unique(data[[var_string]]))

  if (!missing(weight_var)) {
    weight_var <- enquo(weight_var)
    mean <- srvyr::survey_mean
    sd <- srvyr::survey_sd
    group_by <- srvyr::group_by
    summarise <- srvyr::summarise

    # Add in weightings
    data <- data %>%
      filter(!is.na(!!weight_var)) %>%
      srvyr::as_survey(!!weight_var)
    weight_str <- "Weighted"
  } else {
    weight_str <- "Non-Weighted"
  }
  df_temp <- data %>%
    group_by(wave_char) %>%
    summarise(mean = mean(!!sum_var), sd = sd(!!sum_var), wave_start = first(wave_start)) %>%
    mutate(
      lower = mean - sd, upper = mean + sd,
      disp = paste0(signif(mean, 3), " (", signif(sd, 3), "%)"),
      disp_plot = gsub("\\s", "\n", disp),
      wave_start_disp = paste0(wave_char, " (", format(wave_start, format = "%m/%y"), ")"),
      wave_start_fact = factor(wave_start_disp, levels = unique(wave_start_disp))
    )

  table <- df_temp %>% select(wave_char, disp_plot)
  colnames(table)[2] <- quo_name(sum_var)
  table <- table %>%
    kable("latex")
  if (date) {
    xlabel <- "Wave (Date)"
    if (even_date) {
      gg <- ggplot(df_temp, aes(x = wave_start_fact, y = mean, ymin = lower, ymax = upper, group = 1))
    } else {
      gg <- ggplot(df_temp, aes(x = wave_start, y = mean, ymin = lower, ymax = upper, group = 1))
    }
  } else {
    xlabel <- "Wave"
    gg <- ggplot(df_temp, aes(x = wave_char, y = mean, ymin = lower, ymax = upper, group = 1))
  }
  gg <- gg + # geom_point()+
    geom_line() +
    # geom_text(aes(label=disp_plot),size=3,lineheight=0.75)+
    # geom_ribbon(alpha=0.3)+
    labs(y = var_string, x = xlabel) +
    theme_bw()


  if (!is.null(labs)) {
    gg <- gg + scale_y_continuous(
      breaks = labs, labels = names(labs),
      limits = ylim, minor_breaks = y_breaks
    )
  } else {
    gg <- gg + scale_y_continuous(limits = ylim, minor_breaks = y_breaks)
  }
  if (date) {
    gg <- gg +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
    if (!even_date) {
      gg <- gg + scale_x_date(
        breaks = unique(df_temp$wave_start),
        minor_breaks = NULL,
        labels = levels(df_temp$wave_start_fact)
      )
    }
  }
  return(list(data = df_temp, table = table, plot = gg))
}

my.ggsave <- function(filename = default_name(plot), plot = last_plot(), height = 3.5, width = 6.75, dpi = 300, ...) {
  ggsave(filename = filename, plot = plot, height = height, width = width, dpi = dpi, ...)
}
############################# Wave Based Summaries #############################
########### Discrete ############
###### Gender #####
out <- disc_plot(df_filt, gender)$plot + labs(fill = "Gender")
my.ggsave("Figures/SummaryFigures/gender_bywave.png", out)

###### Age Group #####
out <- disc_plot(df_filt, age_group)$plot + labs(fill = "Age Group")
my.ggsave("Figures/SummaryFigures/agegroup_bywave.png", out)

##### EastWest #####
out <- disc_plot(df_filt, eastwest)$plot + labs(fill = "East/West \nGermany")
my.ggsave("Figures/SummaryFigures/eastwest_bywave.png", out)

##### Education #####
out <- disc_plot(df_filt, educ_imp)$plot + labs(fill = "Education\nLevel")
my.ggsave("Figures/SummaryFigures/education_bywave.png", out)

##### Party #####
party_colours <- c(
  "#A6006B", "#000000", "#1AA037", "#E3000F",
  "#FFEF00", "#0489DB", "#eb7405", "#9d9bb0"
)
names(party_colours) <- c(
  "SPD", "CDU/CSU", "GRUNE", "DIE LINKE",
  "FDP", "AfD", "Other", "None"
)

out <- disc_plot(df_filt2, party)$plot +
  labs(fill = "Pary") + scale_fill_manual(
    values = party_colours, labels = names(party_colours),
    breaks = names(party_colours)
  )
out$layers[[2]] <- NULL
my.ggsave("Figures/SummaryFigures/party_bywave.png", out)

partysplit_colours <- party_colours[c("AfD", "GRUNE", "Other")]
out <- disc_plot(df_filt2, party_split1)$plot +
  labs(fill = "Pary") + scale_fill_manual(
    values = partysplit_colours, labels = names(partysplit_colours),
    breaks = names(partysplit_colours)
  )
my.ggsave("Figures/SummaryFigures/partysplit_bywave.png", out)

######## Create new Discrete Variables #######
df_newvars <- df_filt %>%
  mutate(
    across(
      .cols = c(
        "left_vs_right", "tax_and_soc_scale",
        "immigration_difficulty_scale"
      ),
      .fns = ~ (factor(sign(.x), levels = -1:1, labels = c("Left", "Centre", "Right")))
    ),
    econ_vs_clim = factor(sign(econ_vs_clim),
      levels = 1:-1,
      labels = c("Climate\nChange", "Equal", "Economy")
    )
  )

out <- disc_plot(df_newvars, left_vs_right)$plot +
  labs(fill = "Self\nAssigned\nPolitical\nLeaning") +
  scale_fill_discrete(type = brewer.pal(3, "PRGn"))
my.ggsave("Figures/SummaryFigures/politlean_disc_bywave.png", out)

out <- disc_plot(df_newvars, tax_and_soc_scale)$plot +
  labs(fill = "Economic\nLeaning") +
  scale_fill_discrete(type = brewer.pal(3, "PRGn"))
my.ggsave("Figures/SummaryFigures/econlean_disc_bywave.png", out)


out <- disc_plot(df_newvars, immigration_difficulty_scale)$plot +
  labs(fill = "Cultural\nLeaning") +
  scale_fill_discrete(type = brewer.pal(3, "PRGn"))
my.ggsave("Figures/SummaryFigures/cultlean_disc_bywave.png", out)

out <- disc_plot(df_newvars, econ_vs_clim)$plot +
  labs(fill = "Climate\nChange\nImportance") +
  scale_fill_discrete(type = brewer.pal(3, "PRGn"))
my.ggsave("Figures/SummaryFigures/climimp_disc_bywave.png", out)


########### Continuous ##########
##### Political Interest ####
# labs=c(-2,0,2)
# names(labs)=c("Low\nPolitical\nInterest", "Middling\nPolitical\nInterest",
#               "High\nPolitical\nInterest")
out <- cont_plot(df_filt, polit_int)$plot +
  labs(y = "Political Interest", title = "")
my.ggsave("Figures/SummaryFigures/politint_bywave.png", out)

###### Self-Assigned Political Leaning ####
## Non-weighted ##
# labs=c(-5,0,5)
# names(labs)=c("Left", "Centre", "Right")
out <- cont_plot(df_filt, left_vs_right)$plot +
  labs(y = "Self-Assigned Political Leaning") +
  scale_y_continuous(breaks = -5:5, minor_breaks = -5:5, limits = c(-5, 5))
my.ggsave("Figures/SummaryFigures/politlean_bywave.png", out)

## Weighted ##
out <- cont_plot(df_filt, left_vs_right, weight_var = wei5_on)$plot +
  labs(y = "Self-Assigned Political Leaning") +
  scale_y_continuous(breaks = -5:5, minor_breaks = -5:5, limits = c(-5, 5))
my.ggsave("Figures/SummaryFigures/politlean_bywave_w.png", out)

#### Economic Leaning ####
## Non-Weighted ##
# labs=c(-3,0,3)
# names(labs)=c("Left", "Centre", "Right")
out <- cont_plot(df_filt, tax_and_soc_scale)$plot +
  labs(y = "Economic Leaning") +
  scale_y_continuous(breaks = -3:3, minor_breaks = -3:3, limits = c(-3, 3))
my.ggsave("Figures/SummaryFigures/econlean_bywave.png", out)

## Weighted ##
out <- cont_plot(df_filt, tax_and_soc_scale, weight_var = wei5_on)$plot +
  labs(y = "Economic Leaning") +
  scale_y_continuous(breaks = -3:3, minor_breaks = -3:3, limits = c(-3, 3))
out$table
my.ggsave("Figures/SummaryFigures/econlean_bywave_w.png", out)

#### Cultural Leaning ####
## Non-Weighted ##
# names(labs)=c("Left", "Centre", "Right")
out <- cont_plot(df_filt, immigration_difficulty_scale)$plot +
  labs(y = "Cultural Leaning") +
  scale_y_continuous(breaks = -3:3, minor_breaks = -3:3, limits = c(-3, 3))
my.ggsave("Figures/SummaryFigures/cultlean_bywave.png", out)

## Weighted ##
out <- cont_plot(df_filt, immigration_difficulty_scale, weight_var = wei5_on)$plot +
  labs(y = "Cultural Leaning") +
  scale_y_continuous(breaks = -3:3, minor_breaks = -3:3, limits = c(-3, 3))
my.ggsave("Figures/SummaryFigures/cultlean_bywave_w.png", out)

#### Climate Importance ####
## Non-Weighted ##
# names(labs)=c("Economic\nGrowth", "Equal", "Climate\nChange")
gg <- cont_plot(df_filt2, econ_vs_clim)$plot +
  labs(y = "Economic Growth vs Climate Change") +
  scale_y_continuous(breaks = -3:3, minor_breaks = -3:3, limits = c(-3, 3))

gg_date <- cont_plot(df_filt2, econ_vs_clim, date = TRUE, even_date = TRUE)$plot +
  geom_point() +
  labs(y = "Economic Growth vs Climate Change") +
  scale_y_continuous(breaks = -3:3, minor_breaks = -3:3, limits = c(-3, 3))

gg_date_uneven <- cont_plot(df_filt2, econ_vs_clim, date = TRUE, even_date = FALSE)$plot +
  labs(y = "Economic Growth vs Climate Change") +
  scale_y_continuous(breaks = -3:3, minor_breaks = -3:3, limits = c(-3, 3))
my.ggsave("Figures/SummaryFigures/climimp_bywave.png", gg, height = 3.25, width = 6.5)
my.ggsave("Figures/SummaryFigures/climimp_bywave_date.png", gg_date, height = 3.75, width = 6.5)
my.ggsave("Figures/SummaryFigures/climimp_bywave_date_uneven.png", gg_date_uneven, height = 3.75, width = 6.5)

## Weighted ##
gg2 <- cont_plot(df_filt, econ_vs_clim, weight_var = wei5_on)$plot +
  labs(y = "Economic Growth vs Climate Change") +
  scale_y_continuous(breaks = -3:3, minor_breaks = -3:3, limits = c(-3, 3))
my.ggsave("Figures/SummaryFigures/climimp_bywave_w.png", gg2)


#########################################
###### Climate importance Agreement #####
#########################################
for_aggr <- df_filt2 %>%
  group_by(wave_char, econ_vs_clim) %>%
  summarise(count = n())
agreements <- rep(NA, length(wave_ord2))
for (i in 1:length(wave_ord2)) {
  dat <- (for_aggr %>% filter(wave_char == wave_ord2[i]))$count
  agreements[i] <- agreement(dat)
}
print(agreements)


agreement_df <- data.frame(waves = factor(wave_ord2, levels = wave_ord2), agreements = agreements)
gg <- ggplot(agreement_df, aes(x = waves, y = agreements, group = 1)) +
  geom_line() +
  geom_point() +
  labs(x = "Wave", y = "Agreement") +
  coord_cartesian(ylim = c(-1, 1)) +
  scale_x_discrete(labels = paste0(wave_ord2, " (", format(wave_date2, "%m/%y"), ")")) +
  scale_y_continuous(
    name = "Agreement Level",
    breaks = c(-1, 0, 1), limits = c(-1, 1),
    labels = c("-1\n(Polarisation) ", " 0\n(No Agreement)\n", " 1\n(Agreement)")
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
    axis.text.y = element_text(angle = 90, vjust = 1, hjust = 0.5),
    plot.margin = unit(
      c(0.3, 0.1, 0.1, 0.1),
      "inches"
    )
  )
gg
my.ggsave("Figures/SummaryFigures/climafree_bywave.png", gg, height = 4, width = 6.5)

######################################################
################ Climate Importance Bar Plot #########
######################################################
df_clim_imp_bar <- df_filt2 %>%
  mutate(econ_vs_clim = factor(econ_vs_clim, levels = 3:-3)) %>%
  group_by(wave_char, econ_vs_clim) %>%
  summarise(count = n()) %>%
  mutate(
    prop = count / sum(count), disp = paste0(round(100 * prop), "%"),
    dispplot = gsub("\\s", "\n", disp), wavedisp = paste0(wave_char, " (N=", sum(count), ")")
  )

labs <- c(
  "     Climate \n 3  takes \n     precedence", " 2", " 1", " 0", "-1", "-2",
  "     Economy\n-3  takes \n     precedence"
)

gg <- ggplot(df_clim_imp_bar, aes(x = wave_char, y = prop, fill = econ_vs_clim)) +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_fill_manual(
    values = rev(brewer.pal(7, "RdBu")),
    labels = labs, name = "Economic Growth \nvs \nClimate Change"
  ) +
  geom_text(aes(label = dispplot), position = position_stack(vjust = 0.5), size = 3, lineheight = 0.75) +
  theme_minimal() +
  scale_x_discrete(labels = paste0(wave_ord2, " (", format(wave_date2, "%m/%y"), ")")) +
  scale_y_continuous(
    expand = c(0, 0), breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0%", "25%", "50%", "75%", "100%")
  ) +
  theme(
    legend.title.align = 0.5,
    axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white"),
    axis.ticks.y = element_line(colour = "darkgrey")
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(y = "Proportion", x = "Wave")
my.ggsave("Figures/SummaryFigures/clim_prop_bywave.png", gg, height = 6, width = 8)
#######################################################
################### Sankey Diagrams ###################
#######################################################


party_colours <- c(
  "#A6006B", "#000000", "#1AA037", "#E3000F",
  "#FFEF00", "#0489DB", "#eb7405", "#9d9bb0"
)
names(party_colours) <- c(
  "SPD", "CDU/CSU", "GRUNE", "DIE LINKE",
  "FDP", "AfD", "Other", "None"
)

ggplot(
  df_filt2 %>% filter(wave_char != "a1") %>% mutate(freq = 1),
  aes(
    x = wave_char, stratum = party, alluvium = lfdn,
    y = freq,
    fill = party, label = party
  )
) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = 1, width = 1 / 2) +
  # geom_text(stat = "stratum", size = 3) +
  # theme(legend.position = "none") +
  ggtitle("vaccination survey responses at three points in time") +
  scale_fill_manual(
    values = party_colours, labels = names(party_colours),
    breaks = names(party_colours)
  )


## Responses
df_exp <- df_filt %>%
  expand(lfdn, wave_char) %>%
  left_join(df_filt, by = c("lfdn", "wave_char")) %>%
  mutate(indicator = !is.na(econ_vs_clim)) %>%
  select(lfdn, wave_char, indicator) %>%
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
    newvar = factor(newvar, levels = c("Joining Analysis", "Response", "No Response", "Left Analysis"))
  ) %>%
  group_by(lfdn) %>%
  filter(!(newvar == "Joining Analysis" & lead(newvar) == "Joining Analysis") &
    !(newvar == "Left Analysis" & lag(newvar) == "Left Analysis")) %>%
  mutate(stratalpha = if_else(newvar %in% c("Joining Analysis", "Left Analysis"), -1, 1))


response_levels <- c("Left Analysis", "No Response", "Response", "Joining Analysis")

df_exp_2 <- df_exp %>%
  ungroup() %>%
  pivot_wider(
    id_cols = "lfdn", names_from = "wave_char",
    values_from = newvar
  ) %>%
  select(-lfdn) %>%
  make_long(`1`, `2`, `4`, a1, `7`, `10`, `11`, `12`, `13`, `14`, `15`) %>%
  mutate(node_factor = factor(
    node,
    levels = response_levels
  )) %>%
  filter(!is.na(node)) %>%
  group_by(x, node) %>%
  mutate(lab = paste0(n())) %>%
  ungroup()



ggplot(df_exp_2, aes(
  x = x,
  next_x = next_x,
  node = node_factor,
  next_node = next_node,
  fill = node_factor,
  label = lab
)) +
  geom_sankey(flow.alpha = 0.75, node.color = NA, width = 0.2) +
  geom_sankey_label(fill = "White", alpha = 0.5) +
  theme_sankey() +
  labs(x = "Wave", fill = "Response Type") +
  scale_fill_manual(
    breaks = response_levels,
    values = c("#807f7e", "#e41a1c", "#377eb8", "#4daf4a"),
    guide = guide_legend(reverse = TRUE)
  )

my.ggsave("Figures/SummaryFigures/response_bywave.png", width = 9, height = 4.47)
