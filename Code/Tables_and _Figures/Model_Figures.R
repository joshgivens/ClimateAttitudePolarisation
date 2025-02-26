library(tidyr)
library(dplyr)
library(ggplot2)

replace_list <- function(x, values, replacements) {
  c(replacements)[base::match(x, c(values))]
}

# Wave start dates
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
    gg <- temp_dat %>% ggplot(aes(
      x = wave_start_fact, y = predicted, ymin = conf.low, ymax = conf.high,
      linetype = group, group = group
    ))
  } else {
    gg <- temp_dat %>% ggplot(aes(
      x = wave_start, y = predicted, ymin = conf.low, ymax = conf.high,
      linetype = group, group = group
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

plot_func2 <- function(effectdat) {
  effectdat %>% ggplot(aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high)) +
    geom_line() +
    geom_ribbon(alpha = 0.3) +
    labs(y = "Economic Growth vs Climate Change") +
    scale_y_continuous(limits = c(-3, 3), breaks = -3:3) +
    theme_bw()
}

my.ggsave <- function(filename = default_name(plot), plot = last_plot(), height = 3.5, width = 6, dpi = 300, ...) {
  ggsave(filename = filename, plot = plot, height = height, width = width, dpi = dpi, ...)
}
############################### Explanatory Interaction Models ####################################
################## Non-Weighted ######################
#### Separate #####
# self-assigned leaning plot
polit_int_each <- readRDS("ModelData/polit_int_each_eff.RData")

gg1 <- plot_func(polit_int_each$self) +
  scale_linetype_discrete(name = "Self-Assigned\nPolitical Leaning", labels = c("Left", "Centre", "Right"))

# Economic leaning plot
gg2 <- plot_func(polit_int_each$econ) +
  scale_linetype_discrete(name = "Economic Leaning", labels = c("Left", "Centre", "Right"))

# cultural leaning plot
gg3 <- plot_func(polit_int_each$cult) +
  scale_linetype_discrete(name = "Cultural Leaning", labels = c("Left", "Centre", "Right"))


my.ggsave("Figures/ModelFigures/polit_int_each_self.png", gg1)
my.ggsave("Figures/ModelFigures/polit_int_each_econ.png", gg2)
my.ggsave("Figures/ModelFigures/polit_int_each_cult.png", gg3)

#### Separate Econcult LR #####
# self-assigned leaning plot
polit_int_each <- readRDS("ModelData/polit_int_LRmeasures_eff.RData")

# Economic leaning plot
gg2 <- plot_func(polit_int_each$econ) +
  scale_linetype_discrete(name = "Economic Leaning", labels = c("Left", "Centre", "Right"))
# Date Version
gg2_date <- plot_func_date(polit_int_each$econ) +
  scale_linetype_discrete(name = "Economic Leaning", labels = c("Left", "Centre", "Right"))
# Date Version Even Space
gg2_date_uneven <- plot_func_date(polit_int_each$econ, even_space = FALSE) +
  scale_linetype_discrete(name = "Economic Leaning", labels = c("Left", "Centre", "Right"))


# cultural leaning plot
gg3 <- plot_func(polit_int_each$cult) +
  scale_linetype_discrete(name = "Cultural Leaning", labels = c("Left", "Centre", "Right"))
# Date Version
gg3_date <- plot_func_date(polit_int_each$cult) +
  scale_linetype_discrete(name = "Cultural Leaning", labels = c("Left", "Centre", "Right"))
# Date Version Even Space
gg3_date_uneven <- plot_func_date(polit_int_each$cult, even_space = FALSE) +
  scale_linetype_discrete(name = "Cultural Leaning", labels = c("Left", "Centre", "Right"))


my.ggsave("Figures/ModelFigures/polit_int_LRmeasures_econ.png", gg2, height = 3.25, width = 6.5)
my.ggsave("Figures/ModelFigures/polit_int_LRmeasures_econ_date.png", gg2_date, height = 3.75, width = 6.5)
my.ggsave("Figures/ModelFigures/polit_int_LRmeasures_econ_date_uneven.png", gg2_date_uneven, height = 3.75, width = 6.5)

my.ggsave("Figures/ModelFigures/polit_int_LRmeasures_cult.png", gg3, height = 3.25, width = 6.5)
my.ggsave("Figures/ModelFigures/polit_int_LRmeasures_cult_date.png", gg3_date, height = 3.75, width = 6.5)
my.ggsave("Figures/ModelFigures/polit_int_LRmeasures_cult_date_uneven.png", gg3_date_uneven, height = 3.75, width = 6.5)

####  Joint  ######
# self-assigned leaning plot
polit_int_all <- readRDS("ModelData/polit_int_all_eff.RData")
gg1 <- plot_func(polit_int_all$self) +
  scale_linetype_discrete(name = "Self-Assigned\nPolitical Leaning", labels = c("Left", "Centre", "Right"))

# Economic leaning plot
gg2 <- plot_func(polit_int_all$econ) +
  scale_linetype_discrete(name = "Economic Leaning", labels = c("Left", "Centre", "Right"))

# cultural leaning plot
gg3 <- plot_func(polit_int_all$cult) +
  scale_linetype_discrete(name = "Cultural Leaning", labels = c("Left", "Centre", "Right"))


my.ggsave("Figures/ModelFigures/polit_int_all_self.png", gg1)
my.ggsave("Figures/ModelFigures/polit_int_all_econ.png", gg2)
my.ggsave("Figures/ModelFigures/polit_int_all_cult.png", gg3)

#### Econ/Cult ####
# self-assigned leaning plot
polit_int_econcult_test <- readRDS("ModelData/polit_int_econcult_eff.RData")
# Economic leaning plot
gg2 <- plot_func(polit_int_econcult$econ) +
  scale_linetype_discrete(name = "Economic Leaning", labels = c("Left", "Centre", "Right"))
# Date version
gg2_date <- plot_func_date(polit_int_econcult$econ) +
  scale_linetype_discrete(name = "Economic Leaning", labels = c("Left", "Centre", "Right"))
# Uneven date version
gg2_date_uneven <- plot_func_date(polit_int_econcult$econ, even_space = FALSE) +
  scale_linetype_discrete(name = "Economic Leaning", labels = c("Left", "Centre", "Right"))

# cultural leaning plot
gg3 <- plot_func(polit_int_econcult$cult) +
  scale_linetype_discrete(name = "Cultural Leaning", labels = c("Left", "Centre", "Right"))
# Date version
gg3_date <- plot_func_date(polit_int_econcult$cult) +
  scale_linetype_discrete(name = "Cultural Leaning", labels = c("Left", "Centre", "Right"))
# Uneven date version
gg3_date_uneven <- plot_func_date(polit_int_econcult$cult, even_space = FALSE) +
  scale_linetype_discrete(name = "Cultural Leaning", labels = c("Left", "Centre", "Right"))

my.ggsave("Figures/ModelFigures/polit_int_econcult_econ.png", gg2, height = 3.25, width = 6.5)
my.ggsave("Figures/ModelFigures/polit_int_econcult_econ_date.png", gg2_date, height = 3.75, width = 6.5)
my.ggsave("Figures/ModelFigures/polit_int_econcult_econ_date_uneven.png", gg2_date_uneven, height = 3.75, width = 6.5)

my.ggsave("Figures/ModelFigures/polit_int_econcult_cult.png", gg3, height = 3.25, width = 6.5)
my.ggsave("Figures/ModelFigures/polit_int_econcult_cult_date.png", gg3_date, height = 3.75, width = 6.5)
my.ggsave("Figures/ModelFigures/polit_int_econcult_cult_date_uneven.png", gg3_date_uneven, height = 3.75, width = 6.5)

#### Econ/Cult wExtra ####
polit_int_econcult <- readRDS("ModelData/polit_int_econcult_wEconState_eff.RData")
# Economic leaning plot
gg2 <- plot_func(polit_int_econcult$econ) +
  scale_linetype_discrete(name = "Economic Leaning", labels = c("Left", "Centre", "Right"))
# Date version
gg2_date <- plot_func_date(polit_int_econcult$econ) +
  scale_linetype_discrete(name = "Economic Leaning", labels = c("Left", "Centre", "Right"))
# Uneven date version
gg2_date_uneven <- plot_func_date(polit_int_econcult$econ, even_space = FALSE) +
  scale_linetype_discrete(name = "Economic Leaning", labels = c("Left", "Centre", "Right"))

# cultural leaning plot
gg3 <- plot_func(polit_int_econcult$cult) +
  scale_linetype_discrete(name = "Cultural Leaning", labels = c("Left", "Centre", "Right"))
# Date version
gg3_date <- plot_func_date(polit_int_econcult$cult) +
  scale_linetype_discrete(name = "Cultural Leaning", labels = c("Left", "Centre", "Right"))
# Uneven date version
gg3_date_uneven <- plot_func_date(polit_int_econcult$cult, even_space = FALSE) +
  scale_linetype_discrete(name = "Cultural Leaning", labels = c("Left", "Centre", "Right"))

my.ggsave("Figures/ModelFigures/polit_int_econcult wEconState_econ.png", gg2, height = 3.25, width = 6.5)
my.ggsave("Figures/ModelFigures/polit_int_econcult_wEconState_econ_date.png", gg2_date, height = 3.75, width = 6.5)
my.ggsave("Figures/ModelFigures/polit_int_econcult_wEconState_econ_date_uneven.png", gg2_date_uneven, height = 3.75, width = 6.5)

my.ggsave("Figures/ModelFigures/polit_int_econcult_wEconState_cult.png", gg3, height = 3.25, width = 6.5)
my.ggsave("Figures/ModelFigures/polit_int_econcult_wEconState_cult_date.png", gg3_date, height = 3.75, width = 6.5)
my.ggsave("Figures/ModelFigures/polit_int_econcult_wEconState_cult_date_uneven.png", gg3_date_uneven, height = 3.75, width = 6.5)


#### Econ/Cult wAfD ####
polit_int_econcult <- readRDS("ModelData/polit_int_econcult_wAfD_eff.RData")
# Economic leaning plot
gg2 <- plot_func(polit_int_econcult$econ) +
  scale_linetype_discrete(name = "Economic Leaning", labels = c("Left", "Centre", "Right"))
# Date version
gg2_date <- plot_func_date(polit_int_econcult$econ) +
  scale_linetype_discrete(name = "Economic Leaning", labels = c("Left", "Centre", "Right"))
# Uneven date version
gg2_date_uneven <- plot_func_date(polit_int_econcult$econ, even_space = FALSE) +
  scale_linetype_discrete(name = "Economic Leaning", labels = c("Left", "Centre", "Right"))

# cultural leaning plot
gg3 <- plot_func(polit_int_econcult$cult) +
  scale_linetype_discrete(name = "Cultural Leaning", labels = c("Left", "Centre", "Right"))
# Date version
gg3_date <- plot_func_date(polit_int_econcult$cult) +
  scale_linetype_discrete(name = "Cultural Leaning", labels = c("Left", "Centre", "Right"))
# Uneven date version
gg3_date_uneven <- plot_func_date(polit_int_econcult$cult, even_space = FALSE) +
  scale_linetype_discrete(name = "Cultural Leaning", labels = c("Left", "Centre", "Right"))

my.ggsave("Figures/ModelFigures/polit_int_econcult wAfD.png", gg2, height = 3.25, width = 6.5)
my.ggsave("Figures/ModelFigures/polit_int_econcult_wAfD_econ_date.png", gg2_date, height = 3.75, width = 6.5)
my.ggsave("Figures/ModelFigures/polit_int_econcult_wAfD_econ_date_uneven.png", gg2_date_uneven, height = 3.75, width = 6.5)

my.ggsave("Figures/ModelFigures/polit_int_econcult_wAfD_cult.png", gg3, height = 3.25, width = 6.5)
my.ggsave("Figures/ModelFigures/polit_int_econcult_wAfD_cult_date.png", gg3_date, height = 3.75, width = 6.5)
my.ggsave("Figures/ModelFigures/polit_int_econcult_wAfD_cult_date_uneven.png", gg3_date_uneven, height = 3.75, width = 6.5)


#### Econ/Cult LR imp ####
# self-assigned leaning plot
polit_int_econcult_test <- readRDS("ModelData/polit_int_econcult_lr_imp_eff.RData")
# Economic leaning plot
gg2 <- plot_func(polit_int_econcult$econ) +
  scale_linetype_discrete(name = "Economic Leaning", labels = c("Left", "Centre", "Right"))

# cultural leaning plot
gg3 <- plot_func(polit_int_econcult$cult) +
  scale_linetype_discrete(name = "Cultural Leaning", labels = c("Left", "Centre", "Right"))

my.ggsave("Figures/ModelFigures/polit_int_econcult_econ_LR_imp.png", gg2)
my.ggsave("Figures/ModelFigures/polit_int_econcult_cult_LR_imp.png", gg3)
##################   Weighted   ######################
#### Separate #####
# self-assigned leaning plot
polit_int_each_w <- readRDS("ModelData/polit_int_each_eff_w.RData")
gg1 <- plot_func(polit_int_each_w$self) +
  scale_linetype_discrete(name = "Self-Assigned\nPolitical Leaning", labels = c("Left", "Centre", "Right"))

# Economic leaning plot
gg2 <- plot_func(polit_int_each_w$econ) +
  scale_linetype_discrete(name = "Economic Leaning", labels = c("Left", "Centre", "Right")) +
  coord_cartesian(ylim = c(-0.5, 1.7))

# cultural leaning plot
gg3 <- plot_func(polit_int_each_w$cult) +
  scale_linetype_discrete(name = "Cultural Leaning", labels = c("Left", "Centre", "Right"))


my.ggsave("Figures/ModelFigures/polit_int_each_self_w.png", gg1)
my.ggsave("Figures/ModelFigures/polit_int_each_econ_w.png", gg2)
my.ggsave("Figures/ModelFigures/polit_int_each_cult_w.png", gg3)
####  Joint  ######
# self-assigned leaning plot
polit_int_all_w <- readRDS("ModelData/polit_int_all_eff_w.RData")
gg1 <- plot_func(polit_int_all_w$self) +
  scale_linetype_discrete(name = "Self-Assigned\nPolitical Leaning", labels = c("Left", "Centre", "Right"))

# Economic leaning plot
gg2 <- plot_func(polit_int_all_w$econ) +
  scale_linetype_discrete(
    name = "Economic Leaning",
    limits = rev, labels = c("Left", "Centre", "Right")
  )

# cultural leaning plot
gg3 <- plot_func(polit_int_all_w$cult) +
  scale_linetype_discrete(name = "Cultural Leaning", labels = c("Left", "Centre", "Right"))


my.ggsave("Figures/ModelFigures/polit_int_all_self_w.png", gg1)
my.ggsave("Figures/ModelFigures/polit_int_all_econ_w.png", gg2)
my.ggsave("Figures/ModelFigures/polit_int_all_cult_w.png", gg3)
#### Econ/Cult ####
# self-assigned leaning plot
polit_int_econcult_w <- readRDS("ModelData/polit_int_econcult_eff_w.RData")

# Economic leaning plot
gg2 <- plot_func(polit_int_econcult_w$econ) +
  scale_linetype_discrete(
    name = "Economic Leaning",
    limits = rev, labels = c("Left", "Centre", "Right")
  )

# cultural leaning plot
gg3 <- plot_func(polit_int_econcult_w$cult) +
  scale_linetype_discrete(name = "Cultural Leaning", labels = c("Left", "Centre", "Right"))

my.ggsave("Figures/ModelFigures/polit_int_econcult_econ_w.png", gg2)
my.ggsave("Figures/ModelFigures/polit_int_econcult_cult_w.png", gg3)
################################ Basic Models ##################################
#### Separate #####
# self-assigned leaning plot
polit_base_each <- readRDS("ModelData/polit_base_each_eff.RData")
gg1 <- plot_func2(polit_base_each$self) +
  scale_x_continuous(
    name = "Self Assigned Political Leaning", breaks = -5:5,
    minor_breaks = -5:5
  )

# Economic leaning plot
gg2 <- plot_func2(polit_base_each$econ) +
  scale_x_continuous(name = "Economic Leaning", breaks = -3:3, minor_breaks = -3:3)

# cultural leaning plot
gg3 <- plot_func2(polit_base_each$cult) +
  scale_x_continuous(
    name = "Cultural Leaning", breaks = -3:3,
    minor_breaks = -3:3
  )


my.ggsave("Figures/ModelFigures/polit_base_each_self.png", gg1)
my.ggsave("Figures/ModelFigures/polit_base_each_econ.png", gg2)
my.ggsave("Figures/ModelFigures/polit_base_each_cult.png", gg3)

####  Joint #######
# self-assigned leaning plot
polit_base_all <- readRDS("ModelData/polit_base_all_eff.RData")
gg1 <- plot_func2(polit_base_all$self) +
  scale_x_continuous(
    breaks = -5:5, minor_breaks = -5:5,
    name = "Self Assigned Political Leaning"
  )

# Economic leaning plot
gg2 <- plot_func2(polit_base_all$econ) +
  scale_x_continuous(breaks = -3:3, minor_breaks = -3:3, name = "Economic Leaning")

# cultural leaning plot
gg3 <- plot_func2(polit_base_all$cult) +
  scale_x_continuous(breaks = -3:3, minor_breaks = -3:3, name = "Cultural Leaning")

my.ggsave("Figures/ModelFigures/polit_base_all_self.png", gg1)
my.ggsave("Figures/ModelFigures/polit_base_all_econ.png", gg2)
my.ggsave("Figures/ModelFigures/polit_base_all_cult.png", gg3)

#### Econ/Cult ####
# self-assigned leaning plot
polit_base_econcult <- readRDS("ModelData/polit_base_econcult_eff.RData")
# Economic leaning plot
gg2 <- plot_func2(polit_base_econcult$econ) +
  scale_x_continuous(breaks = -3:3, minor_breaks = -3:3, name = "Economic Leaning")


a <- bind_rows(polit_base_econcult, .id = "group") %>%
  mutate(fact_x = factor(x))
to_disp <- -3:3
to_disp[1] <- "-3\n(Left)"
to_disp[7] <- "3\n(Right)"

a %>%
  ggplot(aes(
    x = x, y = predicted, ymin = conf.low, ymax = conf.high,
    linetype = group, group = group
  )) +
  geom_line() +
  geom_ribbon(alpha = 0.3) +
  labs(x = "Ideological Leaning", y = "Economic Growth vs Climate Change") +
  scale_y_continuous(breaks = -3:3, limits = c(-3, 3)) +
  scale_x_continuous(breaks = -3:3, labels = to_disp) +
  theme_bw() +
  scale_linetype_discrete(
    breaks = c("cult", "econ"), labels = c("Cultural", "Economic"),
    name = "Ideological\nLeaning"
  )

# a %>% ggplot(aes(x=group,y=predicted,ymin=conf.low,ymax=conf.high,shape=fact_x))+
# geom_point()+geom_errorbar()

ggsave("Figures/ModelFigures/polit_base_eoncult_both.png")

ggplot(aes)
# cultural leaning plot
gg3 <- plot_func2(polit_base_econcult$cult) +
  scale_x_continuous(breaks = -3:3, minor_breaks = -3:3, name = "Cultural Leaning")


polit_base_all$econ %>% ggplot(aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high)) +
  geom_line() +
  geom_ribbon(alpha = 0.3) +
  labs(y = "Economic Growth vs Climate Change") +
  scale_y_continuous(limits = c(-3, 3), breaks = -3:3) +
  theme_bw()

my.ggsave("Figures/ModelFigures/polit_base_all_econ.png", gg2)
my.ggsave("Figures/ModelFigures/polit_base_all_cult.png", gg3)


############################### All Wave Interaction Models ####################
################## Non-Weighted ######################
#### Separate #####
# self-assigned leaning plot
polit_intall_each <- readRDS("ModelData/polit_intall_each_eff.RData")
gg1 <- plot_func(polit_intall_each$self) +
  scale_linetype_discrete(name = "Self-Assigned\nPolitical Leaning", labels = c("Left", "Centre", "Right"))

# Economic leaning plot
gg2 <- plot_func(polit_intall_each$econ) +
  scale_linetype_discrete(name = "Economic Leaning", labels = c("Left", "Centre", "Right"))

# cultural leaning plot
gg3 <- plot_func(polit_intall_each$cult) +
  scale_linetype_discrete(name = "Cultural Leaning", labels = c("Left", "Centre", "Right"))


my.ggsave("Figures/ModelFigures/polit_intall_each_self.png", gg1)
my.ggsave("Figures/ModelFigures/polit_intall_each_econ.png", gg2)
my.ggsave("Figures/ModelFigures/polit_intall_each_cult.png", gg3)
