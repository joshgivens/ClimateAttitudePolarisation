library(dplyr)
library(ggplot2)
# Read in the data
df <- readRDS("Data/full_tidied.rds")

# Summarise to individual level data
df_sum <- df %>%
  group_by(lfdn) %>%
  summarise(
    kpx_2290s = first(kpx_2290s),
    gender = first(gender),
    age_group = first(age_group),
    age_group2 = first(age_group2)
  )

ggplot(data = df_sum, aes(x = kpx_2290s)) +
  geom_histogram(binwidth = 1)

ggplot(data = df, aes(x = kpx_2290s)) +
  facet_grid(rows = "gender") +
  geom_histogram(binwidth = 1)

ggplot(data = df_sum, aes(x = age_group)) +
  geom_bar(stat = "count")

ggplot(data = df_sum, aes(x = age_group)) +
  facet_grid(rows = "gender") +
  geom_bar(stat = "count")

ggplot(data = df_sum, aes(x = age_group2)) +
  geom_bar(stat = "count")

ggplot(data = df_sum, aes(x = age_group2)) +
  facet_grid(rows = "gender") +
  geom_bar(stat = "count")


#####################################################################
####################### Check linear models #########################
#####################################################################
library(lme4)
# Fit linear model (naive cohort based)
fit <- lm(econ_vs_clim ~ age_group + gender + I(wave - 1) + left_vs_right +
  eastwest, data = df)
summary(fit)

# Fit mixed effects model
fit2 <- lmer(econ_vs_clim ~ age_group +
  gender + left_vs_right + I(wave - 1) + eastwest + (1 | lfdn), data = df)
summary(fit2)

# Fit linear model (naive cohort based)
fit3 <- lm(econ_vs_clim ~ age_group2 + gender + I(wave - 1) + left_vs_right +
  eastwest, data = df)
summary(fit3)

# Fit mixed effects model
fit4 <- lmer(econ_vs_clim ~ age_group2 +
  gender + left_vs_right + I(wave - 1) + eastwest + (1 | lfdn), data = df)
summary(fit4)
