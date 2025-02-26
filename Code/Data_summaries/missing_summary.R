source("Code/Data_summaries/plot_miss.R")
library(dplyr)
library(ggplot2)
library(haven)
library(tidyr)

#######################################################################
########################## Weight Summary #############################
#######################################################################
####### Cast 1to9 ##########
# Read in waves 1 to 9
wave_1_9_raw <- read_dta("Rawdata/ZA6838_w1to9_sA_v5-0-0.dta")
plot.miss(wave_1_9_raw, cols = 11:20)


# wei is a subset of wei2
#  wei3 almost appears to be complement of we2

#######################################################################
######################### imp vars ####################################
#######################################################################
df <- readRDS("Data/full_tidied.rds")

plot.miss(df %>% filter(wave == 15),
  cols = c(
    "gender_eq_imp", "immigration_imp",
    "soc_serv_imp", "foreign_int_imp",
    "econ_and_clim_imp", "combat_terror_imp"
  ), leftmar = 10
)
