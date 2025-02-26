source("Code/Data_summaries/plot_miss.R")
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)


df <- readRDS("Data/full_tidied.rds")
# First filter out to get the data we use in our Final analysis
df_filt <-  df %>% filter(if_all(
  c(econ_vs_clim,wave_char,age_group,
    gender,left_vs_right,polit_int,eastwest,educ_imp,
    populism_imp,extreme_imp,tax_and_soc_scale,immigration_difficulty_scale,lfdn),
  ~!is.na(.x)))



plot.miss(df_filt,cols=c("wei_on","wei2_on","wei3_on","wei4_on","wei5_on"))

# Main weight we want to use is wei5_on
mean(!is.na(df_filt$wei5_on))
#lose over 30% of our observations 

# Now give one observation per person
df_sum <- df_filt %>% group_by(lfdn) %>% summarise(
  across(contains("wei"),~min(.x,na.rm=TRUE)),
  across(contains("wei"),~if_else(is.infinite(.x),as.numeric(NA),.x))
)

plot.miss(df_sum,cols=c("wei_on","wei2_on","wei3_on","wei4_on","wei5_on"))
# It looks like weighting is cross-sectional
# Check wei5_on again
mean(!is.na(df_sum$wei5_on))


# Do weight tests
weight_test <- df_filt %>% group_by(lfdn) %>% 
  mutate(across(contains("wei"),as.numeric)) %>% 
  summarise(across(contains("wei"),.fns=~max(.x,na.rm=TRUE)-min(.x,na.rm=TRUE))) %>% 
  mutate(across(contains("wei"),~if_else(is.infinite(.x),as.numeric(NA),.x)))

# Looking at this, weights 4 and 5 vary between waves while 1,2,3 stay constant.
# Find out what waves the weights are present for
df_wavesum <- df_filt %>% group_by(wave) %>% summarise(
  across(contains("wei"),~mean(!is.na(.x)))
)
print(df_wavesum)
# From this we see that wei 5 does seem to be the way to move forward. 
# As these weights are given for those after wave 5 it appears to be valid to do this.


df_test <- df_filt %>% mutate(current_age=year(wave_end)-kpx_2290s)%>%
  group_by(lfdn) %>% arrange(lfdn,wave_end) %>% 
  mutate(across(c(current_age,educ_imp,eastwest,contains("wei")),
                .fns=list(lag=lag),.names="{.col}_{.fn}"),
         current_age_lag=current_age_lag-1,current_age=current_age+1) %>% 
    ungroup() %>% 
    mutate(across(c(current_age_lag,current_age),
                  ~case_when(.x<=30~"18-30",
                             .x<=45~"30-45",
                             .x<=60~"45-60",
                             TRUE ~ "60+")),
           age_fl=(current_age!=current_age_lag),
           educ_fl=(educ_imp!=educ_imp_lag),
           eastwest_fl=(eastwest!=eastwest_lag),
           weight4_fl=(wei4_on!=wei4_on_lag),
           weight5_fl=(wei5_on!=wei5_on_lag))
## From this data it's clear that the changing weights aren't to do with changing characteristics.

## Now explore if it's a result of category
# Do weight tests
weight_test2 <- df_filt %>% group_by(lfdn) %>% arrange(lfdn,wave_end) %>% 
  mutate(across(contains("wei"),as.numeric)) %>% 
  summarise(across(contains("wei"),.fns=~(max(.x,na.rm=TRUE)-min(.x,na.rm=TRUE))!=0),
            across(c(eastwest,educ_imp,gender),first)) %>% 
  mutate(across(contains("wei"),~if_else(is.infinite(.x),as.numeric(NA),.x)))
