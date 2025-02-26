library(dplyr)

df <- readRDS("Data/full_raw.rds")

wave_df <- df %>%  group_by(wave) %>% summarise(across(.fns=~any(!is.na(.x))))


econ_mod <- wave_df %>% select(wave,X1290,X2880d,X2880j,X1090) %>% 
  arrange(wave)

cult_mod <- wave_df %>% select(wave,X1290,X1210,X1411,X2880x,X1130) %>% 
  arrange(wave) 
