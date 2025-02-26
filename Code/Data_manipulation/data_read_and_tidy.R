library(haven)
library(dplyr)
library(tidyr)
####### Cast 1to9 ##########
#Read in waves 1 to 9
wave_1_9_raw <- read_dta("Rawdata/ZA6838_w1to9_sA_v5-0-0.dta") %>%
  zap_labels()

colnames_long <- NA
for (i in 1:dim(wave_1_9_raw)[2]){
  colnames_long[i] <- attr(wave_1_9_raw[[i]],"label")
}

#get interesting columns
#Set up sruvey independent vars
var_cols <- c("study","sample","n_participation","p_participation",
              "access_panel","ostwest","lfdn","kpx_2280","kpx_2280flag",
              "kpx_2290s","kpx_2290flag")
#set up useful survey vars
q_cols <- c("1500","010","1290","1300","2090a","2090b","2095a","2095b",
            "1285","1287","2880bf","2880ar","2880bj","1090",
            "2880j","2880d","2880x","1411","1210",
            "1130","3103","060","1160",
            "1100","1140","1220","1300","1421","2320","780","790","820")


wave_1_9_sel <- wave_1_9_raw %>% select(all_of(var_cols),matches(paste0("_",q_cols)),
                                        matches("wei"))

# Make long format with row for each participant wave combo
dat_1_9 <- wave_1_9_sel %>% 
  pivot_longer(cols=-c(all_of(var_cols),matches("wei")),
               names_to = c("wave",".value"),
               names_pattern = "kp(.*)_(.*)") %>% 
  filter(!is.na(wave))

rm(wave_1_9_raw, wave_1_9_sel)
################################################################################
################################# Bind waves 10-15 #############################
################################################################################
file_list_10_15 <- paste0("ZA6838_w",10:15,"_sA_v5-0-0.dta")
wave_10_15 <- as.character(10:15)
inlist_10_15 <- list()
outlist_10_15 <- list()
#Create list with tidied version of each data frame
for (i in 1:length(wave_10_15)){
  filepath=paste0("Rawdata/",file_list_10_15[i])
  inlist_10_15[[i]] <- read_dta(filepath) %>% zap_labels()
  outlist_10_15[[i]] <- inlist_10_15[[i]]
  colnames(outlist_10_15[[i]]) <- gsub("kp\\d+_","",colnames(inlist_10_15[[i]]))
  
  outlist_10_15[[i]] <- outlist_10_15[[i]] %>%
    select(all_of(var_cols),starts_with(q_cols),matches("wei")) %>%
    mutate(wave=wave_10_15[i]) %>%
    select(all_of(var_cols),"wave",everything())
}
dat_10_15 <- bind_rows(outlist_10_15)
rm(inlist_10_15, outlist_10_15)
################################################################################
############################## Waves 16-23 #####################################
################################################################################
file_list_16_23 <- c(
  "ZA7722_v2-0-0_wave16.dta", "ZA7723_v2-0-0_wave17.dta",
  "ZA7724_v2-0-0_wave18.dta", "ZA7725_v1-0-0_wave19.dta",
  "ZA7726_v2-0-0_wave20.dta", "ZA7727_v1-0-0_wave21.dta",
  "ZA7728_v1-0-0_wave22.dta", "ZA7729_v1-0-0_wave23.dta")

inlist_16_23 <- list()
outlist_16_23 <- list()

q_cols_16_23 <- c(q_cols,"2280","2290", "2290s","2601")
wave_16_23 <- as.character(16:23)
for (i in 1:length(wave_16_23)){
  filepath=paste0("Rawdata/",file_list_16_23[i])
  inlist_16_23[[i]] <- read_dta(filepath) %>% zap_labels()
  outlist_16_23[[i]] <- inlist_16_23[[i]]
  colnames(outlist_16_23[[i]]) <- gsub("kp\\d+_","",colnames(inlist_16_23[[i]]))
  
  outlist_16_23[[i]] <- outlist_16_23[[i]] %>%
    select("lfdn", starts_with(q_cols_16_23),matches("wei")) %>%
    mutate(wave=wave_16_23[i]) 
}

dat_16_23 <- bind_rows(outlist_16_23) %>% rename("kpx_2280"="2280")
rm(inlist_16_23, outlist_16_23)
################################################################################
############################### Bind all 3 & do imputation #####################
################################################################################
dat_1_23 <- bind_rows(dat_1_9, dat_10_15, dat_16_23) %>%
  arrange(lfdn, wave) %>% 
  select("study","sample","lfdn","wave",everything()) %>% 
  mutate(wave_char=wave, wave=as.numeric(if_else(wave=="a1","4.5",wave)))

dat_1_23_imputed <- dat_1_23 %>% group_by(lfdn) %>% arrange(lfdn,wave) %>% 
  fill(study,sample, kpx_2280, kpx_2290s, ostwest, .direction = "down") %>% 
  ungroup()

colnames(dat_1_23_imputed) = gsub("(^\\d)","X\\1",colnames(dat_1_23_imputed))
################################################################################
########################################## Save ################################
################################################################################
rm(dat_1_23)
saveRDS(dat_1_23_imputed,"Data/full_raw.rds")
write.csv(dat_1_23_imputed,"Data/full_raw.csv")



################################################################################
################################ Testing #######################################
################################################################################
# dat_1_23_imputed_original <- readRDS("Data/full_raw.rds")
# dat_10_15_test <- dat_10_15
# dat_16_23_test <- dat_16_23
# colnames(dat_10_15_test) = gsub("(^\\d)","X\\1",colnames(dat_10_15_test))
# colnames(dat_16_23_test) = gsub("(^\\d)","X\\1",colnames(dat_16_23_test))
# 
# colnames(dat_1_23_imputed_original)[
#   !(colnames(dat_1_23_imputed_original) %in% colnames(dat_10_15_test))]
# colnames(dat_1_23_imputed_original)[
#   !(colnames(dat_1_23_imputed_original) %in% colnames(dat_16_23_test))]
# 
# 
# colnames(dat_1_23_imputed_original)[
#   !(colnames(dat_1_23_imputed_original) %in% colnames(dat_1_23_imputed))]
# 
# 
# # Look for missing cols
# missing_10_15 <- c("2880x","2320","2601")
# missing_16_23 <- c("060", "1220", "1285", "1287", "2880bf","2880bj")
# 
# for (i in 1:6){
#   print(colnames(inlist_10_15[[i]])[
#     grepl(paste0(missing_10_15,collapse = "|"),colnames(inlist_10_15[[i]]))])
# }
# 
# for (i in 1:8){
#   print(colnames(inlist_16_23[[i]])[
#     grepl(paste0(missing_16_23,collapse = "|"),colnames(inlist_16_23[[i]]))])
# }
# 
# for (i in 1:8){
#   print(colnames(outlist_16_23[[i]])[
#     grepl(paste0(missing_16_23,collapse = "|"),colnames(outlist_16_23[[i]]))])
# }
# 
# # Do weight tests
# weight_test <- dat_1_23 %>% group_by(lfdn) %>% 
#   mutate(across(contains("wei"),as.numeric)) %>% 
#   summarise(across(contains("wei"),.fns=~max(.x,na.rm=TRUE)-min(.x,na.rm=TRUE))) %>% 
#   mutate(across(contains("wei"),~if_else(is.infinite(.x),as.numeric(NA),.x)))



























