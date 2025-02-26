library(dplyr)
library(tidyr)

df <- readRDS("Data/full_raw.rds")
df <- df %>%  select(lfdn,everything()) %>% arrange(lfdn,wave) 


unique_vec <- unique(unlist(lapply(df %>% select(-c(1,2,5)),unique)))

# Create function to code NAs and convert to numeric
recode_fun <- function(x){
  x <- as.numeric(trimws(x))
  x <- if_else(x<0,as.numeric(NA),x)
}

replace_list<-function(x,values,replacements){
  c(replacements)[base::match(x, c(values))]
}

#Now rename/recode variables to more readable version
df<- df %>%
  mutate(
    across(.cols=-c("study","p_participation","kpx_2290s","wave_char"),.fns = recode_fun),
    kpx_2290s=as.numeric(gsub("\\D","",kpx_2290s)),
    Age_2022=2022-kpx_2290s,
    age_group_num=ceiling((kpx_2290s-1955)/10),
    age_group=replace_list(
      age_group_num,0:5,
      c("-1955","1956-1965","1966-1975","1976-1985","1986-1995","1996-2005")
    ),
    age_group_num2=ceiling((kpx_2290s-1955)/5),
    age_group2=replace_list(
      age_group_num2,0:10,
      c("-1955","1956-1960","1961-1965","1966-1970","1971-1975","1976-1980",
        "1981-1985","1986-1990","1991-1995","1996-2000","2001-2005")
    ),
    gender=case_when(
      kpx_2280==1~"Male",
      kpx_2280==2~"Female",
      TRUE~as.character(NA)),
    eastwest=case_when(
      ostwest==0 ~ "East",
      ostwest==1 ~ "West",
      TRUE ~ as.character(NA)
    ),
    across(.cols=c("X2090a","X2095a"),.fns=~case_when(
      .x %in% c(1,2,3) ~ "CDU/CSU",
      .x==4 ~ "SPD",
      .x==5 ~ "FDP",
      .x==6 ~ "GRUNE",
      .x==7 ~ "DIE LINKE",
      .x==322 ~ "AfD",
      .x==801 ~ "Other",
      .x==808 ~ "None",
      TRUE ~ as.character(NA)
    ), .names="party_{.col}")) %>% 
  rename(party=party_X2090a, party2=party_X2095a) %>%
  mutate(
    afd_pref=case_when(
      party == "AfD" ~ "1st",
      party2 == "AfD" ~ "2nd",
      !is.na(party) ~ "None",
      TRUE ~ as.character(NA)),
    party_split1=if_else(party %in% c("AfD","GRUNE"),party,"Other"),
    educ=factor(X2320,0:5,
                c("At School","None","Low","Intermediate",
                  "Polytech","University"),
                exclude = c(NA,9)),
    left_vs_right=X1500-6,
    polit_int=3-X010,
    econ_vs_clim=4-X1290,
    tax_and_soc_scale=4-X1090,
    tax_rich_less=4*(3-X2880j)/3,
    leave_income_inequality=4*(3-X2880d)/3,
    immigration_difficulty_scale=X1130-4,
    foreigners_assimilate=4-X1210,
    state_interfere_terror=4-X1411,
    limit_refugees=4*(X2880x-3)/3,
    gender_eq_imp=3-X1160,
    immigration_imp=3-X1140,
    soc_serv_imp=3-X1100,
    foreign_int_imp=3-X1220,
    econ_and_clim_imp=3-X1300,
    combat_terror_imp=3-X1421,
    clim_real=2-X1285,
    clim_gov_lwr_vs_hghr=5-X1287,
    clim_gov_satis=2-abs(3-X1287),
    clim_human=X2880bf-3,
    clim_conseq=X2880bj-3,
    clim_germ_2small=X2880ar-3,
    personal_econ=3-X780,
    personal_econ_future=3-X790,
    general_econ=3-X820,
    populism_sum=rowMeans(cbind(X3103a, X3103b, X3103c,X3103d,X3103e,X3103f,X3103g,
                                X3103h),
                          na.rm=TRUE),
    
    extreme_sum=rowMeans(cbind(X060e,X060f,X060g,X060i,X060j,X060k),
                         na.rm=TRUE),
    populism_sum=if_else(is.nan(populism_sum),as.numeric(NA),populism_sum),
    extreme_sum=if_else(is.nan(extreme_sum),as.numeric(NA),extreme_sum)
  ) %>% 
  arrange(lfdn,wave)

###############################################################
###################### Imputation #############################
###############################################################
df<-df %>% mutate(educ_imp=educ,populism_imp=populism_sum,extreme_imp=extreme_sum) %>% 
  mutate(across(c(foreigners_assimilate, state_interfere_terror,
                     tax_rich_less, leave_income_inequality),~.x,
                .names = "{.col}_imp"))%>% 
  group_by(lfdn) %>% 
  fill(educ_imp,populism_imp,extreme_imp,.direction = "downup") %>% 
  fill(foreigners_assimilate_imp, state_interfere_terror_imp,
       tax_rich_less_imp, leave_income_inequality_imp,.direction = "down") %>%
  ungroup()

df_sum <- df %>% group_by(lfdn) %>% 
  summarise(unqiue_educ=length(na.omit(unique(educ))))

#############################################################################
######################### Add in additonal L/R Variables ####################
#############################################################################
df <- df %>% mutate(
  econ_LR=rowMeans(cbind(tax_and_soc_scale,tax_rich_less, leave_income_inequality)),
  econ_LR_imp=rowMeans(cbind(tax_and_soc_scale,tax_rich_less_imp, leave_income_inequality_imp)),
  cult_LR=rowMeans(cbind(immigration_difficulty_scale,foreigners_assimilate, state_interfere_terror)),
  cult_LR_imp=rowMeans(cbind(immigration_difficulty_scale,foreigners_assimilate_imp, state_interfere_terror_imp)),
  cult2_LR=rowMeans(cbind(immigration_difficulty_scale,foreigners_assimilate)),
  cult2_LR_imp=rowMeans(cbind(immigration_difficulty_scale,foreigners_assimilate_imp)),
  )
#############################################################################
######################### Add in question Dates #############################
#############################################################################

wave_start_vec <- as.Date(
  c("2016/10/06","2017/02/16","2017/05/11","2017/07/06",
    "2017/07/20",
    "2017/08/17",
    "2017/09/04","2017/09/18","2017/09/27","2018/03/15","2018/11/06",
    "2019/05/28","2019/11/05","2020/04/21","2020/11/03","2021/02/25",
    "2021/05/06","2021/07/07","2021/08/11","2021/09/15","2021/09/29",
    "2021/12/09","2022/05/18","2022/10/12"),
  format = "%Y/%m/%d"
  )

waveA_end_vec <- as.Date(
  c("2016/11/10","2017/03/03","2017/05/23","2017/07/17","2017/08/09",
    "2017/08/28",
    "2017/09/13","2017/09/23","2017/10/09","2018/03/26","2018/11/21",
    "2019/06/12","2019/11/19","2020/05/05","2020/11/17","2021/03/12",
    "2021/05/19","2021/07/20","2021/08/24","2021/09/25","2021/11/09",
    "2021/12/21","2022/05/31","2022/11/22"),
  format = "%Y/%m/%d"
)


waveB_end_vec <- as.Date(
  c("2016/11/10","2017/03/03","2017/05/23","2017/07/17","2017/08/09",
    "2017/08/28",
    "2017/09/13","2017/09/23","2017/10/09","2018/03/26","2019/01/31",
    "2019/07/08","2019/12/17","2020/06/01","2020/12/17","2021/03/12"),
  format = "%Y/%m/%d"
)

#########################NOT CORRECT ASSUME ALL FROM SAMPLE A##################
df <- df %>% mutate(
  wave_start=replace_list(wave,c(1:4,4.5,5:23),wave_start_vec),
  wave_end=replace_list(wave,c(1:4,4.5,5:23),waveA_end_vec),
  test_match=replace_list(wave,c(1:4,4.5,5:23),paste0("wave_",c(1:4,4.5,5:23))),
  wave_mid=wave_start+floor((wave_end-wave_start)/2),
  wave_mid_min=min(wave_mid),wave_mid_max=max(wave_mid),
  wave_rescale=as.numeric(wave_mid-wave_mid_min)/as.numeric(wave_mid_max-wave_mid_min)
)


# Create function to give each individuals answer pattern for a given variable
list_out_func <- function(cohort,miss_var,n=15){
  if (max(cohort)>n){
    stop("Need to set n larger")
  }
  cohort <- cohort[!is.na(miss_var)]
  n_times=length(cohort)
  vec_to_paste<-rep(0,n)
  vec_to_paste[cohort] <- 1
  return(list(p=paste(vec_to_paste,collapse = "-"),
              n=n_times))
}

#Create variables for presence of answers to all questionnaires
df <- df %>%
  group_by(lfdn) %>%  
  mutate(across(.cols = c("left_vs_right","polit_int","econ_vs_clim",
                          "econ_and_clim_imp","party"),
                .fns = list(
                  p=function(x)list_out_func(wave,x,23)$p,
                  n=function(x)list_out_func(wave,x,23)$n),
                .names="{.fn}_{.col}"
                )) %>% 
  ungroup() %>% 
  arrange(lfdn,wave)

saveRDS(df,"Data/full_tidied.rds")
write.csv(df,"Data/full_tidied.csv")
