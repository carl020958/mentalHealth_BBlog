pacman::p_load('tidyverse')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#-------------
# import data
#-------------
# data frame name : d
load("../report/survey_2021_long.Rdata")
kuscc_bb_clog_gpa <- read_csv("kuscc_bb_log_gpa.v20.20210719.csv")
kuscc_bb_clog_gpa_grad <- read_csv("kuscc_bb_log_gpa_grad.v20.20210719.csv")

# 일주일 단위로 평균치 산출 및 2021년 1학기 데이터 filter
undergrad_data <- 
  kuscc_bb_clog_gpa %>% 
  group_by(std_id, log_week) %>% 
  summarise(weekly_mean_course_access = mean(value, na.rm = T)) %>% 
  left_join(
    d %>% 
      filter(year == 2021 & month == 3) %>% 
      transmute(std_id = as.numeric(std_id), cate, value) %>% 
      pivot_wider(names_from = cate, values_from = value),
    by = 'std_id'
  ) 


# z score 산출 및 정신건강 데이터와 합치기
undergrad_data_weekly_zscore_group <- 
  undergrad_data %>% 
  select(log_week, weekly_mean_course_access) %>% 
  ungroup() %>% 
  group_by(log_week) %>% 
  mutate(weekly_mean = mean(weekly_mean_course_access),
         weekly_sd = sd(weekly_mean_course_access)) %>% 
  mutate(weekly_z =(weekly_mean_course_access - weekly_mean) / weekly_sd) %>% 
  select(std_id, log_week, weekly_z) %>% 
  pivot_wider(std_id, names_from=log_week, names_prefix="주차_", values_from=weekly_z) %>% 
  left_join(
    undergrad_data %>% 
      select(std_id, cesd_group, gad_group, pss, covid_group, mh_group2),
    by = "std_id"
  )

#-------------
# write data
#-------------
undergrad_data_weekly_zscore_group %>% 
  googlesheets4::write_sheet(
    'https://docs.google.com/spreadsheets/d/1ao_ZU-JDrAjAfyNOXYTVKc4-s3xJoPbNsEe1rrrjhFI/edit#gid=0',
    sheet = '시트1'
  )
