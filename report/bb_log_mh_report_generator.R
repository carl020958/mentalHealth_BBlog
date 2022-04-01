pacman::p_load('rmarkdown', 'googlesheets4', 'tidyverse')

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# -------------
# 데이터 세팅 
# -------------
{

    # weekly log Z score
    weekly_log_mh_df <-
    googlesheets4::read_sheet(
      'https://docs.google.com/spreadsheets/d/1ao_ZU-JDrAjAfyNOXYTVKc4-s3xJoPbNsEe1rrrjhFI/edit#gid=693083546',
      sheet = '시트1'
    )

    ####################
    # survey_2021 data #
    ####################

    load("survey_2021_long.Rdata")

    d <-
      d %>%
      filter(!is.na(value)) %>%
      filter(value != 'NA')

    # value 중 문자열 데이터의 scale 이름 뽑기
    chr_names <-
      d %>%
      filter(value != "NA") %>%
      filter(!is.na(value)) %>%
      filter(std_id == "2009360084") %>%
      mutate(value = as.numeric(value)) %>%
      filter(is.na(value)) %>%
      select(cate) %>%
      unique() %>%
      pull()

    # nuemric인 value의 실태조사별 mean, meanByCate, medianByCate 산출
    long <-
      d %>%
      filter(!cate %in% chr_names) %>%
      mutate(value = as.numeric(value)) %>%
      group_by(year, month, cate) %>%
      mutate(percentile = round(percent_rank(value)*100,1),
             meanByCate = mean(value, na.rm = T),
             medianByCate = median(value, na.rm=T)) %>%
      ungroup() %>%
      mutate(cate2 = paste0(cate, "_", year, month)) %>%
      mutate(x_axis = paste0(year, "년 ", month, "월"))

    # 정신건강 집단 오타 등 수정 및 통일
    d <-
      d %>%
      mutate(value = gsub(" 상태", "", value),
             value =
               case_when(value == "정신적 쇠약 및 심리적인 어려움" ~
                           "정신적 쇠약 및 심리적 어려움",
                         value == "정신적으로 쇠약한" ~
                           "정신적 쇠약",
                         value == "중간수준의 정신건강\r\r\r\r\n및 심리적인 어려움" ~
                           "중간수준의 정신건강 및 심리적 어려움",
                         value == "플로리시, 심리적인 어려움 공존상태" ~
                           "플로리시 및 심리적 어려움",
                         value == "플로리시 및 심리적인 어려움" ~
                           "플로리시 및 심리적 어려움",
                         T ~ value))

    # comment data
    comment_df <-
      googlesheets4::read_sheet(
        'https://docs.google.com/spreadsheets/d/1ao_ZU-JDrAjAfyNOXYTVKc4-s3xJoPbNsEe1rrrjhFI/edit#gid=0',
        sheet = '피봇 테이블 1'
      )
}

# ------------------
# participants list
# ------------------
par <- 
  googlesheets4::read_sheet(
  'https://docs.google.com/spreadsheets/d/1ao_ZU-JDrAjAfyNOXYTVKc4-s3xJoPbNsEe1rrrjhFI/edit#gid=0',
  sheet = '피봇 테이블 1'
  ) %>% 
  select(std_id)

# ------------------
# generate reports
# ------------------
for(i in 1:length(par$std_id)) {
  parID <- par$std_id[i]
  render(
    'reportPaper_bb_log_mh.Rmd',
    
    # file 2
    output_file =  paste(parID, ".html", sep = ''),
    output_dir = './html'
  )
}

