# ------------------------------------------------------ #
# 학생상담센터 X 데이터 허브 = 정신건강 X 블랙보드 로그  #
# ------------------------------------------------------ #

pacman::p_load('flexdashboard', 'plotly', 'highcharter', 'kableExtra', 'formattable', 'tidyverse', 'scales', 'googlesheets4')
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# =========
# Settings
# =========

# ------------
# 수검자 세팅
# ------------

# 수검자 세팅1
# parID <- "2021020006"

#수검자 세팅2
parName <-
  long %>%
  filter(std_id == parID) %>%
  select(name) %>%
  unique() %>%
  pull()

# ----------------
# 학기, 성적 세팅 
# ----------------
term_1 <- c(9:25)
term_1_tickvals <- as.list(9:25)


# ----------
# 함수 세팅 
# ----------
.left_color_bar <- 
  function(color, width){
    paste0('<span style="display: inline-block; direction: ltr; 
         unicode-bidi: plaintext; border-radius: 4px; padding-right: 2px; 
         background-color:', color,'; width: ', width, '%">', width,'</span>')
  }

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

# -------------------
# hovertemplate 세팅 
# -------------------
hovertemplate1 <- 
  paste0("주차: <b>%{x}</b><br>",
         "원점수: <b>%{y: .0f}점</b><extra></extra>")

hovertemplate2 <- 
  paste0("<b>%{text}</b><br>",
         "주차: <b>%{x}</b><br>",
         "원점수: <b>%{y: .0f}점</b><extra></extra>")

hovertemplate1_percentile <- 
  paste0("주차: <b>%{x}</b><br>",
         "백분위: <b>%{y: .0f}%</b><extra></extra>")

hovertemplate2_percentile <- 
  paste0("<b>%{text}</b><br>",
         "주차: <b>%{x}</b><br>",
         "백분위: <b>%{y: .0f}%</b><extra></extra>")


# ------------
#디자인 세팅
# ------------
colSet <- c("#62C3A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F")
colSet_p <- list("#F4B5BD", "#FFC459", "#FFFF00", "#74A089","#85D4E3","#9467BD")


# ------------
# 문항 꾸러미 
# ------------

#코로나 집단
covid_group_type <- c("코로나 대처 우수 집단", "코로나 대처 무관심 집단",
                      "코로나 대처 회피 집단", "코로나 대처 회피 집단")

# 심리적 건강
#정신건강, 불안, 우울은 RMD에

met_wb <- c("mw_social", "mw_psychological", "mw_emotional")

# COVID-19
covid <- c("covid_fear", "covid_guide", "covid_impact")

# 스트레스

# 대학생활
uni <- c("uni_adapt", "uni_maladapt")

uni_adapt <- c("uni_adapt_interpersonal", "uni_adapt_major", "uni_adapt_study")

uni_maladapt <- c("uni_maladapt_studyproblem", "uni_maladapt_alcoholproblem", 
                  "uni_maladapt_anger", "uni_maladapt_eatingproblem", 
                  "uni_maladapt_negativeemotion", "uni_maladapt_socialanxiety")


# # -------------
# # 데이터 세팅 
# # -------------
# {
#   
#   # weekly log Z score
#   weekly_log_mh_df <-
#     googlesheets4::read_sheet(
#       'https://docs.google.com/spreadsheets/d/1ao_ZU-JDrAjAfyNOXYTVKc4-s3xJoPbNsEe1rrrjhFI/edit#gid=693083546',
#       sheet = '시트1'
#     )
#   
#   ####################
#   # survey_2021 data #
#   ####################
#   
#   load("survey_2021_long.Rdata")
#   
#   d <-
#     d %>%
#     filter(!is.na(value)) %>%
#     filter(value != 'NA')
#   
#   # value 중 문자열 데이터의 scale 이름 뽑기
#   chr_names <-
#     d %>%
#     filter(value != "NA") %>%
#     filter(!is.na(value)) %>%
#     filter(std_id == "2009360084") %>%
#     mutate(value = as.numeric(value)) %>%
#     filter(is.na(value)) %>%
#     select(cate) %>%
#     unique() %>%
#     pull()
#   
#   # nuemric인 value의 실태조사별 mean, meanByCate, medianByCate 산출
#   long <-
#     d %>%
#     filter(!cate %in% chr_names) %>%
#     mutate(value = as.numeric(value)) %>%
#     group_by(year, month, cate) %>%
#     mutate(percentile = round(percent_rank(value)*100,1),
#            meanByCate = mean(value, na.rm = T),
#            medianByCate = median(value, na.rm=T)) %>%
#     ungroup() %>%
#     mutate(cate2 = paste0(cate, "_", year, month)) %>%
#     mutate(x_axis = paste0(year, "년 ", month, "월"))
#   
#   # 정신건강 집단 오타 등 수정 및 통일
#   d <-
#     d %>%
#     mutate(value = gsub(" 상태", "", value),
#            value =
#              case_when(value == "정신적 쇠약 및 심리적인 어려움" ~
#                          "정신적 쇠약 및 심리적 어려움",
#                        value == "정신적으로 쇠약한" ~
#                          "정신적 쇠약",
#                        value == "중간수준의 정신건강\r\r\r\r\n및 심리적인 어려움" ~
#                          "중간수준의 정신건강 및 심리적 어려움",
#                        value == "플로리시, 심리적인 어려움 공존상태" ~
#                          "플로리시 및 심리적 어려움",
#                        value == "플로리시 및 심리적인 어려움" ~
#                          "플로리시 및 심리적 어려움",
#                        T ~ value))
#   
#   # comment data
#   comment_df <-
#     googlesheets4::read_sheet(
#       'https://docs.google.com/spreadsheets/d/1ao_ZU-JDrAjAfyNOXYTVKc4-s3xJoPbNsEe1rrrjhFI/edit#gid=0',
#       sheet = '피봇 테이블 1'
#     )
# }

# --------------------------
# data set for bb log graph
# --------------------------
{
    par_T_mean_accum <- 
      weekly_log_mh_df %>% 
        filter(std_id == parID) %>% 
        select(std_id, sprintf('주차_%d', 9:25)) %>% 
        pivot_longer(-std_id, names_to = 'week', values_to = 'mean_course_access') %>% 
        mutate(week = as.numeric(str_extract(week, '[0-9]+'))) %>% 
        accumulate_by(~week)
    
    mh_group_df_accum <- 
      weekly_log_mh_df %>% 
      group_by(mh_group2) %>% 
      summarise(across(contains('주차'), mean, na.rm = T)) %>% 
      ungroup() %>% 
      pivot_longer(-mh_group2, names_to = 'week', values_to = 'mean_course_access') %>% 
      mutate(week = as.numeric(str_extract(week, '[0-9]+'))) %>% 
      accumulate_by(~week) %>% 
      rename(group = mh_group2)
    
    par_mh_group <- 
      weekly_log_mh_df %>%
      filter(std_id == parID) %>% 
      pull(mh_group2)
      
}  


# =========
# Plots
# =========

# ----------------------------
# 정신건강 X 블랙보드 빅데이터
# ----------------------------

# 정신건강 X 블랙보드 로그 코스 빅데이터
{
  sum_na <- sum(is.na(weekly_log_mh_df %>% filter(std_id == parID)))
  
  if (sum_na > 0){
    mhP <- 
      weekly_log_mh_df %>% 
        filter(std_id == parID) %>% 
        select(std_id, sprintf('주차_%d', 9:25)) %>% 
        pivot_longer(-std_id, names_to = 'week', values_to = 'mean_course_access') %>% 
        mutate(week = as.numeric(str_extract(week, '[0-9]+'))) %>% 
        plot_ly(
          x = ~ week,
          y = ~ mean_course_access,
          name = "본인",
          text = "본인",
          type = 'scatter',
          mode = 'lines',
          line = list(dash = "line",
                      color = "#990000",
                      width = 2,
                      simplyfy = F),
          hovertemplate = paste(
            "<b>%{text}</b><br>",
            "<b>주차: %{x}</b><br>",
            "Z점수: <b>%{y: .02f}점</b><extra></extra>")
          
        ) %>%
        
        
        add_trace(
          data = weekly_log_mh_df %>% 
            group_by(mh_group2) %>% 
            summarise(across(contains('주차'), mean, na.rm = T)) %>% 
            ungroup() %>% 
            pivot_longer(-mh_group2, names_to = 'week', values_to = 'mean_course_access') %>% 
            mutate(week = as.numeric(str_extract(week, '[0-9]+'))) %>% 
            rename(group = mh_group2),
          x = ~ week,
          y = ~ mean_course_access,
          name = ~ group,
          text = ~ group,
          type = 'scatter',
          mode = 'lines',
          line = list(dash = "dash",
                      width = 2,
                      color = colSet_p,
                      simplyfy = F),
          hovertemplate = paste(
            "<b>%{text}</b><br>",
            "<b>주차: %{x}</b><br>",
            "Z점수: <b>%{y: .02f}점</b><extra></extra>")
          
        ) %>%
        
        layout(xaxis = list(
          ticktext = list(
            "개강 후 첫째 주",	"개강 후 둘째 주",	"개강 후 셋째 주",	"개강 후 넷째 주", 
            "중간고사 3주 전", "중간고사 2주 전",	"중간고사 1주 전",	"중간고사",	"중간고사",
            "중간고사 후 첫째 주",	"중간고사 후 둘째주",	"중간고사 후 셋째 주",
            "기말고사 3주 전",	"기말고사 2주 전",	"기말고사 1주 전", "기말고사",	"기말고사"
          ),
          tickvals = term_1_tickvals,
          tickmode = "array",
          title = ''
          
        )) %>%
        
        layout(yaxis = list(title = "1학기 블랙보드 코스 로그 기록(Z점수)"
                            
        )) %>%
        
        config(displayModeBar = F)
    
  } else{
    
    mhP <- 
      par_T_mean_accum %>% 
      plot_ly(
        x = ~ week,
        y = ~ mean_course_access,
        name = "본인",
        frame = ~ frame,
        text = "본인",
        type = 'scatter',
        mode = 'lines',
        line = list(dash = "line",
                    color = "#990000",
                    width = 2,
                    simplyfy = F),
        hovertemplate = paste(
          "<b>%{text}</b><br>",
          "<b>주차: %{x}</b><br>",
          "Z점수: <b>%{y: .02f}점</b><extra></extra>")
        
      ) %>%
      
      
      add_trace(
        data = mh_group_df_accum,
        x = ~ week,
        y = ~ mean_course_access,
        name = ~ group,
        frame = ~ frame,
        text = ~ group,
        type = 'scatter',
        mode = 'lines',
        line = list(dash = "dash",
                    width = 2,
                    color = colSet_p,
                    simplyfy = F),
        hovertemplate = paste(
          "<b>%{text}</b><br>",
          "<b>주차: %{x}</b><br>",
          "Z점수: <b>%{y: .02f}점</b><extra></extra>")
        
      ) %>%
      
      layout(xaxis = list(
        ticktext = list(
          "개강 후 첫째 주",	"개강 후 둘째 주",	"개강 후 셋째 주",	"개강 후 넷째 주", 
          "중간고사 3주 전", "중간고사 2주 전",	"중간고사 1주 전",	"중간고사",	"중간고사",
          "중간고사 후 첫째 주",	"중간고사 후 둘째주",	"중간고사 후 셋째 주",
          "기말고사 3주 전",	"기말고사 2주 전",	"기말고사 1주 전", "기말고사",	"기말고사"
        ),
        tickvals = term_1_tickvals,
        tickmode = "array",
        title = "이번 그래프에는 애니메이션 효과가 존재합니다. 우측의 PLAY 버튼을 누르시면 그래프가 움직이면서 제시됩니다"
        
      )) %>%
      
      layout(yaxis = list(title = "1학기 블랙보드 코스 로그 기록(Z점수)"
                          
      )) %>%
      
      animation_opts(
        frame = 200,
        transition = 0,
        redraw = FALSE
      ) %>%
      
      animation_slider(
        hide = T
      ) %>%
      
      animation_button(
        x = 1, xanchor = "left", y = 0, yanchor = "bottom"
      ) %>%
      
      
      config(displayModeBar = F)
  }
  
    
}


# -----------
# 심리적 건강
# -----------

# 정신건강 기록 
{
  
    #표 1 집단
    par_group_kbl <-
      d %>% 
      filter(cate %in% c("mh_group", "cesd_group", "suicide_group") & std_id == parID) %>% 
      transmute(시기 = paste0(year, "년 ", month, "월"), cate, value, name) %>% 
      pivot_wider(-name, names_from = cate, values_from = value) %>% 
      arrange(시기) %>% 
      mutate(
        
        mh_group = 
          cell_spec(mh_group, 
                    "html", 
                    color = case_when(
                      mh_group == "정신적 쇠약 및 심리적 어려움" ~ "#B22222", #firebrick
                      mh_group == "정신적 쇠약" ~ "#CD5C5C", #indian red
                      mh_group == "중간수준의 정신건강 및 심리적 어려움" ~ "#2E8B57", #sea green
                      mh_group == "중간수준의 정신건강" ~ "#3CB371", #medium sea green
                      mh_group == "플로리시 및 심리적 어려움" ~ "#4682B4", #steel blue
                      mh_group == "플로리시" ~ "#1E90FF", #dodger blue
                      T ~ "#FFFFFF")),
        
        cesd_group = 
          cell_spec(cesd_group, 
                    "html", 
                    color = case_when(
                      cesd_group == "고도" ~ "#B22222", #firebrick
                      cesd_group == "중증도" ~ "#FF8C00", #dark orange
                      cesd_group == "경도" ~ "#DAA520", #golden rod
                      cesd_group == "정상" ~ "#2E8B57", #sea green
                      T ~ "#FFFFFF")), #white
        
        suicide_group = 
          cell_spec(suicide_group, 
                    "html", 
                    color = case_when(
                      suicide_group == "고도" ~ "#B22222", #firebrick
                      suicide_group == "중증도" ~ "#FF8C00", #dark orange
                      suicide_group == "경도" ~ "#DAA520", #golden rod
                      suicide_group == "정상" ~ "#2E8B57", #sea green
                      T ~ "#FFFFFF")), #white
        
      ) %>% 
      mutate(across(everything(), ~ str_replace_all(.x, "NA", ""))) %>% 
      transmute(시기, 
                  '정신건강 범주' = mh_group, 
                  '우울 수준' = cesd_group, 
                  '자살사고 수준' = suicide_group) %>% 
      
      kable(format = "html", escape = F, align = c('c', 'c', 'c', 'c')) %>% 
      kable_styling(bootstrap_options = c("condensed", "responsive", "hover", "striped"),
                    fixed_thead = T) %>% 
      column_spec(1, width = '3cm', background = "#E3A2A0") %>% 
      column_spec(c(1,2,3,4), bold = T) %>% 
      row_spec(0, color = "white", background = "#CB5456")
  
  
    #표2 척도별 백분위
    par_group_value_kbl <- 
      long %>% 
      filter(cate %in% c("mw", "cesd") & std_id == parID) %>%
      filter(!is.na(value)) %>% 
      transmute(시기 = paste0(year, "년 ", month, "월"), cate, percentile, name) %>% 
      pivot_wider(-name, names_from = cate, values_from = percentile) %>% 
      arrange(시기) %>% 
      # mutate(
      #   mw = color_bar("#3CB371", fun = .percent)(mw), # medium sea green
      #   cesd = color_bar("#CBC3E3", fun = .percent)(cesd), #light purple
      #   # cesd = color_bar("#CBC3E3", na.rm = T)(cesd), #결측값 처리는 na.rm = T로 함
      # ) %>% 
      mutate(
        mw = .left_color_bar("#3CB371", mw), # medium sea green
        cesd = .left_color_bar("#CBC3E3", cesd)  # light purple
      ) %>%
      
      transmute(시기, 
                  '정신건강(백분위%)' = mw, 
                  '우울(백분위%)' = cesd) %>% 
      mutate(across(everything(), ~ str_replace_all(.x, "NA", ""))) %>% 
      kable(format = "html", escape = F, align = c('c', 'l', 'l')) %>% 
      kable_styling(bootstrap_options = c("condensed", "responsive", "hover", "striped"),
                    fixed_thead = T) %>% 
      column_spec(1, width = '3cm', background = "#E3A2A0") %>% 
      column_spec(c(1,2,3), bold = T) %>% 
      row_spec(0, color = "white", background = "#CB5456")
  
}

# 정신적 웰빙
{
  
    df_wb <- 
      long %>% 
      filter(year == 2021 & month == 3 & cate %in% met_wb & std_id == parID) %>% 
      arrange(match(cate, met_wb))
    
    WB_plot <-
      highchart() %>%
      hc_chart(type = "bar") %>%
      hc_colors(color = c(
        colSet[1], colSet[2], colSet[3]
      )
      ) %>%
      
      hc_plotOptions(column = list(
        pointPadding = 0.2,
        borderWidth = 0,
        colorByPoint =T)
      )  %>%
      
      hc_xAxis(
        crosshair = F,
        categories = c("사회적 웰빙", "심리적 웰빙", "정서적 웰빙"),
        tickmarkPlacement = 'on',
        lineWidth = 2,
        labels = list(
          # enabled = F,
          style = list(fontSize= '12px'))
      ) %>%
      
      hc_yAxis(
        crosshair = T,
        min = 0,
        max = 100,
        labels = list(
          format = "{value}%"
        )
      ) %>%
      
      hc_add_series(
        name = "본인",
        data = df_wb,
        hcaes(cate, percentile),
        marker = list(radius = 24),
        type = "column",
        
        dataLabels = list(
          format = "{point.y:.0f}",
          enabled = T,
          align= "center",
          verticalAlign= "middle",
          color="white",
          style = list(
            fontFamily= 'noto',
            fontSize='25px')
        )
      ) %>% 
      
      hc_tooltip(crosshairs = T,
                 enabled= T,
                 headerFormat = "",
                 pointFormat = "본인점수: <b>{point.value}점</b><br>
                 타인평균: {point.meanByCate:.1f}점")
    
}

# 정신적 웰빙 기록
{
    
    #참가자 정신적 웰빙
    par_WB_df <- 
      long %>% 
      filter(str_detect(cate, "mw_") & std_id == parID) %>% 
      arrange(cate2) %>% 
      mutate(cate = case_when(cate == "mw_emotional" ~ "정서적 웰빙",
                              cate == "mw_psychological" ~ "심리적 웰빙",
                              cate == "mw_social" ~ "사회적 웰빙"))
    
    #정신적 웰빙 소척도들 중 최대값 구하기(원점수 그래프의 최대값 지정하기 위해)
    WB_value <- 
      long %>% 
      filter(str_detect(cate, "mw_")) %>% 
      select(value)
    WB_max <- max(WB_value, na.rm = T)
    
    #hovertemplate 문제로 구함(검사 시행 횟수가 1번일 때 오작동)
    num <- 
      d %>% 
      filter(std_id == parID & cate == 'mw') %>% 
      nrow()
    
    #그래프 1(막대)
    WB_plot1 <- 
      plot_ly() %>% 
      
      add_trace(
        data = par_WB_df,
        x = ~ x_axis,
        y = ~ value,
        color = ~ cate,
        type = "bar",
        text = ~ cate,
        hoverinfo = "text",
        hovertemplate = 
          case_when(num == 1 ~ hovertemplate1,
                    T ~ hovertemplate2),
        texttemplate = "<b>%{y: .0f}점</b>",
        textposition = "inside",
        insidetextanchor = "end",
        insidetextfont = list(color = "#7C7C7C", 
                              size = 9),
        legendgroup = ~ cate,
        showlegend = T
      ) %>% 
      
      layout(
        title = "",
        # bargap = 0.6,
        
        xaxis = list(title = "", 
                     showline = T
        ),
        
        yaxis = list(side = 'right',
                     title = "원점수",
                     showgrid = T,
                     zeroline = T,
                     range = c(0, WB_max))
        
        # legend = list(x = 100, y = 100)
        
      ) %>%
      
      config(displayModeBar = F)
    
    #그래프 2(선)
    WB_plot2 <- 
      
      plot_ly() %>% 
      
      add_trace(
        data = par_WB_df,
        x = ~ x_axis,
        y = ~ percentile,
        color = ~ cate,
        type = "scatter",
        mode = "markers + lines",
        line = list(dash = "dot",
                    width = 2,
                    simplify = T),
        text = ~ cate,
        hoverinfo = "text",
        hovertemplate = 
          case_when(num == 1 ~ hovertemplate1_percentile,
                    T ~ hovertemplate2_percentile),
        # texttemplate = "<b>%{y: .0f}%</b>",
        # textposition = "outside",
        legendgroup = ~ cate,
        showlegend = F
      ) %>% 
      
      layout(
        title = "",
        # bargap = 0.6,
        
        xaxis = list(title = "", 
                     showline = F,
                     ticktest = list(""),
                     tickvals = list(""),
                     tickmode = "array"),
        
        yaxis = list(side = 'left',
                     title = "백분위",
                     showgrid = T,
                     zeroline = T,
                     range = c(0, 100))
        
      ) %>% 
    
    config(displayModeBar = F)
    
    #최종 그래프
    WB_diary_plot <- subplot(WB_plot2, WB_plot1, nrows = 2, shareY = T)
  
}


# -----------
# COVID-19
# -----------

# COVID-19 대처
{
  
  covid_df <- 
    d %>% 
    filter(year == '2021' & month == '3' & cate %in% c("covid_guide", "covid_impact")) %>%
    select(std_id, cate, value) %>%
    pivot_wider(., names_from = cate, values_from = value) %>%
    mutate(across(everything(), ~ as.numeric(.x))) %>% 
    left_join(
      weekly_log_mh_df %>% 
        select(std_id, covid_group),
      by = "std_id") %>% 
    filter(!is.na(covid_group))
  
  par_covid_group <- 
    weekly_log_mh_df %>%
    filter(std_id == parID) %>% 
    pull(covid_group)
  
  
  코로나대처우수색깔 <- colSet[1]
  코로나대처무관심색깔 <- colSet[2]
  코로나대처회피색깔 <- colSet[3]
  코로나대처취약색깔 <- colSet[6]
  
  
  코로나대처우수순서 <- ifelse(par_covid_group == "COVID-19 대처 우수 집단", 2, 1) #노력
  코로나대처무관심순서 <- ifelse(par_covid_group == "COVID-19에 대한 관심이 낮은 집단", 2, 1) #튼튼
  코로나대처회피순서 <- ifelse(par_covid_group == "COVID-19 대처 회피 집단", 2, 1) #터널
  코로나대처취약순서 <- ifelse(par_covid_group == "COVID-19 취약 집단", 2, 1) #부족한 생기
  
  
  covid_group_Plot <-
    highchart() %>%
    hc_chart(type = "scatter",
             zoomType = "xy") %>%
    hc_plotOptions(
      series = list(
        fillOpacity = 0.8,
        stickyTracking= T,
        dashStyle = "ShortDot")
    )  %>%
    hc_xAxis(
      endOnTick = T,
      max = 50,
      minRange = 0,
      title = list(
        text = "코로나 부정적 영향",
        style = list(
          fontSize= '15px'
        )
      ),
      crosshair = list(
        width = 1
      ),
      labels = list(
        format = "{value}"
      )
    ) %>%
    hc_yAxis(
      endOnTick = T,
      max= 30,
      minRange = 0,
      crosshair = list(
        width = 1
      ),
      
      title = list(
        text = "코로나 방역 지침 준수",
        style = list(
          fontSize= '15px'
        )
      ),
      labels = list(
        format = "{value}"
      )
    ) %>%
    
    hc_add_series(
      name = "본인",
      data = covid_df %>% filter(std_id == parID),
      hcaes(covid_impact, covid_guide),
      type = "scatter",
      color="#990000",
      zIndex= 3,
      marker = list(
        symbol = "circle",
        radius = 6
      ),
      dataLabels = list(
        enabled = T,
        align= "center",
        verticalAlign= "below",
        format = "본인",
        y = 0,
        color="white",
        style = list(
          fontFamily= 'noto', fontSize='20px')),
      
      tooltip = list(crosshairs = T,
                     enabled= T,
                     valueDecimals=0,
                     pointFormat = "코로나 대처 집단: <b>{point.covid_group}</b><br>
                                 코로나 부정적 영향: <b>{point.covid_impact:.0f}점</b><br>
                                 코로나 방역 지침 준수: <b>{point.covid_guide:.0f}점</b> ")
    ) %>%
    
    hc_add_series(
      name = "전체 참여자",
      data = covid_df %>% filter(std_id != parID),
      hcaes(covid_impact, covid_guide),
      type = "scatter",
      color = "#E2E2E2",
      zIndex = 0,
      marker = list(
        symbol = "circle",
        radius = 3
      ),
      enableMouseTracking= F
    ) %>%
    
    
    hc_add_series(
      name = "COVID-19에 대한 관심이 낮은 집단",
      type = "line",
      dashStyle = "ShortDot",
      color = 코로나대처취약색깔,
      zIndex = 코로나대처취약순서 ,
      enableMouseTracking= F,
      data= list(
        c(22.5, 17.5),
        c(22.5, 0),
        c(0, 0),
        c(0, 17.5),
        c(22.5, 17.5)
      )) %>%
    
    hc_add_series(
      name = "COVID-19 취약 집단",
      type = "line",
      color = 코로나대처취약색깔,
      zIndex = 코로나대처취약순서,
      dashStyle = "ShortDot",
      enableMouseTracking= F,
      data= list(
        c(50, 30),
        c(50, 17.5),
        c(22.5, 17.5),
        c(22.5, 30),
        c(50, 30)
      )) %>%
    
    
    hc_add_series(
      name = "COVID-19 대처 우수 집단",
      type = "line",
      color = 코로나대처우수색깔,
      zIndex = 코로나대처우수순서,
      dashStyle = "ShortDot",
      enableMouseTracking= F,
      data= list(
        c(0, 17.5),
        c(0, 30),
        c(22.5, 30),
        c(22.5, 17.5),
        c(0, 17.5)
      )) %>%
    
    
    hc_add_series(
      name = "COVID-19 대처 회피 집단",
      type = "line",
      color = 코로나대처무관심색깔,
      zIndex = 코로나대처무관심순서,
      dashStyle = "ShortDot",
      enableMouseTracking= F,
      data= list(
        c(50, 17.5),
        c(50, 0),
        c(22.5, 0),
        c(22.5, 17.5),
        c(50, 17.5)
      ))
}


# COVID-19 대처 수준
{
  
    df_covid <- 
      long %>% 
      filter(year == 2021 & month == 3 & cate %in% covid & std_id == parID) %>% 
      arrange(match(cate, covid)) %>% 
      mutate(cate = case_when(cate == "covid_fear" ~ "COVID-19에 대한 공포",
                              cate == "covid_guide" ~ "COVID-19 방역지침 준수",
                              cate == "covid_impact" ~ "COVID-19의 부정적인 영향"))
    
    
    covid_plot <- 
      highchart() %>%
      
      hc_colors(color = colSet[2]
      ) %>%
      
      hc_chart(polar = T) %>%
      hc_xAxis(
        categories = c("COVID-19에 대한 공포",
                       "COVID-19 방역지침 준수",
                       "COVID-19의 부정적인 영향"),
        tickmarkPlacement = 'on',
        lineWidth = 0,
        labels = list(
          style = list(fontSize= '12px'))
      ) %>%
      
      hc_yAxis(
        gridLineInterpolation = "polygon",
        lineWidth = 0,
        tickInterval = 25,
        min = 0,
        max = 100,
        labels = list(format = "",
                      enabled=F)
      ) %>%
      
      hc_plotOptions(
        column = list(colorByPoint = F),
        series = list(
          fillOpacity = 0.25,
          dataLabels = list(
            enabled = T,
            align = "center",
            verticalAlign = "middle",
            format = "{point.y}",
            y = 0,
            color = "white",
            style = list(fontFamily = 'noto')
            
          ),
          dashStyle = "ShortDot"
        )
      ) %>% 
      
      hc_add_series(
        name = "본인",
        data = df_covid,
        hcaes(cate, percentile),
        marker = list(radius = 12),
        pointWidth = 20,
        pointPlacement = "on",
        type = "area",
        zIndex= 2,
        dataLabels = list(
          enabled = T,
          align= "center",
          verticalAlign= "middle",
          format = "{point.y:.0f}",
          y = 0,
          color="white",
          style = list(
            fontFamily= 'noto', fontSize='20px')
        ) 
        
      ) %>%
      
      hc_tooltip(crosshairs = T,
                 enabled= T,
                 headerFormat = "",
                 pointFormat = "<b>{point.cate}</b><br>
                      원점수: <b>{point.value}</b><br>
                     백분위: <b>{point.percentile:.0f}%</br>")
    
}


# COVID-19 대처 수준 기록
{
  
    par_covid_df <- 
      long %>% 
      filter(str_detect(cate, "covid") & std_id == parID) %>% 
      arrange(cate2) %>% 
      mutate(cate = case_when(cate == "covid_fear" ~ "COVID-19에 대한 공포",
                              cate == "covid_guide" ~ "COVID-19 방역지침 준수",
                              cate == "covid_impact" ~ "COVID-19의 부정적 영향"))
    
    covid_value <- 
      long %>% 
      filter(str_detect(cate, "covid")) %>% 
      select(value)
    covid_max <- max(covid_value, na.rm = T)
    
    #hovertemplate 문제로 구함(검사 시행 횟수가 1번일 때 오작동)
    num <- 
      d %>% 
      filter(std_id == parID & cate == 'covid_fear') %>% 
      nrow()
    
    
    covid_plot1 <- 
      plot_ly() %>% 
      
      add_trace(
        data = par_covid_df,
        x = ~ x_axis,
        y = ~ value,
        color = ~ cate,
        type = "bar",
        text = ~ cate,
        hoverinfo = "text",
        hovertemplate = 
          case_when(num == 1 ~ hovertemplate1,
                    T ~ hovertemplate2),
        texttemplate = "<b>%{y: .0f}점</b>",
        textposition = "inside",
        insidetextfont = list(color = "#7C7C7C", 
                              size = 9),
        legendgroup = ~ cate,
        showlegend = T
      ) %>% 
      
      layout(
        title = "",
        # bargap = 0.6,
        
        xaxis = list(title = "", 
                     showline = T
        ),
        
        yaxis = list(side = 'right',
                     title = "원점수",
                     showgrid = T,
                     zeroline = T,
                     range = c(0, covid_max))
        
        # legend = list(x = 100, y = 100)
        
      ) %>%
      
      config(displayModeBar = F)
    
    
    covid_plot2 <- 
      plot_ly() %>% 
      
      add_trace(
        data = par_covid_df,
        x = ~ x_axis,
        y = ~ percentile,
        color = ~ cate,
        type = "scatter",
        mode = "markers + lines",
        line = list(dash = "dot",
                    width = 2,
                    simplify = T),
        text = ~ cate,
        hoverinfo = "text",
        hovertemplate = 
          case_when(num == 1 ~ hovertemplate1_percentile,
                    T ~ hovertemplate2_percentile),
        # texttemplate = "<b>%{y: .0f}%</b>",
        # textposition = "outside",
        legendgroup = ~ cate,
        showlegend = F
      ) %>% 
      
      layout(
        title = "",
        # bargap = 0.6,
        
        xaxis = list(title = "", 
                     showline = F,
                     ticktest = list(""),
                     tickvals = list(""),
                     tickmode = "array"),
        
        yaxis = list(side = 'left',
                     title = "백분위",
                     showgrid = T,
                     zeroline = T,
                     range = c(0, 100))
        
      ) %>% 
    
    config(displayModeBar = F)
    
    covid_diary_plot <- subplot(covid_plot2, covid_plot1, nrows = 2, shareY = T)
  
}


# -----------
# 스트레스
# -----------

# 지각된 스트레스(PS)
{
  
    pss_df <- 
      long %>%
      filter(year == 2021 & month == 3 & std_id == parID & cate == "pss")
    
    pssPlot <- 
      highchart() %>%
      
      hc_chart(type = "bar") %>%
      
      hc_colors(color = c(
        colSet[6])
      ) %>% 
      
      hc_plotOptions(column = list(
        pointPadding = 0.2,
        borderWidth = 0,
        colorByPoint =F)
      )  %>%
      
      hc_xAxis(
        crosshair = F,
        
        title = list(text="지각된 스트레스"),
        tickmarkPlacement = 'on',
        lineWidth = 0,
        labels = list(
          enabled = F,
          style = list(fontSize= '15px'))
      ) %>%
      
      hc_yAxis(
        crosshair = T,
        min = 0,
        max = 100,
        labels = list(
          format = "{value}%"
        )
      ) %>%
      
      hc_add_series(
        name = "본인",
        data = pss_df,
        hcaes(cate, percentile),
        marker = list(radius = 12),
        type = "column",
        dataLabels = list(
          format = "{point.y:.0f}",
          enabled = T,
          align= "center",
          verticalAlign= "middle",
          color="white",
          style = list(
            fontFamily= 'noto',
            fontSize='30px')
        )
      ) %>%
      
      hc_tooltip(crosshairs = T,
                 enabled= T,
                 headerFormat = "<b>지각된 스트레스</b><br>",
                 pointFormat = "본인점수: <b>{point.value}점</b><br>
                     타인평균: {point.meanByCate:.1f}점")
    
}

# 게임 중독
{
  
    par_game_addict <- 
      d %>% 
      filter(year == 2021 & month == 3 & std_id == parID & cate == "game_group") %>% 
      mutate(value = case_when(value == "게임중독 아님" ~ "정상 수준",
                               value == "게임중독" ~ "중독 수준")) %>% pull(value)
    
    game_addict_df <- 
      long %>% 
      filter(year == 2021 & month ==3 & std_id == parID & cate == "game_addiction") %>%
      mutate(percentile = case_when(is.na(percentile) ~ 0,
                                    T ~ percentile),
             value = case_when(is.na(value) ~ 0,
                               T ~ value))
    
    game_addi_plot <- 
      highchart() %>%
      
      hc_chart(type = "bar") %>%
      
      hc_colors(color = c(
        colSet[3])
      ) %>% 
      
      hc_plotOptions(column = list(
        pointPadding = 0.2,
        borderWidth = 0,
        colorByPoint =F)
      )  %>%
      
      hc_xAxis(
        crosshair = F,
        title = list(text="게임 중독"),
        tickmarkPlacement = 'on',
        lineWidth = 0,
        labels = list(
          enabled = F,
          style = list(fontSize= '15px'))
      ) %>%
      
      hc_yAxis(
        crosshair = T,
        min = 0,
        max = 100,
        labels = list(
          format = "{value}%"
        )
      ) %>%
      
      hc_add_series(
        name = "본인",
        data = game_addict_df,
        hcaes(cate, percentile),
        marker = list(radius = 12),
        type = "column",
        
        dataLabels = list(
          format = "{point.y:.0f}",
          enabled = T,
          align= "center",
          verticalAlign= "middle",
          color="white",
          style = list(
            fontFamily= 'noto',
            fontSize='30px')
        )
      ) %>%
      
      hc_tooltip(crosshairs = T,
                 enabled= T,
                 headerFormat = "<b>게임 중독 정도</b><br>",
                 pointFormat = "본인점수: <b>{point.value}점</b><br>
                     타인평균: {point.meanByCate:.1f}점")
    
}


# --------
# 대학생활
# --------

# 전체 대학생활 만족 및 부적응 수준 기록
{
  
    par_uni_value_kbl <-
      long %>%
      filter(cate %in% uni & std_id == parID) %>%
      filter(!is.na(value)) %>%
      transmute(시기 = paste0(year, "년 ", month, "월"), cate, percentile, name) %>%
      pivot_wider(-name, names_from = cate, values_from = percentile) %>%
      arrange(시기) %>%
      mutate(
        uni_adapt = .left_color_bar("#3CB371", uni_adapt), # medium sea green
        uni_maladapt = .left_color_bar("#CBC3E3", uni_maladapt) # light purple
      ) %>%
      transmute(시기,
                  '전체 대학생활 만족 수준(백분위%)' = uni_adapt,
                  '전체 대학생활 부적응 수준(백분위%)' = uni_maladapt
      ) %>%
      mutate(across(everything(), ~ str_replace_all(.x, "NA", ""))) %>%
      kable(format = "html", escape = F, align = c('c', 'l', 'l')) %>%
      kable_styling(bootstrap_options = c("condensed", "responsive", "hover", "striped"),
                    fixed_thead = T) %>%
      column_spec(1, width = '3cm', background = "#E3A2A0") %>%
      column_spec(c(1,2,3), bold = T) %>%
      row_spec(0, color = "white", background = "#CB5456")
  
}


# 대학생활 만족 수준
{
  
    df_uni_adapt <- 
      long %>% 
      filter(year == 2021 & month == 3 & cate %in% uni_adapt & std_id == parID) %>% 
      arrange(match(cate, uni_adapt)) %>% 
      mutate(cate = case_when(cate == "uni_adapt_interpersonal" ~ "대인관계 만족도",
                              cate == "uni_adapt_major" ~ "전공 만족도",
                              cate == "uni_adapt_study" ~ "학업 만족도"))
    
    
    uni_adapt_plot <- 
      highchart() %>%
      
      hc_colors(color = colSet[1]
      ) %>%
      
      hc_chart(polar = T) %>%
      hc_xAxis(
        categories = c("대인관계 만족도",
                       "전공 만족도",
                       "학업 만족도"),
        tickmarkPlacement = 'on',
        lineWidth = 0,
        labels = list(
          style = list(fontSize= '15px'))
      ) %>%
      
      hc_yAxis(
        gridLineInterpolation = "polygon",
        lineWidth = 0,
        tickInterval = 25,
        min = 0,
        max = 100,
        labels = list(format = "",
                      enabled=F)
      ) %>%
      
      hc_plotOptions(
        column = list(colorByPoint = F),
        series = list(
          fillOpacity = 0.25,
          dataLabels = list(
            enabled = T,
            align = "center",
            verticalAlign = "middle",
            format = "{point.y}",
            y = 0,
            color = "white",
            style = list(fontFamily = 'noto')
            
          ),
          dashStyle = "ShortDot"
        )
      ) %>% 
      
      hc_add_series(
        name = "본인",
        data = df_uni_adapt,
        hcaes(cate, percentile),
        marker = list(radius = 12),
        pointWidth = 20,
        pointPlacement = "on",
        type = "area",
        zIndex= 2,
        dataLabels = list(
          enabled = T,
          align= "center",
          verticalAlign= "middle",
          format = "{point.y:.0f}",
          y = 0,
          color="white",
          style = list(
            fontFamily= 'noto', fontSize='20px')
        ) 
        
      ) %>%
      
      hc_tooltip(crosshairs = T,
                 enabled= T,
                 headerFormat = "",
                 pointFormat = "<b>{point.cate}</b><br>
                      원점수: <b>{point.value}</b><br>
                     백분위: <b>{point.percentile:.0f}%</br>")
}


# 대학생활 만족 수준 기록
{
  
    par_uni_adapt_df <- 
      long %>% 
      filter(str_detect(cate, "uni_adapt_") & std_id == parID) %>% 
      arrange(cate2) %>% 
      mutate(cate = case_when(cate == "uni_adapt_interpersonal" ~ "대인관계 만족도",
                              cate == "uni_adapt_major" ~ "전공 만족도",
                              cate == "uni_adapt_study" ~ "학업 만족도"))
    
    uni_adapt_value <- 
      long %>% 
      filter(str_detect(cate, "uni_adapt_")) %>% 
      select(value)
    uni_adapt_max <- max(uni_adapt_value, na.rm = T)
    
    #hovertemplate 문제로 구함(검사 시행 횟수가 1번일 때 오작동)
    num <- 
      d %>% 
      filter(std_id == parID & cate == 'uni_adapt') %>% 
      nrow()
    
    
    uni_adapt_plot1 <- 
      plot_ly() %>% 
      
      add_trace(
        data = par_uni_adapt_df,
        x = ~ x_axis,
        y = ~ value,
        color = ~ cate,
        type = "bar",
        text = ~ cate,
        hoverinfo = "text",
        hovertemplate = 
          case_when(num == 1 ~ hovertemplate1,
                    T ~ hovertemplate2),
        texttemplate = "<b>%{y: .0f}점</b>",
        textposition = "inside",
        insidetextanchor = "end",
        insidetextfont = list(color = "#7C7C7C", 
                              size = 9),
        legendgroup = ~ cate,
        showlegend = T
      ) %>% 
      
      layout(
        title = "",
        # bargap = 0.6,
        
        xaxis = list(title = "", 
                     showline = T
        ),
        
        yaxis = list(side = 'right',
                     title = "원점수",
                     showgrid = T,
                     zeroline = T,
                     range = c(0, uni_adapt_max))
        
        # legend = list(x = 100, y = 100)
        
      ) %>%
      
      config(displayModeBar = F)
    
    
    uni_adapt_plot2 <- 
      
      plot_ly() %>% 
      
      add_trace(
        data = par_uni_adapt_df,
        x = ~ x_axis,
        y = ~ percentile,
        color = ~ cate,
        type = "scatter",
        mode = "markers + lines",
        line = list(dash = "dash",
                    width = 2,
                    simplify = T),
        text = ~ cate,
        hoverinfo = "text",
        hovertemplate = 
          case_when(num == 1 ~ hovertemplate1_percentile,
                    T ~ hovertemplate2_percentile),
        # texttemplate = "<b>%{y: .0f}%</b>",
        # textposition = "outside",
        legendgroup = ~ cate,
        showlegend = F
      ) %>% 
      
      layout(
        title = "",
        # bargap = 0.6,
        
        xaxis = list(title = "", 
                     showline = F,
                     ticktest = list(""),
                     tickvals = list(""),
                     tickmode = "array"),
        
        yaxis = list(side = 'left',
                     title = "백분위",
                     showgrid = T,
                     zeroline = T,
                     range = c(0, 100))
        
      ) %>% 
    
    config(displayModeBar = F)
    
    uni_adapt_diary_plot <- subplot(uni_adapt_plot2, uni_adapt_plot1, nrows = 2, shareY = T)
  
  
}


# 대학생활 부적응 수준
{
  
    df_uni_maladapt <- 
      long %>% 
      filter(year == 2021 & month == 3 & cate %in% uni_maladapt & std_id == parID) %>% 
      arrange(match(cate, uni_maladapt)) %>% 
      mutate(cate = case_when(cate == "uni_maladapt_studyproblem" ~ "학업 문제",
                              cate == "uni_maladapt_alcoholproblem" ~ "음주 문제",
                              cate == "uni_maladapt_anger" ~ "적대감 문제",
                              cate == "uni_maladapt_eatingproblem" ~ "섭식 문제",
                              cate == "uni_maladapt_negativeemotion" ~ "부정적 정서 문제",
                              cate == "uni_maladapt_socialanxiety" ~ "사회불안 문제"))
    
    
    uni_maladapt_plot <- 
      highchart() %>%
      
      hc_colors(color = colSet[2]
      ) %>%
      
      hc_chart(polar = T) %>%
      hc_xAxis(
        categories = c("학업 문제",
                       "음주 문제",
                       "적대감 문제",
                       "섭식 문제",
                       "부정적 정서 문제",
                       "사회불안 문제"
        ),
        tickmarkPlacement = 'on',
        lineWidth = 0,
        labels = list(
          style = list(fontSize= '15px'))
      ) %>%
      
      hc_yAxis(
        gridLineInterpolation = "polygon",
        lineWidth = 0,
        tickInterval = 25,
        min = 0,
        max = 100,
        labels = list(format = "",
                      enabled=F)
      ) %>%
      
      hc_plotOptions(
        column = list(colorByPoint = F),
        series = list(
          fillOpacity = 0.25,
          dataLabels = list(
            enabled = T,
            align = "center",
            verticalAlign = "middle",
            format = "{point.y}",
            y = 0,
            color = "white",
            style = list(fontFamily = 'noto')
            
          ),
          dashStyle = "ShortDot"
        )
      ) %>% 
      
      hc_add_series(
        name = "본인",
        data = df_uni_maladapt,
        hcaes(cate, percentile),
        marker = list(radius = 12),
        pointWidth = 20,
        pointPlacement = "on",
        type = "area",
        zIndex= 2,
        dataLabels = list(
          enabled = T,
          align= "center",
          verticalAlign= "middle",
          format = "{point.y:.0f}",
          y = 0,
          color="white",
          style = list(
            fontFamily= 'noto', fontSize='20px')
        ) 
        
      ) %>%
      
      hc_tooltip(crosshairs = T,
                 enabled= T,
                 headerFormat = "",
                 pointFormat = "<b>{point.cate}</b><br>
                      원점수: <b>{point.value}</b><br>
                     백분위: <b>{point.percentile:.0f}%</br>")
}


# 대학생활 부적응 수준 기록
{
  
    par_uni_maladapt_df <- 
      long %>% 
      filter(str_detect(cate, "uni_maladapt_") & std_id == parID) %>% 
      filter(cate != "uni_maladapt_suicidalideation") %>%
      arrange(cate2) %>% 
      mutate(cate = case_when(cate == "uni_maladapt_alcoholproblem" ~ "음주 문제",
                              cate == "uni_maladapt_anger" ~ "적대감 문제",
                              cate == "uni_maladapt_eatingproblem" ~ "섭식 문제",
                              cate == "uni_maladapt_negativeemotion" ~ "부정적 정서 문제",
                              cate == "uni_maladapt_socialanxiety" ~ "사회불안 문제",
                              cate == "uni_maladapt_studyproblem" ~ "학업 문제"))
    # ,cate == "uni_maladapt_suicidalideation" ~ "자살 사고"))
    
    uni_maladapt_value <- 
      long %>% 
      filter(str_detect(cate, "uni_maladapt_")) %>% 
      select(value)
    uni_maladapt_max <- max(uni_maladapt_value, na.rm = T)
    
    #hovertemplate 문제로 구함(검사 시행 횟수가 1번일 때 오작동)
    num <- 
      d %>% 
      filter(std_id == parID & cate == 'uni_maladapt_alcoholproblem') %>% 
      nrow()
    
    
    uni_maladapt_plot1 <- 
      plot_ly() %>% 
      
      add_trace(
        data = par_uni_maladapt_df,
        x = ~ x_axis,
        y = ~ value,
        color = ~ cate,
        type = "bar",
        text = ~ cate,
        hoverinfo = "text",
        hovertemplate = 
          case_when(num == 1 ~ hovertemplate1,
                    T ~ hovertemplate2),
        texttemplate = "<b>%{y: .0f}점</b>",
        textposition = "outside",
        outsidetextfont = list(color = "#7C7C7C", 
                               size = 6),
        legendgroup = ~ cate,
        showlegend = T
      ) %>% 
      
      layout(
        title = "",
        # bargap = 0.6,
        
        xaxis = list(title = "", 
                     showline = T
        ),
        
        yaxis = list(side = 'right',
                     title = "원점수",
                     showgrid = T,
                     zeroline = T,
                     range = c(0, uni_maladapt_max))
        
        # legend = list(x = 100, y = 100)
        
      ) %>%
      
      config(displayModeBar = F)
    
    
    uni_maladapt_plot2 <- 
      plot_ly() %>% 
      
      add_trace(
        data = par_uni_maladapt_df,
        x = ~ x_axis,
        y = ~ percentile,
        color = ~ cate,
        type = "scatter",
        mode = "markers + lines",
        line = list(
          # dash = "dot",
          width = 2,
          simplify = T),
        text = ~ cate,
        hoverinfo = "text",
        hovertemplate = 
          case_when(num == 1 ~ hovertemplate1_percentile,
                    T ~ hovertemplate2_percentile),
        # texttemplate = "<b>%{y: .0f}%</b>",
        # textposition = "outside",
        legendgroup = ~ cate,
        showlegend = F
      ) %>% 
      
      layout(
        title = "",
        # bargap = 0.6,
        
        xaxis = list(title = "", 
                     showline = F,
                     ticktest = list(""),
                     tickvals = list(""),
                     tickmode = "array"),
        
        yaxis = list(side = 'left',
                     title = "백분위",
                     showgrid = T,
                     zeroline = T,
                     range = c(0, 100))
        
      ) %>% 
    
    config(displayModeBar = F)
    
    uni_maladapt_diary_plot <- subplot(uni_maladapt_plot2, uni_maladapt_plot1, nrows = 2, shareY = T)
  
}

# -------------
# 심리학적 조언 
# -------------
{
    comment_col_names <- sprintf('r_%d', 1:9)
    comment <- c()
    for (i in 1:9){
      comment[i] <- 
        comment_df %>% 
        filter(std_id == parID) %>% 
        pull(comment_col_names[i])
    }
}
