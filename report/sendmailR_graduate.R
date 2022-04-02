# --------
# setting
# --------
# Sys.setenv(JAVA_HOME = '/Library/Java/JavaVirtualMachines/jdk1.8.0_231.jdk/Contents/Home') #ZSU_17
Sys.setenv(JAVA_HOME = '/Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home') #ZSU_15

#JAVA 8로 환경 변수 설정되어 있는지 확인
Sys.getenv('JAVA_HOME')

pacman::p_load('rJava', 'mailR', 'devtools', 'magrittr', 'tidyverse')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ------------------
# list to send mail
# ------------------

par_info <- 
  googlesheets4::read_sheet(
  'https://docs.google.com/spreadsheets/d/1ao_ZU-JDrAjAfyNOXYTVKc4-s3xJoPbNsEe1rrrjhFI/edit#gid=1494048188',
  sheet = '시트3')


# 결과지 주소
list <- list.files(path = "./html",
                   full.names = T,
                   pattern = "*.html")

# 최종 DF
par <- 
  par_info %>% 
  mutate(std_id = as.character(std_id)) %>% 
  left_join(data.frame(file_directory = list) %>% 
            mutate(std_id = str_extract(
              pattern = "(?<=\\.\\/html\\/).*(?=\\.html)", string = file_directory)),
            # mutate(Name = stringi::stri_trans_nfc(Name)),
            by = "std_id")
par <- par %>% 
  filter(!is.na(file_directory))

# 확인용 만들기
example_dir <- par[1,3]

par <- bind_rows(
  data.frame(std_id = "확인용", 
             email = "js94park@naver.com", 
             file_directory = example_dir), 
  par)

# ------
# check 
# ------

par <- par %>% mutate(email = gsub(" ", "", email))

par <- par %>% 
  mutate(email = gsub(" ", "", email),
         email = gsub(",", ".", email)) %>% 
  mutate(n = 1:n())

#.con
par %>% filter(str_detect(email, "\\.con"))

par <- par %>%
  mutate(email = gsub("\\.con", "\\.com", email))

#check email
rbind(
  par %>% filter(str_detect(email, "kroea\\.")),
  par %>% filter(str_detect(email, "gamil\\.")),
  par %>% filter(str_detect(email, "kprea\\.")),
  par %>% filter(str_detect(email, "korra\\.")),
  par %>% filter(str_detect(email, "korrea\\.")),
  par %>% filter(str_detect(email, "koreq\\.")),
  par %>% filter(str_detect(email, "korae\\.")),
  par %>% filter(str_detect(email, "\\.ke")),
  par %>% filter(str_detect(email, "naveer\\.")),
  par %>% filter(str_detect(email, "navaer\\.")),
  par %>% filter(str_detect(email, "\\.acc\\.kr")),
  par %>% filter(str_detect(email, "\\.ackr")),
  par %>% filter(str_detect(email, "ac\\.jr")),
  par %>% filter(str_detect(email, "korea\\.com")),
  par %>% filter(str_detect(email, "korea\\.c\\.kr")),
  par %>% filter(str_detect(email, "ac\\.mr")),
  par %>% filter(str_detect(email, "\\.ackr")),
  par %>% filter(str_detect(email, "ac\\.r")),
  par %>% filter(str_detect(email, "navee\\.")),
  par %>% filter(str_detect(email, "portal\\.korea")),
  par %>% filter(str_detect(email, "korea\\.co\\.kr")),
  par %>% filter(str_detect(email, "[가-힣]")),
  par %>% filter(!str_detect(email, "@")),
  
  par %>% mutate(string_length = nchar(email)) %>% filter(string_length < 12) %>% select(-string_length)
) %>% 
  arrange(n) %>% 
  unique() %>% 
  View()

#check file_directory
par %>% filter(is.na(email))
par %>% filter(is.na(file_directory))

#최종 점검
par %>% View()

# ------------------------
# send to the first email
# ------------------------
{
send.mail(from         = "psy_test@naver.com",                      # 보내는 사람 주소
          to           = par$email[1],                              # 받는 사람 주소
          # cc           = "js94park@naver.com",                    # 참조
          # bcc           = "js94park@naver.com",                   # 숨은 참조
          subject      = "[학생상담센터] 빅데이터 기반 온라인 학교생활 결과지", # 메일제목
          body         =
            "안녕하세요, 고려대학교 학생상담센터입니다. <br><br>

지난 학기 블랙보드 로그 기록과 정신건강 실태조사를 결합하여 제작된 빅데이터 기반 온라인 학교생활 결과지를 보내드립니다. <br>
다음 학기 계획 세우시는데 본 결과지가 도움이 되었으면 합니다.<br><br>

<b>본 결과지 이용 시 아래의 사항을 숙지해 주시기 바랍니다.</b> <br>
1. 본 결과지는 핸드폰으로는 열리지 않으므로 컴퓨터나 노트북을 이용하여 다운 받으십시오.<br>
2. 본 결과지는 구글 크롬으로 열어야 오류 없이 확인하실 수 있습니다. <br>
3. 약 5MB의 대용량 결과지인 관계로 내용이 화면에 나오는 데 오래 걸릴 수 있습니다. <br>
4. 인터넷 익스프롤러에서는 파일이 열리지 않으며 크롬, 파이어폭스, 사파리 등 그 외 대부분의 브라우저에서는 정상적으로 열립니다. <br><br>

학생상담센터는 언제나 여러분의 건강하고 효과적인 학교 생활을 돕기 위해 최선을 다하겠습니다. <br>
항상 학생상담센터 프로그램에 관심을 가져주시고 적극적으로 참여해주셔서 감사드립니다. <br>

고려대학교 학생상담센터 드림",                         # 메일내용
          smtp         = list(host.name = "smtp.naver.com",         # 메일서버 연동 정보
                              port = 587,
                              user.name = "psy_test@naver.com",
                              passwd = "***************",
                              ssl = TRUE),
          encoding     = "utf-8",                                   # 인코딩(고정값)
          authenticate = TRUE,                                      # 인증사용 여부(고정값)
          send         = TRUE,
          html = TRUE,
          attach.files = c(par$file_directory[1]),
          # file.names = c("option"),
          # file.descriptions = c("option"),
          debug = F)

  print(paste0(1, "th email sent to [", par$email[1],"], ", length(par$email) - 1, " email left"))
}

# -----
# send
# -----
for(i in 1:length(par$email)){

  send.mail(from         = "psy_test@naver.com",                      # 보내는 사람 주소
            to           = par$email[i],                              # 받는 사람 주소
            # cc           = "js94park@naver.com",                    # 참조
            # bcc           = "js94park@naver.com",                   # 숨은 참조
            subject      = "[학생상담센터] 빅데이터 기반 온라인 학교생활 결과지", # 메일제목
            body         =
              "안녕하세요, 고려대학교 학생상담센터입니다. <br><br>

지난 학기 블랙보드 로그 기록과 정신건강 실태조사를 결합하여 제작된 빅데이터 기반 온라인 학교생활 결과지를 보내드립니다. <br>
다음 학기 계획 세우시는데 본 결과지가 도움이 되었으면 합니다.<br><br>

<b>본 결과지 이용 시 아래의 사항을 숙지해 주시기 바랍니다.</b> <br>
1. 본 결과지는 핸드폰으로는 열리지 않으므로 컴퓨터나 노트북을 이용하여 다운 받으십시오.<br>
2. 본 결과지는 구글 크롬으로 열어야 오류 없이 확인하실 수 있습니다. <br>
3. 약 5MB의 대용량 결과지인 관계로 내용이 화면에 나오는 데 오래 걸릴 수 있습니다. <br>
4. 인터넷 익스프롤러에서는 파일이 열리지 않으며 크롬, 파이어폭스, 사파리 등 그 외 대부분의 브라우저에서는 정상적으로 열립니다. <br><br>

학생상담센터는 언제나 여러분의 건강하고 효과적인 학교 생활을 돕기 위해 최선을 다하겠습니다. <br>
항상 학생상담센터 프로그램에 관심을 가져주시고 적극적으로 참여해주셔서 감사드립니다. <br>

고려대학교 학생상담센터 드림",                         # 메일내용
            smtp         = list(host.name = "smtp.naver.com",         # 메일서버 연동 정보
                                port = 587,
                                user.name = "psy_test@naver.com",
                                passwd = "***************",
                                ssl = TRUE),
            encoding     = "utf-8",                                   # 인코딩(고정값)
            authenticate = TRUE,                                      # 인증사용 여부(고정값)
            send         = TRUE,
            html = TRUE,
            attach.files = c(par$file_directory[i]),
            # file.names = c("option"),
            # file.descriptions = c("option"),
            debug = F)


  print(paste0(i, "th email sent to [", par$email[i],"], ", length(par$email) - i, " email left"))

}
