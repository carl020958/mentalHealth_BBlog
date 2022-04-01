pacman::p_load('tidyverse')

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 전체 파일 가져오기
all_files <-
  tibble(path = list.files(path = 'html', full.names = T)) %>%
  mutate(path = stringi::stri_trans_nfc(path)) %>%
  pull(path)

# ------------------------
# upload in google drive
# ------------------------
for(i in all_files){
  print(i)
  print(paste0(i, "th file uploaded [", length(all_files) - i, "] files left"))
  googledrive::drive_upload(
    media = i,
    path = googledrive::as_id('https://drive.google.com/drive/u/0/folders/1PdEnn45tiFxMP_YP-Oh8AfA8WREyWYWs')
  )
}

# check
drive_list <- 
  googledrive::drive_ls('https://drive.google.com/drive/u/0/folders/1PdEnn45tiFxMP_YP-Oh8AfA8WREyWYWs')
