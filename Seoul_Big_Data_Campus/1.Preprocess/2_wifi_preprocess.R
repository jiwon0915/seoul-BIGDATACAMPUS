#=============================
# 와이파이 데이터를 불러와서 지역명에서 행정동 추출하기 위해 카카오API 사용
# 중복데이터를 제거하고 오류처리를 거듭합니다.
# 데이터 분석의 기준이 행정동이기 때문에, 행정동을 추출하는 과정입니다.

library(httr)
library(tidyverse)
library(jsonlite)

# usethis::edit_r_environ()
# 새로운 api 넣고 저장한다음 껐다 키기

# Sys.getenv('KAKAO_MAP_API_KEY')
# 이 코드 실행했을때 'KaKaoAK 본인api' 이렇게 나오면 된 것


data = read.csv("data/seoul_wifi.csv")

# 지역명 unique 처리
data_uniq = data[-which(duplicated(data$지역명)),]

# 지역명을 카카오api에 검색, 행정동 가져옴. 
dong_1 = NULL

for(i in 1:nrow(data_uniq)){
  print(i)
  addr = data_uniq$지역명[i]
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',query = list(query = addr),
             add_headers(Authorization = Sys.getenv('KAKAO_MAP_API_KEY')))
  coord <- res %>% content(as = 'text') %>% fromJSON()
  
  dong<-coord$documents$address$region_3depth_h_name
  if(length(dong)>=2){dong=dong[1]}
  if(length(dong)==0){dong="error"}
  dong_1<-c(dong_1,dong)
}
  
#668에서 에러발생
  
dong_2<-NULL

for(i in 669:nrow(data_uniq)){
  print(i)
  addr = data_uniq$지역명[i]
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = addr),
             add_headers(Authorization = Sys.getenv('KAKAO_MAP_API_KEY')))
  coord <- res %>% content(as = 'text') %>% fromJSON()
  dong<-coord$documents$address$region_3depth_h_name
  if(length(dong)>=2){dong=dong[1]}
  if(length(dong)==0){dong="error"}
  dong_2<-c(dong_2,dong)
}
  
# 668,992에서 에러발생

dong_3<-NULL

for(i in 993:nrow(data_uniq)){
  print(i)
  addr = data_uniq$지역명[i]
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = addr),
             add_headers(Authorization = Sys.getenv('KAKAO_MAP_API_KEY')))
  coord <- res %>% content(as = 'text') %>% fromJSON()
  dong<-coord$documents$address$region_3depth_h_name
  if(length(dong)>=2){dong=dong[1]}
  if(length(dong)==0){dong="error"}
  dong_3<-c(dong_3,dong)
}

# 668,992,1340에서 에러발생

dong_4<-NULL

for(i in 1341:nrow(data_uniq)){
  print(i)
  addr = data_uniq$지역명[i]
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = addr),
             add_headers(Authorization = Sys.getenv('KAKAO_MAP_API_KEY')))
  coord <- res %>% content(as = 'text') %>% fromJSON()
  dong<-coord$documents$address$region_3depth_h_name
  if(length(dong)>=2){dong=dong[1]}
  if(length(dong)==0){dong="error"}
  dong_4<-c(dong_4,dong)
}
  
# 668,992,1340,1350에서 에러발생

dong_5<-NULL

for(i in 1351:nrow(data_uniq)){
  print(i)
  addr = data_uniq$지역명[i]
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = addr),
             add_headers(Authorization = Sys.getenv('KAKAO_MAP_API_KEY')))
  coord <- res %>% content(as = 'text') %>% fromJSON()
  dong<-coord$documents$address$region_3depth_h_name
  if(length(dong)>=2){dong=dong[1]}
  if(length(dong)==0){dong="error"}
  dong_5<-c(dong_5,dong)
}
  
# 668,992,1340,1350,1358에서 에러발생

dong_6<-NULL

for(i in 1359:nrow(data_uniq)){
  print(i)
  addr = data_uniq$지역명[i]
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = addr),
             add_headers(Authorization = Sys.getenv('KAKAO_MAP_API_KEY')))
  coord <- res %>% content(as = 'text') %>% fromJSON()
  dong<-coord$documents$address$region_3depth_h_name
  if(length(dong)>=2){dong=dong[1]}
  if(length(dong)==0){dong="error"}
  dong_6<-c(dong_6,dong)
}

# 668,992,1340,1350,1358,1360에서 에러발생

dong_7<-NULL

for(i in 1361:nrow(data_uniq)){
  print(i)
  addr = data_uniq$지역명[i]
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = addr),
             add_headers(Authorization = Sys.getenv('KAKAO_MAP_API_KEY')))
  coord <- res %>% content(as = 'text') %>% fromJSON()
  dong<-coord$documents$address$region_3depth_h_name
  if(length(dong)>=2){dong=dong[1]}
  if(length(dong)==0){dong="error"}
  dong_7<-c(dong_7,dong)
}

# 668,992,1340,1350,1358,1360,1366에서 에러발생

dong_8<-NULL

for(i in 1367:nrow(data_uniq)){
  print(i)
  addr = data_uniq$지역명[i]
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = addr),
             add_headers(Authorization = Sys.getenv('KAKAO_MAP_API_KEY')))
  coord <- res %>% content(as = 'text') %>% fromJSON()
  dong<-coord$documents$address$region_3depth_h_name
  if(length(dong)>=2){dong=dong[1]}
  if(length(dong)==0){dong="error"}
  dong_8<-c(dong_8,dong)
}

# 668,992,1340,1350,1358,1360,1366,1372에서 에러발생
dong_9<-NULL

for(i in 1373:nrow(data_uniq)){
  print(i)
  addr = data_uniq$지역명[i]
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = addr),
             add_headers(Authorization = Sys.getenv('KAKAO_MAP_API_KEY')))
  coord <- res %>% content(as = 'text') %>% fromJSON()
  dong<-coord$documents$address$region_3depth_h_name
  if(length(dong)>=2){dong=dong[1]}
  if(length(dong)==0){dong="error"}
  dong_9<-c(dong_9,dong)
}

# 668,992,1340,1350,1358,1360,1366,1372,1389에서 에러발생

dong_10<-NULL

for(i in 1390:nrow(data_uniq)){
  print(i)
  addr = data_uniq$지역명[i]
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = addr),
             add_headers(Authorization = Sys.getenv('KAKAO_MAP_API_KEY')))
  coord <- res %>% content(as = 'text') %>% fromJSON()
  dong<-coord$documents$address$region_3depth_h_name
  if(length(dong)>=2){dong=dong[1]}
  if(length(dong)==0){dong="error"}
  dong_10<-c(dong_10,dong)
}

# 668,992,1340,1350,1358,1360,1366,1372,1389,1410에서 에러발생

dong_11<-NULL

for(i in 1411:nrow(data_uniq)){
  print(i)
  addr = data_uniq$지역명[i]
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = addr),
             add_headers(Authorization = Sys.getenv('KAKAO_MAP_API_KEY')))
  coord <- res %>% content(as = 'text') %>% fromJSON()
  dong<-coord$documents$address$region_3depth_h_name
  if(length(dong)>=2){dong=dong[1]}
  if(length(dong)==0){dong="error"}
  dong_11<-c(dong_11,dong)
}

# 668,992,1340,1350,1358,1360,1366,1372,1389,1410,1436에서 에러발생

dong_12<-NULL

for(i in 1437:nrow(data_uniq)){
  print(i)
  addr = data_uniq$지역명[i]
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = addr),
             add_headers(Authorization = Sys.getenv('KAKAO_MAP_API_KEY')))
  coord <- res %>% content(as = 'text') %>% fromJSON()
  dong<-coord$documents$address$region_3depth_h_name
  if(length(dong)>=2){dong=dong[1]}
  if(length(dong)==0){dong="error"}
  dong_12<-c(dong_12,dong)
}

# 668,992,1340,1350,1358,1360,1366,1372,1389,1410,1436,1451에서 에러발생

dong_13<-NULL

for(i in 1452:nrow(data_uniq)){
  print(i)
  addr = data_uniq$지역명[i]

  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = addr),
             add_headers(Authorization = Sys.getenv('KAKAO_MAP_API_KEY')))
  coord <- res %>% content(as = 'text') %>% fromJSON()
  dong<-coord$documents$address$region_3depth_h_name
  if(length(dong)>=2){dong=dong[1]}
  if(length(dong)==0){dong="error"}
  dong_13<-c(dong_13,dong)
}

# 668,992,1340,1350,1358,1360,1366,1372,1389,1410,1436,1451,1481에서 에러발생

dong_14<-NULL

for(i in 1482:nrow(data_uniq)){
  print(i)
  addr = data_uniq$지역명[i]
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = addr),
             add_headers(Authorization = Sys.getenv('KAKAO_MAP_API_KEY')))
  coord <- res %>% content(as = 'text') %>% fromJSON()
  dong<-coord$documents$address$region_3depth_h_name
  if(length(dong)>=2){dong=dong[1]}
  if(length(dong)==0){dong="error"}
  dong_14<-c(dong_14,dong)
}

# 668,992,1340,1350,1358,1360,1366,1372,1389,1410,1436,1451,1481,1493에서 에러발생

dong_15<-NULL

for(i in 1494:nrow(data_uniq)){
  print(i)
  addr = data_uniq$지역명[i]
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = addr),
             add_headers(Authorization = Sys.getenv('KAKAO_MAP_API_KEY')))
  coord <- res %>% content(as = 'text') %>% fromJSON()
  dong<-coord$documents$address$region_3depth_h_name
  if(length(dong)>=2){dong=dong[1]}
  if(length(dong)==0){dong="error"}
  dong_15<-c(dong_15,dong)
}

# 668,992,1340,1350,1358,1360,1366,1372,1389,1410,1436,1451,1481,1493,1501에서 에러발생

dong_16<-NULL

for(i in 1502:nrow(data_uniq)){
  print(i)
  addr = data_uniq$지역명[i]
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = addr),
             add_headers(Authorization = Sys.getenv('KAKAO_MAP_API_KEY')))
  coord <- res %>% content(as = 'text') %>% fromJSON()
  dong<-coord$documents$address$region_3depth_h_name
  if(length(dong)>=2){dong=dong[1]}
  if(length(dong)==0){dong="error"}
  dong_16<-c(dong_16,dong)
}

# 668,992,1340,1350,1358,1360,1366,1372,1389,1410,1436,1451,1481,1493,1501,3092에서 에러발생

dong_17<-NULL

for(i in 3093:nrow(data_uniq)){
  print(i)
  addr = data_uniq$지역명[i]
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = addr),
             add_headers(Authorization = Sys.getenv('KAKAO_MAP_API_KEY')))
  coord <- res %>% content(as = 'text') %>% fromJSON()
  dong<-coord$documents$address$region_3depth_h_name
  if(length(dong)>=2){dong=dong[1]}
  if(length(dong)==0){dong="error"}
  dong_17<-c(dong_17,dong)
}

# 668,992,1340,1350,1358,1360,1366,1372,1389,1410,1436,1451,1481,1493,1501,3092.3101에서 에러발생

dong_18<-NULL

for(i in 3102:nrow(data_uniq)){
  print(i)
  addr = data_uniq$지역명[i]
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = addr),
             add_headers(Authorization = Sys.getenv('KAKAO_MAP_API_KEY')))
  coord <- res %>% content(as = 'text') %>% fromJSON()
  dong<-coord$documents$address$region_3depth_h_name
  if(length(dong)>=2){dong=dong[1]}
  if(length(dong)==0){dong="error"}
  dong_18<-c(dong_18,dong)
}

# 668,992,1340,1350,1358,1360,1366,1372,1389,1410,1436,1451,1481,1493,1501,3092.3101,4023에서 에러발생

dong_19<-NULL

for(i in 4024:nrow(data_uniq)){
  print(i)
  addr = data_uniq$지역명[i]
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = addr),
             add_headers(Authorization = Sys.getenv('KAKAO_MAP_API_KEY')))
  coord <- res %>% content(as = 'text') %>% fromJSON()
  dong<-coord$documents$address$region_3depth_h_name
  if(length(dong)>=2){dong=dong[1]}
  if(length(dong)==0){dong="error"}
  dong_19<-c(dong_19,dong)
}

# na_list<-c(668,992,1340,1350,1358,1360,1366,1372,1389,1410,1436,1451,1481,1493,1501,3092,3101,4023)

dong_final<-c(dong_1,NA,dong_2,NA,dong_3,NA,dong_4,NA,dong_5,
              NA,dong_6,NA,dong_7,NA,dong_8,NA,dong_9,NA,dong_10,
              NA,dong_11,NA,dong_12,NA,dong_13,NA,dong_14,NA,dong_15,
              NA,dong_16,NA,dong_17,NA,dong_18,NA,dong_19)

data_uniq$dong<-dong_final

rm(dong_1,dong_2,dong_3,dong_4,dong_5,dong_6,dong_7,dong_8,dong_9,
   dong_10,dong_11,dong_12,dong_13,dong_14,dong_15,dong_16,dong_17,
   dong_18,dong_19)
rm(i)
rm(dong_final)

# write.csv(data_uniq, file = "data/와이파이_수정필요.csv")

# data_uniq<-read.csv("와이파이_수정필요.csv")

# data_uniq %>% head()

# xy 좌표겹치는거 유니크처리
data_uniq$xy = paste0(data_uniq$설치위치.X좌표.,
                      data_uniq$설치위치.Y좌표.)
data_uniq = data_uniq[!duplicated(data_uniq$xy),]

# 행정동 검색했을때 동 안나오거나 에러떠서 NA처리한 obs 처리필요
idx_error_NA<-which(data_uniq$dong=="error"|is.na(data_uniq$dong)==TRUE)

data_err<-data_uniq[idx_error_NA,]

# data_err %>% head()

# write.csv(data_err$지역명, file="data/err_data.csv", row.names=F)

## 파이썬에 가져가서 데이터 전처리를 마무리 하겠습니다.



