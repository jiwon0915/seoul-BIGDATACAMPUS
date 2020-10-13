#=============================
library(dplyr)

library(httr)
library(tidyverse)
library(jsonlite)

# usethis::edit_r_environ()
# 새로운 api 넣고 저장한다음 껐다 키기

# Sys.getenv('KAKAO_MAP_API_KEY')
# 이 코드 실행했을때 'KaKaoAK 본인api' 이렇게 나오면 된 것


data = read.csv("서울시 공공와이파이 위치정보.csv")

# 지역명 unique 처리
data_uniq = data[-which(duplicated(data$지역명)),]

# 지역명을 카카오api에 검색, 행정동 가져옴. 
dong_1=NULL
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

# write.csv(data_uniq,file="와이파이_수정필요.csv")

# data_uniq<-read.csv("와이파이_수정필요.csv")

# data_uniq %>% head()

# xy 좌표겹치는거 유니크처리
data_uniq$xy=paste0(data_uniq$설치위치.X좌표.,
                    data_uniq$설치위치.Y좌표.)
data_uniq = data_uniq[!duplicated(data_uniq$xy),]

# 행정동 검색했을때 동 안나오거나 에러떠서 NA처리한 obs 처리필요
idx_error_NA<-which(data_uniq$dong=="error"|is.na(data_uniq$dong)==TRUE)

data_err<-data_uniq[idx_error_NA,]

# data_err %>% head()

# write.csv(data_err$지역명,file="err_data.csv",row.names=F)

####파이썬에서 주소처리####
addr_err<-read_csv("err_addr2.csv")
#############################

addr_err<-data.frame(addr_err)

for(i in 1:nrow(addr_err)){
  addr_err$y[i]=strsplit(addr_err$위경도[i],',')[[1]][1]
  addr_err$x[i]=strsplit(addr_err$위경도[i],',')[[1]][2]
  }

addr_err<-addr_err %>% select(-위경도)
addr_err<-addr_err %>% select(-X1)

data_uniq[idx_error_NA,'설치위치.X좌표.']=addr_err$x
data_uniq[idx_error_NA,'설치위치.Y좌표.']=addr_err$y

# 다시한번 유니크처리
data_uniq$xy=paste0(data_uniq$설치위치.X좌표.,
                    data_uniq$설치위치.Y좌표.)
data_uniq = data_uniq[!duplicated(data_uniq$xy),]

addr_err$xy=paste0(addr_err$x,addr_err$y)
addr_err = addr_err[!duplicated(addr_err$xy),]

idx_error_NA<-which(data_uniq$dong=="error"|is.na(data_uniq$dong)==TRUE)

# 이제 제대로 불러온 주소로 다시 행정동 불러와보자
dong_err<-NULL
for(i in 1:nrow(addr_err)){
  print(i)
  addr = addr_err$주소[i]
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = addr),
             add_headers(Authorization = Sys.getenv('KAKAO_MAP_API_KEY')))
  coord <- res %>% content(as = 'text') %>% fromJSON()
  dong<-ifelse(is.null(coord$documents$address$region_3depth_h_name),"NULL",
               coord$documents$address$region_3depth_h_name)
  dong_err<-c(dong_err,dong)
}

# dong_err

# length(idx_error_NA)
data_uniq[idx_error_NA,'dong']=dong_err

# 그래도 null인것은 직접 & 이상한 지역명은 고쳐줌
# data_uniq[which(data_uniq$dong=="NULL"),c(4,11)]   
  
dong_sub<-c('신사동','신사동','논현1동','천호2동',
            '번3동','수유3동','송중동','등촌1동',
            '발산1동','가양3동','신원동','은천동','청룡동',
            '구의2동','구로5동','오류2동','고척1동','시흥4동',
            '하계2동','상계3.4동','공릉2동','중계4동','월계2동',
            '용신동','신대방2동','연희동','홍은1동','금호1가동',
            '성수1가1동','성수1가1동','성수1가1동','응봉동','장위3동',
            '보문동','월곡2동','월곡2동','신월2동','목2동','여의동','영등포본동','신길1동',
            '신길1동','회현동','왕십리2동','회현동','소공동','청운효자동')
  
data_uniq[which(data_uniq$지역명=='x'),'지역명']='구로5동 501-22'
data_uniq[which(data_uniq$지역명=='구로5동 501-22'),
          c('설치위치.X좌표.','설치위치.Y좌표.','xy')]=c('126.884342','37.500370','126.88434237.500370')
  
data_uniq[which(data_uniq$지역명=='성북구 원곡동 852-2'),
          c('지역명','설치위치.X좌표.','설치위치.Y좌표.','xy')]=
  c('서울특별시 성북구 상월곡동 97-22','127.047797','37.605879',
    '127.04779737.605879')

data_uniq[which(data_uniq$지역명=='용산구 후암로 60길 21'),
          c('지역명','설치위치.X좌표.','설치위치.Y좌표.','xy')]=
  c('서울특별시 중구 남대문로5가 후암로60길','126.974251',
    '37.554778','126.97425137.554778')

data_uniq[which(data_uniq$dong=="NULL"),'dong']=dong_sub

rm(dong_sub,dong_err)

# 신사동 중복 문제 해결
data_uniq[which(data_uniq$dong=="신사동"&
                data_uniq$구명=="강남구"),'dong']=c("신사동_강")
data_uniq[which(data_uniq$dong=="신사동"&
                data_uniq$구명=="관악구"),'dong']=c("신사동_관")
  
# data_uniq %>% group_by(dong) %>% summarise(n=n())

# 카카오api 검색 시 오류 대신 공백처리된 dong 처리
data_uniq[which(data_uniq$dong==""),'dong']=c('하계2동','잠실본동','당산1동','명동')

# write.csv(data_uniq,file='wifi_dong.csv',row.names = F)
  
dong_wifi_n<-data_uniq %>% group_by(dong) %>% summarise(n=n())
  
# write.csv(dong_wifi_n,file = "dong_wifi_n.csv",row.names = F)
  
###################
#행정동 코드랑 조인
seoul_dong<-read.csv("행정동코드.csv", fileEncoding = "UCS-2LE")
colnames(seoul_dong)[5]="dong"

seoul_dong[which(seoul_dong$CT_NM.시군구명.=="강남구"&seoul_dong$dong=="신사동"),'dong']<-"신사동_강"
seoul_dong[which(seoul_dong$CT_NM.시군구명.=="관악구"&seoul_dong$dong=="신사동"),'dong']<-"신사동_관"
join<-dong_wifi_n %>% left_join(seoul_dong,by=c('dong'))
err_dong<-join[which(is.na(join$H_SDNG_CD.통계청행정동코드.)==TRUE),'dong'] %>% unlist()

# data_uniq[which(data_uniq$dong %in% err_dong),]

# 지역명을 카카오 api에 검색했을 때 서울외 지역으로 떠서 오류(위경도 잘못x 카카오 잘못) -> 애초에 구글에 검색할까? 생각해봤지만 여기는 행정동이 바로 안뜨기 때문에 카카오에 넣은 것 그리고 구글 정확도 100%라고 장담불가, 하루에 2500개 제한 
# 구글에 지역명 검색해서 가져온 위경도 좌표도 틀린 경우 O(독산1동)
# 상세설치장소 입력 오류(위경도는 맞음)
data_uniq[which(data_uniq$dong %in% err_dong),'dong']<-c('도곡2동','오류2동','오류2동',
                                                         '오류2동','오류2동','오류1동','독산1동',
                                                         '상암동','양재2동')
  
# 상세설치장소/지역명/좌표 입력오류 수정
data_uniq[which(data_uniq$상세설치장소=='영동3교 남단하부 산책로'),'지역명']<-c('서울특별시 강남구 도곡2동')
data_uniq[which(data_uniq$지역명=='오류로 75'),'지역명']<-c('서울 구로구 오류로 73')
data_uniq[which(data_uniq$지역명=='경인로 221'),'지역명']<-c('서울 구로구 경인로 220')
data_uniq[which(data_uniq$지역명=='금천구 금하로1가길9'),c('지역명',
    '설치위치.X좌표.','설치위치.Y좌표.','xy')]<-c('금천구 금하로1가길10',
    '126.891810','37.452609','126.89181037.452609')
  
data_uniq[which(data_uniq$지역명=='상암동 481번지(난지하늘다리 우측 상암A2 강관주)'),'지역명']<-c("서울시 마포구 상암동")
  
data_uniq[which(data_uniq$지역명=='경기도 과천시 장군마을3길 30'),c('지역명','설치위치.X좌표.','설치위치.Y좌표.','xy')]<-c('서울 서초구 양재동 202-3',
    '127.033411455815','37.4647789552063','127.03341145581537.4647789552063')
  
  
# 재그룹화
dong_wifi_n<-data_uniq %>% group_by(dong) %>% summarise(n=n())
  
# 다시 조인
join<-dong_wifi_n %>% full_join(seoul_dong,by=c('dong'))
join[which(is.na(join$n)==TRUE),'n']=0
  
  
# write.csv(data_uniq, file='wifi_dong.csv',row.names=F)
# write.csv(join,file="dong_wifi_with_dongcode.csv",row.names = F)

a<-read.csv('wifi_dong.csv')
a[which(a$dong=="종로1.2.3.4가동"),]
