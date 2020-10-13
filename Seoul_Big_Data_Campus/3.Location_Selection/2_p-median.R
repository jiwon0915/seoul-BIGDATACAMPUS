#### p-median 알고리즘으로 우선입지를 선정하는 코드
#### 번3동, 상계3.4동 대상

library(dplyr)
library(data.table)
##### p=median function #####
library(tbart) # P-Median 함수가 저장되어 있는 패키지
library(geosphere) # 위경도로 거리를 구할 수 있는 패키지

# 거리계산한 행렬을 먼저 생성하고, 이거를 사용해서 p-median을 사용하자.
make_dis_mat <- function(lon, lat, p_median) {
  n = length(lon)
  dist_mat <- matrix(NA, n, n)
  for (i in 1:n) {
    lon1 <- lon[i]
    lat1 <- lat[i]
    if (i %% 20 == 0) {
      cat(i, '')
    }
    for (j in 1:n) {
      lon2 <- lon[j]
      lat2 <- lat[j]
      dist_mat[i, j] <- distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
    }
  } # 거리를 1대1 대응을 통해 구함
  return(dist_mat)
}

p_median <- function(distance_matrix, num_p, seed) {
  set.seed(seed) # 시드 지정
  initial_value = sample(1:nrow(distance_matrix), num_p) # 초기값을 랜덤으로 p개 선정
  result_vec <- tb.raw(distance_matrix, initial_value, T)  # 초기값 넣어서 p_median 알고리즘 적용
  distance_matrix[-result_vec, -result_vec] <- 0
  distance_matrix[result_vec, ] <- 0 # 중심점들과 연결점들만 남김
  
  for_vec <-  1:nrow(distance_matrix)
  for_vec <- for_vec %>% setdiff(result_vec)
  for (i in for_vec) {
    a <- distance_matrix[i, result_vec]
    a[which(a != distance_matrix[i, result_vec] %>% min)] <- 0 
    # 중심점과 연결점의 조합 중 최단거리만 남김
    distance_matrix[i, result_vec] <- a 
  }
  return(distance_matrix)
}

### 데이터 불러오기
dat <- fread("data/building_five.csv")
dat_w <- fread("data/wifi_dong.csv")


#########################
###### 번3동
b3_b <- dat %>% filter(dong == "번3동") #번3동 건물
b3_w <- dat_w %>% filter(dong == "번3동") #번3동 와이파이

sg34_b <- dat %>% filter(dong == "상계3.4동")
sg34_w <- dat_w %>% filter(dong == "상계3.4동")

b3_bp <- b3_b %>% filter(건물용도 %in%
                               c("관망탑", "미술관", "파출소", 
                                 "노인복지시설","우체국", "기타교육연구및복지시설", 
                                 "근로복지시설", "동사무소", "기타공공업무시설"))
b3_b %>% filter(건물용도 == "기타공공업무시설") #뭐있는지 확인해본거

sg34_bp <- sg34_b %>% filter(건물용도 %in% 
                                   c("노인복지시설", "우체국", "사회복지시설", "파출소", "기타공공업무시설", "동사무소"))

sg34_b %>% filter(건물용도 =="초등학교") #초등학교는 여러개 있으니까 위에서 안넣고 뭐뭐있는지만 확인해서 일일이 찍어서 넣은거
unique(sg34_b$건물용도)

# 공원이랑 초등학교 좌표, 빠진 공공장소 직접 넣기

b3_bp <- rbind(b3_bp, list(NA, NA, "오현초등학교", NA, NA, NA, 37.623169, 127.046779, NA, NA, NA),
                      list(NA, NA, "번동초등학교", NA, NA, NA, 37.628947, 127.040778, NA, NA, NA)) 

sg34_bp <- rbind(sg34_bp, list(NA, NA, "신상계초등학교", NA, NA, NA, 37.667973, 127.075775, NA, NA, NA),
                          list(NA, NA, "덕암초등학교", NA, NA, NA, 37.666551, 127.081293, NA, NA, NA),
                          list(NA, NA, "한국 SGI 노원문화회관", NA, NA, NA, 37.668248, 127.076457, NA, NA, NA),
                          list(NA, NA, "수락산당고개지구공원", NA, NA, NA, 37.668740, 127.076942, NA, NA, NA))

b3_b <- rbind(b3_b, b3_bp, b3_bp, b3_bp) # 가중치줄라고 여러번 rbind

b3_bwd <- data.frame()

for (i in 1:nrow(b3_w)){
  lon_w <- as.numeric(b3_w$설치위치.X좌표.[i]) 
  lat_w <- as.numeric(b3_w$설치위치.Y좌표.[i])
  for (j in 1:nrow(b3_b)){
    lon_b <- b3_b$long[j]
    lat_b <- b3_b$lat[j]
    
    distance <- distm(c(lon_w, lat_w), c(lon_b, lat_b), fun = distHaversine)
    temp <- b3_b[j,]
    
    if(distance < 50){
      b3_bwd <- rbind(b3_bwd, temp) # 거리 50m 보다 작으면 데이터프레임에 추가
    }
  }
}

nrow(unique(b3_bwd)) # 겹치는 곳 있을 수도 있으니
b3_bl <- anti_join(b3_b, b3_bwd) # 원래 빌딩목록에서 anti join



######

b3_lon <- b3_bl$long
b3_lat <- b3_bl$lat

# 거리행렬 계산
b3_distmat = make_dis_mat(b3_lon, b3_lat)

# P-median 알고리즘 계산
b3_1 = p_median(b3_distmat, 1, seed = 42)
# Configuration: 143  Score: 105483.7 
b3_2 = p_median(b3_distmat, 2, seed = 42)
# Configuration:  133 225  Score: 67716.86
b3_3 = p_median(b3_distmat, 3, seed = 42)
# Configuration:  90 247 300  Score: 52781.62 
b3_4 = p_median(b3_distmat, 4, seed = 42)
# Configuration:  110 19 163 79  Score: 46288.42 
b3_5 = p_median(b3_distmat, 5, seed = 42)
# Configuration:  110 16 300 79 6  Score: 40079.72 
b3_6 = p_median(b3_distmat, 6, seed = 42)
# Configuration:  16 6 151 79 218 110  Score: 36367.26
b3_7 = p_median(b3_distmat, 7, seed = 42)
# Configuration:  6 233 110 79 178 151 218  Score: 33905.12 
b3_8 = p_median(b3_distmat, 8, seed = 42)
# Configuration:  111 233 178 79 6 148 218 344  Score: 32012.81 

options(scipen=999)
plot(x = 1:8, y = c(105483.7, 67716.86,52781.62 , 46288.42 , 40079.72, 36367.26, 33905.12, 32012.81), type = 'b', ylab = '', main = '번3동')
options(scipen =0)

b3_5 %>% View

unique(sg34_4[,51])

#########################
#### 상계3.4동

sg34_b <- dat %>% filter(dong == "상계3.4동")
sg34_w <- dat_w %>% filter(dong == "상계3.4동") 

sg34_bwd <- data.frame() # 기존 와이파이 반경 내에 있는 건물 담아둘 데이터프레임

for (i in 1:nrow(sg34_w)){
  lon_w <- as.numeric(sg34_w$설치위치.X좌표.[i]) 
  lat_w <- as.numeric(sg34_w$설치위치.Y좌표.[i])
  for (j in 1:nrow(sg34_b)){
    lon_b <- sg34_b$long[j]
    lat_b <- sg34_b$lat[j]
    
    distance <- distm(c(lon_w, lat_w), c(lon_b, lat_b), fun = distHaversine)
    temp <- sg34_b[j,]
    
    if(distance < 50){
      sg34_bwd <- rbind(sg34_bwd, temp) # 거리 50m 보다 작으면 데이터프레임에 추가
    }
  }
}

nrow(unique(sg34_bwd)) # 겹치는 곳 있을 수도 있으니
sg34_bwd <- unique(sg34_bwd)
sg34_bl <- anti_join(sg34_b, sg34_bwd) # 원래 빌딩목록에서 anti join

sg34_lon <- sg34_bl$long
sg34_lat <- sg34_bl$lat

sg34_distmat = make_dis_mat(sg34_lon, sg34_lat)

sg34_1 = p_median(sg34_distmat, 1, seed = 42)
# Configuration:  2806  Score: 1789002 
sg34_2 = p_median(sg34_distmat, 2, seed = 42)
# Configuration:  1628 2683  Score: 1094281
sg34_3 = p_median(sg34_distmat, 3, seed = 42)
# Configuration:  1622 2486 1202  Score: 839918.6
sg34_4 = p_median(sg34_distmat, 4, seed = 42)
# Configuration:  1622 2772 3157 1202  Score: 695454.9 
sg34_5 = p_median(sg34_distmat, 5, seed = 42)
# Configuration:  237 2772 861 3447 3157  Score: 616191.8 
sg34_6 = p_median(sg34_distmat, 6, seed = 42)
# Configuration:  2977 2600 861 1521 2303 3666  Score: 550372.3
sg34_7 = p_median(sg34_distmat, 7, seed = 42)
#Configuration:  3191 2600 861 3248 2303 11 238  Score: 513023.4 
sg34_8 = p_median(sg34_distmat, 8, seed = 42)
# Configuration:  798 3293 3478 1567 2948 1668 475 3585  Score: 490427 

options(scipen=999)
plot(x = 1:8, y = c(1789002, 1094281, 839918.6, 695454.9, 616191.8,550372.3, 513023.4, 490427), type = 'b', ylab = '', main = '상계34동')
options(scipen =0)

############