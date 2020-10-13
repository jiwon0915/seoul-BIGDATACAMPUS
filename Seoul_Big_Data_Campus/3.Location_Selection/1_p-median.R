# P-median알고리즘을 활용해 도봉1동, 장위2동, 삼성동의 우선입지선정을 진행한다.
# 상계3.4동과 번3동은 다른 파일을 열어주세요


## P-median by buliding data

library(tbart) # P-Median 함수가 저장되어 있는 패키지
library(geosphere) # 위경도로 거리를 구할 수 있는 패키지
library(data.table)
library(tidyverse)

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

################### 여기까진 함수정의 ######################3

## 데이터 불러오기
data = fread('data/building_five.csv')
wifi <- fread("data/wifi_dong.csv")

##################### 도봉 1동 ####################

dobong1 = data %>% filter(dong == '도봉1동')

yongdo = c('우체국', '초등학교', '노인복지시설', '사회복지시설', '동사무소', '기타문화및집회시설', '중학교', '파출소',
           '기타교육연구및복지시설')
dobong1 %>% filter(건물용도 %in% yongdo) %>% select(-건물용도코드)

dobong1_lon = data %>% filter(dong == '도봉1동') %>% select(long) %>% unlist() %>% as.vector()
dobong1_lat = data %>% filter(dong == '도봉1동') %>% select(lat) %>% unlist() %>% as.vector()
dobong1_lon = c(dobong1_lon, rep(127.0453, 3), rep(127.0403, 3), rep(127.03932, 3), rep(127.0421, 3), rep(127.0385, 3), 
                rep(127.0434, 3), rep(127.0386, 3), rep(127.0433, 3), rep(127.0436, 3), rep(127.0434, 3))
dobong1_lat = c(dobong1_lat, rep(37.68222, 3), rep(37.67172, 3), rep(37.67232, 3), rep(37.69427, 3), rep(37.67985, 3), 
                rep(37.67869, 3),rep(37.68734, 3), rep(37.68122, 3), rep(37.68213, 3), rep(37.67948, 3))

# 가중치를 3배 주었다

dobong1_wifi <- wifi %>% filter(dong == "도봉1동") #도봉1 와이파이

dobong1_lon_bwd <- c() # 기존 와이파이 반경 내에 있는 건물 담아둘 벡터
dobong1_lat_bwd <- c()

for (i in 1:nrow(dobong1_wifi)){
  dobong1_wifi_lon <- as.numeric(dobong1_wifi$설치위치.X좌표.[i]) 
  dobong1_wifi_lat <- as.numeric(dobong1_wifi$설치위치.Y좌표.[i])
  for (j in 1:length(dobong1_lon)){
    distance <- distm(c(dobong1_wifi_lon, dobong1_wifi_lat), c(dobong1_lon[j], dobong1_lat[j]), fun = distHaversine)
    if(distance < 100){
      dobong1_lon_bwd <- c(dobong1_lon_bwd, dobong1_lon[j]) # 거리 50m 보다 작으면 데이터프레임에 추가
      dobong1_lat_bwd <- c(dobong1_lat_bwd, dobong1_lat[j]) # 거리 50m 보다 작으면 데이터프레임에 추가
    }
  }
}
length(unique(dobong1_lon_bwd)) # 겹치는 있을 수도 있으니 확인

dobong1_lon = dobong1_lon[!dobong1_lon %in% dobong1_lon_bwd]
dobong1_lat = dobong1_lat[!dobong1_lat %in% dobong1_lat_bwd]

rm(dobong1_lat_bwd); rm(dobong1_lon_bwd)

### 해당건물좌표를 기반으로 거리행렬 계산
dobong1_distmat = make_dis_mat(dobong1_lon, dobong1_lat)

### P-median알고리즘을 중심수를 변화해가며 적용
dobong1_1 = p_median(dobong1_distmat, 1, seed = 42)
# Configuration:  1127  Score: 1341934 
dobong1_2 = p_median(dobong1_distmat, 2, seed = 42)
# Configuration:  2072 1672  Score: 915655
dobong1_3 = p_median(dobong1_distmat, 3, seed = 42)
# Configuration:  460 1012 1068  Score: 679303.1   
dobong1_4 = p_median(dobong1_distmat, 4, seed = 42)
# Configuration:  459 1012 1772 967  Score: 570185.1 
dobong1_5 = p_median(dobong1_distmat, 5, seed = 42)
# Configuration:  967 1980 1772 366 215  Score: 505507.2 
dobong1_6 = p_median(dobong1_distmat, 6, seed = 42)
# Configuration:  589 216 1811 691 1980 1616  Score: 458560.3
dobong1_7 = p_median(dobong1_distmat, 7, seed = 42)
# Configuration:  845 996 2035 1724 1978 1222 866  Score: 398823.4 
dobong1_8 = p_median(dobong1_distmat, 8, seed = 42)
# Configuration:  690 996 1073 1724 1978 1221 1886 845  Score: 360962.1


# 결과확인
matrix(c(dobong1_lat[c(967, 1980, 1772, 366, 215)], dobong1_lon[c(967, 1980, 1772, 366, 215)]), nrow = 5, ncol = 2)
dobong1_lat[c(967, 1980, 1772, 366, 215)]
dobong1_lon[c(967, 1980, 1772, 366, 215)]

# Elbow point 시각화!
plot(x = 1:8, y = c(1341934 , 915655, 679303.1, 570185.1, 505507.2, 458560.3, 398823.4, 360962.1), 
     type = 'b', ylab = '', main = '도봉1동')


#############################
# 장위2동
# 전체적인 과정은 동일하다.

jangwe2 = data %>% filter(dong == '장위2동')
jangwe2$건물용도 %>% unique

yongdo = c('우체국', '초등학교', '노인복지시설', '사회복지시설', '동사무소', '기타문화및집회시설', '중학교', '파출소',
           '기타교육연구및복지시설')
jangwe2 %>% filter(건물용도 %in% yongdo) %>% select(-건물용도코드)

jangwe2_lon = data %>% filter(dong == '장위2동') %>% select(long) %>% unlist() %>% as.vector()
jangwe2_lat = data %>% filter(dong == '장위2동') %>% select(lat) %>% unlist() %>% as.vector()
jangwe2_lon = c(jangwe2_lon, rep(127.0555, 3), rep(127.0511, 3), rep(127.0545, 3), rep(127.0524, 3), rep(127.0500, 3),
                rep(127.0464, 3), rep(127.0514, 3))
jangwe2_lat = c(jangwe2_lat, rep(37.61268, 3), rep(37.61276, 3), rep(37.61239, 3), rep(37.60877, 3), rep(37.61144, 3),
                rep(37.61035, 3), rep(37.61002, 3))
jangwe2_lat %>% length

jangwe2_wifi <- wifi %>% filter(dong == "장위2동") #장위2 와이파이
jangwe2_bwd <- data.frame() # 기존 와이파이 반경 내에 있는 건물 담아둘 데이터프레임

for (i in 1:nrow(jangwe2_wifi)){
  jangwe2_wifi_lon <- as.numeric(jangwe2_wifi$설치위치.X좌표.[i]) 
  jangwe2_wifi_lat <- as.numeric(jangwe2_wifi$설치위치.Y좌표.[i])
  for (j in 1:length(jangwe2_lon)){
    
    distance <- distm(c(jangwe2_wifi_lon, jangwe2_wifi_lat), c(jangwe2_lon[j], jangwe2_lat[j]), fun = distHaversine)
    temp <- jangwe2[j,]
    
    if(distance < 100){
      jangwe2_bwd <- rbind(jangwe2_bwd, temp) # 거리 50m 보다 작으면 데이터프레임에 추가
    }
  }
}

nrow(unique(jangwe2_bwd)) # 겹치는 있을 수도 있으니
jangwe2 <- anti_join(jangwe2, jangwe2_bwd) # 원래 빌딩목록에서 anti join
jangwe2_lon = jangwe2 %>% select(long) %>% unlist() %>% as.vector()
jangwe2_lat = jangwe2 %>% select(lat) %>% unlist() %>% as.vector()

# 거리행렬 계산
jangwe2_distmat = make_dis_mat(jangwe2_lon, jangwe2_lat)

# P-median알고리즘 시행
jangwe2_1 = p_median(jangwe2_distmat, 1, seed = 42)
# Configuration:  1499  Score: 789820 
jangwe2_2 = p_median(jangwe2_distmat, 2, seed = 42)
# Configuration:  1974 597  Score: 550525.8 
jangwe2_3 = p_median(jangwe2_distmat, 3, seed = 42)
# Configuration:  1377 1587 1246  Score: 393023.9 
jangwe2_4 = p_median(jangwe2_distmat, 4, seed = 42)
# Configuration:  562 1035 158 1377  Score: 337058.3 
jangwe2_5 = p_median(jangwe2_distmat, 5, seed = 42)
# Configuration:  274 1377 1789 579 1035  Score: 301660.9 
jangwe2_6 = p_median(jangwe2_distmat, 6, seed = 42)
# Configuration:  631 2291 1789 2009 1523 1377  Score: 274789.7 
jangwe2_7 = p_median(jangwe2_distmat, 7, seed = 42)
# Configuration:  631 2291 1789 408 1523 2009 1038  Score: 255229.1
jangwe2_8 = p_median(jangwe2_distmat, 8, seed = 42)
# Configuration:  631 2291 1789 303 2009 2161 2107 1523  Score: 241781.5 

plot(x = 1:8, y = c(789820 , 550525.8, 393023.9, 337058.3, 301660.9, 274789.7, 255229.1, 241781.5), type = 'b', ylab = '')

# 결과확인
jangwe2_idx = which(apply(jangwe2_3, 2, sum) != 0)
data %>% filter(dong == '장위2동') %>% .[jangwe2_idx, ]


#################################

# 삼성동

samsung = data %>% filter(dong == '삼성동')
yongdo = c('우체국', '초등학교', '노인복지시설', '사회복지시설', '동사무소', '기타문화및집회시설', '중학교', '파출소',
           '기타교육연구및복지시설', '생활편익시설', '고등학교', '청소년수련원(관)', '기타공공업무시설')
samsung %>% filter(건물용도 %in% yongdo) %>% select(-건물용도코드)

samsung_lon = data %>% filter(dong == '삼성동') %>% select(long) %>% unlist() %>% as.vector()
samsung_lat = data %>% filter(dong == '삼성동') %>% select(lat) %>% unlist() %>% as.vector()
samsung_lat = c(samsung_lat, rep(37.46448, 3), rep(37.46696, 3), rep(37.46319, 3), rep(37.46412, 3), rep(37.46679, 3),
                rep(37.47011, 3), rep(37.45958, 3), rep(37.47047, 3), rep(37.46194, 3), rep(37.46249, 3))

samsung_lon = c(samsung_lon, rep(126.9259, 3), rep(126.9276, 3), rep(126.9323, 3), rep(126.9318, 3), rep(126.9312, 3),
                rep(126.9330, 3), rep(126.9330, 3), rep(126.9280, 3), rep(126.9302, 3), rep(126.9317, 3))

samsung_wifi <- wifi %>% filter(dong == "삼성동") #장위2 와이파이
samsung_bwd <- data.frame() # 기존 와이파이 반경 내에 있는 건물 담아둘 데이터프레임
for (i in 1:nrow(samsung_wifi)){
  samsung_wifi_lon <- as.numeric(samsung_wifi$설치위치.X좌표.[i]) 
  samsung_wifi_lat <- as.numeric(samsung_wifi$설치위치.Y좌표.[i])
  for (j in 1:length(samsung_lon)){
    distance <- distm(c(samsung_wifi_lon, samsung_wifi_lat), c(samsung_lon[j], samsung_lat[j]), fun = distHaversine)
    temp <- samsung[j,]
    if(distance < 100){
      samsung_bwd <- rbind(samsung_bwd, temp) # 거리 50m 보다 작으면 데이터프레임에 추가
    }
  }
}

nrow(unique(samsung_bwd)) # 겹치는 있을 수도 있으니
samsung <- anti_join(samsung, samsung_bwd) # 원래 빌딩목록에서 anti join
samsung_lon = samsung %>% select(long) %>% unlist() %>% as.vector()
samsung_lat = samsung %>% select(lat) %>% unlist() %>% as.vector()

# 거리행렬 계산
samsung_distmat = make_dis_mat(samsung_lon, samsung_lat)

# P-median 알고리즘 시행
samsung_1 = p_median(samsung_distmat, 1, seed = 42)
# Configuration:  244  Score: 830316.2
samsung_2 = p_median(samsung_distmat, 2, seed = 42)
# Configuration:  1295 105  Score: 453444 
samsung_3 = p_median(samsung_distmat, 3, seed = 42)
# Configuration:  164 640 1801  Score: 331903.1 
samsung_4 = p_median(samsung_distmat, 4, seed = 42)
# Configuration:  842 1857 1743 957  Score: 279527.5 
samsung_5 = p_median(samsung_distmat, 5, seed = 42)
# Configuration:  842 1857 210 492 957  Score: 238761.7 
samsung_6 = p_median(samsung_distmat, 6, seed = 42)
# Configuration:  843 916 210 921 492 962  Score: 211199.7 
samsung_7 = p_median(samsung_distmat, 7, seed = 42)
# Configuration:  842 909 406 1815 492 956 1333  Score: 189282.4
samsung_8 = p_median(samsung_distmat, 8, seed = 42)
# Configuration:  842 406 1333 1815 375 492 633 956  Score: 175134.8 

plot(x = 1:8, y = c(830316.2 , 453444, 331903.1, 279527.5, 238761.7, 211199.7, 189282.4, 175134.8), type = 'b', ylab = '')

# 결과확인
samsung_idx = which(apply(samsung_3, 2, sum) != 0)
data %>% filter(dong == '삼성동') %>% .[samsung_idx, ]



















