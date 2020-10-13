## map_haengjeongdong
## 행정동 경계지도 메이킹 코드

# 동별경계 기본지도 메이킹 코드

library(ggmap)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(sf)
library(data.table)
library(tidyverse)


#http://data.nsdi.go.kr/dataset/20171206ds00001/resource/cbc32e40-4a5c-48a6-ae39-1292379cb9ff?view_id=722c9b2e-6654-45c4-84d1-3a842864ea3c
# 국가공간정보포털에서 행정동 별 구분이 있는 지도를 위한 shp파일 가져옴

# https://lovetoken.github.io/r/data_visualization/2018/04/15/sp_proj4string_spTransform.html
# rgdal 패키지의 readOGR패키지로 shape파일 불러옴
dong_boundary  <- readOGR("data/Z_SOP_BND_ADM_DONG_PG.shp")

# Coordinate Reference System
from_crs = CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel +units=m")
# 투영방법, 어디를 중심으로? 서울을 중심으로 projetion!
# 구의 이심률, 타원체의 구격 지정
proj4string(dong_boundary) <- from_crs  # Coordinate Reference System만 변경하기 위한 함수

dong_boundary # 기존에 crs값이 NA였는데, 지정해서 문제 해결!
to_crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# projection 위한 함수

# sptransform 함수를 통해 toCRS의 형식으로 변환!
dong_boundary <- spTransform(dong_boundary, to_crs)
dong_boundary
#dong_boundary$ADM_DR_NM # %>% unique %>% length
#(dong_boundary$읍면동명칭 == '화곡1동') %>% sum

# 현재까지는 spatialpolygon dataframe 이기 때문에, 이를 dataframe으로 바꿔주는 fortify 함수 사용

dong_boundary <- fortify(dong_boundary, region = 'ADM_DR_CD')
dong_boundary$id <- as.numeric(dong_boundary$id)


dong_boundary <- 
  dong_boundary %>% 
  filter(id <= 1174099) # 서울만 추출

dong_boundary = dong_boundary %>% select(-order, -hole, -piece, -group)

dong_boundary %>% head()

#dong_boundary = fread('dong_boundary.csv')
dong_code = fread('data/code_haengjeong_statistics.csv', data.table = F)
dong_code = dong_code %>% dplyr::select(읍면동코드, 읍면동명칭) %>% filter(읍면동코드 <= 1174099)
dong_code %>% glimpse

dong_boundary = dong_boundary %>% inner_join(dong_code, by = c('id' = '읍면동코드'))

dong_bound_list = dong_boundary$읍면동명칭 %>% unique
dong_code_list = dong_code$읍면동명칭 %>% unique

dong_bound_list %>% setdiff(dong_code_list)

dong_plot <- 
  ggplot() + 
  geom_polygon(data = dong_boundary, aes(x=long, y=lat, group=id), fill = 'white', color = 'black', alpha = 0.9)

dong_plot +
  coord_map() +
  theme_bw() +
  labs(title = "서울 행정동 경계 지도") + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5))

write.csv(dong_boundary, 'data/dong_boundary.csv', row.names = F)



