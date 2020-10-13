# 빌딩정보 전처리

library(ggmap)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(sf)
library(data.table)
library(tidyverse)

# 전체적인 변환 과정은 이전의 행정동지도 메이킹과 동일합니다.
# 변환과정이 오래 걸립니다.
building <- readOGR('data/TL_SPBD_BULD.shp')
from_crs = CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=GRS80 +units=m +no_defs")
proj4string(building) <- from_crs  # Coordinate Reference System만 변경하기 위한 함수
to_crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
building <- spTransform(building, to_crs)
building$data
building@polygons[[i]]@labpt[1:2]

k = NULL
for (i in 1:654595){
  coord = building@polygons[[i]]@labpt[1:2]
  k <- rbind(k,coord)
  if (i %% 100) {
    cat(i, '')
  }
}

head(k)
colnames(k) <- c('long','lat')
write.csv(k,'building_coord.csv', row.names = F)

bb <- building@data
bbb <- cbind(bb,k)
write.csv(bbb,'building_coord_maybe_fin.csv', row.names = F)

## 빌딩정보를 가져와서 행정동 코드와 합친다.
data <- fread('building_coord_maybe_fin.csv', encoding = 'UTF-8')
code <- fread('행정동코드북.csv')

data2 <- data %>% select(BDTYP_CD,BULD_NM,BULD_NM_DC,GRO_FLO_CO,UND_FLO_CO,TOTAL_AREA,long,lat,ADMI_CD) %>% filter(ADMI_CD == '11305606' |ADMI_CD =='11350665'|ADMI_CD =='11320521'|ADMI_CD =='11290770'|ADMI_CD =='11620745')

colnames(data2)[9] <- '행정동코드'

data3 <- left_join(data2, code)

bui_co <- fread('건물용도코드.csv', encoding = 'UTF-8')
colnames(bui_co) <- c('BDTYP_CD','건물용도')

data3 <- left_join( data3, bui_co)
colnames(data3) <-c('건물용도코드','건물명','상세건물명','지상층수','지하층수','연면적','long','lat','행정동코드','dong','건물용도')

data3 <- data3 %>% select(dong,행정동코드,건물명, 상세건물명,건물용도코드, 건물용도,  lat,long,'지상층수','지하층수','연면적')

write.csv(data3,'building_five.csv',row.names = F)