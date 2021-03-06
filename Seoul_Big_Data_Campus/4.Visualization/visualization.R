# visualization

library(tidyverse)
library(plotly)

# 행정동 경계지도

dong_boundary = data.table::fread('data/dong_boundary.csv')

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


# 실제 와이파이 분포

wifi = data.table::fread('data/wifi_dong.csv')

wifi = wifi %>% mutate(long = as.numeric(설치위치.X좌표.),
                       lat = as.numeric(설치위치.Y좌표.)) %>% select(-설치위치.X좌표., -설치위치.Y좌표.)

idx = NULL
idx = union(which((wifi_longlat$long > 127.2) & (wifi_longlat$long < 128.5)), which(wifi_longlat$lat < 37.44))
idx = c(idx, which(wifi_longlat$long < 126.75))

ggplot() + 
  geom_polygon(data = dong_boundary, aes(x=long, y=lat, group=id), fill = 'white', color = 'black', alpha = 0.9) +
  geom_point(data = wifi[-idx, ], aes(x = long, y = lat), col = 'royalblue3', alpha = 0.8, size = 2) + 
  coord_map() +
  theme_bw() + 
  labs(title = '개별 와이파이 분포') + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5))



# 행정동별 와이파이 분포 비교

dong_wifi = data.table::fread('data/dong_wifi_with_dongcode.csv')
dong_wifi %>% glimpse
dong_boundary %>% glimpse

dong_wifi_forplot = dong_boundary %>% inner_join(dong_wifi, by = c('읍면동명칭' = 'dong'))

ggplot() + 
  geom_polygon(data = dong_boundary, aes(x=long, y=lat, group=id), fill = 'white', color = 'black', alpha = 0.9) +
  geom_polygon(data = dong_wifi_forplot, aes(x=long, y=lat, group=id, fill = n), alpha = 0.9) + 
  scale_fill_gradient(low = "skyblue", high = "Dark blue", space = "Lab", guide = "colourbar") +
  coord_map() +
  theme_bw() + 
  labs(title = '행정동별 와이파이 분포') + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5))

# 와이파이 개별 + 행정동

ggplot() + 
  geom_polygon(data = dong_boundary, aes(x=long, y=lat, group=id), fill = 'white', color = 'black', alpha = 0.9) +
  geom_polygon(data = dong_wifi_forplot, aes(x=long, y=lat, group=id, fill = n), alpha = 0.9) + 
  scale_fill_gradient(low = "skyblue", high = "Dark blue", space = "Lab", guide = "colourbar") +
  geom_point(data = wifi[-idx, ], aes(x = long, y = lat), col = 'royalblue3', alpha = 0.8, size = 2) + 
  coord_map() +
  theme_bw() + 
  labs(title = '개별+행정동 와이파이 분포') + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5))
    

# 행정동별 생활금융통계 비교

finance = data.table::fread('data/living_finance.csv')
finance %>% glimpse

finance_forplot = dong_boundary %>% inner_join(finance, by = c('읍면동명칭' = '행정동명'))

ggplot() + 
  geom_polygon(data = dong_boundary, aes(x=long, y=lat, group=id), fill = 'white', color = 'black', alpha = 0.9) +
  geom_polygon(data = finance_forplot, aes(x=long, y=lat, group=id, fill = 평균소득), alpha = 0.9) + 
  scale_fill_gradient(low = "skyblue", high = "Dark blue", space = "Lab", guide = "colourbar") +
  coord_map() +
  theme_bw()+ 
  labs(title = '가계 행정동별 평균소득 분포') + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5))


# 행정동별 정보화 점수 비교

gap = data.table::fread('data/informatization_score.csv')
gap %>% glimpse

gap = gap %>% select(동, 정보화점수)

gap %>% arrange(desc(정보화점수))

gap_forplot = dong_boundary %>% inner_join(gap, by = c('읍면동명칭' = '동'))

ggplot() + 
  geom_polygon(data = dong_boundary, aes(x=long, y=lat, group=id), fill = 'white', color = 'black', alpha = 0.9) +
  geom_polygon(data = gap_forplot, aes(x=long, y=lat, group=id, fill = 정보화점수), alpha = 0.9) + 
  scale_fill_gradient(low = "skyblue", high = "Dark blue", space = "Lab", guide = "colourbar") +
  coord_map() +
  theme_bw()+ 
  labs(title = '행정동별 정보화점수 분포') + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5))



## 타겟 행정동 시각화

target = c('등촌3동', '방화3동', '번2동', '번3동', '삼성동', '가양2동', '가양3동', '남영동', 
           '도봉1동', '둔촌1동', '장위2동', '종로1.2.3.4가동', '중계2.3동', '중계4동', '회현동', 
           '청량리동', '삼청동', '상계3.4동', '수서동', '월계2동', '을지로동')


ggplot() +
  geom_polygon(data = dong_boundary, aes(x=long, y=lat, group=id), fill = 'white', color = 'black', alpha = 0.5) +
  geom_polygon(data = dong_boundary %>% filter(읍면동명칭 %in% target), aes(x = long, y = lat, group = id), 
               fill = 'cornflowerblue', color = 'black', alpha = 0.8) + 
  coord_map() + 
  theme_bw() + 
  labs(title = "타겟 행정동 분포") + 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5))


target_wifi = wifi %>% filter(dong %in% target) %>% filter(lat > 37.4)
target_wifi$dong %>% unique
#target_wifi %>% select(구명, dong, 설치주소, long, lat) %>% arrange(long) %>% View
idx = c(1, 15, 46, 83, 239, 265)

plot_target_wifi = 
  ggplot() +
  geom_polygon(data = dong_boundary, aes(x=long, y=lat, group=id), fill = 'white', color = 'black', alpha = 0.5) +
  geom_polygon(data = dong_boundary %>% filter(읍면동명칭 %in% target), aes(x = long, y = lat, group = id), 
               fill = 'cornflowerblue', color = 'black', alpha = 0.8) + 
  geom_point(data = target_wifi %>% arrange(long) %>% .[-idx, ], aes(x = long, y = lat), 
             fill = 'blue', color = 'Dark blue', alpha = 0.5, size = 1.5) +
  coord_map() + 
  theme_bw() + 
  labs(title = "타겟 행정동&와이파이 분포 확인") + 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5))

plot_target_wifi

