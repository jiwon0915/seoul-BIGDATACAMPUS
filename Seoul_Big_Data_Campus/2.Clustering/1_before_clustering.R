# 클러스터링에 쓰일 변수들을 하나로 모으자

library(data.table)
library(dplyr)

acc = fread('data/accessibility.csv')
acc = acc %>% select(-code, -province)
acc %>% glimpse

wifi = fread('data/dong_wifi_with_dongcode.csv')
wifi = wifi %>% select(dong, n)
wifi %>% glimpse

inform = fread('data/informatization_score.csv')
inform = inform %>% select(동, 정보화점수)
inform %>% glimpse

income = fread('data/living_finance.csv')
income = income %>% select(행정동명, 평균소득)
income %>% glimpse

living_pop = fread('data/living_pop.csv')
living_pop %>% glimpse

data = 
  wifi %>% 
  left_join(inform, by = c('dong' = '동')) %>% 
  left_join(income, by = c('dong' = '행정동명')) %>% 
  left_join(living_pop) %>% 
  left_join(acc)

data %>% glimpse
colnames(data) = c('dong', '와이파이개수', '정보화점수', '평균소득', '평균생활인구', '접근성점수')

write.csv(data, 'data/dataset.csv', row.names = F)
