library(data.table)
library(tidyverse)

### 19년의 월별 행정동 생활인구를 다 가져와서 평균을 내는 과정이다.

### 사용된 행정동 코드는 행안부 기준이다.

ppl1902 <- fread("data/LOCAL_PEOPLE_DONG_201902.csv", encoding = "UTF-8", fill = TRUE)
cols <- colnames(ppl1902[,-33])
ppl1902 <- ppl1902[,-c(33)]
colnames(ppl1902) <- cols

ppl1901 <- fread("data/LOCAL_PEOPLE_DONG_201901.csv", encoding = "UTF-8")
ppl1901 <- ppl1901[,-c(33:38)]
colnames(ppl1901) <- cols

ppl1903 <- fread("data/LOCAL_PEOPLE_DONG_201903.csv", encoding = "UTF-8")
ppl1903 <- ppl1903[,-c(33:38)]
colnames(ppl1903) <- cols

ppl1904 <- fread("data/LOCAL_PEOPLE_DONG_201904.csv", encoding = "UTF-8")
ppl1904 <- ppl1904[,-c(33)]
colnames(ppl1904) <- cols

ppl1905 <- fread("data/LOCAL_PEOPLE_DONG_201905.csv", encoding = "UTF-8")
ppl1905 <- ppl1905[,-c(33)]
colnames(ppl1905) <- cols

ppl1906 <- fread("data/LOCAL_PEOPLE_DONG_201906.csv", encoding = "UTF-8", fill = TRUE)
ppl1906 <- ppl1906[,-c(33)]
colnames(ppl1906) <- cols

ppl1907 <- fread("data/LOCAL_PEOPLE_DONG_201907.csv", encoding = "UTF-8")
cols <- colnames(ppl1907[,-1])
ppl1907 <- ppl1907[,-c(33)]
colnames(ppl1907) <- cols

ppl1908 <- fread("data/LOCAL_PEOPLE_DONG_201908.csv", encoding = "UTF-8")
cols <- colnames(ppl1908[,-1])
ppl1908 <- ppl1908[,-c(33)]
colnames(ppl1908) <- cols

ppl1909 <- fread("data/LOCAL_PEOPLE_DONG_201909.csv", encoding = "UTF-8", fill = TRUE)
ppl1909 <- ppl1909[,-c(33)]
colnames(ppl1909) <- cols

ppl1910 <- fread("data/LOCAL_PEOPLE_DONG_201910.csv", encoding = "UTF-8" , fill = TRUE, data.table = F)
ppl1910 <- ppl1910[,-33]
colnames(ppl1910) <- cols

ppl1911 <- fread("data/LOCAL_PEOPLE_DONG_201911.csv", encoding = "UTF-8")
cols <- colnames(ppl1911[,-1])
ppl1911 <- ppl1911[,-33]
colnames(ppl1911) <- cols

ppl1912 <- fread("data/LOCAL_PEOPLE_DONG_201912.csv", encoding = "UTF-8")
cols <- colnames(ppl1912[,-1])
ppl1912 <- ppl1912[,-c(33)]
colnames(ppl1912) <- cols


bungi191 = rbind(ppl1901, ppl1902, ppl1903)
bungi192 = rbind(ppl1904, ppl1905, ppl1906)
bungi193 = rbind(ppl1907, ppl1908, ppl1909)
bungi194 = rbind(ppl1910, ppl1911, ppl1912)

dat19 <- rbind(bungi193,bungi194,bungi191, bungi192)

# 전체를 rbing로 묶어주고, 평균을 낸다

code = fread("data/code_haengjeong_government.csv")

# 행안부 코드 기준

join1819 <- left_join(dat19, code, by = c('행정동코드' = '행정동코드'))
length(unique(join1819$dong))
length(unique(join1819$행정동코드))  # 424개로 문제 없다.
join1819 %>% head

datmean1 <- join1819 %>% select(총생활인구수, dong) %>% group_by(dong) %>% summarise(mean = mean(총생활인구수))
datmean1 %>% head()

write.csv(datmean1,file = "data/living_pop.csv", row.names = F)

