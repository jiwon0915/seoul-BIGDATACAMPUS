library(data.table)
library(dplyr)

## 데이터 불러오기
senior <- read.table("data/resident(senior)2019.txt", sep="\t", fill = T, fileEncoding = "UTF-8", stringsAsFactors = F, header = T)
dis <- read.table("data/disabled.txt", sep = "\t", fill=T, fileEncoding = "UTF-8", stringsAsFactors = F, header = T)
bl <- read.table("data/basic living.txt", sep = "\t", fill=T, fileEncoding = "UTF-8", stringsAsFactors = F, header = T)

## 필요한 컬럼만 추출
senior_dong <- senior %>% filter(행정동 !="소계") %>% select(자치구, 행정동, 인구, X65세이상고령자)
dis_dong <- dis %>% filter(동 != "소계" & 동 != "기타") %>% select(동, 합계)
bl_dong <- bl %>% filter(동 != "소계" & 동 != "기타") %>% select(동, `총.수급자.2`)

## 불필요한 행 삭제
senior_dong <- senior_dong[-(1:3),]
dis_dong <- dis_dong[-(1:3),]
bl_dong <- bl_dong[-(1:3),]

## 동 이름으로 join하기 위해 신사동 구분
senior_dong$행정동[duplicated(senior_dong$행정동)]
which(senior_dong$행정동 == "신사동")
senior_dong[329,2] <- "신사동_관"
senior_dong[358,2] <- "신사동_강"
#
which(dis_dong$동 == "신사동")
dis_dong[329,1] <- "신사동_관"
dis_dong[358,1] <- "신사동_강"
#
which(bl_dong$동 == "신사동")
bl_dong[329,1] <- "신사동_관"
bl_dong[358,1] <- "신사동_강"


## 동 기준으로 join
joined <- senior_dong
joined <- full_join(joined, dis_dong, by = c('행정동' = '동'))
joined <- full_join(joined, bl_dong, by = c('행정동' = '동'))

joined %>% glimpse

## 여의도동법정동인데 잘못 들어가있어서 바꾸기
joined[425,]
which(joined$행정동 == "여의동")
joined[286,5] <- 755

joined <- joined[-425,]
sum(is.na(joined)) # 구로구 항동 2020년 신설

## 컬럼명 정리
colnames(joined) <- c("구", "동", "인구", "고령자", "장애인구", "기초생활수급인구" )
str(joined)


library(stringr)

joined$일반인구 <- 
  joined$인구 %>% str_replace_all(',', '') %>% as.numeric() - 
  joined$고령자 %>% str_replace_all(',', '') %>% as.numeric() - 
  joined$장애인구 %>% str_replace_all(',', '') %>% as.numeric() - 
  joined$기초생활수급인구%>% str_replace_all(',', '') %>% as.numeric()

## 각 비율
joined$인구 <- str_replace_all(joined$인구, ",", "")
joined$인구 <- as.numeric(joined$인구)
joined$고령자 <- str_replace_all(joined$고령자, ",", "")
joined$고령자 <- as.numeric(joined$고령자)
joined$장애인구 <- str_replace_all(joined$장애인구, ",", "")
joined$장애인구 <- as.numeric(joined$장애인구)
joined$기초생활수급인구 <- str_replace_all(joined$기초생활수급인구, ",", "")
joined$기초생활수급인구 <- as.numeric(joined$기초생활수급인구)

str(joined)

joined$고령자비율 <- (joined$고령자/joined$인구)
joined$장애인비율 <- (joined$장애인구/joined$인구)
joined$기초비율 <- (joined$기초생활수급인구/joined$인구)
joined$일반비율 <- (joined$일반인구/joined$인구)

## 정보화점수
## 기준에 맞게 계산한다.
joined$정보화점수 <- (joined$고령자비율 * 0.5) + (joined$장애인비율 * 0.669) + (joined$기초비율 * 0.835) + (joined$일반비율*1.0)
joined[9,2] <- "종로5.6가동"

joined %>% glimpse


##
write.csv(joined,"data/informatization_score.csv", row.names=F)

