# Clustering 클러스터링 종합
# Kmeans, Kmedoid, Hierarchical, Gaussian Mixture

library(data.table)
library(tidyverse)
library(cluster)
library(mclust)
library(ClusterR)
library(fpc)
library(NbClust)
library(factoextra)

data = fread('data/dataset.csv')
data %>% glimpse

##############
####### Kmeans

# 평균소득과 정보화점수만 이용. 이상치 제거 형태
data_clus1 = data %>% select(평균소득, 정보화점수)
# 이상치제거
Q1 = quantile(data$와이파이개수, probs = c(0.25),na.rm = TRUE) 
Q3 = quantile(data$와이파이개수, probs = c(0.75),na.rm = TRUE)
LC = Q1 - 1.5 * (Q3 - Q1) 
UC = Q3 + 1.5 * (Q3 - Q1) 
data_clus1 <- data_clus1[which((data$와이파이개수 >= LC) & (data$와이파이개수 <= UC)),]
# 데이터 스케일링
data_clus1 = data_clus1 %>% scale() %>% as.data.frame()

set.seed(42)
k4 = kmeans(data_clus1, centers = 4, nstart = 25) 
## 4개의 중심이 nbclust함수상에서 괜찮게 나왔고, 행정동의 묶이는 형태도 해석하기에 용이했다.

f = 
  fviz_nbclust(data_clus1, kmeans, method = "silhouette",
               diss = dist(data_clus1, method = "euclidean"))+
  labs(subtitle = "Silhouette method")

# 실루엣 시각화
f
f$data
k4$cluster %>% table  # 4번이 타겟 클러스터
kmeans_result = k4$cluster

# 클러스터링 결과 시각화
fviz_cluster(k4, geom = c("point","text"), data = data_clus1)+  
  theme(panel.background = element_rect(fill = 'white', color = 'black', linetype='solid'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(face = "bold", size = 16),
        axis.text.x = element_text(size = 7, face = "bold", colour = "grey50"),
        axis.text.y = element_text(size = 7, face = "bold", colour = "grey50"), 
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(face = "bold", size = 10))


# 실루엣: k=4일때 0.3997
# n = 38 인 클러스터가 타겟 클러스터 


###############
####### Kmedoid

# 평균소득과 정보화점수만 이용
data_clus1 = data %>% select(평균소득, 정보화점수)
# 데이터 스케일링
data_clus1 = data_clus1 %>% scale() %>% as.data.frame()

# kmedoid 클러스터링
set.seed(42)
pam7 = pam(data_clus1 ,k=7, stand=FALSE)
pam7$silinfo$avg.width
pam7$clustering %>% table  ## 4번이 타겟클러스터

kmedoid_result = pam7$clustering

# 클러스터링 결과 시각화
fviz_cluster(pam7) + 
  theme(panel.background = element_rect(fill = 'white', color = 'black', linetype='solid'), 
        axis.title.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        plot.title = element_text(face = "bold", size = 16),
        axis.text.x = element_text(size = 7, face = "bold", colour = "grey50"), 
        axis.text.y = element_text(size = 7, face = "bold", colour = "grey50"), 
        legend.title = element_text(face = "bold", size = 12), 
        legend.text = element_text(face = "bold", size = 10)) 

# 실루엣: k=7일때 0.34
# n = 19인 4번 클러스터가 타겟 클러스터 


####################
####### Hierarchical

data_clus1 = data %>% select(평균소득, 정보화점수)
data_clus1 = data_clus1 %>% scale() %>% as.data.frame()

clust1 = eclust(data_clus1, 'hclust', hc_method = 'average', k = 4)
clust1$silinfo # 실루엣 0.414
hclust_result = clust1$cluster

fviz_cluster(clust1) + 
  theme(panel.background = element_rect(fill = 'white', color = 'black', linetype='solid')) + 
  labs(x = '평균소득', y = "정보화격차점수", title = '')

data %>% cbind(hclust_result) %>% 
  select(-dong) %>% group_by(hclust_result) %>% 
  summarise(평균와이파이 = mean(와이파이개수),
            정보화점수 = mean(정보화점수),
            평균소득 = mean(평균소득),
            평균생활인구 = mean(평균생활인구),
            평균접근성 = mean(접근성점수))


########################
####### Gaussian Mixture

# 평균소득과 정보화점수만 이용
data_clus1 = data %>% select(평균소득, 정보화점수)
# 데이터 스케일링
data_clus1 = data_clus1 %>% scale() %>% as.data.frame()

(hc1 <- hc(data_clus1, modelName = "VVV", use = "SVD"))
# 초기값과 군집개수를 설정하기 위한 방법법
clust_result_bic = data_clus1 %>% mclustBIC(initialization = list(hcPairs = hc1))
plot(clust_result_bic)
clust_result_bic %>% summary

mod <- Mclust(data_clus1, x = clust_result_bic)
plot(mod, what = "classification")
plot(mod, what = "uncertainty")
gaussian_result = mod$classification
gaussian_result %>% table   ## 3번 클러스터 21개 행정동이 타겟

cs = cluster.stats(dist(data_clus1), gaussian_result)
cs[c("within.cluster.ss","avg.silwidth")]
## 실루엣 0.36


gmm_df = data.frame(data_clus1$평균소득, data_clus1$정보화점수, factor(gaussian_result))
colnames(gmm_df) = c('평균소득', '정보화점수', 'cluster')
gmm_df %>% head

# 가우시안 믹스쳐 클러스터링 결과 시각화
gmm_df %>% 
  ggplot(aes(x = 평균소득, y = 정보화점수)) +
  geom_point(aes(col = cluster), lwd = 2) + 
  stat_ellipse(aes(col = cluster), type = 'norm', lty = 2, lwd = 1) + 
  #stat_ellipse(aes(col = cluster), geom = 'polygon', alpha = 0.1) + 
  theme(panel.background = element_rect(fill = 'white', color = 'black', linetype='solid'), 
        axis.title.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        plot.title = element_text(face = "bold", size = 16),
        axis.text.x = element_text(size = 7, face = "bold", colour = "grey50"), 
        axis.text.y = element_text(size = 7, face = "bold", colour = "grey50"), 
        legend.title = element_text(face = "bold", size = 12), 
        legend.text = element_text(face = "bold", size = 10)) + 
  labs(x = '평균소득', y = "정보화격차점수", title = '가우시안혼합모형 클러스터링 결과')
















