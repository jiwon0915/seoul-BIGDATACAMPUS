###########


library(data.table)
library(tidyverse)
library(geosphere)

wifi = fread('data/wifi_dong.csv')
building = fread('data/building_five.csv')
dong_doundary = fread('data/dong_boundary.csv')

##########################
#도봉1동
dobong1_boundary <- dong_boundary %>% filter(id  == 1110064) #동 boundary
dobong1_wifi <- wifi %>% filter(dong == '도봉1동') %>% select(설치위치.X좌표.,설치위치.Y좌표.)
dobong1_building <- building %>% filter(dong == '도봉1동')
colnames(dobong1_wifi) <- c('long','lat')
dobong1_wifi$long <- dobong1_wifi$long %>% as.numeric
dobong1_wifi$lat <- dobong1_wifi$lat %>% as.numeric

###도봉1동 현재와이파이와  거리구하기
dist_dobong1 <- matrix(nrow = nrow(dobong1_building), ncol = nrow(dobong1_wifi))
for (i in 1:nrow(dobong1_building)) {
  lon1 <- dobong1_building$long[i]
  lat1 <- dobong1_building$lat[i]
  for (j in 1:nrow(dobong1_wifi)) {
    lon2 <- dobong1_wifi$long[j]
    lat2 <- dobong1_wifi$lat[j]
    
    dist_dobong1[i,j] <-  distHaversine(c(lon1, lat1), c(lon2, lat2))
  }
  if (i %% 10 == 0) {
    print(i)
  }
}

dist_dobong1 <- dist_dobong1 %>% as.data.frame()
dist_dobong1$idx <- 1:nrow(dist_dobong1)
###현재와이파이와 dist 100m 미만 제거
dobong1_nowifi_idx <- dist_dobong1 %>% filter(V1 > 50 & V2 > 50 & V3 > 50 & V4 > 50 & V5 > 50 & V6 > 50,V7 > 50,V8 > 50,V9 > 50,V10 > 50)
dobong1_nowifi_build <- dobong1_building[c(dobong1_nowifi_idx$idx),]

dobong1_lat = seq(min(dobong1_building$lat),max(dobong1_building$lat),0.00008)
dobong1_long = seq(min(dobong1_building$long),max(dobong1_building$long),0.0001)

dobong1_coord_fin <- NULL
dobong1_coord <- NULL
for (i in dobong1_lat){
  for(j in dobong1_long){
    dobong1_coord$lat <- i
    dobong1_coord$long <-j
    dobong1_coord <- dobong1_coord %>% as.data.frame()
    dobong1_coord_fin <- rbind(dobong1_coord_fin,dobong1_coord)
  }
}


#p=1 일때의 좌표값
dobong1_coord_fin <- cbind(dobong1_coord_fin,X = rowSums(distm(dobong1_coord_fin[,c(2,1)],
                                                               dobong1_nowifi_build[,8:7], fun = distHaversine) <= 50))
p1 <- dobong1_coord_fin
#빠른 연산을 위해 상위 2% 좌표값에 대해서만 연산(실제 quantile 보면 사실상 한 2%가 적절함)
quantile(dobong1_coord_fin$X,0.95)
#P = 2일때
dobong1_fin <- dobong1_coord_fin[order(desc(dobong1_coord_fin$X)),] %>% filter(X >quantile(dobong1_coord_fin$X,0.98))

row.names(dobong1_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(dobong1_fin)){
  for (j in 1:nrow(dobong1_fin)){
    if (distm(dobong1_fin[i,2:1],dobong1_fin[j,2:1],fun = distHaversine)>100){ 
      cord1.x = dobong1_fin[i,1]
      cord1.y = dobong1_fin[i,2] 
      cord2.x = dobong1_fin[j,1] 
      cord2.y = dobong1_fin[j,2] 
      sum = dobong1_fin[i,3]+dobong1_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p2 <- data1

#P = 3일때
row.names(dobong1_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p2)){
  for (j in 1:nrow(dobong1_fin)){
    if (distm(dobong1_fin[j,2:1],p2[i,2:1],fun = distHaversine)>100 & 
        distm(dobong1_fin[j,2:1],p2[i,4:3],fun = distHaversine)>100 ){
      
      cord1.x = p2[i,1]
      cord1.y = p2[i,2] 
      cord2.x = p2[i,3] 
      cord2.y = p2[i,4]
      cord3.x = dobong1_fin[j,1]
      cord3.y = dobong1_fin[j,2]
      sum = p2$sum[i] + dobong1_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}

p3 <- data1

#p=4일때
row.names(dobong1_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p3)){
  for (j in 1:nrow(dobong1_fin)){
    if (distm(dobong1_fin[j,2:1],p3[i,2:1],fun = distHaversine)>100 & 
        distm(dobong1_fin[j,2:1],p3[i,4:3],fun = distHaversine) > 100 & 
        distm(dobong1_fin[j,2:1],p3[i,6:5],fun = distHaversine)>100){
      cord1.x = p3[i,1]
      cord1.y = p3[i,2] 
      cord2.x = p3[i,3] 
      cord2.y = p3[i,4]
      cord3.x = p3[i,5] 
      cord3.y = p3[i,6]
      cord4.x = dobong1_fin[j,1]
      cord4.y = dobong1_fin[j,2]
      sum = p3$sum[i] + dobong1_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p4 <- data1

#p=5일때
row.names(dobong1_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p4)){
  for (j in 1:nrow(dobong1_fin)){
    if (distm(dobong1_fin[j,2:1],p4[i,2:1],fun = distHaversine)>100 & 
        distm(dobong1_fin[j,2:1],p4[i,4:3],fun = distHaversine) > 100 & 
        distm(dobong1_fin[j,2:1],p4[i,6:5],fun = distHaversine)>100 & 
        distm(dobong1_fin[j,2:1],p4[i,8:7],fun = distHaversine)>100){
      cord1.x = p4[i,1]
      cord1.y = p4[i,2] 
      cord2.x = p4[i,3] 
      cord2.y = p4[i,4]
      cord3.x = p4[i,5] 
      cord3.y = p4[i,6]
      cord4.x = p4[i,7] 
      cord4.y = p4[i,8]
      cord5.x = dobong1_fin[j,1]
      cord5.y = dobong1_fin[j,2]
      sum = p4$sum[i] + dobong1_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,cord5.x,cord5.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p5 <- data1

#p=6일때
row.names(dobong1_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p5)){
  for (j in 1:nrow(dobong1_fin)){
    if (distm(dobong1_fin[j,2:1],p5[i,2:1],fun = distHaversine)>100 & 
        distm(dobong1_fin[j,2:1],p5[i,4:3],fun = distHaversine) > 100 & 
        distm(dobong1_fin[j,2:1],p5[i,6:5],fun = distHaversine)>100& 
        distm(dobong1_fin[j,2:1],p5[i,8:7],fun = distHaversine)>100 & 
        distm(dobong1_fin[j,2:1],p5[i,10:9],fun = distHaversine)>100){
      cord1.x = p5[i,1]
      cord1.y = p5[i,2] 
      cord2.x = p5[i,3] 
      cord2.y = p5[i,4]
      cord3.x = p5[i,5] 
      cord3.y = p5[i,6]
      cord4.x = p5[i,7] 
      cord4.y = p5[i,8]
      cord5.x = p5[i,9] 
      cord5.y = p5[i,10]
      cord6.x = dobong1_fin[j,1]
      cord6.y = dobong1_fin[j,2]
      sum = p5$sum[i] + dobong1_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,cord5.x,cord5.y,cord6.x,cord6.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p6 <- data1

#p=7일때
row.names(dobong1_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p6)){
  for (j in 1:nrow(dobong1_fin)){
    if (distm(dobong1_fin[j,2:1],p6[i,2:1],fun = distHaversine)>100 & 
        distm(dobong1_fin[j,2:1],p6[i,4:3],fun = distHaversine) > 100 & 
        distm(dobong1_fin[j,2:1],p6[i,6:5],fun = distHaversine)>100 & 
        distm(dobong1_fin[j,2:1],p6[i,8:7],fun = distHaversine)>100& 
        distm(dobong1_fin[j,2:1],p6[i,10:9],fun = distHaversine)>100 & 
        distm(dobong1_fin[j,2:1],p6[i,12:11],fun = distHaversine)>100){
      cord1.x = p6[i,1]
      cord1.y = p6[i,2] 
      cord2.x = p6[i,3] 
      cord2.y = p6[i,4]
      cord3.x = p6[i,5] 
      cord3.y = p6[i,6]
      cord4.x = p6[i,7] 
      cord4.y = p6[i,8]
      cord5.x = p6[i,9] 
      cord5.y = p6[i,10]
      cord6.x = p6[i,11] 
      cord6.y = p6[i,12]
      cord7.x = dobong1_fin[j,1]
      cord7.y = dobong1_fin[j,2]
      sum = p6$sum[i] + dobong1_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,cord5.x,cord5.y,cord6.x,cord6.y, cord7.x,cord7.y, sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p7 <- data1

#p=8일때
row.names(dobong1_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p7)){
  for (j in 1:nrow(dobong1_fin)){
    if (distm(dobong1_fin[j,2:1],p7[i,2:1],fun = distHaversine)>100 & 
        distm(dobong1_fin[j,2:1],p7[i,4:3],fun = distHaversine) > 100 & 
        distm(dobong1_fin[j,2:1],p7[i,6:5],fun = distHaversine)>100 & 
        distm(dobong1_fin[j,2:1],p7[i,8:7],fun = distHaversine)>100& 
        distm(dobong1_fin[j,2:1],p7[i,10:9],fun = distHaversine)>100 & 
        distm(dobong1_fin[j,2:1],p7[i,12:11],fun = distHaversine)>100& 
        distm(dobong1_fin[j,2:1],p7[i,14:13],fun = distHaversine)>100){
      cord1.x = p7[i,1]
      cord1.y = p7[i,2] 
      cord2.x = p7[i,3] 
      cord2.y = p7[i,4]
      cord3.x = p7[i,5] 
      cord3.y = p7[i,6]
      cord4.x = p7[i,7] 
      cord4.y = p7[i,8]
      cord5.x = p7[i,9] 
      cord5.y = p7[i,10]
      cord6.x = p7[i,11] 
      cord6.y = p7[i,12]
      cord7.x = p7[i,13] 
      cord7.y = p7[i,14]
      cord8.x = dobong1_fin[j,1]
      cord8.y = dobong1_fin[j,2]
      sum = p7$sum[i] + dobong1_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,cord5.x,cord5.y,cord6.x,cord6.y, 
                    cord7.x,cord7.y,cord8.x,cord8.y, sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p8 <- data1

marginal1 <- max(p1$X)   
marginal2 <-max(p2$sum) - max(p1$X) 
marginal3 <-max(p3$sum) - max(p2$sum)
marginal4 <-max(p4$sum) - max(p3$sum)
marginal5 <-max(p5$sum) - max(p4$sum)
marginal6 <-max(p6$sum) - max(p5$sum)
marginal7 <-max(p7$sum) - max(p6$sum)
marginal8 <-max(p8$sum)- max(p7$sum)

marginal_mat_dobong1 <- matrix(c(1,marginal1,2,marginal2,3,marginal3,4,marginal4,5,marginal5,6,marginal6,7,marginal7,8,marginal8),
                               ncol = 2,byrow = T ) %>% as.data.frame()

colnames(marginal_mat_dobong1) <- c('와이파이수','한계효용')


#############################################33
##################3번3동
bun3_boundary <- dong_boundary %>% filter(id  == 1109062) #동 boundary
bun3_wifi <- wifi %>% filter(dong == '번3동') %>% select(설치위치.X좌표.,설치위치.Y좌표.)
bun3_building <- building %>% filter(dong == '번3동')
colnames(bun3_wifi) <- c('long','lat')
bun3_wifi$long <- bun3_wifi$long %>% as.numeric
bun3_wifi$lat <- bun3_wifi$lat %>% as.numeric

###번3동 현재와이파이와  거리구하기
dist_bun3 <- matrix(nrow = nrow(bun3_building), ncol = nrow(bun3_wifi))
for (i in 1:nrow(bun3_building)) {
  lon1 <- bun3_building$long[i]
  lat1 <- bun3_building$lat[i]
  for (j in 1:nrow(bun3_wifi)) {
    lon2 <- bun3_wifi$long[j]
    lat2 <- bun3_wifi$lat[j]
    
    dist_bun3[i,j] <-  distHaversine(c(lon1, lat1), c(lon2, lat2))
  }
  if (i %% 10 == 0) {
    print(i)
  }
}

dist_bun3 <- dist_bun3 %>% as.data.frame()
dist_bun3$idx <- 1:nrow(dist_bun3)
###현재와이파이와 dist 50m 미만 제거
bun3_nowifi_idx <- dist_bun3 %>% filter(V1 > 50 & V2 > 50 & V3 > 50 & V4 > 50 & V5 > 50 & V6 > 50)
bun3_nowifi_build <- bun3_building[c(bun3_nowifi_idx$idx),]

bun3_lat = seq(min(bun3_building$lat),max(bun3_building$lat),0.00008)
bun3_long = seq(min(bun3_building$long),max(bun3_building$long),0.0001)

bun3_coord_fin <- NULL
bun3_coord <- NULL
for (i in bun3_lat){
  for(j in bun3_long){
    bun3_coord$lat <- i
    bun3_coord$long <-j
    bun3_coord <- bun3_coord %>% as.data.frame()
    bun3_coord_fin <- rbind(bun3_coord_fin,bun3_coord)
  }
}

#p=1 일때의 좌표값
bun3_coord_fin <- cbind(bun3_coord_fin,X = rowSums(distm(bun3_coord_fin[,c(2,1)],bun3_nowifi_build[,8:7], fun = distHaversine) <= 50))
p1 <- bun3_coord_fin
#빠른 연산을 위해 상위 5% 좌표값에 대해서만 연산(실제 quantile 보면 사실상 한 2%가 적절함)
quantile(bun3_coord_fin$X,0.95)
#P = 2일때
bun3_fin <- bun3_coord_fin[order(desc(bun3_coord_fin$X)),] %>% filter(X >quantile(bun3_coord_fin$X,0.95))

row.names(bun3_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(bun3_fin)){
  for (j in 1:nrow(bun3_fin)){
    if (distm(bun3_fin[i,2:1],bun3_fin[j,2:1],fun = distHaversine)>100){ 
      cord1.x = bun3_fin[i,1]
      cord1.y = bun3_fin[i,2] 
      cord2.x = bun3_fin[j,1] 
      cord2.y = bun3_fin[j,2] 
      sum = bun3_fin[i,3]+bun3_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p2 <- data1

#P = 3일때
row.names(bun3_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p2)){
  for (j in 1:nrow(bun3_fin)){
    if (distm(bun3_fin[j,2:1],p2[i,2:1],fun = distHaversine)>100 & 
        distm(bun3_fin[j,2:1],p2[i,4:3],fun = distHaversine)>100 ){
      
      cord1.x = p2[i,1]
      cord1.y = p2[i,2] 
      cord2.x = p2[i,3] 
      cord2.y = p2[i,4]
      cord3.x = bun3_fin[j,1]
      cord3.y = bun3_fin[j,2]
      sum = p2$sum[i] + bun3_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}

p3 <- data1

#p=4일때
row.names(bun3_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p3)){
  for (j in 1:nrow(bun3_fin)){
    if (distm(bun3_fin[j,2:1],p3[i,2:1],fun = distHaversine)>100 & 
        distm(bun3_fin[j,2:1],p3[i,4:3],fun = distHaversine) > 100 & 
        distm(bun3_fin[j,2:1],p3[i,6:5],fun = distHaversine)>100){
      cord1.x = p3[i,1]
      cord1.y = p3[i,2] 
      cord2.x = p3[i,3] 
      cord2.y = p3[i,4]
      cord3.x = p3[i,5] 
      cord3.y = p3[i,6]
      cord4.x = bun3_fin[j,1]
      cord4.y = bun3_fin[j,2]
      sum = p3$sum[i] + bun3_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p4 <- data1

#p=5일때
row.names(bun3_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p4)){
  for (j in 1:nrow(bun3_fin)){
    if (distm(bun3_fin[j,2:1],p4[i,2:1],fun = distHaversine)>100 & 
        distm(bun3_fin[j,2:1],p4[i,4:3],fun = distHaversine) > 100 & 
        distm(bun3_fin[j,2:1],p4[i,6:5],fun = distHaversine)>100 & 
        distm(bun3_fin[j,2:1],p4[i,8:7],fun = distHaversine)>100){
      cord1.x = p4[i,1]
      cord1.y = p4[i,2] 
      cord2.x = p4[i,3] 
      cord2.y = p4[i,4]
      cord3.x = p4[i,5] 
      cord3.y = p4[i,6]
      cord4.x = p4[i,7] 
      cord4.y = p4[i,8]
      cord5.x = bun3_fin[j,1]
      cord5.y = bun3_fin[j,2]
      sum = p4$sum[i] + bun3_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,cord5.x,cord5.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p5 <- data1

#p=6일때
row.names(bun3_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p5)){
  for (j in 1:nrow(bun3_fin)){
    if (distm(bun3_fin[j,2:1],p5[i,2:1],fun = distHaversine)>100 & 
        distm(bun3_fin[j,2:1],p5[i,4:3],fun = distHaversine) > 100 & 
        distm(bun3_fin[j,2:1],p5[i,6:5],fun = distHaversine)>100& 
        distm(bun3_fin[j,2:1],p5[i,8:7],fun = distHaversine)>100& 
        distm(bun3_fin[j,2:1],p5[i,10:9],fun = distHaversine)>100){
      cord1.x = p5[i,1]
      cord1.y = p5[i,2] 
      cord2.x = p5[i,3] 
      cord2.y = p5[i,4]
      cord3.x = p5[i,5] 
      cord3.y = p5[i,6]
      cord4.x = p5[i,7] 
      cord4.y = p5[i,8]
      cord5.x = p5[i,9] 
      cord5.y = p5[i,10]
      cord6.x = bun3_fin[j,1]
      cord6.y = bun3_fin[j,2]
      sum = p5$sum[i] + bun3_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,cord5.x,cord5.y,cord6.x,cord6.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p6 <- data1

#p=7일때
row.names(bun3_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p6)){
  for (j in 1:nrow(bun3_fin)){
    if (distm(bun3_fin[j,2:1],p6[i,2:1],fun = distHaversine)>100 & 
        distm(bun3_fin[j,2:1],p6[i,4:3],fun = distHaversine) > 100 & 
        distm(bun3_fin[j,2:1],p6[i,6:5],fun = distHaversine)>100 & 
        distm(bun3_fin[j,2:1],p6[i,8:7],fun = distHaversine)>100& 
        distm(bun3_fin[j,2:1],p6[i,10:9],fun = distHaversine)>100 & 
        distm(bun3_fin[j,2:1],p6[i,12:11],fun = distHaversine)>100){
      cord1.x = p6[i,1]
      cord1.y = p6[i,2] 
      cord2.x = p6[i,3] 
      cord2.y = p6[i,4]
      cord3.x = p6[i,5] 
      cord3.y = p6[i,6]
      cord4.x = p6[i,7] 
      cord4.y = p6[i,8]
      cord5.x = p6[i,9] 
      cord5.y = p6[i,10]
      cord6.x = p6[i,11] 
      cord6.y = p6[i,12]
      cord7.x = bun3_fin[j,1]
      cord7.y = bun3_fin[j,2]
      sum = p6$sum[i] + bun3_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,cord5.x,cord5.y,cord6.x,cord6.y, cord7.x,cord7.y, sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p7 <- data1

#p=8일때
row.names(bun3_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p7)){
  for (j in 1:nrow(bun3_fin)){
    if (distm(bun3_fin[j,2:1],p7[i,2:1],fun = distHaversine)>100 & 
        distm(bun3_fin[j,2:1],p7[i,4:3],fun = distHaversine) > 100 & 
        distm(bun3_fin[j,2:1],p7[i,6:5],fun = distHaversine)>100 & 
        distm(bun3_fin[j,2:1],p7[i,8:7],fun = distHaversine)>100& 
        distm(bun3_fin[j,2:1],p7[i,10:9],fun = distHaversine)>100 & 
        distm(bun3_fin[j,2:1],p7[i,12:11],fun = distHaversine)>100& 
        distm(bun3_fin[j,2:1],p7[i,14:13],fun = distHaversine)>100){
      cord1.x = p7[i,1]
      cord1.y = p7[i,2] 
      cord2.x = p7[i,3] 
      cord2.y = p7[i,4]
      cord3.x = p7[i,5] 
      cord3.y = p7[i,6]
      cord4.x = p7[i,7] 
      cord4.y = p7[i,8]
      cord5.x = p7[i,9] 
      cord5.y = p7[i,10]
      cord6.x = p7[i,11] 
      cord6.y = p7[i,12]
      cord7.x = p7[i,13] 
      cord7.y = p7[i,14]
      cord8.x = bun3_fin[j,1]
      cord8.y = bun3_fin[j,2]
      sum = p7$sum[i] + bun3_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,cord5.x,cord5.y,cord6.x,cord6.y, 
                    cord7.x,cord7.y,cord8.x,cord8.y, sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p8 <- data1

marginal1 <- max(p1$X)   
marginal2 <-max(p2$sum) - max(p1$X) 
marginal3 <-max(p3$sum) - max(p2$sum)
marginal4 <-max(p4$sum) - max(p3$sum)
marginal5 <-max(p5$sum) - max(p4$sum)
marginal6 <-max(p6$sum) - max(p5$sum)
marginal7 <-max(p7$sum) - max(p6$sum)
marginal8 <-max(p8$sum)- max(p7$sum)

marginal_mat_bun3 <- matrix(c(1,marginal1,2,marginal2,3,marginal3,4,marginal4,5,marginal5,6,marginal6,7,marginal7,8,marginal8),
                            ncol = 2,byrow = T ) %>% as.data.frame()

colnames(marginal_mat_bun3) <- c('와이파이수','한계효용')


########################
###########삼성동
samsung_boundary <- dong_boundary %>% filter(id  == 1121082) #동 boundary
samsung_wifi <- wifi %>% filter(dong == '삼성동') %>% select(설치위치.X좌표.,설치위치.Y좌표.)
samsung_building <- building %>% filter(dong == '삼성동')
colnames(samsung_wifi) <- c('long','lat')
samsung_wifi$long <- samsung_wifi$long %>% as.numeric
samsung_wifi$lat <- samsung_wifi$lat %>% as.numeric

###삼성동 현재와이파이와  거리구하기
dist_samsung <- matrix(nrow = nrow(samsung_building), ncol = nrow(samsung_wifi))
for (i in 1:nrow(samsung_building)) {
  lon1 <- samsung_building$long[i]
  lat1 <- samsung_building$lat[i]
  for (j in 1:nrow(samsung_wifi)) {
    lon2 <- samsung_wifi$long[j]
    lat2 <- samsung_wifi$lat[j]
    
    dist_samsung[i,j] <-  distHaversine(c(lon1, lat1), c(lon2, lat2))
  }
  if (i %% 10 == 0) {
    print(i)
  }
}

dist_samsung <- dist_samsung %>% as.data.frame()
dist_samsung$idx <- 1:nrow(dist_samsung)
###현재와이파이와 dist 100m 미만 제거
samsung_nowifi_idx <- dist_samsung %>% filter(V1 > 50 & V2 > 50 & V3 > 50 & V4 > 50 & V5 > 50 & V6 > 50,
                                              V7 > 50,V8 > 50,V9 > 50,V10 > 50,V11 > 50)
samsung_nowifi_build <- samsung_building[c(samsung_nowifi_idx$idx),]

samsung_lat = seq(min(samsung_building$lat),max(samsung_building$lat),0.00008)
samsung_long = seq(min(samsung_building$long),max(samsung_building$long),0.0001)

samsung_coord_fin <- NULL
samsung_coord <- NULL
for (i in samsung_lat){
  for(j in samsung_long){
    samsung_coord$lat <- i
    samsung_coord$long <-j
    samsung_coord <- samsung_coord %>% as.data.frame()
    samsung_coord_fin <- rbind(samsung_coord_fin,samsung_coord)
  }
}

#p=1 일때의 좌표값
samsung_coord_fin <- cbind(samsung_coord_fin,X = rowSums(distm(samsung_coord_fin[,c(2,1)],
                                                               samsung_nowifi_build[,8:7], fun = distHaversine) <= 50))
p1 <- samsung_coord_fin
#데이터가 크지 않아서 위해 상위 25% 좌표값에 대해서만 연산
quantile(samsung_coord_fin$X,0.75)
#P = 2일때
samsung_fin <- samsung_coord_fin[order(desc(samsung_coord_fin$X)),] %>% filter(X >quantile(samsung_coord_fin$X,0.75))

row.names(samsung_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(samsung_fin)){
  for (j in 1:nrow(samsung_fin)){
    if (distm(samsung_fin[i,2:1],samsung_fin[j,2:1],fun = distHaversine)>100){ 
      cord1.x = samsung_fin[i,1]
      cord1.y = samsung_fin[i,2] 
      cord2.x = samsung_fin[j,1] 
      cord2.y = samsung_fin[j,2] 
      sum = samsung_fin[i,3]+samsung_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p2 <- data1

#P = 3일때
row.names(samsung_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p2)){
  for (j in 1:nrow(samsung_fin)){
    if (distm(samsung_fin[j,2:1],p2[i,2:1],fun = distHaversine)>100 & 
        distm(samsung_fin[j,2:1],p2[i,4:3],fun = distHaversine)>100 ){
      
      cord1.x = p2[i,1]
      cord1.y = p2[i,2] 
      cord2.x = p2[i,3] 
      cord2.y = p2[i,4]
      cord3.x = samsung_fin[j,1]
      cord3.y = samsung_fin[j,2]
      sum = p2$sum[i] + samsung_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}

p3 <- data1

#p=4일때
row.names(samsung_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p3)){
  for (j in 1:nrow(samsung_fin)){
    if (distm(samsung_fin[j,2:1],p3[i,2:1],fun = distHaversine)>100 & 
        distm(samsung_fin[j,2:1],p3[i,4:3],fun = distHaversine) > 100 & 
        distm(samsung_fin[j,2:1],p3[i,6:5],fun = distHaversine)>100){
      cord1.x = p3[i,1]
      cord1.y = p3[i,2] 
      cord2.x = p3[i,3] 
      cord2.y = p3[i,4]
      cord3.x = p3[i,5] 
      cord3.y = p3[i,6]
      cord4.x = samsung_fin[j,1]
      cord4.y = samsung_fin[j,2]
      sum = p3$sum[i] + samsung_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p4 <- data1

#p=5일때
row.names(samsung_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p4)){
  for (j in 1:nrow(samsung_fin)){
    if (distm(samsung_fin[j,2:1],p4[i,2:1],fun = distHaversine)>100 & 
        distm(samsung_fin[j,2:1],p4[i,4:3],fun = distHaversine) > 100 & 
        distm(samsung_fin[j,2:1],p4[i,6:5],fun = distHaversine)>100 & 
        distm(samsung_fin[j,2:1],p4[i,8:7],fun = distHaversine)>100){
      cord1.x = p4[i,1]
      cord1.y = p4[i,2] 
      cord2.x = p4[i,3] 
      cord2.y = p4[i,4]
      cord3.x = p4[i,5] 
      cord3.y = p4[i,6]
      cord4.x = p4[i,7] 
      cord4.y = p4[i,8]
      cord5.x = samsung_fin[j,1]
      cord5.y = samsung_fin[j,2]
      sum = p4$sum[i] + samsung_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,cord5.x,cord5.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p5 <- data1

#p=6일때
row.names(samsung_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p5)){
  for (j in 1:nrow(samsung_fin)){
    if (distm(samsung_fin[j,2:1],p5[i,2:1],fun = distHaversine)>100 & 
        distm(samsung_fin[j,2:1],p5[i,4:3],fun = distHaversine) > 100 & 
        distm(samsung_fin[j,2:1],p5[i,6:5],fun = distHaversine)>100& 
        distm(samsung_fin[j,2:1],p5[i,8:7],fun = distHaversine)>100& 
        distm(samsung_fin[j,2:1],p5[i,10:9],fun = distHaversine)>100){
      cord1.x = p5[i,1]
      cord1.y = p5[i,2] 
      cord2.x = p5[i,3] 
      cord2.y = p5[i,4]
      cord3.x = p5[i,5] 
      cord3.y = p5[i,6]
      cord4.x = p5[i,7] 
      cord4.y = p5[i,8]
      cord5.x = p5[i,9] 
      cord5.y = p5[i,10]
      cord6.x = samsung_fin[j,1]
      cord6.y = samsung_fin[j,2]
      sum = p5$sum[i] + samsung_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,cord5.x,cord5.y,cord6.x,cord6.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p6 <- data1

#p=7일때
row.names(samsung_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p6)){
  for (j in 1:nrow(samsung_fin)){
    if (distm(samsung_fin[j,2:1],p6[i,2:1],fun = distHaversine)>100 & 
        distm(samsung_fin[j,2:1],p6[i,4:3],fun = distHaversine) > 100 & 
        distm(samsung_fin[j,2:1],p6[i,6:5],fun = distHaversine)>100 & 
        distm(samsung_fin[j,2:1],p6[i,8:7],fun = distHaversine)>100& 
        distm(samsung_fin[j,2:1],p6[i,10:9],fun = distHaversine)>100 & 
        distm(samsung_fin[j,2:1],p6[i,12:11],fun = distHaversine)>100){
      cord1.x = p6[i,1]
      cord1.y = p6[i,2] 
      cord2.x = p6[i,3] 
      cord2.y = p6[i,4]
      cord3.x = p6[i,5] 
      cord3.y = p6[i,6]
      cord4.x = p6[i,7] 
      cord4.y = p6[i,8]
      cord5.x = p6[i,9] 
      cord5.y = p6[i,10]
      cord6.x = p6[i,11] 
      cord6.y = p6[i,12]
      cord7.x = samsung_fin[j,1]
      cord7.y = samsung_fin[j,2]
      sum = p6$sum[i] + samsung_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,cord5.x,cord5.y,cord6.x,cord6.y, cord7.x,cord7.y, sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p7 <- data1

#p=8일때
row.names(samsung_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p7)){
  for (j in 1:nrow(samsung_fin)){
    if (distm(samsung_fin[j,2:1],p7[i,2:1],fun = distHaversine)>100 & 
        distm(samsung_fin[j,2:1],p7[i,4:3],fun = distHaversine) > 100 & 
        distm(samsung_fin[j,2:1],p7[i,6:5],fun = distHaversine)>100 & 
        distm(samsung_fin[j,2:1],p7[i,8:7],fun = distHaversine)>100& 
        distm(samsung_fin[j,2:1],p7[i,10:9],fun = distHaversine)>100 & 
        distm(samsung_fin[j,2:1],p7[i,12:11],fun = distHaversine)>100&
        distm(samsung_fin[j,2:1],p7[i,14:13],fun = distHaversine)>100){
      cord1.x = p7[i,1]
      cord1.y = p7[i,2] 
      cord2.x = p7[i,3] 
      cord2.y = p7[i,4]
      cord3.x = p7[i,5] 
      cord3.y = p7[i,6]
      cord4.x = p7[i,7] 
      cord4.y = p7[i,8]
      cord5.x = p7[i,9] 
      cord5.y = p7[i,10]
      cord6.x = p7[i,11] 
      cord6.y = p7[i,12]
      cord7.x = p7[i,13] 
      cord7.y = p7[i,14]
      cord8.x = samsung_fin[j,1]
      cord8.y = samsung_fin[j,2]
      sum = p7$sum[i] + samsung_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,cord5.x,cord5.y,cord6.x,cord6.y, 
                    cord7.x,cord7.y,cord8.x,cord8.y, sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p8 <- data1

##한계효용

marginal1 <- max(p1$X)   
marginal2 <-max(p2$sum) - max(p1$X) 
marginal3 <-max(p3$sum) - max(p2$sum)
marginal4 <-max(p4$sum) - max(p3$sum)
marginal5 <-max(p5$sum) - max(p4$sum)
marginal6 <-max(p6$sum) - max(p5$sum)
marginal7 <-max(p7$sum) - max(p6$sum)
marginal8 <-max(p8$sum)- max(p7$sum)

marginal_mat_samsung <- matrix(c(1,marginal1,2,marginal2,3,marginal3,4,marginal4,5,marginal5,6,marginal6,7,marginal7,8,marginal8),
                               ncol = 2,byrow = T ) %>% as.data.frame()

colnames(marginal_mat_samsung) <- c('와이파이수','한계효용')


#######################
#####상계3.4동

sangye34_boundary <- dong_boundary %>% filter(id  == 1111076) #동 boundary
sangye34_wifi <- wifi %>% filter(dong == '상계3.4동') %>% select(설치위치.X좌표.,설치위치.Y좌표.)
sangye34_building <- building %>% filter(dong == '상계3.4동')
colnames(sangye34_wifi) <- c('long','lat')
sangye34_wifi$long <- sangye34_wifi$long %>% as.numeric
sangye34_wifi$lat <- sangye34_wifi$lat %>% as.numeric

###상계3.4동 현재와이파이와  거리구하기
dist_sangye34 <- matrix(nrow = nrow(sangye34_building), ncol = nrow(sangye34_wifi))
for (i in 1:nrow(sangye34_building)) {
  lon1 <- sangye34_building$long[i]
  lat1 <- sangye34_building$lat[i]
  for (j in 1:nrow(sangye34_wifi)) {
    lon2 <- sangye34_wifi$long[j]
    lat2 <- sangye34_wifi$lat[j]
    
    dist_sangye34[i,j] <-  distHaversine(c(lon1, lat1), c(lon2, lat2))
  }
  if (i %% 10 == 0) {
    print(i)
  }
}

dist_sangye34 <- dist_sangye34 %>% as.data.frame()
dist_sangye34$idx <- 1:nrow(dist_sangye34)
###현재와이파이와 dist 50m 미만 제거
sangye34_nowifi_idx <- dist_sangye34 %>% filter(V1 > 50 & V2 > 50 & V3 > 50 & V4 > 50 & V5 > 50 & V6 > 50,
                                                V7 > 50,V8 > 50,V9 > 50,V10 > 50,V11 > 50 )
sangye34_nowifi_build <- sangye34_building[c(sangye34_nowifi_idx$idx),]

sangye34_lat = seq(min(sangye34_building$lat),max(sangye34_building$lat),0.00008)
sangye34_long = seq(min(sangye34_building$long),max(sangye34_building$long),0.0001)

sangye34_coord_fin <- NULL
sangye34_coord <- NULL
for (i in sangye34_lat){
  for(j in sangye34_long){
    sangye34_coord$lat <- i
    sangye34_coord$long <-j
    sangye34_coord <- sangye34_coord %>% as.data.frame()
    sangye34_coord_fin <- rbind(sangye34_coord_fin,sangye34_coord)
  }
}

#p=1 일때의 좌표값
sangye34_coord_fin <- cbind(sangye34_coord_fin,X = rowSums(distm(sangye34_coord_fin[,c(2,1)],
                                                                 sangye34_nowifi_build[,8:7], fun = distHaversine) <= 50))
p1 <- sangye34_coord_fin
#빠른 연산을 위해 상위 2% 좌표값에 대해서만 연산(실제 quantile 보면 사실상 한 2%가 적절함)
quantile(sangye34_coord_fin$X,0.98)
#P = 2일때
sangye34_fin <- sangye34_coord_fin[order(desc(sangye34_coord_fin$X)),] %>% filter(X >quantile(sangye34_coord_fin$X,0.98))

row.names(sangye34_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(sangye34_fin)){
  for (j in 1:nrow(sangye34_fin)){
    if (distm(sangye34_fin[i,2:1],sangye34_fin[j,2:1],fun = distHaversine)>100){ 
      cord1.x = sangye34_fin[i,1]
      cord1.y = sangye34_fin[i,2] 
      cord2.x = sangye34_fin[j,1] 
      cord2.y = sangye34_fin[j,2] 
      sum = sangye34_fin[i,3]+sangye34_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p2 <- data1

#P = 3일때
row.names(sangye34_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p2)){
  for (j in 1:nrow(sangye34_fin)){
    if (distm(sangye34_fin[j,2:1],p2[i,2:1],fun = distHaversine)>100 & 
        distm(sangye34_fin[j,2:1],p2[i,4:3],fun = distHaversine)>100 ){
      
      cord1.x = p2[i,1]
      cord1.y = p2[i,2] 
      cord2.x = p2[i,3] 
      cord2.y = p2[i,4]
      cord3.x = sangye34_fin[j,1]
      cord3.y = sangye34_fin[j,2]
      sum = p2$sum[i] + sangye34_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}

p3 <- data1

#p=4일때
row.names(sangye34_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p3)){
  for (j in 1:nrow(sangye34_fin)){
    if (distm(sangye34_fin[j,2:1],p3[i,2:1],fun = distHaversine)>100 & 
        distm(sangye34_fin[j,2:1],p3[i,4:3],fun = distHaversine) > 100 & 
        distm(sangye34_fin[j,2:1],p3[i,6:5],fun = distHaversine)>100){
      cord1.x = p3[i,1]
      cord1.y = p3[i,2] 
      cord2.x = p3[i,3] 
      cord2.y = p3[i,4]
      cord3.x = p3[i,5] 
      cord3.y = p3[i,6]
      cord4.x = sangye34_fin[j,1]
      cord4.y = sangye34_fin[j,2]
      sum = p3$sum[i] + sangye34_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p4 <- data1

#p=5일때
row.names(sangye34_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p4)){
  for (j in 1:nrow(sangye34_fin)){
    if (distm(sangye34_fin[j,2:1],p4[i,2:1],fun = distHaversine)>100 & 
        distm(sangye34_fin[j,2:1],p4[i,4:3],fun = distHaversine) > 100 & 
        distm(sangye34_fin[j,2:1],p4[i,6:5],fun = distHaversine)>100 & 
        distm(sangye34_fin[j,2:1],p4[i,8:7],fun = distHaversine)>100){
      cord1.x = p4[i,1]
      cord1.y = p4[i,2] 
      cord2.x = p4[i,3] 
      cord2.y = p4[i,4]
      cord3.x = p4[i,5] 
      cord3.y = p4[i,6]
      cord4.x = p4[i,7] 
      cord4.y = p4[i,8]
      cord5.x = sangye34_fin[j,1]
      cord5.y = sangye34_fin[j,2]
      sum = p4$sum[i] + sangye34_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,cord5.x,cord5.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p5 <- data1

#p=6일때
row.names(sangye34_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p5)){
  for (j in 1:nrow(sangye34_fin)){
    if (distm(sangye34_fin[j,2:1],p5[i,2:1],fun = distHaversine)>100 & 
        distm(sangye34_fin[j,2:1],p5[i,4:3],fun = distHaversine) > 100 & 
        distm(sangye34_fin[j,2:1],p5[i,6:5],fun = distHaversine)>100& 
        distm(sangye34_fin[j,2:1],p5[i,8:7],fun = distHaversine)>100& 
        distm(sangye34_fin[j,2:1],p5[i,10:9],fun = distHaversine)>100){
      cord1.x = p5[i,1]
      cord1.y = p5[i,2] 
      cord2.x = p5[i,3] 
      cord2.y = p5[i,4]
      cord3.x = p5[i,5] 
      cord3.y = p5[i,6]
      cord4.x = p5[i,7] 
      cord4.y = p5[i,8]
      cord5.x = p5[i,9] 
      cord5.y = p5[i,10]
      cord6.x = sangye34_fin[j,1]
      cord6.y = sangye34_fin[j,2]
      sum = p5$sum[i] + sangye34_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,cord5.x,cord5.y,cord6.x,cord6.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p6 <- data1

#p=7일때
row.names(sangye34_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p6)){
  for (j in 1:nrow(sangye34_fin)){
    if (distm(sangye34_fin[j,2:1],p6[i,2:1],fun = distHaversine)>100 & 
        distm(sangye34_fin[j,2:1],p6[i,4:3],fun = distHaversine) > 100 & 
        distm(sangye34_fin[j,2:1],p6[i,6:5],fun = distHaversine)>100 & 
        distm(sangye34_fin[j,2:1],p6[i,8:7],fun = distHaversine)>100& 
        distm(sangye34_fin[j,2:1],p6[i,10:9],fun = distHaversine)>100 & 
        distm(sangye34_fin[j,2:1],p6[i,12:11],fun = distHaversine)>100){
      cord1.x = p6[i,1]
      cord1.y = p6[i,2] 
      cord2.x = p6[i,3] 
      cord2.y = p6[i,4]
      cord3.x = p6[i,5] 
      cord3.y = p6[i,6]
      cord4.x = p6[i,7] 
      cord4.y = p6[i,8]
      cord5.x = p6[i,9] 
      cord5.y = p6[i,10]
      cord6.x = p6[i,11] 
      cord6.y = p6[i,12]
      cord7.x = sangye34_fin[j,1]
      cord7.y = sangye34_fin[j,2]
      sum = p6$sum[i] + sangye34_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,cord5.x,cord5.y,cord6.x,cord6.y, 
                    cord7.x,cord7.y, sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p7 <- data1

#p=8일때
row.names(sangye34_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p7)){
  for (j in 1:nrow(sangye34_fin)){
    if (distm(sangye34_fin[j,2:1],p7[i,2:1],fun = distHaversine)>100 & 
        distm(sangye34_fin[j,2:1],p7[i,4:3],fun = distHaversine) > 100 & 
        distm(sangye34_fin[j,2:1],p7[i,6:5],fun = distHaversine)>100 & 
        distm(sangye34_fin[j,2:1],p7[i,8:7],fun = distHaversine)>100& 
        distm(sangye34_fin[j,2:1],p7[i,10:9],fun = distHaversine)>100 & 
        distm(sangye34_fin[j,2:1],p7[i,12:11],fun = distHaversine)>100& distm(sangye34_fin[j,2:1],p7[i,14:13],fun = distHaversine)>100){
      cord1.x = p7[i,1]
      cord1.y = p7[i,2] 
      cord2.x = p7[i,3] 
      cord2.y = p7[i,4]
      cord3.x = p7[i,5] 
      cord3.y = p7[i,6]
      cord4.x = p7[i,7] 
      cord4.y = p7[i,8]
      cord5.x = p7[i,9] 
      cord5.y = p7[i,10]
      cord6.x = p7[i,11] 
      cord6.y = p7[i,12]
      cord7.x = p7[i,13] 
      cord7.y = p7[i,14]
      cord8.x = sangye34_fin[j,1]
      cord8.y = sangye34_fin[j,2]
      sum = p7$sum[i] + sangye34_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,cord5.x,cord5.y,cord6.x,cord6.y, 
                    cord7.x,cord7.y,cord8.x,cord8.y, sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p8 <- data1


marginal1 <- max(p1$X)   
marginal2 <-max(p2$sum) - max(p1$X) 
marginal3 <-max(p3$sum) - max(p2$sum)
marginal4 <-max(p4$sum) - max(p3$sum)
marginal5 <-max(p5$sum) - max(p4$sum)
marginal6 <-max(p6$sum) - max(p5$sum)
marginal7 <-max(p7$sum) - max(p6$sum)
marginal8 <-max(p8$sum)- max(p7$sum)

marginal_mat_sangye34 <- matrix(c(1,marginal1,2,marginal2,3,marginal3,4,marginal4,5,marginal5,6,marginal6,7,marginal7,8,marginal8),
                                ncol = 2,byrow = T ) %>% as.data.frame()

colnames(marginal_mat_sangye34) <- c('와이파이수','한계효용')


###############################
##########장위2동

jangwi2_boundary <- dong_boundary %>% filter(id  == 1108077) #동 boundary
jangwi2_wifi <- wifi %>% filter(dong == '장위2동') %>% select(설치위치.X좌표.,설치위치.Y좌표.)
jangwi2_building <- building %>% filter(dong == '장위2동')
colnames(jangwi2_wifi) <- c('long','lat')
jangwi2_wifi$long <- jangwi2_wifi$long %>% as.numeric
jangwi2_wifi$lat <- jangwi2_wifi$lat %>% as.numeric

###장위2동 현재와이파이와  거리구하기
dist_jangwi2 <- matrix(nrow = nrow(jangwi2_building), ncol = nrow(jangwi2_wifi))
for (i in 1:nrow(jangwi2_building)) {
  lon1 <- jangwi2_building$long[i]
  lat1 <- jangwi2_building$lat[i]
  for (j in 1:nrow(jangwi2_wifi)) {
    lon2 <- jangwi2_wifi$long[j]
    lat2 <- jangwi2_wifi$lat[j]
    
    dist_jangwi2[i,j] <-  distHaversine(c(lon1, lat1), c(lon2, lat2))
  }
  if (i %% 10 == 0) {
    print(i)
  }
}

dist_jangwi2 <- dist_jangwi2 %>% as.data.frame()
dist_jangwi2$idx <- 1:nrow(dist_jangwi2)
###현재와이파이와 dist 50m 미만 제거
jangwi2_nowifi_idx <- dist_jangwi2 %>% filter(V1 > 50 & V2 > 50 & V3 > 50 & V4 > 50 & V5 > 50)
jangwi2_nowifi_build <- jangwi2_building[c(jangwi2_nowifi_idx$idx),]

jangwi2_lat = seq(min(jangwi2_building$lat),max(jangwi2_building$lat),0.00008)
jangwi2_long = seq(min(jangwi2_building$long),max(jangwi2_building$long),0.0001)

jangwi2_coord_fin <- NULL
jangwi2_coord <- NULL
for (i in jangwi2_lat){
  for(j in jangwi2_long){
    jangwi2_coord$lat <- i
    jangwi2_coord$long <-j
    jangwi2_coord <- jangwi2_coord %>% as.data.frame()
    jangwi2_coord_fin <- rbind(jangwi2_coord_fin,jangwi2_coord)
  }
}


#p=1 일때의 좌표값
jangwi2_coord_fin <- cbind(jangwi2_coord_fin,X = rowSums(distm(jangwi2_coord_fin[,c(2,1)],
                                                               jangwi2_nowifi_build[,8:7], fun = distHaversine) <= 50))
p1 <- jangwi2_coord_fin
#장위2동은 좌표수가 적어서 상위 50% 좌표값에 대해서만 연산(실제 quantile 보면 사실상 한 2%가 적절함)
quantile(jangwi2_coord_fin$X,0.50)
#P = 2일때
jangwi2_fin <- jangwi2_coord_fin[order(desc(jangwi2_coord_fin$X)),] %>% filter(X >quantile(jangwi2_coord_fin$X,0.50))

row.names(jangwi2_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(jangwi2_fin)){
  for (j in 1:nrow(jangwi2_fin)){
    if (distm(jangwi2_fin[i,2:1],jangwi2_fin[j,2:1],fun = distHaversine)>100){ 
      cord1.x = jangwi2_fin[i,1]
      cord1.y = jangwi2_fin[i,2] 
      cord2.x = jangwi2_fin[j,1] 
      cord2.y = jangwi2_fin[j,2] 
      sum = jangwi2_fin[i,3]+jangwi2_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p2 <- data1

#P = 3일때
row.names(jangwi2_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p2)){
  for (j in 1:nrow(jangwi2_fin)){
    if (distm(jangwi2_fin[j,2:1],p2[i,2:1],fun = distHaversine)>100 & 
        distm(jangwi2_fin[j,2:1],p2[i,4:3],fun = distHaversine)>100 ){
      
      cord1.x = p2[i,1]
      cord1.y = p2[i,2] 
      cord2.x = p2[i,3] 
      cord2.y = p2[i,4]
      cord3.x = jangwi2_fin[j,1]
      cord3.y = jangwi2_fin[j,2]
      sum = p2$sum[i] + jangwi2_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}

p3 <- data1

#p=4일때
row.names(jangwi2_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p3)){
  for (j in 1:nrow(jangwi2_fin)){
    if (distm(jangwi2_fin[j,2:1],p3[i,2:1],fun = distHaversine)>100 & 
        distm(jangwi2_fin[j,2:1],p3[i,4:3],fun = distHaversine) > 100 & 
        distm(jangwi2_fin[j,2:1],p3[i,6:5],fun = distHaversine)>100){
      cord1.x = p3[i,1]
      cord1.y = p3[i,2] 
      cord2.x = p3[i,3] 
      cord2.y = p3[i,4]
      cord3.x = p3[i,5] 
      cord3.y = p3[i,6]
      cord4.x = jangwi2_fin[j,1]
      cord4.y = jangwi2_fin[j,2]
      sum = p3$sum[i] + jangwi2_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p4 <- data1

#p=5일때
row.names(jangwi2_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p4)){
  for (j in 1:nrow(jangwi2_fin)){
    if (distm(jangwi2_fin[j,2:1],p4[i,2:1],fun = distHaversine)>100 & 
        distm(jangwi2_fin[j,2:1],p4[i,4:3],fun = distHaversine) > 100 & 
        distm(jangwi2_fin[j,2:1],p4[i,6:5],fun = distHaversine)>100 & 
        distm(jangwi2_fin[j,2:1],p4[i,8:7],fun = distHaversine)>100){
      cord1.x = p4[i,1]
      cord1.y = p4[i,2] 
      cord2.x = p4[i,3] 
      cord2.y = p4[i,4]
      cord3.x = p4[i,5] 
      cord3.y = p4[i,6]
      cord4.x = p4[i,7] 
      cord4.y = p4[i,8]
      cord5.x = jangwi2_fin[j,1]
      cord5.y = jangwi2_fin[j,2]
      sum = p4$sum[i] + jangwi2_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,cord5.x,cord5.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p5 <- data1

#p=6일때
row.names(jangwi2_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p5)){
  for (j in 1:nrow(jangwi2_fin)){
    if (distm(jangwi2_fin[j,2:1],p5[i,2:1],fun = distHaversine)>100 & 
        distm(jangwi2_fin[j,2:1],p5[i,4:3],fun = distHaversine) > 100 & 
        distm(jangwi2_fin[j,2:1],p5[i,6:5],fun = distHaversine)>100& 
        distm(jangwi2_fin[j,2:1],p5[i,8:7],fun = distHaversine)>100& 
        distm(jangwi2_fin[j,2:1],p5[i,10:9],fun = distHaversine)>100){
      cord1.x = p5[i,1]
      cord1.y = p5[i,2] 
      cord2.x = p5[i,3] 
      cord2.y = p5[i,4]
      cord3.x = p5[i,5] 
      cord3.y = p5[i,6]
      cord4.x = p5[i,7] 
      cord4.y = p5[i,8]
      cord5.x = p5[i,9] 
      cord5.y = p5[i,10]
      cord6.x = jangwi2_fin[j,1]
      cord6.y = jangwi2_fin[j,2]
      sum = p5$sum[i] + jangwi2_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,cord5.x,cord5.y,cord6.x,cord6.y,sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p6 <- data1

#p=7일때
row.names(jangwi2_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p6)){
  for (j in 1:nrow(jangwi2_fin)){
    if (distm(jangwi2_fin[j,2:1],p6[i,2:1],fun = distHaversine)>100 & 
        distm(jangwi2_fin[j,2:1],p6[i,4:3],fun = distHaversine) > 100 &
        distm(jangwi2_fin[j,2:1],p6[i,6:5],fun = distHaversine)>100 & 
        distm(jangwi2_fin[j,2:1],p6[i,8:7],fun = distHaversine)>100& 
        distm(jangwi2_fin[j,2:1],p6[i,10:9],fun = distHaversine)>100 & 
        distm(jangwi2_fin[j,2:1],p6[i,12:11],fun = distHaversine)>100){
      cord1.x = p6[i,1]
      cord1.y = p6[i,2] 
      cord2.x = p6[i,3] 
      cord2.y = p6[i,4]
      cord3.x = p6[i,5] 
      cord3.y = p6[i,6]
      cord4.x = p6[i,7] 
      cord4.y = p6[i,8]
      cord5.x = p6[i,9] 
      cord5.y = p6[i,10]
      cord6.x = p6[i,11] 
      cord6.y = p6[i,12]
      cord7.x = jangwi2_fin[j,1]
      cord7.y = jangwi2_fin[j,2]
      sum = p6$sum[i] + jangwi2_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,cord5.x,cord5.y,cord6.x,cord6.y, 
                    cord7.x,cord7.y, sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p7 <- data1

#p=8일때
row.names(jangwi2_fin) <- NULL
data <- NULL
data1 <- NULL
for (i in 1:nrow(p7)){
  for (j in 1:nrow(jangwi2_fin)){
    if (distm(jangwi2_fin[j,2:1],p7[i,2:1],fun = distHaversine)>100 & 
        distm(jangwi2_fin[j,2:1],p7[i,4:3],fun = distHaversine) > 100 & 
        distm(jangwi2_fin[j,2:1],p7[i,6:5],fun = distHaversine)>100 & 
        distm(jangwi2_fin[j,2:1],p7[i,8:7],fun = distHaversine)>100& 
        distm(jangwi2_fin[j,2:1],p7[i,10:9],fun = distHaversine)>100 & 
        distm(jangwi2_fin[j,2:1],p7[i,12:11],fun = distHaversine)>100& 
        distm(jangwi2_fin[j,2:1],p7[i,14:13],fun = distHaversine)>100){
      cord1.x = p7[i,1]
      cord1.y = p7[i,2] 
      cord2.x = p7[i,3] 
      cord2.y = p7[i,4]
      cord3.x = p7[i,5] 
      cord3.y = p7[i,6]
      cord4.x = p7[i,7] 
      cord4.y = p7[i,8]
      cord5.x = p7[i,9] 
      cord5.y = p7[i,10]
      cord6.x = p7[i,11] 
      cord6.y = p7[i,12]
      cord7.x = p7[i,13] 
      cord7.y = p7[i,14]
      cord8.x = jangwi2_fin[j,1]
      cord8.y = jangwi2_fin[j,2]
      sum = p7$sum[i] + jangwi2_fin[j,3]
      data <- cbind(cord1.x,cord1.y,cord2.x,cord2.y,cord3.x,cord3.y,cord4.x,cord4.y,cord5.x,cord5.y,cord6.x,cord6.y, 
                    cord7.x,cord7.y,cord8.x,cord8.y, sum)
      data1 <- rbind(data1,data) %>% as.data.frame()
      max <- max(data1$sum)
    }
    
  }
  print(i)
  print(data1$sum[i])
  if (max > max(data1$sum[i])){
    break}
}
p8 <- data1

##한계효용
marginal1 <- max(p1$X)   
marginal2 <-max(p2$sum) - max(p1$X) 
marginal3 <-max(p3$sum) - max(p2$sum)
marginal4 <-max(p4$sum) - max(p3$sum)
marginal5 <-max(p5$sum) - max(p4$sum)
marginal6 <-max(p6$sum) - max(p5$sum)
marginal7 <-max(p7$sum) - max(p6$sum)
marginal8 <-max(p8$sum)- max(p7$sum)

marginal_mat_jangwi2 <- matrix(c(1,marginal1,2,marginal2,3,marginal3,4,marginal4,5,marginal5,6,marginal6,7,marginal7,8,marginal8),
                               ncol = 2,byrow = T ) %>% as.data.frame()

colnames(marginal_mat_jangwi2) <- c('와이파이수','한계효용')


################시각화

####################
marginal_mat_bun3$행정동 <- '번3동'
marginal_mat_sangye34$행정동 <- '상계3.4동'
marginal_mat_jangwi2$행정동 <- '장위2동'
marginal_mat_samsung$행정동 <- '삼성동'
marginal_mat_dobong1$행정동 <- '도봉1동'

#scale 그 자기 행정동 내 퍼센트로 
marginal_mat_bun3$수요 <- marginal_mat_bun3$한계효용 / sum(marginal_mat_bun3$한계효용) 
marginal_mat_sangye34$수요 <- marginal_mat_sangye34$한계효용 / sum(marginal_mat_sangye34$한계효용) 
marginal_mat_jangwi2$수요 <- marginal_mat_jangwi2$한계효용 / sum(marginal_mat_jangwi2$한계효용) 
marginal_mat_samsung$수요 <- marginal_mat_samsung$한계효용 / sum(marginal_mat_samsung$한계효용) 
marginal_mat_dobong1$수요 <- marginal_mat_dobong1$한계효용 / sum(marginal_mat_dobong1$한계효용) 

margin <- rbind(marginal_mat_bun3, marginal_mat_sangye34)
margin1 <- rbind(margin,marginal_mat_jangwi2)
margin2 <- rbind(margin1,marginal_mat_samsung)
margin_fin<- rbind(margin2,marginal_mat_dobong1)
margin_fin$수요 <- margin_fin$수요 * 100

#####시각화
library(ggplot2)
ggplot(data = margin_fin, aes(x= 와이파이수, y = 수요,group = 행정동, color = 행정동))+
  geom_point(size=4) +
  geom_line(size=1.3)+
  geom_point( size=2.5,
              color='#FFFFFF')+
  theme(panel.background = element_blank())


##얘는 원래 그 수치로 한건데 스케일떄매 잘 안보여서 위의 plot이 나음
ggplot(data = margin_fin, aes(x= 와이파이수, y = 한계효용 ,group = 행정동, color = 행정동))+
  geom_point(size=4) +
  geom_line(size=1.3)+
  geom_point( size=2.5,
              color='#FFFFFF')+
  theme(panel.background = element_blank())