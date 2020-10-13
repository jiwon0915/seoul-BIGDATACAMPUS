library(dplyr)
library(data.table)


finance = fread("data/living_finance_before.csv")
dongco = fread('data/code_haengjeong.csv')[,5:6]
colnames(dongco) = c('행정동코드','행정동명')

finance2 = left_join(dongco, finance)
finance2 %>% head()

write.csv(finance2, 'data/living_finance.csv', row.names = F)
