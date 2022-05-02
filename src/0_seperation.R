rm(list=ls())
library(tidyverse)
library(MatchIt)
if(!dir.exists("./Data/processed/")) dir.create("./Data/processed/")
if(!dir.exists("./Data/processed/1.5T")) dir.create("./Data/processed/1.5T")

# info --------------------
info <- read_csv('./Data/raw/1.5T/qc_info_285subjs_sanlm.csv')
info$sex
info$diag

# 分性别match IQR_perc 和 age，用于提取测试集（20 DSLD，20 TDC）
## male ------------
dsld_m <- info %>% filter(sex=='m',diag==1)
df_m <- info %>% filter(sex=='m') #把male提出来

### 原始，不匹配的距离结果
m.out0 <- matchit(diag ~ age + IQR_perc,
                  data = df_m,
                  method = NULL, distance = "glm")
summary(m.out0)

### 匹配的距离结果
m.out1 <- matchit(diag ~ age + IQR_perc,
                  data = df_m,
                  method = "nearest", distance = "glm")
m.out1
summary(m.out1)
plot(m.out1, type = "jitter", interactive = FALSE)
plot(m.out1, type = "qq")

#eCDF plot
plot(m.out1, type = "ecdf")
#density plot
plot(m.out1, type = "density")

### 提取match的数据作为测试集
test_m <- match.data(m.out1)
head(test_m)
test_m_info <- test_m%>%
  select(age,diag,IQR_perc)
test_m_info

## female ------------
dsld_f <- info %>% filter(sex=='f',diag==1)
df_f <- info %>% filter(sex=='f')

m.out0 <- matchit(diag ~ age + IQR_perc,
                  data = df_f,
                  method = NULL, distance = "glm")
summary(m.out0)

m.out1 <- matchit(diag ~ age + IQR_perc,
                  data = df_f,
                  method = "nearest", distance = "glm")
m.out1
summary(m.out1)
plot(m.out1, type = "jitter", interactive = FALSE)
plot(m.out1, type = "qq")

#eCDF plot
plot(m.out1, type = "ecdf")
#density plot
plot(m.out1, type = "density")

test_f <- match.data(m.out1)
head(test_f)
test_f_info <- test_f%>%
  select(age,diag,IQR_perc)
test_f_info

# test ----------
### 合并male和female的test set
test_new <- rbind(test_m,test_f)%>%
  add_column(test=1)%>%
  select(id,sex,age,diag,test)
train_new <- info %>%
  filter(!(id%in%test_new$id))%>%
  add_column(test=0)%>%
  select(id,sex,age,diag,test)
demog <- rbind(train_new,test_new)%>%
  rename(subjID=id,fineAGE=age)%>%
  mutate(subjID=paste0(subjID,'/'))
write_csv(demog,'./data/processed/1.5T/demog.csv')
