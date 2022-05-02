rm(list=ls())
library(tidyverse)
library(gamlss)

# choose 2nd order of 
# thick ---------------------
thick_train <- read_csv("./Data/processed/1.5T/thick.csv")%>%
  filter(test==0)
colnames(thick_train)
roi_predictors <- colnames(thick_train)[6:67]
train_data <- thick_train%>%
  mutate(sex_code = dplyr::recode(sex,m=1,f=0))

thick_sex <- read_csv('./Output/1.5T/Anova/anova_thick.csv')
roi_sex <- thick_sex %>% filter(Psex<0.1) %>%
  pull(rownames)

source("./src/sex_RE/select_addterm.R")
table(Gaic) # number of features favoring poly3
#fp: 3; poly:59
length(roi_predictors) #62 # 59/62 prefer poly3
rm(list=ls())

# thick norm ---------------------
thick_train<-read_csv("./Data/norm/thick_norm.csv")%>%
  filter(test==0)
colnames(thick_train)
roi_predictors <- colnames(thick_train)[6:67]
train_data <- thick_train%>%
  mutate(sex_code = dplyr::recode(sex,m=1,f=0))

thick_sex <- read_csv('./Output/1.5T/Anova/anova_thick_norm.csv')
roi_sex <- thick_sex %>% filter(Psex<0.1) %>%
  pull(rownames)

source("./src/sex_RE/select_addterm.R")
table(Gaic) #56
#fp: 6; poly:56
length(roi_predictors) #62
rm(list=ls())

# stats --------------------
stats_train<-
  read_csv("./Data/norm/stats_norm.csv")%>%
  filter(test==0)
colnames(stats_train)
roi_predictors<-colnames(stats_train)[6:16]
train_data <- stats_train%>%
  mutate(sex_code = dplyr::recode(sex,m=1,f=0))

stats_sex <- read_csv('./Output/1.5T/Anova/anova_stats.csv')
roi_sex <- stats_sex %>% filter(Psex<0.1) %>%
  pull(rownames)

source("./src/sex_RE/select_addterm.R")
table(Gaic) #poly: 11
length(roi_predictors) #11
rm(list=ls())

# vol --------------- 
#used normalized
vol_train <- read_csv("./Data/norm/vol_norm.csv")%>%
  filter(test==0)
colnames(vol_train)
roi_predictors <- colnames(vol_train)[6:25]
train_data <- vol_train%>%
  mutate(sex_code = dplyr::recode(sex,m=1,f=0))

vol_sex <- read_csv('./Output/1.5T/Anova/anova_vol.csv')
roi_sex <- vol_sex %>% filter(Psex<0.1) %>%
  pull(rownames)

source("./src/sex_RE/select_addterm.R")
table(Gaic)
#fp: 2; poly: 18
length(roi_predictors) #20
rm(list=ls())
