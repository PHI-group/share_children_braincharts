rm(list=ls())
library(tidyverse)

# stats =================
train_data <- read_csv("./Data/norm/stats_norm.csv")%>%
  filter(test==0)
colnames(train_data)
roi_predictors <- colnames(train_data)[6:16]
stats_sex <- read_csv('./Output/1.5T/Anova/anova_stats.csv')
roi_sex <- stats_sex %>% filter(Psex<0.1) %>%
  pull(rownames)

source("./src/sex_RE/select_dist.R")
length(roi_predictors) #11
dist_vote #all NO
rm(list=ls())

# vol ==========
train_data <- read_csv("./Data/norm/vol_norm.csv")%>%
  filter(test==0)
colnames(train_data)
roi_predictors <- colnames(train_data)[6:25]

vol_sex <- read_csv('./Output/1.5T/Anova/anova_vol.csv')
roi_sex <- vol_sex %>% filter(Psex<0.1) %>%
  pull(rownames)

source("./src/sex_RE/select_dist.R")
length(roi_predictors) #20
dist_vote #all NO
rm(list=ls())

# thick norm =========
train_data <- read_csv("./Data/norm/thick_norm.csv")%>%
  filter(test==0)
colnames(train_data)
roi_predictors <- colnames(train_data)[6:67]

thick_sex <- read_csv('./Output/1.5T/Anova/anova_thick_norm.csv')
roi_sex <- thick_sex %>% filter(Psex<0.1) %>%
  pull(rownames)

source("./src/sex_RE/select_dist.R")
length(roi_predictors) #62
dist_vote #all NO
rm(list=ls())

# thick ==================
train_data <- read_csv("./Data/processed/1.5T/thick.csv")%>%
  filter(test==0)
colnames(train_data)
roi_predictors <- colnames(train_data)[6:67]

thick_sex <- read_csv('./Output/1.5T/Anova/anova_thick.csv')
roi_sex <- thick_sex %>% filter(Psex<0.1) %>%
  pull(rownames)

source("./src/sex_RE/select_dist.R")
length(roi_predictors) #62
dist_vote #all NO
rm(list=ls())
