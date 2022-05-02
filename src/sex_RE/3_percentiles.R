rm(list = ls())
library(tidyverse)

# fit the GAMLSS model
# thick --------------
pw<-"./Output/1.5T/thick"
if(!dir.exists(pw)) dir.create(pw)

train_data <- read_csv("./Data/processed/1.5T/thick.csv")%>%
  filter(test==0)%>%
  mutate(sex_code = as.factor(dplyr::recode(sex,m=1,f=0)))
test_data <- read_csv("./Data/processed/1.5T/thick.csv")%>%
  filter(test==1)%>%
  mutate(sex_code = as.factor(dplyr::recode(sex,m=1,f=0)))
colnames(train_data)
roi_predictors <- colnames(train_data)[6:67]

thick_sex <- read_csv('./Output/1.5T/Anova/anova_thick.csv')
roi_sex <- thick_sex %>% filter(Psex<0.1) %>%
  pull(rownames)

## fit model ===============
# fit GAMLSS model and model comparison, save the models into model files
if(!dir.exists(paste0(pw,"/models")))  dir.create(paste0(pw,"/models"))
K <- 2 # penality

for(i in roi_predictors){
  X_temp <- train_data
  source("./src/sex_RE/percentile_fit.R")
  filename_i<-paste0(pw,"/models/",i,".rds")
  saveRDS(m,filename_i)
}

## quantiles of trainset ============
cent <- c(10,50,90)
ct_mat <- data.frame(fineAGE=rep(train_data$fineAGE,length(cent)),
                     sex=rep(train_data$sex_code,length(cent)),
                     ct = rep(cent,each=length(train_data$fineAGE)))

for(i in roi_predictors){
  m <- readRDS(paste0(pw,"/models/",i,".rds"))
  fname <- m$family[1]
  qfun <- paste("q", fname, sep = "")
  ct_mat_i <- NULL
  for(ct in cent){
    newcall <- call(qfun, ct/100, 
                    mu = fitted(m, "mu"), 
                    sigma = fitted(m, "sigma"))
    ll <- eval(newcall)
    ct_mat_i <- c(ct_mat_i,ll)
  }
  ct_mat[,i] <- ct_mat_i
}
write_csv(ct_mat,paste0(pw,'/quantiles.csv'))

## percent of testset ==============
# generate predicted precentiles for the test set, return pat_pct
source("./src/sex_RE/percentile_pred.R")

# Thick norm ---------------
rm(list=ls())
pw<-"./Output/1.5T/thick_norm"
if(!dir.exists(pw)) dir.create(pw)

train_data <- read_csv("./Data/norm/thick_norm.csv")%>%
  filter(test==0)%>%
  mutate(sex_code = as.factor(dplyr::recode(sex,m=1,f=0)))
test_data <- read_csv("./Data/norm/thick_norm.csv")%>%
  filter(test==1)%>%
  mutate(sex_code = as.factor(dplyr::recode(sex,m=1,f=0)))
colnames(train_data)
roi_predictors <- colnames(train_data)[6:67]

thick_norm_sex <- read_csv('./Output/1.5T/Anova/anova_thick_norm.csv')
roi_sex <- thick_norm_sex %>% filter(Psex<0.1) %>%
  pull(rownames)

## fit model ==============
# fit GAMLSS model and model comparison, save the models into model files
if(!dir.exists(paste0(pw,"/models")))  dir.create(paste0(pw,"/models"))
K <- 2 # penality

for(i in roi_predictors){
  X_temp <- train_data
  source("./src/sex_RE/percentile_fit.R")
  filename_i<-paste0(pw,"/models/",i,".rds")
  saveRDS(m,filename_i)
}

## quantiles of trainset ============
cent <- c(10,50,90)
ct_mat <- data.frame(fineAGE=rep(train_data$fineAGE,length(cent)),
                     sex=rep(train_data$sex_code,length(cent)),
                     ct = rep(cent,each=length(train_data$fineAGE)))

for(i in roi_predictors){
  m <- readRDS(paste0(pw,"/models/",i,".rds"))
  fname <- m$family[1]
  qfun <- paste("q", fname, sep = "")
  ct_mat_i <- NULL
  for(ct in cent){
    newcall <- call(qfun, ct/100, 
                    mu = fitted(m, "mu"), 
                    sigma = fitted(m, "sigma"))
    ll <- eval(newcall)
    ct_mat_i <- c(ct_mat_i,ll)
  }
  ct_mat[,i] <- ct_mat_i
}
write_csv(ct_mat,paste0(pw,'/quantiles.csv'))

## percent of testset ==============
# generate predicted precentiles for the test set, return pat_pct
source("./src/sex_RE/percentile_pred.R")

#  Stats ===================
rm(list=ls())
pw<-"./Output/1.5T/stats"
if(!dir.exists(pw)) dir.create(pw)

train_data <- read_csv("./Data/norm/stats_norm.csv")%>%
  filter(test==0)%>%
  mutate(sex_code = as.factor(dplyr::recode(sex,m=1,f=0)))
test_data <- read_csv("./Data/norm/stats_norm.csv")%>%
  filter(test==1)%>%
  mutate(sex_code = as.factor(dplyr::recode(sex,m=1,f=0)))
colnames(train_data)
roi_predictors <- colnames(train_data)[6:16]

stats_sex <- read_csv('./Output/1.5T/Anova/anova_stats.csv')
roi_sex <- stats_sex %>% filter(Psex<0.1) %>%
  pull(rownames)

## fit model ==============
# fit GAMLSS model and model comparison, save the models into model files
if(!dir.exists(paste0(pw,"/models")))  dir.create(paste0(pw,"/models"))
K <- 2 # penality

for(i in roi_predictors){
  X_temp <- train_data
  source("./src/sex_RE/percentile_fit.R")
  filename_i<-paste0(pw,"/models/",i,".rds")
  saveRDS(m,filename_i)
}

## quantiles of trainset ============
cent <- c(10,50,90)
ct_mat <- data.frame(fineAGE=rep(train_data$fineAGE,length(cent)),
                     sex=rep(train_data$sex_code,length(cent)),
                     ct = rep(cent,each=length(train_data$fineAGE)))

for(i in roi_predictors){
  m <- readRDS(paste0(pw,"/models/",i,".rds"))
  fname <- m$family[1]
  qfun <- paste("q", fname, sep = "")
  ct_mat_i <- NULL
  for(ct in cent){
    newcall <- call(qfun, ct/100, 
                    mu = fitted(m, "mu"), 
                    sigma = fitted(m, "sigma"))
    ll <- eval(newcall)
    ct_mat_i <- c(ct_mat_i,ll)
  }
  ct_mat[,i] <- ct_mat_i
}
write_csv(ct_mat,paste0(pw,'/quantiles.csv'))

## percent of testset ==============
# generate predicted precentiles for the test set, return pat_pct
source("./src/sex_RE/percentile_pred.R")

#  Vol ==================
rm(list=ls())
pw<-"./Output/1.5T/vol"
if(!dir.exists(pw)) dir.create(pw)

train_data <- read_csv("./Data/norm/vol_norm.csv")%>%
  filter(test==0)%>%
  mutate(sex_code = as.factor(dplyr::recode(sex,m=1,f=0)))
test_data <- read_csv("./Data/norm/vol_norm.csv")%>%
  filter(test==1)%>%
  mutate(sex_code = as.factor(dplyr::recode(sex,m=1,f=0)))
colnames(train_data)
roi_predictors <- colnames(train_data)[6:25]

vol_sex <- read_csv('./Output/1.5T/Anova/anova_vol.csv')
roi_sex <- vol_sex %>% filter(Psex<0.1) %>%
  pull(rownames)

## fit model ==============
# fit GAMLSS model and model comparison, save the models into model files
if(!dir.exists(paste0(pw,"/models")))  dir.create(paste0(pw,"/models"))
K <- 2 # penality

for(i in roi_predictors){
  X_temp <- train_data
  source("./src/sex_RE/percentile_fit.R")
  filename_i<-paste0(pw,"/models/",i,".rds")
  saveRDS(m,filename_i)
}

## quantiles of trainset ============
cent <- c(10,50,90)
ct_mat <- data.frame(fineAGE=rep(train_data$fineAGE,length(cent)),
                     sex=rep(train_data$sex_code,length(cent)),
                     ct = rep(cent,each=length(train_data$fineAGE)))

for(i in roi_predictors){
  m <- readRDS(paste0(pw,"/models/",i,".rds"))
  fname <- m$family[1]
  qfun <- paste("q", fname, sep = "")
  ct_mat_i <- NULL
  for(ct in cent){
    newcall <- call(qfun, ct/100, 
                    mu = fitted(m, "mu"), 
                    sigma = fitted(m, "sigma"))
    ll <- eval(newcall)
    ct_mat_i <- c(ct_mat_i,ll)
  }
  ct_mat[,i] <- ct_mat_i
}
write_csv(ct_mat,paste0(pw,'/quantiles.csv'))

## percent of testset ==============
# generate predicted precentiles for the test set, return pat_pct
source("./src/sex_RE/percentile_pred.R")
