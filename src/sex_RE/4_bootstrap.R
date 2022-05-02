rm(list=ls())
library(tidyverse)

# set variable ins=1

## already generated 1000 bootstrap sets
# bootstrap sets ----------------
#num_m <- 133; num_f <- 112
#set.seed(1)
#N = 1000 # num of boot
#boot_m <- boot_f <- NULL
#for(i in 1:N){
# boot_m <- rbind(boot_m, sample(num_m,num_m,replace = T))
# boot_f <- rbind(boot_f, sample(num_f,num_f,replace = T))
#}
#write_csv(as.data.frame(boot_m),"./Output/1.5T/boot/male_bootset.csv")
#write_csv(as.data.frame(boot_f),"./Output/1.5T/boot/female_bootset.csv")

# thick ============
pw<-'./Output/1.5T/thick/boot/'
if(!dir.exists(pw)) dir.create(pw)

thick_train <-  read_csv("./Data/processed/1.5T/thick.csv")%>%
  dplyr::filter(test==0)%>%
  mutate(sex_code = as.factor(dplyr::recode(sex,m=1,f=0)))
roi_predictors <- colnames(thick_train)[6:67]

thick_sex <- read_csv('./Output/1.5T/Anova/anova_thick.csv')
roi_sex <- thick_sex %>% dplyr::filter(Psex<0.1) %>%
  pull(rownames)

thick_train_m<- thick_train%>%dplyr::filter(sex=='m')
thick_train_f<- thick_train%>%dplyr::filter(sex=='f')
num_m <- nrow(thick_train_m)
num_f <- nrow(thick_train_f)

boot_m <- read_csv("./Output/1.5T/boot/male_bootset.csv")
boot_f <- read_csv("./Output/1.5T/boot/female_bootset.csv")
N <- nrow(boot_m) # number of bootstrap, say 1000
K <- 2

new_data <- data.frame(
  fineAGE=thick_train$fineAGE,
  sex_code=thick_train$sex_code)

### bootstrap median =============

for(i in roi_predictors){
  ct_mat_i <- matrix(nrow = nrow(thick_train), ncol = N)
  
  for(j in 1:N){
    X_temp <- rbind(thick_train_m[as.numeric(boot_m[j,]),],
                        thick_train_f[as.numeric(boot_f[j,]),])
    
    flag <- try(source('./src/sex_RE/percentile_fit.R'))
    
    if(typeof(flag)!='character') ct_mat_i[,j] <- predict(m, what="mu", newdata=new_data,
                                                          type = "link")
  }
  filename_i<-paste0(pw,i,".csv")
  write.csv(ct_mat_i,filename_i)
}

# thick norm ===========
rm(list=ls())
pw<-'./Output/1.5T/thick_norm/boot/'
if(!dir.exists(pw)) dir.create(pw)

thick_train <-  read_csv("./Data/norm/thick_norm.csv")%>%
  dplyr::filter(test==0)%>%
  mutate(sex_code = as.factor(dplyr::recode(sex,m=1,f=0)))
roi_predictors <- colnames(thick_train)[6:67]

thick_sex <- read_csv('./Output/1.5T/Anova/anova_thick_norm.csv')
roi_sex <- thick_sex %>% dplyr::filter(Psex<0.1) %>%
  pull(rownames)

thick_train_m<- thick_train%>%dplyr::filter(sex=='m')
thick_train_f<- thick_train%>%dplyr::filter(sex=='f')
num_m <- nrow(thick_train_m)
num_f <- nrow(thick_train_f)

boot_m <- read_csv("./Output/1.5T/boot/male_bootset.csv")
boot_f <- read_csv("./Output/1.5T/boot/female_bootset.csv")
N <- nrow(boot_m) # number of bootstrap, say 1000
K <- 2

new_data <- data.frame(
  fineAGE=thick_train$fineAGE,
  sex_code=thick_train$sex_code)

### bootstrap median ==============
for(i in roi_predictors){
  ct_mat_i <- matrix(nrow = nrow(thick_train), ncol = N)
  m0 <- readRDS(paste0("./Output/1.5T/thick_norm/models/",i,".rds"))
  formula_mu <- formula(m0,what='mu')
  formula_sig <- formula(m0,what='sigma')
  
  for(j in 1:N){
    X_temp <- rbind(thick_train_m[as.numeric(boot_m[j,]),],
                    thick_train_f[as.numeric(boot_f[j,]),])
    
    flag <- try(source('./src/sex_RE/percentile_fit.R'))
    
    if(typeof(flag)!='character') ct_mat_i[,j] <- predict(m, what="mu", newdata=new_data,
                                                          type = "link")
  }
  filename_i<-paste0(pw,i,".csv")
  write.csv(ct_mat_i,filename_i)
}

# vol ==========
rm(list=ls())
pw<-'./Output/1.5T/vol/boot/'
if(!dir.exists(pw)) dir.create(pw)

vol_train <-  read_csv("./Data/norm/vol_norm.csv")%>%
  dplyr::filter(test==0)%>%
  mutate(sex_code = as.factor(dplyr::recode(sex,m=1,f=0)))
roi_predictors <- colnames(vol_train)[6:25]

vol_sex <- read_csv('./Output/1.5T/Anova/anova_vol.csv')
roi_sex <- vol_sex %>% dplyr::filter(Psex<0.1) %>%
  pull(rownames)

vol_train_m<- vol_train%>%dplyr::filter(sex=='m')
vol_train_f<- vol_train%>%dplyr::filter(sex=='f')

boot_m <- read_csv("./Output/1.5T/boot/male_bootset.csv")
boot_f <- read_csv("./Output/1.5T/boot/female_bootset.csv")
N <- nrow(boot_m)
K <- 2

new_data <- data.frame(
  fineAGE=vol_train$fineAGE,
  sex_code=vol_train$sex_code)

### bootstrap median =============
for(i in roi_predictors){
  ct_mat_i <- matrix(nrow = nrow(vol_train), ncol = N)
  m0 <- readRDS(paste0("./Output/1.5T/vol/models/",i,".rds"))
  formula_mu <- formula(m0,what='mu')
  formula_sig <- formula(m0,what='sigma')
  
  for(j in 1:N){
    X_temp <- rbind(vol_train_m[as.numeric(boot_m[j,]),],
                    vol_train_f[as.numeric(boot_f[j,]),])
    
    flag <- try(source('./src/sex_RE/percentile_fit.R'))
    
    if(typeof(flag)!='character') ct_mat_i[,j] <- predict(m, what="mu", newdata=new_data,
                                                          type = "link")
  }
  filename_i<-paste0(pw,i,".csv")
  write.csv(ct_mat_i,filename_i)
}

# stats ====================
rm(list=ls())
pw<-'./Output/1.5T/stats/boot/'
if(!dir.exists(pw)) dir.create(pw)

stats_train <-  read_csv("./Data/norm/stats_norm.csv")%>%
  dplyr::filter(test==0)%>%
  mutate(sex_code = as.factor(dplyr::recode(sex,m=1,f=0)))
roi_predictors <- colnames(stats_train)[6:16]

stats_sex <- read_csv('./Output/1.5T/Anova/anova_stats.csv')
roi_sex <- stats_sex %>% dplyr::filter(Psex<0.1) %>%
  pull(rownames)

stats_train_m<- stats_train%>%dplyr::filter(sex=='m')
stats_train_f<- stats_train%>%dplyr::filter(sex=='f')

boot_m <- read_csv("./Output/1.5T/boot/male_bootset.csv")
boot_f <- read_csv("./Output/1.5T/boot/female_bootset.csv")
N <- nrow(boot_m)
K <- 2

new_data <- data.frame(
  fineAGE=stats_train$fineAGE,
  sex_code=stats_train$sex_code)

### bootstrap median ===============
for(i in roi_predictors){
  ct_mat_i <- matrix(nrow = nrow(stats_train), ncol = N)
  m0 <- readRDS(paste0("./Output/1.5T/stats/models/",i,".rds"))
  formula_mu <- formula(m0,what='mu')
  formula_sig <- formula(m0,what='sigma')
  
  for(j in 1:N){
    X_temp <- rbind(stats_train_m[as.numeric(boot_m[j,]),],
                    stats_train_f[as.numeric(boot_f[j,]),])
    
    flag <- try(source('./src/sex_RE/percentile_fit.R'))
    
    if(typeof(flag)!='character') ct_mat_i[,j] <- predict(m, what="mu", newdata=new_data,
                                                          type = "link")
  }
  filename_i<-paste0(pw,i,".csv")
  write.csv(ct_mat_i,filename_i)
}
