rm(list = ls())
library(tidyverse)
library(car)
if(!dir.exists('./Output')) dir.create('./Output')
if(!dir.exists('./Output/1.5T')) dir.create('./Output/1.5T')

# age, age^2, sex, ANOVA, results go to 'Output/1.5T/Anova', not related to GAMLSS model

pw <- './Output/1.5T/Anova'
if(!dir.exists(pw)) dir.create(pw)

# THICK ------------------
thick_train<- read_csv("./Data/processed/1.5T/thick.csv")%>%
  filter(test==0)
colnames(thick_train)
roi_predictors <- colnames(thick_train)[6:67]
train_data <- thick_train

P_VAL <- data.frame(
  matrix(rep(0,6*length(roi_predictors)),ncol=6))
rownames(P_VAL) <- roi_predictors
colnames(P_VAL) <- c('FfineAGE','FfineAGE2','Fsex',
                     'PfineAGE','PfineAGE2','Psex')

for(k in 1:length(roi_predictors)){
  roi <- roi_predictors[k]
  formula_i <- formula(paste0(roi,
                              "~fineAGE+I(fineAGE^2)+sex"))
  AOV_K <- Anova(lm(formula_i,data=train_data),type=3)
  P_VAL[k,] <- c(AOV_K$`F value`[2:4],
                 AOV_K$`Pr(>F)`[2:4])
}
P_VAL$sex_FDR <- p.adjust(P_VAL$`Psex`,method = 'BH')
sum(P_VAL$sex_FDR<0.05) #3
P_VAL$rownames <- rownames(P_VAL)
P_VAL <- P_VAL%>%
  dplyr::select(c(rownames,FfineAGE:sex_FDR))
write_csv(P_VAL,paste0(pw,'/anova_thick.csv'))

# thick norm --------------
thick_train<- read_csv("./Data/norm/thick_norm.csv")%>%
  filter(test==0)
colnames(thick_train)
roi_predictors <- colnames(thick_train)[6:67]
train_data <- thick_train

P_VAL <- data.frame(
  matrix(rep(0,6*length(roi_predictors)),ncol=6))
rownames(P_VAL) <- roi_predictors
colnames(P_VAL) <- c('FfineAGE','FfineAGE2','Fsex',
                   'PfineAGE','PfineAGE2','Psex')

for(k in 1:length(roi_predictors)){
  roi <- roi_predictors[k]
  formula_i <- formula(paste0(roi,
                            "~fineAGE+I(fineAGE^2)+sex"))
  AOV_K <- Anova(lm(formula_i,data=train_data),type=3)
  P_VAL[k,] <- c(AOV_K$`F value`[2:4],
               AOV_K$`Pr(>F)`[2:4])
}
P_VAL$sex_FDR <- p.adjust(P_VAL$`Psex`,method = 'BH')
sum(P_VAL$sex_FDR<0.05) #0
P_VAL$rownames <- rownames(P_VAL)
P_VAL <- P_VAL%>%
  dplyr::select(c(rownames,FfineAGE:sex_FDR))
write_csv(P_VAL,paste0(pw,'/anova_thick_norm.csv'))

# VOL ------------------- used normalized
rm(list = ls())
pw <- './Output/1.5T/Anova'

vol_train <- read_csv("./Data/norm/vol_norm.csv")%>%
  filter(test==0)
colnames(vol_train)
roi_predictors <- colnames(vol_train)[6:25]
train_data <- vol_train

P_VAL <- data.frame(
  matrix(rep(0,6*length(roi_predictors)),ncol=6))
rownames(P_VAL) <- roi_predictors
colnames(P_VAL) <- c('FfineAGE','FfineAGE2','Fsex',
                     'PfineAGE','PfineAGE2','Psex')

for(k in 1:length(roi_predictors)){
  formula_i <- formula(paste0(roi_predictors[k],
                              "~fineAGE+I(fineAGE^2)+sex"))
  AOV_K <- Anova(lm(formula_i,data=train_data),type=3)
  P_VAL[k,] <- c(AOV_K$`F value`[2:4],
                 AOV_K$`Pr(>F)`[2:4])
}
P_VAL$sex_FDR <- p.adjust(P_VAL$`Psex`,method = 'BH')
sum(P_VAL$sex_FDR<0.05) #1
P_VAL$rownames <- rownames(P_VAL)
P_VAL <- P_VAL%>%
  dplyr::select(c(rownames,FfineAGE:sex_FDR))
write_csv(P_VAL,paste0(pw,'/anova_vol.csv'))

# stats ------------
rm(list = ls())
pw <- './Output/1.5T/Anova'

stats_train<-
  read_csv("./Data/norm/stats_norm.csv")%>%
  filter(test==0)
colnames(stats_train)
roi_predictors<-colnames(stats_train)[6:16]
train_data <- stats_train

P_VAL <- data.frame(
  matrix(rep(0,6*length(roi_predictors)),ncol=6))
rownames(P_VAL) <- roi_predictors
colnames(P_VAL) <- c('FfineAGE','FfineAGE2','Fsex',
                     'PfineAGE','PfineAGE2','Psex')

for(k in 1:length(roi_predictors)){
  formula_i <- formula(paste0(roi_predictors[k],
                              "~fineAGE+I(fineAGE^2)+sex"))
  AOV_K <- Anova(lm(formula_i,data=train_data),type=3)
  P_VAL[k,] <- c(AOV_K$`F value`[2:4],
                 AOV_K$`Pr(>F)`[2:4])
}
P_VAL$sex_FDR <- p.adjust(P_VAL$`Psex`,method = 'BH')
sum(P_VAL$sex_FDR<0.05) #6
P_VAL$rownames <- rownames(P_VAL)
P_VAL <- P_VAL%>%
  dplyr::select(c(rownames,FfineAGE:sex_FDR))
write_csv(P_VAL,paste0(pw,'/anova_stats.csv'))
