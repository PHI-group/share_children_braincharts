rm(list = ls())
library(tidyverse)
library(pROC)
library(klaR)

# classify DSLD using percentiles
if(!dir.exists("./Output/1.5T/clf"))  dir.create('./Output/1.5T/clf')
source('./src/sex_RE/clf_cv.R')
gamma = NA; lambda = NA
## TRY using optimization in CV.

# not normalize thick ------------
## thick ==================
perc_thick_1.5T <- read_csv("./Output/1.5T/thick/perc.csv")
roi_predictors_thick <- colnames(perc_thick_1.5T)[-c(1:3)]

thick_clf_cv<-clf_cv(perc_thick_1.5T,
                     roi_predictors_thick,
                     gamma = gamma, lambda = lambda)

## vol =====================
perc_vol_1.5T <- read_csv("./Output/1.5T/vol/perc.csv")
roi_predictors_vol <- colnames(perc_vol_1.5T)[-c(1:3)]

vol_clf_cv<-clf_cv(perc_vol_1.5T,
                   roi_predictors_vol,
                   gamma = gamma, lambda = lambda)

## stats =====================
perc_stats_1.5T <- read_csv("./Output/1.5T/stats/perc.csv")
roi_predictors_stats <- colnames(perc_stats_1.5T)[-c(1:3,14)]

stats_clf_cv<-clf_cv(perc_stats_1.5T,
                     roi_predictors_stats,
                     gamma = gamma, lambda = lambda)

## full ===================
perc_full_1.5T <- perc_thick_1.5T%>%
  merge(perc_vol_1.5T)%>%merge(perc_stats_1.5T)
roi_predictors_full<-colnames(perc_full_1.5T)[-(1:3)]

full_clf_cv<-clf_cv(perc_full_1.5T,
                    roi_predictors_full,
                    gamma = gamma, lambda = lambda)

## roc  ====================
clf_cv_result<-data.frame(feature=c('thick',
                                    'vol', 
                                    'stats',
                                    'full'),
                          rbind(c(thick_clf_cv$Acc,
                                  thick_clf_cv$AUC),
                                c(vol_clf_cv$Acc,
                                  vol_clf_cv$AUC),
                                c(stats_clf_cv$Acc,
                                  stats_clf_cv$AUC),
                                c(full_clf_cv$Acc,
                                  full_clf_cv$AUC)))
colnames(clf_cv_result)[-1]<-c('Acc','AUC')
clf_cv_result
write_csv(clf_cv_result,
          "./Output/1.5T/clf/clf_cv_results.csv")

roc_cv_list<-list(full_clf_cv$ROC, 
                  thick_clf_cv$ROC,
                  vol_clf_cv$ROC,
                  stats_clf_cv$ROC)
names(roc_cv_list)<-
  c(paste0('All features (AUC: ',
           round(full_clf_cv$AUC,3),')'),
    paste0('Cortical thickness (AUC: ',
           round(thick_clf_cv$AUC,3),')'),
    paste0('Subcortical volume (AUC: ',
           round(vol_clf_cv$AUC,3),')'),
    paste0('General statistics (AUC: ',
           round(stats_clf_cv$AUC,3),')'))

ggroc(roc_cv_list,
      legacy.axes =T)+
  labs(title="ROC curves in recognizing patients \n on the 1.5T scanner")+
  theme_light()%+replace%
  theme(plot.title=element_text(vjust=2,hjust=0.5,
                                size = 15),
        axis.text = element_text(size=8),
        axis.title = element_text(size=10),
        legend.position = c(0.6,0.45),
        legend.title=element_blank(),
        legend.background = 
          element_rect(fill="transparent",colour=NA),
        legend.key = element_rect(colour = NA,
                                  fill = NA),
        legend.text=element_text(size=12))
ggsave("./Output/1.5T/clf/roc_cv.svg",
       width = 6,height = 6)

## importance ===============
roi_predictors_full<-colnames(perc_full_1.5T)[-(1:3)]
auc_imp <- sapply(roi_predictors_full, function(i){
  fit_i <- clf_cv(perc_full_1.5T,i,gamma=1,lambda=1)
  fit_i$AUC
})
auc_imp <- sort(auc_imp,decreasing = T)
auc_imp <- as.data.frame(auc_imp)%>%
  rownames_to_column()
head(auc_imp,10)
write_csv(auc_imp,"./Output/1.5T/clf/imp.csv")

# normalize thick -----------------
## thick norm ==============
perc_thick_norm_1.5T <- read_csv("./Output/1.5T/thick_norm/perc.csv")
roi_predictors_thick_norm <- colnames(perc_thick_norm_1.5T)[-c(1:3)]

thick_norm_clf_cv<-clf_cv(perc_thick_norm_1.5T,
                          roi_predictors_thick_norm,
                          gamma = gamma, lambda = lambda)
## stats ===================== 
colnames(perc_stats_1.5T)
roi_predictors_stats <- colnames(perc_stats_1.5T)[4:11]

stats_clf_cv_noth <- clf_cv(perc_stats_1.5T,
                     roi_predictors_stats,
                     gamma = gamma, lambda = lambda)

## full ===================
perc_full_1.5T_noth <- perc_thick_norm_1.5T%>%
  merge(perc_vol_1.5T)%>%merge(perc_stats_1.5T)
colnames(perc_full_1.5T_noth)
roi_predictors_full_noth<-colnames(perc_full_1.5T_noth)[-c(1:3,94,95)]

full_clf_cv_noth<-clf_cv(perc_full_1.5T_noth,
                    roi_predictors_full_noth,
                    gamma = gamma, lambda = lambda)

## roc ====================
clf_cv_result_noth <- data.frame(feature=c('thick',
                                    'vol', 
                                    'stats',
                                    'full'),
                          rbind(c(thick_norm_clf_cv$Acc,
                                  thick_norm_clf_cv$AUC),
                                c(vol_clf_cv$Acc,
                                  vol_clf_cv$AUC),
                                c(stats_clf_cv_noth$Acc,
                                  stats_clf_cv_noth$AUC),
                                c(full_clf_cv_noth$Acc,
                                  full_clf_cv_noth$AUC)))
colnames(clf_cv_result_noth)[-1]<-c('Acc','AUC')
clf_cv_result_noth
write_csv(clf_cv_result_noth,
          "./Output/1.5T/clf/clf_cv_results_noth.csv")

roc_cv_list_noth <- list(full_clf_cv_noth$ROC, 
                  thick_norm_clf_cv$ROC,
                  vol_clf_cv$ROC,
                  stats_clf_cv_noth$ROC)
names(roc_cv_list_noth)<-
  c(paste0('All features (AUC: ',
           round(full_clf_cv_noth$AUC,3),')'),
    paste0('Cortical thickness (AUC: ',
           round(thick_norm_clf_cv$AUC,3),')'),
    paste0('Subcortical volume (AUC: ',
           round(vol_clf_cv$AUC,3),')'),
    paste0('General statistics (AUC: ',
           round(stats_clf_cv_noth$AUC,3),')'))

ggroc(roc_cv_list_noth,
      legacy.axes =T)+
  labs(title="ROC curves in recognizing patients \n on the 1.5T scanner")+
  theme_light()%+replace%
  theme(plot.title=element_text(vjust=2,hjust=0.5,
                                size = 15),
        axis.text = element_text(size=8),
        axis.title = element_text(size=10),
        legend.position = c(0.6,0.45),
        legend.title=element_blank(),
        legend.background = 
          element_rect(fill="transparent",colour=NA),
        legend.key = element_rect(colour = NA,
                                  fill = NA),
        legend.text=element_text(size=12))
ggsave("./Output/1.5T/clf/roc_cv_noth.svg",
       width = 6,height = 6)

## importance ===============
auc_imp_noth <- sapply(roi_predictors_full_noth, function(i){
  fit_i <- clf_cv(perc_full_1.5T_noth,i,gamma=1,lambda=1)
  fit_i$AUC
})
auc_imp_noth <- sort(auc_imp_noth,decreasing = T)
auc_imp_noth <- as.data.frame(auc_imp_noth)%>%
  rownames_to_column()
head(auc_imp_noth,10)
write_csv(auc_imp_noth,"./Output/1.5T/clf/imp_noth.csv")
