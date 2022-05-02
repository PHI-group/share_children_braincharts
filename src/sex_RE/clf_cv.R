library(caret)
library(klaR)
clf_cv<-function(perc, roi_predictors,gamma = NA, lambda = NA, K=10){
  target<-perc$diag
  set.seed(12345)
  spl_order<-createFolds(target,k=K)
  feature<-as.matrix(perc[,roi_predictors])
  pred_target<-NULL
  true_target<-NULL
  
  for(i in 1:K){
    ind_i<-spl_order[[i]]
    train_feature<-feature[-ind_i,]
    train_target<-as.factor(target[-ind_i])
    
    test_feature<-data.frame(feature[ind_i,])
    colnames(test_feature)<-roi_predictors
    test_target<-target[ind_i]
    
    train_data<-data.frame(train_feature,
                           diag=train_target)
    colnames(train_data)<-c(roi_predictors,'diag')
    set.seed(12345)
    fit<- rda(diag~.,data=train_data,gamma = gamma, lambda = lambda)
    true_target<-c(true_target,test_target)
    pred_target<-c(pred_target,
                   predict(fit,
                           test_feature)$posterior[,2])
    
  }
  acc<-
    mean(true_target==(pred_target>0.5))
  roc_1<-
    roc(response=true_target,
        predictor=pred_target,algorithm = 1,
        levels=c(0,1),direction='<')
  auc1<-auc(roc_1)
  return(list(Acc=acc,AUC=auc1,ROC=roc_1))
}