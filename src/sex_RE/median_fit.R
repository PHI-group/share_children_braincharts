med_data<-
  data.frame(fineAGE=sort(unique(train_data$fineAGE)))
new_data<-data.frame(
  fineAGE=sort(unique(train_data$fineAGE)),
  sex_code=0)
X_temp<-train_data

for(i in roi_predictors){
  
  m<-readRDS(paste0(pw,"/models/",i,".rds"))
  
  mu_i<-as.data.frame(predict(m, what="mu", 
                                  newdata=new_data,
                                  type = "link"))
  names(mu_i)<-i
  med_data<-bind_cols(med_data,mu_i)
}
write_csv(med_data,
          paste0('./Output/1.5T/clustering/',
                 feat,'_median.csv'))
