library(gamlss)

pat_pct <- test_data %>% dplyr::select(subjID,fineAGE,diag)
new_data <- data.frame(
  fineAGE=test_data$fineAGE,
  sex_code=test_data$sex_code)
X_temp <- train_data

for(i in roi_predictors){
  m<-readRDS(paste0(pw,"/models/",i,".rds"))
  
  # predict each parameter for the new data (distribution parameters for the specific age)
  mu_i <- predict(m, what="mu", 
                  newdata=new_data,
                  type = "link")
  sig_i<-exp(predict(m, what="sigma", 
                     newdata=new_data,
                     type = "link"))
  
  # derive percentile based on the age-specific distribution
  fname <- m$family[1]
  pfun <- paste("p", fname, sep = "")
  percentile_i<-test_data%>% pull(as.name(i))%>%
    call(pfun,.,mu=mu_i,sigma=sig_i) %>%
    eval()%>%tibble()
  
  names(percentile_i)<-i
  
  pat_pct<-pat_pct%>%add_column(percentile_i)
}

write_csv(pat_pct,paste0(pw,'/perc.csv'))