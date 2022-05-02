library(gamlss)
dist_sele<-function(DATA,formula_i){
  m <- try(gamlss(formula_i,data=DATA,family = NO))
  t1 <- chooseDist(m,k=2,type='realAll',parallel='snow',ncpus=4)
  d1 <- getOrder(t1)
  chosen_dist <- names(d1)[1]
  return(chosen_dist)
}

dist_vote <- vector("list", 6)
for(i in 1:6){
  DATA <- train_data%>%dplyr::filter(i<=fineAGE,fineAGE<i+1)
  chosen_dist <- NULL
  
  for(j in 1:length(roi_predictors)){
    roi <- roi_predictors[j]
    if(roi %in% roi_sex) formula_j<-as.formula(paste0(roi,"~re(random=~1|sex)")) else
      formula_j<-as.formula(paste0(roi,"~1"))
    
    chosen_dist[j] <- dist_sele(DATA,formula_j)
  }
  
  dist_vote[[i]] <- table(chosen_dist)
  rm(list='DATA')
}

