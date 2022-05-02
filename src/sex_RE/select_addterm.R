# choose model of age-dependent curves, 2nd order vs. fractional power, criteria: AIC
# return Gaic for both models
Gaic<-NULL

for(i in roi_predictors){
  formula_poly<-as.formula(paste0(i,'~poly(fineAGE,2)+re(random=~1|sex_code)'))
  formula_fp<-as.formula(paste0(i,'~fp(fineAGE,2)+re(random=~1|sex_code)'))
  if(i %in% roi_sex){
    poly3<-try(gamlss(formula_poly,
                      sigma.formula=~poly(fineAGE,2)+re(random=~1|sex_code),
                      data=train_data, family=NO,
                      n.cyc=500,cyc=100,bf.cyc=100))
    
    fp3<- try(gamlss(formula_fp, 
                     sigma.formula=~fp(fineAGE,2)+re(random=~1|sex_code),
                     data=train_data, family=NO,
                     n.cyc=500,cyc=100,bf.cyc=100))
  } else{
    poly3<-try(gamlss(formula_poly,
                      sigma.formula=~1,
                      data=train_data, family=NO,
                      n.cyc=500,cyc=100,bf.cyc=100))
    
    fp3<- try(gamlss(formula_fp, 
                     sigma.formula=~1,
                     data=train_data, family=NO,
                     n.cyc=500,cyc=100,bf.cyc=100))
  }

  if((typeof(poly3)=='character')&(typeof(fp3)=='character')) { # see if fit can success
    Gaic<-c(Gaic,NA)
  } else if(typeof(fp3)=='character') {
    Gaic<-c(Gaic,'poly3')
  } else if(typeof(poly3)=='character'){
    Gaic<-c(Gaic,'fp3')
  } else{
    Gaic<-c(Gaic,rownames(GAIC(poly3,fp3,k = 2)[1,])) # select the better one for each feature
    }
}

