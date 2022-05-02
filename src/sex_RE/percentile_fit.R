library(gamlss)
con <- gamlss.control(n.cyc=500,c.crit=0.001)
i.con <- glim.control(cc=0.001,cyc=100,bf.cyc=100,bf.tol = 0.001,
                      glm.trace = T, bf.trace = T)
if(i %in% roi_sex){
  formula_mu <- as.formula(paste0(i,'~poly(fineAGE,2)+re(random=~1|sex_code)'))
  m <- gamlss(formula_mu, data=X_temp, 
                   family=NO,control=con,i.control = i.con)
  q <- Q.stats(m,xvar=X_temp$fineAGE,table=TRUE)
  if(any(q[nrow(q),2:3]<0.05)){
    formula_sig <- as.formula('~poly(fineAGE,2)+re(random=~1|sex_code)')
    m2 <- gamlss(formula_mu,
                 sigma.formula=formula_sig,
                 data=X_temp,family=NO,
                 control=con,i.control = i.con)
    if(GAIC(m2,k=K) < GAIC(m,k=K)){
      m <- m2
    }
  }
}else{
  formula_mu <- as.formula(paste0(i,'~poly(fineAGE,2)'))
  m <- gamlss(formula_mu, data=X_temp, 
                   family=NO,control=con,i.control = i.con)
  q <- Q.stats(m,xvar=X_temp$fineAGE,table=TRUE)
  if(any(q[nrow(q),2:3]<0.05)){
    formula_sig <- as.formula('~poly(fineAGE,2)')
    m2 <- gamlss(formula_mu,
                 sigma.formula=formula_sig,
                 data=X_temp,family=NO,
                 control=con,i.control = i.con)
    if(GAIC(m2,k=K) < GAIC(m,k=K)){
      m <- m2
    }
  }
}
