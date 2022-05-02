library("gamlss")
library(tidyverse)
library("ggplot2")
library("latex2exp")
library("scales")
library("grid")
# script to generate the final figure 4A
if(!dir.exists('./Output/figures')) dir.create('./Output/figures')

# fig4 A ---------------
rm(list = ls())

pw <- "./Output/1.5T/vol"
X_temp <- read_csv("./Data/processed/1.5T/thick.csv")%>%
  filter(test==0)%>%
  mutate(sex_code = as.factor(dplyr::recode(sex,m=1,f=0)))
test_data <- read_csv("./Data/processed/1.5T/thick.csv")%>%
  filter(test==1)%>%
  mutate(sex_code = as.factor(dplyr::recode(sex,m=1,f=0)))
new_data<-data.frame(
  fineAGE=test_data$fineAGE,
  sex_code=test_data$sex_code)

cent = c(5,25,50,75,95)
i <- 'rh_lateraloccipital_thickness'
formula_mu <- as.formula(paste0(i,'~poly(fineAGE,2)'))
m <- gamlss(formula_mu, data=X_temp, family=NO)
q <- Q.stats(m,xvar=X_temp$fineAGE,table=TRUE)
if(any(q[nrow(q),]<0.05)){
  formula_sig <- as.formula('~poly(fineAGE,2)')
  m2 <- gamlss(formula_mu,
               sigma.formula=formula_sig,
               data=X_temp,family=NO)
  if(GAIC(m2,k=2) < GAIC(m,k=2)){
    m <- m2
  }
}

fname <- m$family[1]
qfun <- paste("q", fname, sep = "")
ct_mat<-NULL
for(ct in cent){
  newcall <- call(qfun, ct/100, 
                  mu = fitted(m, "mu")[order(X_temp$fineAGE)], 
                  sigma = fitted(m, "sigma")[order(X_temp$fineAGE)])
  ll <- eval(newcall)
  ct_mat_c<-data.frame(x=X_temp$fineAGE[order(X_temp$fineAGE)],
                       sex=X_temp$sex_code[order(X_temp$fineAGE)],
                       y=ll,
                       centile=ct)
  ct_mat<-rbind(ct_mat,ct_mat_c)
}

colnames(X_temp)[colnames(X_temp)==i]<-'x'
lan_i<-test_data[test_data$diag==1,c(i,'fineAGE')]
colnames(lan_i)[1]<-"x"
nor_i<-test_data[test_data$diag==0,c(i,'fineAGE')]
colnames(nor_i)[1]<-"x"

p <-  ggplot(X_temp,aes(fineAGE,x))+
  geom_point(color="#999999",size=0.8,alpha=0.8)+
  geom_point(aes(fineAGE,x),
             data=lan_i,color="red",shape=21)+
  geom_point(aes(fineAGE,x),data=nor_i,
             color="black",shape=21)+
  geom_line(data=ct_mat,aes(x,y,group=centile),
            color = '#E69F00')

p<-p+labs(x="Precise Age",
          y= "Thickness (mm)",
            title='Growth curce of cortical thickness \n of R. lateral occipital')+
    theme(plot.title=element_text(hjust=0.5))
p

data_leg <- data.frame(x=c(6.5,6.5),y=c(3.2,3.3))
p+theme_bw()%+replace%
  theme(legend.position = 'none',
        plot.title=element_text(hjust=0.5,vjust=2,size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=1:7,labels=c('1','2','3','4','5','6','7'),limits = c(1,7.2))+
  geom_point(data = data_leg,aes(x,y),color=c("black","red"),shape=21)+
  annotate("text",x = 6.9, y=3.2,label='TDC')+
  annotate("text",x = 6.96, y=3.3,label='DSLD')+
  annotate("text",x = 7.13, y=2.83,label='95', size = 3)+
  annotate("text",x = 7.13, y=2.69,label='75', size = 3)+
  annotate("text",x = 7.13, y=2.58,label='50', size = 3)+
  annotate("text",x = 7.13, y=2.48,label='25', size = 3)+
  annotate("text",x = 7.13, y=2.34,label='5', size = 3)

ggsave("./Output/figures/fig4A.svg",
       width = 6,height = 6)
