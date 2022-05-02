library(scales)
library(latex2exp)
library(grid)

par('mar' = c(1,1,1,1))
sci_10<-function(x)  as.character(x/(10^y_scl))

med <- read_csv(paste0(pw,'/quantiles.csv'))%>%dplyr::filter(ct==50)
ct_mat <- read_csv(paste0(pw,'/quantiles.csv'))%>%
  arrange(by=fineAGE)

for(roi in roi_predictors){
  X_temp <- train_data
  ct_temp <- ct_mat
  
  y_scl<-as.numeric(strsplit(scientific(range(X_temp[,roi])[1]),
                             "e")[[1]][2])
  scl<-TeX(sprintf("$\\times 10^{%d}$",y_scl))
  
  med_boot <- read_csv(paste0(pw,'/boot/',roi,'.csv'))[,-1]
  if(any(is.na(med_boot))) {
    na_ind <- apply(med_boot,2,function(u) any(is.na(u)))
    med_boot <- med_boot[,!na_ind]
  }
  
  boot_ci <- apply(med_boot,1,function(u) quantile(u,probs=c(0.025,0.975)))
  df_boot_ci <- data.frame(fineAGE=med$fineAGE,sex=med$sex,x=pull(med,roi),
                           Cl=boot_ci[2,],Cu=boot_ci[1,])%>%
    mutate(low=2*x-Cu,high=2*x-Cl)%>%
    arrange(by=fineAGE)%>%
    distinct()
  
  colnames(ct_temp)[colnames(ct_temp)==roi] <- 'x'
  colnames(X_temp)[colnames(X_temp)==roi] <- 'x'
  X_m <- X_temp%>%dplyr::filter(sex_code==1)
  X_f <- X_temp%>%dplyr::filter(sex_code==0)
  
  if(roi %in% roi_sex){
    df_boot_ci_m <- df_boot_ci%>%dplyr::filter(sex==1)
    df_boot_ci_f <- df_boot_ci%>%dplyr::filter(sex==0)
    ct_temp_m <- ct_temp%>%dplyr::filter(sex==1)
    ct_temp_f <- ct_temp%>%dplyr::filter(sex==0)
    
    p <- ggplot(X_m,aes(x=fineAGE))+
      geom_point(aes(y=x,color='boy'),
                 size=0.8,
                 alpha=0.8)+
      geom_point(aes(y=x,color='girl'),
                 size=0.8,
                 alpha=0.8,
                 data=X_f)+
      geom_ribbon(data=df_boot_ci_m,
                  aes(x=fineAGE,ymin=low,ymax=high,
                      fill='boy'),
                  alpha=0.3)+
      geom_ribbon(data=df_boot_ci_f,
                  aes(x=fineAGE,ymin=low,ymax=high,
                      fill='girl'),
                  alpha=0.3)+
      geom_line(data=ct_temp_m,
                aes(y=x,group=ct,
                    color='boy'))+
      geom_line(data=ct_temp_f,
                aes(y=x,group=ct,
                    color='girl'))+
      guides(color = guide_legend(title='Gender'),
             fill = guide_legend(title='Gender'))
    
  }else{
    p <- ggplot(X_m,aes(x=fineAGE))+
      geom_point(aes(y=x,color='boy'),
                 size=0.8,
                 alpha=0.8)+
      geom_point(aes(y=x,color='girl'),
                 size=0.8,
                 alpha=0.8,
                 data=X_f)+
      geom_ribbon(data=df_boot_ci,
                  aes(x=fineAGE,ymin=low,ymax=high),
                  fill='yellow',
                  alpha=0.3)+
      geom_line(data=ct_temp,
                aes(y=x,group=ct),
                color = '#E69F00')+
      guides(color = guide_legend(title='Gender'))
  }
  
  p<-p+labs(x="Precise Age",
            y=y_lab,
            title=roi)+
    theme(plot.title=element_text(hjust=0.5))+
    scale_y_continuous(labels=sci_10)
  
  svg(filename=paste0(pw,"/plots/",roi,".svg"))  
  grid.newpage()  ##新建页面
  pushViewport(viewport(layout = grid.layout(1,1)))
  vplayout <- function(x,y){
    viewport(layout.pos.row = x, layout.pos.col = y)
  }
  print(p, vp = vplayout(1,1))   ###将（1,1)和(1,2)的位置画图c
  if(y_scl!=0) grid.text(scl, x = unit(0.08, "npc"), 
                         y = unit(0.97, "npc"), 
                         gp=gpar(col="black",fontsize=9))
  dev.off()
}
