rm(list = ls())
library(tidyverse)
#library("cluster")
library(ggdendro)
library(colorspace)
library(dendextend)
library(grid)
library(scales)
library(hybridHclust)

if(!dir.exists('./Output/1.5T/clustering/')) dir.create('./Output/1.5T/clustering/')
par('mar'=c(1,1,1,1))

# thick ----------------------------
train_data <- read_csv("./Data/processed/1.5T/thick.csv")%>%
  filter(test==0)%>%
  mutate(sex_code = dplyr::recode(sex,m=1,f=0))
roi_predictors <- colnames(train_data)[6:67]
pw<-"./Output/1.5T/thick"
feat <- 'thick'
source("./src/sex_RE/median_fit.R", encoding = 'UTF-8')

thick_med <- med_data
initial_thick_med <- thick_med[rep(1,nrow(thick_med)), ]
standard_thick_med <- thick_med/initial_thick_med
med_age <- thick_med$fineAGE
standard_thick_med <- standard_thick_med[,-1]

X <- t(standard_thick_med)
thick_hh <- hybridHclust(X)  # clustering
ggplot(thick_hh$height %>%
         as_tibble() %>%
         add_column(groups = length(thick_hh$height):1) %>%
         rename(height=value),
       aes(x=groups, y=height)) +
  geom_point() +
  geom_line()
ggsave("./Output/1.5T/clustering/thick_scree.svg")

plot(thick_hh)
thick_cut<-rect.hclust(thick_hh,k=4)

rownames(X) <- roi_predictors
thick_cls<-list(rownames(X)[thick_cut[[1]]],
               rownames(X)[thick_cut[[2]]],
               rownames(X)[thick_cut[[3]]],
               rownames(X)[thick_cut[[4]]])
thick_1<-data.frame(x=med_age,
                   y=rowMeans(standard_thick_med[,thick_cls[[1]]])-1)
thick_2<-data.frame(x=med_age,
                   y=rowMeans(standard_thick_med[,thick_cls[[2]]])-1)
thick_3<-data.frame(x=med_age,
                   y=rowMeans(standard_thick_med[,thick_cls[[3]]])-1)
thick_4<-data.frame(x=med_age,
                    y=rowMeans(standard_thick_med[,thick_cls[[4]]])-1)

thick_1_med<-
  ggplot(thick_1,aes(x,y))+
  geom_line(size=1,color=rainbow_hcl(4)[1])+
  labs(x="Age",y="Change relative to Age 1",
       title="Cluster 1")+
    scale_y_continuous(labels = scales::percent)+
  theme_light()  %+replace% theme( 
    axis.title = element_text(size=30),
    axis.text = element_text(size=25),
    panel.border = element_rect(colour =rainbow_hcl(4)[1],
                                size=2,
                                fill='transparent' ),
    panel.grid = element_line(colour = "grey74"),
    plot.title=element_text(colour = rainbow_hcl(4)[1],
                            size = 40,
                            hjust=0.5,vjust=2))

thick_2_med<-
  ggplot(thick_2,aes(x,y))+
  geom_line(size=1,color=rainbow_hcl(4)[2])+
  labs(x="Age",y="Change relative to Age 1",
       title = "Cluster 2")+
  scale_y_continuous(labels = scales::percent)+
  theme_light()  %+replace%  theme( 
    axis.title = element_text(size=30),
    axis.text = element_text(size=25),
    panel.border = element_rect(colour =rainbow_hcl(4)[2],
                                size=2,
                                fill='transparent' ),
    panel.grid = element_line(colour = "grey74"),
    plot.title=element_text(colour = rainbow_hcl(4)[2],
                            size = 40,
                            hjust=0.5,vjust=2))

thick_3_med<-
ggplot(thick_3,aes(x,y))+
  geom_line(size=1,color=rainbow_hcl(4)[3])+
  labs(x="Age",y="Change relative to Age 1",
       title = "Cluster 3")+
  scale_y_continuous(labels = scales::percent)+
  theme_light()  %+replace%  theme( 
    axis.title = element_text(size=30),
    axis.text = element_text(size=25),
    panel.border = element_rect(colour =rainbow_hcl(4)[3],
                                size=2,
                                fill='transparent' ),
    panel.grid = element_line(colour = "grey74"),
    plot.title=element_text(colour = rainbow_hcl(4)[3],
                            size = 40,
                            hjust=0.5,vjust=2))


thick_4_med<-
  ggplot(thick_4,aes(x,y))+
  geom_line(size=1,color=rainbow_hcl(4)[4])+
  labs(x="Age",y="Change relative to Age 1",
       title = "Cluster 4")+
  scale_y_continuous(labels = scales::percent)+
  theme_light()  %+replace%  theme( 
    axis.title = element_text(size=30),
    axis.text = element_text(size=25),
    panel.border = element_rect(colour =rainbow_hcl(4)[4],
                                size=2,
                                fill='transparent' ),
    panel.grid = element_line(colour = "grey74"),
    plot.title=element_text(colour = rainbow_hcl(4)[4],
                            size = 40,
                            hjust=0.5,vjust=2))

source("./src/sex_RE/multiplot.R")
svg("./Output/1.5T/clustering/thick_med.svg",
    width = 20,
    height = 20)
multiplot(thick_1_med,thick_3_med,
          thick_2_med, thick_4_med,cols = 2)
dev.off()

thick_clust.gr<-data.frame(
  num=unlist(thick_cut),
  clust=rep(c("1","2","3","4"),
            times=sapply(thick_cut,length)))
thick_clust.gr<-thick_clust.gr[
  order(thick_clust.gr$num),]

thick_hh$labels<-rownames(X)
thick_hh$leaf_labels<-thick_clust.gr$clust
thick_dend <- as.dendrogram(thick_hh)

# Color the branches based on the clusters:
thick_dend <- color_branches(thick_dend, k=4)

# Manually match the labels, as much as possible, to the real classification of the flowers:
labels_colors(thick_dend) <-"black"
#rainbow_hcl(3)[sort_levels_values(
#as.numeric(th_clust.gr$clust)[order.dendrogram(th_dend)])]

# We hang the dendrogram a bit:
thick_dend <- hang.dendrogram(thick_dend,hang=-1)#0.001)

# reduce the size of the labels:
thick_dend <- set(thick_dend, "labels_cex", 0.5)

thick_ggd1 <- as.ggdend(thick_dend)

logr_trans<-trans_new(name="logr_trans",
                      function(u) sign(u)*abs(u)^(1/3),function(w) w^3)

ggplot(thick_ggd1)+
  geom_hline(yintercept=1,linetype=2)+
  scale_y_continuous(trans=logr_trans,
                     limits=c(-10,10))+
  xlab(NULL)+theme_light()%+replace% theme(     
    axis.text = element_blank(),
    axis.title = element_blank())
ggsave("./Output/1.5T/clustering/thick_cls.svg",
       width = 10, height = 5)

# normalize thick ----------------------------
train_data <- read_csv("./Data/norm/thick_norm.csv")%>%
  filter(test==0)%>%
  mutate(sex_code = dplyr::recode(sex,m=1,f=0))
roi_predictors <- colnames(train_data)[6:67]
pw<-"./Output/1.5T/thick_norm"
feat <- 'thick_norm'
source("./src/sex_RE/median_fit.R", encoding = 'UTF-8')

thick_med<-med_data
initial_thick_med<-thick_med[rep(1,nrow(thick_med)), ]
standard_thick_med<-thick_med/initial_thick_med
med_age<-thick_med$fineAGE
standard_thick_med<-standard_thick_med[,-1]

X<-t(standard_thick_med)
thick_hh<-hybridHclust(X)
ggplot(thick_hh$height %>%
         as_tibble() %>%
         add_column(groups = length(thick_hh$height):1) %>%
         rename(height=value),
       aes(x=groups, y=height)) +
  geom_point() +
  geom_line()
ggsave("./Output/1.5T/clustering/thick_norm_scree.svg")

plot(thick_hh)
thick_cut<-rect.hclust(thick_hh,k=4)

rownames(X) <- roi_predictors
thick_cls<-list(rownames(X)[thick_cut[[1]]],
                rownames(X)[thick_cut[[2]]],
                rownames(X)[thick_cut[[3]]],
                rownames(X)[thick_cut[[4]]])
thick_1<-data.frame(x=med_age,
                    y=rowMeans(standard_thick_med[,thick_cls[[1]]])-1)
thick_2<-data.frame(x=med_age,
                    y=rowMeans(standard_thick_med[,thick_cls[[2]]])-1)
thick_3<-data.frame(x=med_age,
                    y=rowMeans(standard_thick_med[,thick_cls[[3]]])-1)
thick_4<-data.frame(x=med_age,
                    y=rowMeans(standard_thick_med[,thick_cls[[4]]])-1)

thick_1_med<-
  ggplot(thick_1,aes(x,y))+
  geom_line(size=1,color=rainbow_hcl(4)[1])+
  labs(x="Age",y="Change relative to Age 1",
       title="Cluster 1")+
  scale_y_continuous(labels = scales::percent)+
  theme_light()  %+replace% theme( 
    axis.title = element_text(size=30),
    axis.text = element_text(size=25),
    panel.border = element_rect(colour =rainbow_hcl(4)[1],
                                size=2,
                                fill='transparent' ),
    panel.grid = element_line(colour = "grey74"),
    plot.title=element_text(colour = rainbow_hcl(4)[1],
                            size = 40,
                            hjust=0.5,vjust=2))

thick_2_med<-
  ggplot(thick_2,aes(x,y))+
  geom_line(size=1,color=rainbow_hcl(4)[2])+
  labs(x="Age",y="Change relative to Age 1",
       title = "Cluster 2")+
  scale_y_continuous(labels = scales::percent)+
  theme_light()  %+replace%  theme( 
    axis.title = element_text(size=30),
    axis.text = element_text(size=25),
    panel.border = element_rect(colour =rainbow_hcl(4)[2],
                                size=2,
                                fill='transparent' ),
    panel.grid = element_line(colour = "grey74"),
    plot.title=element_text(colour = rainbow_hcl(4)[2],
                            size = 40,
                            hjust=0.5,vjust=2))

thick_3_med<-
  ggplot(thick_3,aes(x,y))+
  geom_line(size=1,color=rainbow_hcl(4)[3])+
  labs(x="Age",y="Change relative to Age 1",
       title = "Cluster 3")+
  scale_y_continuous(labels = scales::percent)+
  theme_light()  %+replace%  theme( 
    axis.title = element_text(size=30),
    axis.text = element_text(size=25),
    panel.border = element_rect(colour =rainbow_hcl(4)[3],
                                size=2,
                                fill='transparent' ),
    panel.grid = element_line(colour = "grey74"),
    plot.title=element_text(colour = rainbow_hcl(4)[3],
                            size = 40,
                            hjust=0.5,vjust=2))


thick_4_med<-
  ggplot(thick_4,aes(x,y))+
  geom_line(size=1,color=rainbow_hcl(4)[4])+
  labs(x="Age",y="Change relative to Age 1",
       title = "Cluster 4")+
  scale_y_continuous(labels = scales::percent)+
  theme_light()  %+replace%  theme( 
    axis.title = element_text(size=30),
    axis.text = element_text(size=25),
    panel.border = element_rect(colour =rainbow_hcl(4)[4],
                                size=2,
                                fill='transparent' ),
    panel.grid = element_line(colour = "grey74"),
    plot.title=element_text(colour = rainbow_hcl(4)[4],
                            size = 40,
                            hjust=0.5,vjust=2))

source("./src/sex_RE/multiplot.R")
svg("./Output/1.5T/clustering/thick_norm_med.svg",
    width = 20,
    height = 20)
multiplot(thick_1_med,thick_3_med,
          thick_2_med, thick_4_med,cols = 2)
dev.off()

thick_clust.gr<-data.frame(
  num=unlist(thick_cut),
  clust=rep(c("1","2","3","4"),
            times=sapply(thick_cut,length)))
thick_clust.gr<-thick_clust.gr[
  order(thick_clust.gr$num),]

thick_hh$labels<-rownames(X)
thick_hh$leaf_labels<-thick_clust.gr$clust
thick_dend <- as.dendrogram(thick_hh)

# Color the branches based on the clusters:
thick_dend <- color_branches(thick_dend, k=4)

# Manually match the labels, as much as possible, to the real classification of the flowers:
labels_colors(thick_dend) <-"black"
#rainbow_hcl(3)[sort_levels_values(
#as.numeric(th_clust.gr$clust)[order.dendrogram(th_dend)])]

# We hang the dendrogram a bit:
thick_dend <- hang.dendrogram(thick_dend,hang=-1)#0.001)

# reduce the size of the labels:
thick_dend <- set(thick_dend, "labels_cex", 0.5)

thick_ggd1 <- as.ggdend(thick_dend)

logr_trans<-trans_new(name="logr_trans",
                      function(u) sign(u)*abs(u)^(1/3),function(w) w^3)

ggplot(thick_ggd1)+
  geom_hline(yintercept=1,linetype=2)+
  scale_y_continuous(trans=logr_trans,
                     limits=c(-10,10))+
  xlab(NULL)+theme_light()%+replace% theme(     
    axis.text = element_blank(),
    axis.title = element_blank())
ggsave("./Output/1.5T/clustering/thick_norm_cls.svg",
       width = 10, height = 5)

# vol ------------------------
train_data <- read_csv("./Data/norm/vol_norm.csv")%>%
  filter(test==0)%>%
  mutate(sex_code = dplyr::recode(sex,m=1,f=0))
roi_predictors<-colnames(train_data)[6:25]

pw<-"./Output/1.5T/vol"
feat <- 'vol'
source("./src/sex_RE/median_fit.R", encoding = 'UTF-8')

vol_med <- med_data
initial_vol_med <- vol_med[rep(1,nrow(vol_med)), ]
standard_vol_med <- vol_med/initial_vol_med
med_age <- vol_med$fineAGE
standard_vol_med<-standard_vol_med[,-1]

X<-t(standard_vol_med)
vol_hh<-hybridHclust(X)
ggplot(vol_hh$height %>%
         as_tibble() %>%
         add_column(groups = length(vol_hh$height):1) %>%
         rename(height=value),
       aes(x=groups, y=height)) +
  geom_point() +
  geom_line()
ggsave("./Output/1.5T/clustering/vol_scree.svg")

plot(vol_hh)
vol_cut<-rect.hclust(vol_hh,k=3)

rownames(X) <- roi_predictors
vol_cls<-list(rownames(X)[vol_cut[[1]]],
              rownames(X)[vol_cut[[2]]],
              rownames(X)[vol_cut[[3]]])#,
              #rownames(X)[vol_cut[[4]]])
vol_1<-data.frame(x=med_age,
                  y=rowMeans(standard_vol_med[,vol_cls[[1]]])-1)
vol_2<-data.frame(x=med_age,
                  y=rowMeans(standard_vol_med[,vol_cls[[2]]])-1)
vol_3<-data.frame(x=med_age,
                  y=rowMeans(standard_vol_med[,vol_cls[[3]]])-1)
#vol_4<-data.frame(x=med_age,
 #                 y=rowMeans(standard_vol_med[,vol_cls[[4]]])-1)

vol_1_med <-
  ggplot(vol_1,aes(x,y))+
  geom_line(size=1,color=rainbow_hcl(4)[1])+
  labs(x="Age",y="Change relative to Age 1",
       title="Cluster 1")+
    scale_y_continuous(labels = scales::percent)+
  theme_light()  %+replace% theme( 
    axis.title = element_text(size=30),
    axis.text = element_text(size=25),
    panel.border = element_rect(colour =rainbow_hcl(4)[1],
                                size=2,
                                fill='transparent' ),
    panel.grid = element_line(colour = "grey74"),
    plot.title=element_text(colour = rainbow_hcl(4)[1],
                            size = 40,
                            hjust=0.5,vjust=2))

vol_2_med <-
  ggplot(vol_2,aes(x,y))+
  geom_line(size=1,color=rainbow_hcl(4)[2])+
  labs(x="Age",y="Change relative to Age 1",
       title = "Cluster 2")+
    scale_y_continuous(labels = scales::percent)+
  theme_light()  %+replace%  theme( 
    axis.title = element_text(size=30),
    axis.text = element_text(size=25),
    panel.border = element_rect(colour =rainbow_hcl(4)[2],
                                size=2,
                                fill='transparent' ),
    panel.grid = element_line(colour = "grey74"),
    plot.title=element_text(colour = rainbow_hcl(4)[2],
                            size = 40,
                            hjust=0.5,vjust=2))

vol_3_med <-
  ggplot(vol_3,aes(x,y))+
  geom_line(size=1,color=rainbow_hcl(4)[3])+
  labs(x="Age",y="Change relative to Age 1",
       title = "Cluster 3")+
    scale_y_continuous(labels = scales::percent)+
  theme_light()  %+replace%  theme( 
    axis.title = element_text(size=30),
    axis.text = element_text(size=25),
    panel.border = element_rect(colour =rainbow_hcl(4)[3],
                                size=2,
                                fill='transparent' ),
    panel.grid = element_line(colour = "grey74"),
    plot.title=element_text(colour = rainbow_hcl(4)[3],
                            size = 40,
                            hjust=0.5,vjust=2))

#vol_4_med <- ggplot(vol_4,aes(x,y))+
  #geom_line(size=1,color=rainbow_hcl(4)[4])+
  #labs(x="Age",y="Change relative to Age 1",
   #    title = "Cluster 4")+
    #scale_y_continuous(labels = scales::percent)+
  #theme_light()  %+replace%  theme(  axis.title = element_text(size=30),
    #axis.text = element_text(size=25),    panel.border = element_rect(colour =rainbow_hcl(4)[4],
                          #size=2,                                fill='transparent' ),
    #panel.grid = element_line(colour = "grey74"),
    #plot.title=element_text(colour = rainbow_hcl(4)[4],
     #                       size = 40,    hjust=0.5,vjust=2))

source("./src/sex_RE/multiplot.R")
svg("./Output/1.5T/clustering/vol_med.svg",width = 30,
    height = 10)
multiplot(vol_1_med,vol_2_med,
          vol_3_med, cols = 3)
dev.off()

vol_clust.gr<-data.frame(
  num=unlist(vol_cut),
  clust=rep(c("1","2","3"),
            times=sapply(vol_cut,length)))
vol_clust.gr<-vol_clust.gr[
  order(vol_clust.gr$num),]

vol_hh$labels<-rownames(X)
vol_hh$leaf_labels<-vol_clust.gr$clust
vol_dend <- as.dendrogram(vol_hh)

# Color the branches based on the clusters:
vol_dend <- color_branches(vol_dend, k=3)


# Manually match the labels, as much as possible, to the real classification of the flowers:
labels_colors(vol_dend) <-"black"
#rainbow_hcl(3)[sort_levels_values(
#as.numeric(th_clust.gr$clust)[order.dendrogram(th_dend)])]

# We hang the dendrogram a bit:
vol_dend <- hang.dendrogram(vol_dend,hang=-1)#0.001)

# reduce the size of the labels:
vol_dend <- set(vol_dend, "labels_cex", 0.5)

vol_ggd1 <- as.ggdend(vol_dend)

logr_trans<-trans_new(name="logr_trans",
                      function(u) sign(u)*abs(u)^(1/3),function(w) w^3)


ggplot(vol_ggd1)+
  geom_hline(yintercept=2.5,linetype=2)+
  scale_y_continuous(trans=logr_trans,
                     limits=c(-10,20))+
  xlab(NULL)+theme_light()%+replace% theme(     
    axis.text = element_blank(),
    axis.title = element_blank())
ggsave("./Output/1.5T/clustering/vol_cls.svg",
       width = 10, height = 5)
