rm(list = ls())
library(tidyverse)
library(gamlss)
library(latex2exp)

# thick ----------------
pw <- './Output/1.5T/thick'
if(!dir.exists(paste0(pw,"/plots"))) dir.create(paste0(pw,"/plots"))

train_data <- read_csv("./Data/processed/1.5T/thick.csv")%>%
  filter(test==0)%>%
  mutate(sex_code = as.factor(dplyr::recode(sex,m=1,f=0)))
roi_predictors <- colnames(train_data)[6:67]

thick_sex <- read_csv('./Output/1.5T/Anova/anova_thick.csv')
roi_sex <- thick_sex %>% dplyr::filter(Psex<0.1) %>%
  pull(rownames)
y_lab <- "Thickness (mm)"

source('./src/sex_RE/plot.R')

# thick norm -----------
rm(list = ls())
pw <- './Output/1.5T/thick_norm'
if(!dir.exists(paste0(pw,"/plots"))) dir.create(paste0(pw,"/plots"))

train_data <- read_csv("./Data/norm/thick_norm.csv")%>%
  dplyr::filter(test==0)%>%
  mutate(sex_code = as.factor(dplyr::recode(sex,m=1,f=0)))
roi_predictors <- colnames(train_data)[6:67]

thick_sex <- read_csv('./Output/1.5T/Anova/anova_thick_norm.csv')
roi_sex <- thick_sex %>% dplyr::filter(Psex<0.1) %>%
  pull(rownames)
y_lab <- "Thickness / Mean Thickness"

source('./src/sex_RE/plot.R')

# stats -----------
rm(list = ls())
pw <- './Output/1.5T/stats'
if(!dir.exists(paste0(pw,"/plots"))) dir.create(paste0(pw,"/plots"))

train_data <-  read_csv("./Data/norm/stats_norm.csv")%>%
  dplyr::filter(test==0)%>%
  mutate(sex_code = as.factor(dplyr::recode(sex,m=1,f=0)))

stats_sex <- read_csv('./Output/1.5T/Anova/anova_stats.csv')
roi_sex <- stats_sex %>% dplyr::filter(Psex<0.1) %>%
  pull(rownames)

colnames(train_data)
roi_predictors <- colnames(train_data)[6:13]
y_lab <- "Volume / TIV"
source('./src/sex_RE/plot.R')

roi_predictors <- colnames(train_data)[14:16]
y_lab <- "Thickness (mm)"
source('./src/sex_RE/plot.R')

# vol ---------------
rm(list = ls())
pw <- './Output/1.5T/vol'
if(!dir.exists(paste0(pw,"/plots"))) dir.create(paste0(pw,"/plots"))

train_data <-  read_csv("./Data/norm/vol_norm.csv")%>%
  dplyr::filter(test==0)%>%
  mutate(sex_code = as.factor(dplyr::recode(sex,m=1,f=0)))
roi_predictors <- colnames(train_data)[6:25]

vol_sex <- read_csv('./Output/1.5T/Anova/anova_vol.csv')
roi_sex <- vol_sex %>% dplyr::filter(Psex<0.1) %>%
  pull(rownames)
y_lab <- TeX(sprintf("Volume / TIV $\\times 10^%d$",3))

source('./src/sex_RE/plot.R')
