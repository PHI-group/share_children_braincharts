rm(list = ls())
library(tidyverse)

# 1.5T ---------------------
demog <- read_csv("./Data/processed/1.5T/demog.csv")

## thick ---------------------
thick_hc_l<-read_table("./Data/raw/1.5T/lh.DKT.thickness.txt")
thick_hc_r<-read_table("./Data/raw/1.5T/rh.DKT.thickness.txt")
colnames(thick_hc_r)
thick_hc_r$rh_temporalpole_thickness
thick_hc_r<-thick_hc_r%>%
  dplyr::select(-'rh_temporalpole_thickness')
colnames(thick_hc_l)[1]<-
  colnames(thick_hc_r)[1]<-'subjID'
thick_hc<-merge(thick_hc_l,thick_hc_r)
colnames(thick_hc)

thick_p_l<-read_table("./Data/raw/1.5T/lh.DKT.thickness.roi.txt")
thick_p_r<-read_table("./Data/raw/1.5T/rh.DKT.thickness.roi.txt")
thick_p_r$rh_temporalpole_thickness
thick_p_r<-thick_p_r%>%
  dplyr::select(-'rh_temporalpole_thickness')
colnames(thick_p_l)[1]<-
  colnames(thick_p_r)[1]<-'subjID'
thick_p<-merge(thick_p_l,thick_p_r)
colnames(thick_p)

thick <- rbind(thick_hc,thick_p)
thick <- merge(demog,thick)

thick_feat <- thick %>%
  dplyr::select(-c('BrainSegVolNotVent','eTIV',
            'lh_MeanThickness_thickness',
            'rh_MeanThickness_thickness'))

write_csv(thick_feat,"./Data/processed/1.5T/thick.csv")

##  vol ------------------
vol_hc<-read.table("./Data/raw/1.5T/aseg_stats.txt",header = T)
colnames(vol_hc)
colnames(vol_hc)[1]<-'subjID'

vol_p<-read.table("./Data/raw/1.5T/aseg_stats.roi.txt",header = T)
colnames(vol_p)[1]<-'subjID'
colnames(vol_p)

vol <- rbind(vol_hc,vol_p)
vol <- merge(demog,vol)

vol_feat<- vol %>%
  dplyr::select(subjID,sex:test,
         Left.Cerebellum.White.Matter:Left.Pallidum,
         Left.Hippocampus,Left.Amygdala,Left.Accumbens.area,
         Left.VentralDC,
         Right.Cerebellum.White.Matter:Right.VentralDC)

write_csv(vol_feat,"./Data/processed/1.5T/vol.csv")

##  stats ---------------------
thick_stats <- thick%>%
  dplyr::select(subjID,sex:test,
                lh_MeanThickness_thickness,
                rh_MeanThickness_thickness)%>%
  mutate(MeanThickness = (lh_MeanThickness_thickness+
           rh_MeanThickness_thickness)/2)

vol_stats <- vol %>%
  dplyr::select(subjID,sex:test,
         BrainSegVol,
         lhCortexVol:TotalGrayVol)%>%
  rename(eTIV = BrainSegVol)

stats <- merge(thick_stats,vol_stats)
colnames(stats)

write_csv(stats,"./Data/processed/1.5T/stats.csv")

# 3T -----------------
rm(list = ls())
library(tidyverse)
library(readxl)
if(!dir.exists("./Data/processed/3T")) dir.create("./Data/processed/3T")

HC_demog <- read_csv("./Data/raw/3T/final_57HC_clinic_2020-05-03.csv",
           locale=locale(encoding='UTF-8'))
colnames(HC_demog)

HC_demog <- HC_demog%>%
  mutate(sex=dplyr::recode(sex,'male'='m',
                             'female'='f'))%>%
  dplyr::select(mrID,sex,ageInInfo)%>%
  rename(subjID=mrID,fineAGE=ageInInfo)%>%
  add_column(diag=0,test=1)

dsld_demog <- read_excel('./Data/raw/3T/combo_info_languagedelay_2020-05-08.xlsx')
colnames(dsld_demog)
dsld_demog <- dsld_demog%>%
  mutate(sex=dplyr::recode(sex,'male'='m',
                             'female'='f'))%>%
  dplyr::select(mrID,sex,ageInInfo)%>%
  rename(subjID=mrID,fineAGE=ageInInfo)%>%
  add_column(diag=1,test=1)

demog <- rbind(HC_demog,dsld_demog)
write_csv(demog,"./Data/processed/3T/demog.csv")

## thick -----------
thick_l<-read_table("./Data/raw/3T/hc/lh.DKT.thickness.txt")
thick_r<-read_table("./Data/raw/3T/hc/rh.DKT.thickness.txt")
colnames(thick_r)
thick_r$rh_temporalpole_thickness
thick_r<-thick_r%>%
  dplyr::select(-'rh_temporalpole_thickness')
colnames(thick_l)[1]<-
  colnames(thick_r)[1]<-'subjID'
thick_HC <- merge(thick_l,thick_r)

thick_p_l<-read_table("./Data/raw/3T/dsld/lh.DKT.thickness.txt")
thick_p_r<-read_table("./Data/raw/3T/dsld/rh.DKT.thickness.txt")
colnames(thick_p_r)
colnames(thick_p_l)[1]<-
  colnames(thick_p_r)[1]<-'subjID'
thick_p <- merge(thick_p_l,thick_p_r)
colnames(thick_p)

thick <- rbind(thick_HC,thick_p)
thick <- merge(demog,thick)

thick_feat <- thick %>%
  dplyr::select(-c('BrainSegVolNotVent','eTIV',
                   'lh_MeanThickness_thickness',
                   'rh_MeanThickness_thickness'))

write_csv(thick_feat,"./Data/processed/3T/thick.csv")

## vol ----------
vol_hc <- read.table("./Data/raw/3T/hc/aseg_stats.txt",header = T)
colnames(vol_hc)
vol_p <- read.table("./Data/raw/3T/dsld/aseg_stats.txt",header = T)
colnames(vol_p)

colnames(vol_hc)[1] <- colnames(vol_p)[1] <- 'subjID'

vol <- rbind(vol_hc,vol_p)
vol <- merge(demog, vol)
colnames(vol)

vol_feat<- vol %>%
  dplyr::select(subjID,sex:test,
         Left.Cerebellum.White.Matter:Left.Pallidum,
         Left.Hippocampus,Left.Amygdala,Left.Accumbens.area,
         Left.VentralDC,
         Right.Cerebellum.White.Matter:Right.VentralDC)

write_csv(vol_feat,"./Data/processed/3T/vol.csv")

## stats ----------
thick_stats <- thick%>%
  dplyr::select(subjID,sex:test,
                lh_MeanThickness_thickness,
                rh_MeanThickness_thickness)

vol_stats <- vol %>%
  dplyr::select(subjID,sex:test,
         BrainSegVol,
         lhCortexVol:TotalGrayVol)%>%
  rename(eTIV = BrainSegVol)

stats <- merge(thick_stats,vol_stats)
colnames(stats)

write_csv(stats,"./Data/processed/3T/stats.csv")
