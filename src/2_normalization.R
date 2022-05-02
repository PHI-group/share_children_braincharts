rm(list = ls())
library(tidyverse)
if(!dir.exists('./Data/norm'))  dir.create('./Data/norm')

# normalize features, results go to 'Data/norm'
# vol ----------------
## normalize volume against eTIV (which has been replaced with brainseg volume)
vol<-read_csv("./Data/processed/1.5T/vol.csv")
stats<-read_csv("./Data/processed/1.5T/stats.csv")

colnames(vol)
vol_norm<-vol[,6:25]/stats$eTIV
vol_norm<-bind_cols(vol[,1:5],
                    vol_norm*10^3)

write_csv(vol_norm,
          "./Data/norm/vol_norm.csv")

# stats ----------
## normalize against eTIV
colnames(stats)
stats_norm<-stats[,(10:17)]/stats$eTIV
stats_norm<-bind_cols(stats[,1:5],
                      stats_norm,
                      stats[,6:8])
write_csv(stats_norm,
          "./Data/norm/stats_norm.csv")

# thick ---------------------------
## normalize thickness features in each hemisphere relative to the mean thickness of the same hemisphere 
thick<-read_csv("./Data/processed/1.5T/thick.csv")
colnames(thick)

lh_thick_norm <- thick[,6:36]/stats$lh_MeanThickness_thickness
rh_thick_norm <- thick[,37:67]/stats$rh_MeanThickness_thickness
thick_norm <- bind_cols(thick[,1:5],
                        lh_thick_norm,
                        rh_thick_norm)
write_csv(thick_norm,
          "./Data/norm/thick_norm.csv")
