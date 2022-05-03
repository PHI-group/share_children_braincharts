# Children brain growth charts: data, code, and results

Last update: 2022-5-3

This repository shares the data, code, and results for computing the children brain growth curves.

### Data

- Raw:  extracted from image processing pipeline
- Processed: cleaned up for statistical modelling
- Norm: normalized regional feature: volume/TBV, thickness/mean_thickness

## src

All scripts for statistical modelling. Most of them are R scripts, with some exceptions being R_notebook. 

The scripts in the 'sex_RE' folder are for the GAMLSS models that fit growth curves for each brain image feature. Those outside the 'sex_RE' folder preprocess the data for GAMLSS modelling.

The names of the scripts are self-explainary. The technical details are described in the reference paper.

### Output

The output folder present results of the statistical analyses:

- Anova: results of two-way anova (age by sex) of the image features
- Stats, thick, vol, thick_norm: GAMLSS models (.rds) and growth curve plots (.svg) for the corresponding brain image features. 
- Clustering: results of the hierarchical clustering analysis of the brain features based on their age-dependence.
- Clf: results of classification analysis to recognize children with DSLD, based on the 'developmental scores' of multiple brain regions.
-  Dlsd_risk: results of the DLSD risk ratio analysis based on the 'developmental scores' of multiple brain regions.

### Reference

Please cite this paper for using these materials:

Hongxi Zhang, Jia Li, Xiaoli Su, Yang Hu, Tianmei Liu, Shaoqing Ni, Haifeng Li, Xi-Nian Zuo, Junfen Fu, Ti-Fei Yuan, Zhi Yang, Growth charts of brain morphometry for preschool children,NeuroImage,2022,119178, https://doi.org/10.1016/j.neuroimage.2022.119178.

