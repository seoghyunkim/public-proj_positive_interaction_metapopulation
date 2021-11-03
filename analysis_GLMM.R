#' ---
#' title: GLMM analysis
#' author: S. Kim, C. L. Dolph, and A. Terui
#' output:
#'   html_document:
#'     theme: paper
#'     toc: true
#'     toc_float: true
#' ---

#' # Session information
sessionInfo()

#' # Load library
# Load library
rm(list=ls(all.names=T)) # clear all data

library(tidyverse)
library(glmmTMB)
library(gridExtra)
library(ggpubr)
library(effects)
library(MuMIn)
library(DHARMa)

#'# Read data 

#' Fish data
fish_dat <- read_csv("./data_outcome/pos_meta_dat.csv")
fish_dat$X1 <- NULL

#' Filter southern redbelly dace distribution (retain 71 watersheds)
#'- This is to remove HUC4 sub-basin where SRD are not distributed.
SRD_dat  <- fish_dat  %>%
  filter(HUC4 %in% c("HUC4_12","HUC4_33","HUC4_35","HUC4_36","HUC4_39","HUC4_42","HUC4_43","HUC4_44","HUC4_45"))

#'- Common shiner data (110 watersheds)
CMS_dat  <- fish_dat


##### GLMM using package glmmTMB #####  
#'- Exponential correlation methods to account for spatial correlation


#'## Common Shiner Model
#'- Exponential correlation function was included to account for spatial autocorrelation

#'- Scale coordinates and create a dummy group factor to be used as a random term
CMS_dat$pos <- numFactor(scale(CMS_dat$Lat, center = FALSE), scale(CMS_dat$Lon, center = FALSE))
CMS_dat$ID <- factor(rep(1, nrow(CMS_dat)))

#'- Model
CMS_tmb_exp <- glmmTMB(CMS_prop ~ scale(HHC_prop) + scale(CRC_prop) + 
                         scale(HHC_prop):scale(Per_Agriculture) + scale(CRC_prop):scale(Per_Agriculture) +
                         scale(Area) +  scale(Per_Agriculture) + scale(Elevation) + exp(pos + 0 | ID), 
                         family=binomial, weights = SiteNUM, data= CMS_dat, na.action ="na.fail")
summary(CMS_tmb_exp)

#'- Model selection and averaging
CMS_exp.dred = dredge(CMS_tmb_exp)
CMS_exp.avg <- model.avg(CMS_exp.dred, rank = "AIC")
summary(CMS_exp.avg)
# 95% CI based on competing models  
confint(CMS_exp.avg)
# Relative variable importance based on all models  
importance(CMS_exp.dred)


#'## Southern Redbelly Dace Model
#'- Exponential correlation function was included to account for spatial autocorrelation  

#'- Scale coordinates and create a dummy group factor to be used as a random term
SRD_dat$pos <- numFactor(scale(SRD_dat$Lat, center = FALSE), scale(SRD_dat$Lon, center = FALSE))
SRD_dat$ID <- factor(rep(1, nrow(SRD_dat)))

#'- Model 
SRD_tmb_exp <- glmmTMB(SRD_prop ~ scale(HHC_prop) + scale(CRC_prop) + scale(CSR_prop) +
                         scale(HHC_prop):scale(Per_Agriculture) + scale(CRC_prop):scale(Per_Agriculture) + scale(CSR_prop):scale(Per_Agriculture) + 
                         scale(Area) + scale(Per_Agriculture) + scale(Elevation) + exp(pos + 0 |ID), 
                         family=binomial, weights= SiteNUM, data= SRD_dat, na.action ="na.fail") 
summary(SRD_tmb_exp)

#' - Model selection and averaging
SRD_exp.dred = dredge(SRD_tmb_exp)
SRD_exp.avg <- model.avg(SRD_exp.dred, rank = "AIC")
summary(SRD_exp.avg)
# 95% CI based on competing models
confint(SRD_exp.avg) # 95% confidence interval
# Relative variable importance based on all models 
importance(SRD_exp.dred)
