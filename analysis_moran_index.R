#' ---
#' title: GLMM analysis
#' author: S. Kim, C. Dolph, and A. Terui
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
library(DHARMa)
library(gridExtra)

#'# Read data 

#' Fish data
HHC_fish_dat <- read_csv("./data_outcome/pos_meta_dat.csv")
HHC_fish_dat$X1 <- NULL

#' remove correlation
#' - This is for GLMM, particularly in SRD model. There are strong correlation between elevation and temperature; CMS and HHC and CRC; CRS and agriculture.
HHC_dat <- HHC_fish_dat %>% 
  mutate(resid_Temp = resid(lm(Temperature ~ Elevation, data = .)))

#' Filter SRD distribution (retain 71 watersheds)
#'- This is to remove HUC4 sub-basin where SRD are not distributed.
SRD_dat  <- HHC_dat  %>%
  filter(HUC4 %in% c("HUC4_12","HUC4_33","HUC4_35","HUC4_36","HUC4_39","HUC4_42","HUC4_43","HUC4_44","HUC4_45"))

#'- Common shiner data (110 watersheds)
CMS_dat  <- HHC_dat



#### Moran's I ####
#'# Moran's I spatial-autocorrelation test 
#'

#'## Common shiner
#'- Binomial GLM (null model) to obtain a residual
CMS_glm1 <- glm(cbind(CMS_num, SiteNUM-CMS_num)~ 1, family= binomial, data= CMS_dat)

#'- Residual
CMS_dat$resid <- resid(CMS_glm1)

#'- Check the distribution
ggplot(data= CMS_dat, aes(y = Lat, x = Lon, size = resid)) +
  geom_point() +
  scale_size_continuous(range = c(-10,10))

#'- Moran'I test
CMS_glm_sim <- simulateResiduals(CMS_glm1)
testSpatialAutocorrelation(CMS_glm_sim, y = CMS_dat$Lat, x = CMS_dat$Lon, plot = T)



#'## Southern Redbelly Dace
#'- Binomial GLM to obtain a residual
SRD_glm1 <- glm(cbind(SRD_num, SiteNUM-SRD_num)~ 1, family= binomial, data= SRD_dat)

#'- Residual
SRD_dat$resid <- resid(SRD_glm1)

#'- Check the distribution
ggplot(data= SRD_dat, aes(y = Lat, x = Lon, size = resid)) +
  geom_point() +
  scale_size_continuous(range = c(-10,10))

#'- Moran'I test
SRD_glm_sim <- simulateResiduals(SRD_glm1)
testSpatialAutocorrelation(SRD_glm_sim, y = SRD_dat$Lat, x = SRD_dat$Lon, plot = T)


