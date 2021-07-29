#' ---
#' title: Raw data plot using correlation
#' author: A. Terui & S. Kim
#' output:
#'   html_document:
#'     theme: paper
#'     toc: true
#'     toc_float: true
#'     number_sections: TRUE
#' ---


#' # Session information
sessionInfo()


#' # Load library

# Load library
rm(list=ls(all.names=T)) # clear all data

library(tidyverse)
library(PerformanceAnalytics)
library(moonBook)
library(Hmisc)
library(ggpubr)
library(corrplot)
library(Hmisc)


#'# Read data 

#' Fish data
HHC_fish_dat <- read_csv("./data_outcome/HHC_fish_dat_1000km_2021-03-08.csv")
HHC_fish_dat$X1 <- NULL
HHC_fish_dat$Area <- HHC_fish_dat$Area/1000000 # transfer m^2 to km^2

#' Branching Probability data
bp_dat <- read_csv("./data_outcome/bp_res_1000km_2021-03-09.csv") # read shapefile (dbf file)
bp_dat = bp_dat %>% select(WatershedID = X1, BP_1km) # change column name

#' Combine data
HHC_fish_dat <- HHC_fish_dat %>%
  left_join(bp_dat, by = "WatershedID")

#' Filter SRD distribution
SRD_dat  <- HHC_fish_dat  %>%
  filter(HUC4 %in% c("HUC4_12","HUC4_33","HUC4_35","HUC4_36","HUC4_39","HUC4_42","HUC4_43","HUC4_44","HUC4_45"))


#'# Data summary

#' Simple summary
summary(HHC_fish_dat)    

#' Table
mytable(HHC_fish_dat, digits = 2)


#### Correlation ####
#' # Correlation plot 
#' This plot is to check relationships between variables 

#' ## Correlation plot - 5 target species
chart.Correlation(HHC_fish_dat[, c(6:7,13:15)], method="spearman", histogram=TRUE, cex = 10) 
# CMS: common shiner, SRD: southern redbelly dace, HHC: hornyhead chub; CRC: creek chub, CSR: central stoneroller 

#' ## Correlation plot - 5 target species with 6 predators
chart.Correlation(HHC_fish_dat[, c(6,7,13:21)], method="spearman", histogram=TRUE, cex = 10) 
corrplot(cor(HHC_fish_dat[, c(6,7,13:21)]), type="upper", method = 'color', addCoef.col = "black", diag = FALSE, tl.col = "black")  
# CMS: common shiner, SRD: southern redbelly dace, HHC: hornyhead chub; CRC: creek chub, CSR: central stoneroller 
# LMB: largemouth bass, SMB: smallmouth bass, NTP: northern pike, BKT: brook trout, RBT: rainbow trout, BNT: brown trout

#' ## Correlation plot - all environmental variables
chart.Correlation(HHC_fish_dat[, c(23,33, 24:32)], method="spearman", histogram=TRUE, cex = 10) 
corrplot(cor(HHC_fish_dat[, c(23,33, 24:32)]), type="upper", method = 'color', addCoef.col = "black", diag = FALSE, tl.col = "black") 

#' ## Correlation plot - target environmental variables
chart.Correlation(HHC_fish_dat[, c(23,33,25,28,31)], method="spearman", histogram=TRUE, cex = 10) 
corrplot(cor(HHC_fish_dat[, c(23,33,25,28,31)]), type="upper", method = 'color', addCoef.col = "black", diag = FALSE, tl.col = "black") 

#' ## Correlation plot - all species species and target environmental variables
chart.Correlation(HHC_fish_dat[, c(6,7,13:21, 23,33,25,28,31)], method="spearman", histogram=TRUE, cex = 10) 
corrplot(cor(HHC_fish_dat[, c(6,7,13:21, 23,33,25,28,31)]), type="upper", method = 'color', addCoef.col = "black", diag = FALSE, tl.col = "black") 

#' ## Correlation plot - target species species and target environmental variables
chart.Correlation(HHC_fish_dat[, c(6,7,13:15, 23,25,26,27,28,29)], method="spearman", histogram=TRUE, cex = 10) 
corrplot(cor(HHC_fish_dat[, c(6,7,13:15, 23,25,26,27,28,29)]), type="upper", method = 'color', addCoef.col = "black", diag = FALSE, tl.col = "black") 

#' ## Spearman Correlation Table 
cor_01 <- rcorr(as.matrix(HHC_fish_dat[, c(6,7,13:15, 23,25,26,27,28,29)]), type = c("spearman"))
cor_01

#' ## Correlation plot by species (nest associate)
#'
# CMS
corrplot(cor(HHC_fish_dat[, c(6,13:15,16:21,23,33,25,28,31)]), type="upper", method = 'color', addCoef.col = "black", diag = FALSE, tl.col = "black")  
# SRD
corrplot(cor(SRD_dat[, c(7,13:15,16:21,23,33,25,28,31)]), type="upper", method = 'color', addCoef.col = "black", diag = FALSE, tl.col = "black") 
# HHC
corrplot(cor(HHC_fish_dat[, c(13,23,33,25,28,31)]), type="upper", method = 'color', addCoef.col = "black", diag = FALSE, tl.col = "black") 
# CRC
corrplot(cor(HHC_fish_dat[, c(14,23,33,25,28,31)]), type="upper", method = 'color', addCoef.col = "black", diag = FALSE, tl.col = "black")
# CSR
corrplot(cor(HHC_fish_dat[, c(15,23,33,25,28,31)]), type="upper", method = 'color', addCoef.col = "black", diag = FALSE, tl.col = "black")




#'# Data summary

#'- Simple scatter plot
#'

#' ## HHC
HHC_BP <- ggplot(HHC_fish_dat, aes(x= BP_1km, y= HHC_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 


HHC_Area <- ggplot(HHC_fish_dat, aes(x= Area, y= HHC_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 


HHC_Agri <- ggplot(HHC_fish_dat, aes(x= Per_Agriculture , y= HHC_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 


HHC_Ele <- ggplot(HHC_fish_dat, aes(x= Elevation, y= HHC_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 


HHC_Lat <- ggplot(HHC_fish_dat, aes(x= Lat, y= HHC_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 




#' ## CRC
CRC_BP <- ggplot(HHC_fish_dat, aes(x= BP_1km, y= CRC_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 


CRC_Area <- ggplot(HHC_fish_dat, aes(x= Area, y= CRC_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 


CRC_Agri <- ggplot(HHC_fish_dat, aes(x= Per_Agriculture , y= CRC_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 


CRC_Ele <- ggplot(HHC_fish_dat, aes(x= Elevation, y= CRC_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 


CRC_Lat <- ggplot(HHC_fish_dat, aes(x= Lat, y= CRC_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 


CRC_BP
CRC_Area
CRC_Agri
CRC_Ele
CRC_Lat

#' ## CSR
CSR_BP <- ggplot(HHC_fish_dat, aes(x= BP_1km, y= CSR_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 


CSR_Area <- ggplot(HHC_fish_dat, aes(x= Area, y= CSR_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 


CSR_Agri <- ggplot(HHC_fish_dat, aes(x= Per_Agriculture , y= CSR_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 


CSR_Ele <- ggplot(HHC_fish_dat, aes(x= Elevation, y= CSR_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 


CSR_Lat <- ggplot(HHC_fish_dat, aes(x= Lat, y= CSR_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 


CSR_BP
CSR_Area
CSR_Agri
CSR_Ele
CSR_Lat


#' ## CMS
CMS_BP <- ggplot(HHC_fish_dat, aes(x= BP_1km, y= CMS_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 


CMS_Area <- ggplot(HHC_fish_dat, aes(x= Area, y= CMS_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 


CMS_Agri <- ggplot(HHC_fish_dat, aes(x= Per_Agriculture , y= CMS_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 


CMS_Ele <- ggplot(HHC_fish_dat, aes(x= Elevation, y= CMS_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 


CMS_Lat <- ggplot(HHC_fish_dat, aes(x= Lat, y= CMS_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 


CMS_BP
CMS_Area
CMS_Agri
CMS_Ele
CMS_Lat

#' ## SRD
SRD_BP <- ggplot(SRD_dat, aes(x= BP_1km, y= SRD_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 


SRD_Area <- ggplot(SRD_dat, aes(x= Area, y= SRD_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 


SRD_Agri <- ggplot(SRD_dat, aes(x= Per_Agriculture , y= SRD_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 


SRD_Ele <- ggplot(SRD_dat, aes(x= Elevation, y= SRD_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 


SRD_Lat <- ggplot(SRD_dat, aes(x= Lat, y= SRD_prop)) + 
  geom_point(aes(size= SiteNUM)) + 
  geom_smooth(method=lm, se= T) +
  theme(legend.position = "none") 

SRD_BP
SRD_Area
SRD_Agri
SRD_Ele
SRD_Lat  



#' ## Combine figures by covariate 
figure01 <- ggarrange(HHC_Area, CRC_Area, CSR_Area, CMS_Area, SRD_Area, 
                      HHC_BP, CRC_BP, CSR_BP, CMS_BP, SRD_BP,
                      HHC_Agri, CRC_Agri, CSR_Agri, CMS_Agri, SRD_Agri,
                      HHC_Ele, CRC_Ele, CSR_Ele, CMS_Ele, SRD_Ele,
                      HHC_Lat, CRC_Lat, CSR_Lat, CMS_Lat, SRD_Lat,
                      ncol = 5, nrow = 5)
figure01  

figure02 <- ggarrange(HHC_Area, CRC_Area, CSR_Area, CMS_Area, SRD_Area, 
                      HHC_BP, CRC_BP, CSR_BP, CMS_BP, SRD_BP,
                      ncol = 5, nrow = 2)
figure03 <- ggarrange(HHC_Agri, CRC_Agri, CSR_Agri, CMS_Agri, SRD_Agri,
                      HHC_Ele, CRC_Ele, CSR_Ele, CMS_Ele, SRD_Ele,
                      ncol = 5, nrow = 2)
figure04 <- ggarrange(HHC_Lat, CRC_Lat, CSR_Lat, CMS_Lat, SRD_Lat,
                      ncol = 5, nrow = 1)

# export figure by 2100 x 700

figure02
figure03
figure04
