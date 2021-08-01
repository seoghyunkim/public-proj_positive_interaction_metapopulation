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
library(spaMM)
library(performance)
library(ggpubr)

#'# Read data 

#' Fish data
fish_dat <- read_csv("./data_outcome/pos_meta_dat.csv")
fish_dat$X1 <- NULL

#' remove correlation
#' - This is for GLMM, particularly in SRD model. There are strong correlation between elevation and temperature; CMS and HHC and CRC; CRS and agriculture.
fish_dat <- fish_dat %>% 
           mutate(resid_Temp = resid(lm(Temperature ~ Elevation, data = .)))

#' Filter SRD distribution (retain 71 watersheds)
#'- This is to remove HUC4 sub-basin where SRD are not distributed.
  SRD_dat  <- fish_dat  %>%
    filter(HUC4 %in% c("HUC4_12","HUC4_33","HUC4_35","HUC4_36","HUC4_39","HUC4_42","HUC4_43","HUC4_44","HUC4_45"))

#'- Common shiner data (110 watersheds)
    CMS_dat  <- fish_dat


##### GLMM using package spaMM #####  
#'- Matern correlation function was included to account for spatial autocorrelation

#'## Common Shiner Model

#'- Model
  CMS_mod <- fitme(cbind(CMS_num, SiteNUM-CMS_num)~ scale(HHC_prop) + scale(CRC_prop) + 
                   scale(Area) +  scale(Per_Agriculture) + scale(Elevation) + scale(resid_Temp) + Matern(1|Lat + Lon), 
                   family= binomial, data= CMS_dat, method="ML")
  
  summary(CMS_mod)
  
#'- Model validation - Pearson Correlation
  cor.test(predict(CMS_mod), CMS_dat$CMS_prop)
  
  cms_val <- ggplot() + 
    geom_point(aes(as.vector(predict(CMS_mod)), CMS_dat$CMS_prop)) + 
    scale_x_continuous("Predicted value", breaks = seq(0,1,0.2)) +
    scale_y_continuous("Observed value", breaks = seq(0,1,0.2)) +
    labs(title= "Common Shiner") +
    theme_bw() +
    theme(text=element_text(face="bold", size=17, color="black"),
          legend.position = "none",  # This is to remove legend
          panel.border=element_rect(colour='black'),
          panel.grid.major=element_line(colour=NA),
          panel.grid.minor=element_line(colour=NA),
          axis.title.y = element_text(size = rel(0.9)),
          axis.title.x = element_text(size = rel(0.9)),
          axis.text.x = element_text(size = rel(0.9)),
          axis.text.y = element_text(size = rel(0.9)))  # remove grid
    theme(panel.background = element_rect(fill = "white"), legend.position=c(0.10,0.80))

#'- Extract 95% CI  
  CMS_mod_coef <- as.data.frame(summary(CMS_mod)$beta_table)
  row0 <- row.names(CMS_mod_coef) %in% c('(Intercept)')
  row1 <- row.names(CMS_mod_coef) %in% c('scale(HHC_prop)')
  row2 <- row.names(CMS_mod_coef) %in% c('scale(CRC_prop)')
  row3 <- row.names(CMS_mod_coef) %in% c('scale(Area)')
  row4 <- row.names(CMS_mod_coef) %in% c('scale(Per_Agriculture)')
  row5 <- row.names(CMS_mod_coef) %in% c('scale(Elevation)')
  row6 <- row.names(CMS_mod_coef) %in% c('scale(resid_Temp)')
  
  lower0 <- CMS_mod_coef[row0,'Estimate'] - 1.96*CMS_mod_coef[row0, 'Cond. SE']
  upper0 <- CMS_mod_coef[row0,'Estimate'] + 1.96*CMS_mod_coef[row0, 'Cond. SE']
  
  lower1 <- CMS_mod_coef[row1,'Estimate'] - 1.96*CMS_mod_coef[row1, 'Cond. SE']
  upper1 <- CMS_mod_coef[row1,'Estimate'] + 1.96*CMS_mod_coef[row1, 'Cond. SE']
  
  lower2 <- CMS_mod_coef[row2,'Estimate'] - 1.96*CMS_mod_coef[row2, 'Cond. SE']
  upper2 <- CMS_mod_coef[row2,'Estimate'] + 1.96*CMS_mod_coef[row2, 'Cond. SE']
  
  lower3 <- CMS_mod_coef[row3,'Estimate'] - 1.96*CMS_mod_coef[row3, 'Cond. SE']
  upper3 <- CMS_mod_coef[row3,'Estimate'] + 1.96*CMS_mod_coef[row3, 'Cond. SE']

  lower4 <- CMS_mod_coef[row4,'Estimate'] - 1.96*CMS_mod_coef[row4, 'Cond. SE']
  upper4 <- CMS_mod_coef[row4,'Estimate'] + 1.96*CMS_mod_coef[row4, 'Cond. SE']
  
  lower5 <- CMS_mod_coef[row5,'Estimate'] - 1.96*CMS_mod_coef[row5, 'Cond. SE']
  upper5 <- CMS_mod_coef[row5,'Estimate'] + 1.96*CMS_mod_coef[row5, 'Cond. SE']
  
  lower6 <- CMS_mod_coef[row6,'Estimate'] - 1.96*CMS_mod_coef[row6, 'Cond. SE']
  upper6 <- CMS_mod_coef[row6,'Estimate'] + 1.96*CMS_mod_coef[row6, 'Cond. SE']
  
  cms_low <- rbind(lower0, lower1, lower2, lower3 ,lower4, lower5, lower6)
  cms_upp <- rbind(upper0, upper1, upper2, upper3, upper4, upper5, upper6)
  
   cms_ci <- as.data.frame(cbind( cms_low,  cms_upp))
   names( cms_ci) <- c("lower", "upper")
   cms.CI_name <- c("Intercept","Hornyhead chub", "Creek chub", "Catchment area", "Agriculture", "Elevation", "Temperature")
   cms_ci$variable <-  cms.CI_name
   cms_ci
   
   cms_ci2 =  cms_ci %>%
    rowwise() %>%
    mutate(mean_effect = mean(c(lower, upper), na.rm=T)) %>%
    mutate(name = fct_relevel(variable,"Intercept",  "Hornyhead chub", "Creek chub", "Catchment area", "Agriculture", "Elevation", "Temperature")) %>% #  This is to fix row order for ggplot
    select(variable= name, lower, upper, mean_effect)
  
  
#'- 95% CI plot
  cms_CI_plot <- ggplot( cms_ci2, aes(x= variable, y= mean_effect)) + 
    geom_errorbar(aes(ymin=lower, ymax= upper), width=0.2, size=1) + 
    geom_point(mapping= aes(x= variable, y= mean_effect), size=4, shape=21, fill="black") +
    geom_hline(aes(yintercept = 0), linetype="dashed", color = "black", size= 0.9) +
    labs(title= "Common Shiner", x ="Explanatory variable", y = "Standardized effect size") +
    theme_bw() +
    theme(text=element_text(face="bold", size=17, color="black"),
          legend.position = "none",  # This is to remove legend
          panel.border=element_rect(colour='black'),
          panel.grid.major=element_line(colour=NA),
          panel.grid.minor=element_line(colour=NA),
          axis.title.y = element_text(size = rel(0.9)),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 45,  vjust = 1, hjust = 1, size = rel(0.9)),
          axis.text.y = element_text(size = rel(0.9)))  # remove grid
  theme(panel.background = element_rect(fill = "white"), legend.position=c(0.10,0.80))
  
  cms_CI_plot
  
#'-  Regression plot
  CMS_HHC_efct <- plot_effects(CMS_mod,focal_var="HHC_prop", rgb.args = col2rgb("black"))
  CMS_CRC_efct <- plot_effects(CMS_mod,focal_var="CRC_prop", rgb.args = col2rgb("black"))
  CMS_elv_efct <- plot_effects(CMS_mod,focal_var="Elevation", rgb.args = col2rgb("black"))
  
CMS_HHC_fig <- ggplot() +
  geom_ribbon(data = CMS_HHC_efct, aes(x = focal_var, ymin = low, ymax = up), alpha = 0.2) +
  geom_line(data = CMS_HHC_efct, aes(x = focal_var, y = pointp), size = 1) +
  geom_point(data = CMS_dat, aes(x = HHC_prop, y = CMS_prop), shape=21, colour="black") +
  scale_y_continuous("Occupancy", limits=c(0,1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous("Hornyhead chub",  breaks = seq(0, 1, 0.2)) +     
  theme_bw() +
  theme(text=element_text(face="bold", size=8),  
        legend.position = "none",                     # remove legend panel
        panel.border=element_rect(colour='black'),
        panel.grid.major=element_line(colour=NA),
        panel.grid.minor=element_line(colour=NA),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)))  # remove grid
  theme(panel.background = element_rect(fill = "white")) 

CMS_CRC_fig <- ggplot() +
  geom_ribbon(data = CMS_CRC_efct, aes(x = focal_var, ymin = low, ymax = up), alpha = 0.2) +
  geom_line(data = CMS_CRC_efct, aes(x = focal_var, y = pointp), size = 1) +
  geom_point(data = CMS_dat, aes(x = CRC_prop, y = CMS_prop), shape=21, colour="black") +
  scale_y_continuous("Occupancy", limits=c(0,1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous("Creek chub",  breaks = seq(0, 1, 0.2)) +     
  theme_bw() +
  theme(text=element_text(face="bold", size=8),  
        legend.position = "none",                     # remove legend panel
        panel.border=element_rect(colour='black'),
        panel.grid.major=element_line(colour=NA),
        panel.grid.minor=element_line(colour=NA),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)))  # remove grid
  theme(panel.background = element_rect(fill = "white")) 

CMS_elv_fig <- ggplot() +
  geom_ribbon(data = CMS_elv_efct, aes(x = focal_var, ymin = low, ymax = up), alpha = 0.2) +
  geom_line(data = CMS_elv_efct, aes(x = focal_var, y = pointp), size = 1) +
  geom_point(data = CMS_dat, aes(x = Elevation, y = CMS_prop), shape=21, colour="black") +
  scale_y_continuous("Occupancy", limits=c(0,1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous("Elevation (m)",  breaks = seq(120, 520, 100)) +     
  theme_bw() +
  theme(text=element_text(face="bold", size=8),  
        legend.position = "none",                     # remove legend panel
        panel.border=element_rect(colour='black'),
        panel.grid.major=element_line(colour=NA),
        panel.grid.minor=element_line(colour=NA),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)))  # remove grid
  theme(panel.background = element_rect(fill = "white")) 

CMS_agri_fig <- ggplot() +
    geom_point(data = CMS_dat, aes(x = Per_Agriculture, y = CMS_prop), shape=21, colour="black") +
    scale_y_continuous("Occupancy", limits=c(0,1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous("Agriculture (%)",  breaks = seq(0, 100, 20)) +     
    theme_bw() +
    theme(text=element_text(face="bold", size=8),  
          legend.position = "none",                     # remove legend panel
          panel.border=element_rect(colour='black'),
          panel.grid.major=element_line(colour=NA),
          panel.grid.minor=element_line(colour=NA),
          axis.title.y = element_text(size = rel(1.5)),
          axis.title.x = element_text(size = rel(1.5)),
          axis.text.x = element_text(size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)))  # remove grid
  theme(panel.background = element_rect(fill = "white")) 

CMS_temp_fig <- ggplot() +
    geom_point(data = CMS_dat, aes(x = Temperature, y = CMS_prop), shape=21, colour="black") +
    scale_y_continuous("Occupancy", limits=c(0,1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous("Temperature (°C)", breaks = seq(4, 15, 2)) +    
    theme_bw() +
    theme(text=element_text(face="bold", size=8),  
          legend.position = "none",                     # remove legend panel
          panel.border=element_rect(colour='black'),
          panel.grid.major=element_line(colour=NA),
          panel.grid.minor=element_line(colour=NA),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = rel(1.5)),
          axis.text.x = element_text(size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)))  # remove grid
  theme(panel.background = element_rect(fill = "white")) 
  
CMS_area_fig <- ggplot() +
    geom_point(data = CMS_dat, aes(x = Area, y = CMS_prop), shape=21, colour="black") +
    scale_y_continuous("Occupancy", limits=c(0,1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous("Watershed area (km2)",  breaks = seq(200, 1000, 200)) +  
    theme_bw() +
    theme(text=element_text(face="bold", size=8),  
          legend.position = "none",                     # remove legend panel
          panel.border=element_rect(colour='black'),
          panel.grid.major=element_line(colour=NA),
          panel.grid.minor=element_line(colour=NA),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = rel(1.5)),
          axis.text.x = element_text(size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)))  # remove grid
  theme(panel.background = element_rect(fill = "white")) 

#'- Combine figures
ggarrange(CMS_HHC_fig, CMS_CRC_fig, CMS_area_fig, CMS_agri_fig,  CMS_elv_fig, CMS_temp_fig,
          ncol = 3, nrow = 2) # resolution 900 X 600


##### SRD model #####
#'## Southern Redbelly Dace Model

#'- mod model
  SRD_mod <- fitme(cbind(SRD_num, SiteNUM-SRD_num)~ scale(HHC_prop) + scale(CRC_prop) + scale(CSR_prop) + 
                   scale(Area) +  scale(Per_Agriculture) + scale(Elevation) + scale(resid_Temp) + Matern(1|Lat + Lon),
                   family= binomial, data= SRD_dat, method="ML")
  summary(SRD_mod)
  
#'- Model validation - Pearson Correlation
  cor.test(predict(SRD_mod), SRD_dat$SRD_prop)
  
  srd_val <-ggplot() + 
    geom_point(aes(as.vector(predict(SRD_mod)), SRD_dat$SRD_prop)) + 
    scale_x_continuous("Predicted value", limits =  c(0,1)) +
    scale_y_continuous("Observed value", limits =  c(0,1)) +
    labs(title= "Southern redbelly dace") +
    theme_bw() +
    theme(text=element_text(face="bold", size=17, color="black"),
          legend.position = "none",  # This is to remove legend
          panel.border=element_rect(colour='black'),
          panel.grid.major=element_line(colour=NA),
          panel.grid.minor=element_line(colour=NA),
          axis.title.y = element_text(size = rel(0.9)),
          axis.title.x = element_text(size = rel(0.9)),
          axis.text.x = element_text(size = rel(0.9)),
          axis.text.y = element_text(size = rel(0.9)))  # remove grid
  theme(panel.background = element_rect(fill = "white"), legend.position=c(0.10,0.80))
  
  srd_val

#'- Model validation plot: CMS and SRD
  ggarrange(cms_val, srd_val,
            ncol = 1, nrow = 2)

#'- Extract 95% CI  
  SRD_mod_coef <- as.data.frame(summary(SRD_mod)$beta_table)
  row0 <- row.names(SRD_mod_coef) %in% c('(Intercept)')
  row1 <- row.names(SRD_mod_coef) %in% c('scale(HHC_prop)')
  row2 <- row.names(SRD_mod_coef) %in% c('scale(CRC_prop)')
  row3 <- row.names(SRD_mod_coef) %in% c('scale(CSR_prop)')
  row4 <- row.names(SRD_mod_coef) %in% c('scale(Area)')
  row5 <- row.names(SRD_mod_coef) %in% c('scale(Per_Agriculture)')
  row6 <- row.names(SRD_mod_coef) %in% c('scale(Elevation)')
  row7 <- row.names(SRD_mod_coef) %in% c('scale(resid_Temp)')
  
  lower0 <- SRD_mod_coef[row0,'Estimate'] - 1.96*SRD_mod_coef[row0, 'Cond. SE']
  upper0 <- SRD_mod_coef[row0,'Estimate'] + 1.96*SRD_mod_coef[row0, 'Cond. SE']
  
  lower1 <- SRD_mod_coef[row1,'Estimate'] - 1.96*SRD_mod_coef[row1, 'Cond. SE']
  upper1 <- SRD_mod_coef[row1,'Estimate'] + 1.96*SRD_mod_coef[row1, 'Cond. SE']
  
  lower2 <- SRD_mod_coef[row2,'Estimate'] - 1.96*SRD_mod_coef[row2, 'Cond. SE']
  upper2 <- SRD_mod_coef[row2,'Estimate'] + 1.96*SRD_mod_coef[row2, 'Cond. SE']
  
  lower3 <- SRD_mod_coef[row3,'Estimate'] - 1.96*SRD_mod_coef[row3, 'Cond. SE']
  upper3 <- SRD_mod_coef[row3,'Estimate'] + 1.96*SRD_mod_coef[row3, 'Cond. SE']
  
  lower4 <- SRD_mod_coef[row4,'Estimate'] - 1.96*SRD_mod_coef[row4, 'Cond. SE']
  upper4 <- SRD_mod_coef[row4,'Estimate'] + 1.96*SRD_mod_coef[row4, 'Cond. SE']
  
  lower5 <- SRD_mod_coef[row5,'Estimate'] - 1.96*SRD_mod_coef[row5, 'Cond. SE']
  upper5 <- SRD_mod_coef[row5,'Estimate'] + 1.96*SRD_mod_coef[row5, 'Cond. SE']
  
  lower6 <- SRD_mod_coef[row6,'Estimate'] - 1.96*SRD_mod_coef[row6, 'Cond. SE']
  upper6 <- SRD_mod_coef[row6,'Estimate'] + 1.96*SRD_mod_coef[row6, 'Cond. SE']
  
  lower7 <- SRD_mod_coef[row7,'Estimate'] - 1.96*SRD_mod_coef[row7, 'Cond. SE']
  upper7 <- SRD_mod_coef[row7,'Estimate'] + 1.96*SRD_mod_coef[row7, 'Cond. SE']
  
  srd_low <- rbind(lower0, lower1, lower2, lower3 ,lower4, lower5, lower6, lower7)
  srd_upp <- rbind(upper0, upper1, upper2, upper3, upper4, upper5, upper6, upper7)
  
  srd_ci <- as.data.frame(cbind(srd_low, srd_upp))
  names(srd_ci) <- c("lower", "upper")
  srd.CI_name <- c("Intercept", "Hornyhead chub", "Creek chub", "Central stoneroller", "Catchment area", "Agriculture", "Elevation", "Temperature")
  srd_ci$variable <- srd.CI_name
  srd_ci
  
  srd_ci2 = srd_ci %>%
    rowwise() %>%
    mutate(mean_effect = mean(c(lower, upper), na.rm=T)) %>%
    mutate(name = fct_relevel(variable,"Intercept",  "Hornyhead chub", "Creek chub", "Central stoneroller", "Catchment area", "Agriculture", "Elevation", "Temperature")) %>% #  This is to fix row order for ggplot
    select(variable= name, lower, upper, mean_effect)
  
  
#'- 95% CI plot
  srd_CI_plot <- ggplot(srd_ci2, aes(x= variable, y= mean_effect)) + 
    geom_errorbar(aes(ymin=lower, ymax= upper), width=0.2, size=1) + 
    geom_point(mapping= aes(x= variable, y= mean_effect), size=4, shape=21, fill="black") +
    geom_hline(aes(yintercept = 0), linetype="dashed", color = "black", size= 0.9) +
    labs(title= "Southern Redbelly Dace", x ="Explanatory variable", y = "Standardized effect size") +
    theme_bw() +
    theme(text=element_text(face="bold", size=17, color="black"),
          legend.position = "none",  # This is to remove legend
          panel.border=element_rect(colour='black'),
          panel.grid.major=element_line(colour=NA),
          panel.grid.minor=element_line(colour=NA),
          axis.title.y = element_text(size = rel(0.9)),
          axis.title.x = element_text(size = rel(0.9)),
          axis.text.x = element_text(angle = 45,  vjust = 1, hjust = 1, size = rel(0.9)),
          axis.text.y = element_text(size = rel(0.9)))  # remove grid
  theme(panel.background = element_rect(fill = "white"), legend.position=c(0.10,0.80))
  
  srd_CI_plot

  
#'- Regression plot
  SRD_CSR_efct <- plot_effects(SRD_mod,focal_var="CSR_prop", rgb.args = col2rgb("black"))
  SRD_agri_efct <- plot_effects(SRD_mod,focal_var="Per_Agriculture", rgb.args = col2rgb("black"))

  SRD_HHC_fig <- ggplot() +
    geom_point(data = SRD_dat, aes(x = HHC_prop, y = SRD_prop), shape=21, colour="black") +
    scale_y_continuous("Occupancy", limits=c(0,1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous("Hornyhead chub",  breaks = seq(0, 1, 0.2)) +     
    theme_bw() +
    theme(text=element_text(face="bold", size=8),  
          legend.position = "none",                     # remove legend panel
          panel.border=element_rect(colour='black'),
          panel.grid.major=element_line(colour=NA),
          panel.grid.minor=element_line(colour=NA),
          axis.title.y = element_text(size = rel(1.5)),
          axis.title.x = element_text(size = rel(1.5)),
          axis.text.x = element_text(size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)))  # remove grid
  theme(panel.background = element_rect(fill = "white")) 
  
  SRD_CRC_fig <- ggplot() +
    geom_point(data = SRD_dat, aes(x = CRC_prop, y = SRD_prop), shape=21, colour="black") +
    scale_y_continuous("Occupancy", limits=c(0,1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous("Creek chub",  breaks = seq(0, 1, 0.2)) +     
    theme_bw() +
    theme(text=element_text(face="bold", size=8),  
          legend.position = "none",                     # remove legend panel
          panel.border=element_rect(colour='black'),
          panel.grid.major=element_line(colour=NA),
          panel.grid.minor=element_line(colour=NA),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = rel(1.5)),
          axis.text.x = element_text(size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)))  # remove grid
  theme(panel.background = element_rect(fill = "white")) 

  SRD_CSR_fig <- ggplot() +
    geom_ribbon(data = SRD_CSR_efct, aes(x = focal_var, ymin = low, ymax = up), alpha = 0.2) +
    geom_line(data = SRD_CSR_efct, aes(x = focal_var, y = pointp), size = 1) +
    geom_point(data = SRD_dat, aes(x = CSR_prop, y = SRD_prop), shape=21, colour="black") +
    scale_y_continuous("Occupancy", limits=c(0,1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous("Central stoneroller",  breaks = seq(0, 1, 0.2)) +     
    theme_bw() +
    theme(text=element_text(face="bold", size=8),  
          legend.position = "none",                     # remove legend panel
          panel.border=element_rect(colour='black'),
          panel.grid.major=element_line(colour=NA),
          panel.grid.minor=element_line(colour=NA),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = rel(1.5)),
          axis.text.x = element_text(size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)))  # remove grid
  theme(panel.background = element_rect(fill = "white")) 
  
  SRD_elv_fig <- ggplot() +
    geom_point(data = SRD_dat, aes(x = Elevation, y = SRD_prop), shape=21, colour="black") +
    scale_y_continuous("Occupancy",limits=c(0,1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous("Elevation (m)",  breaks = seq(120, 520, 100)) +     
    theme_bw() +
    theme(text=element_text(face="bold", size=8),  
          legend.position = "none",                     # remove legend panel
          panel.border=element_rect(colour='black'),
          panel.grid.major=element_line(colour=NA),
          panel.grid.minor=element_line(colour=NA),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = rel(1.5)),
          axis.text.x = element_text(size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)))  # remove grid
  theme(panel.background = element_rect(fill = "white")) 
  
  SRD_agri_fig <- ggplot() +
    geom_ribbon(data = SRD_agri_efct, aes(x = focal_var, ymin = low, ymax = up), alpha = 0.2) +
    geom_line(data = SRD_agri_efct, aes(x = focal_var, y = pointp), size = 1) +
    geom_point(data = SRD_dat, aes(x = Per_Agriculture, y = SRD_prop), shape=21, colour="black") +
    scale_y_continuous("Occupancy", limits=c(0,1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous("Agriculture (%)",  breaks = seq(0, 100, 20)) +     
    theme_bw() +
    theme(text=element_text(face="bold", size=8),  
          legend.position = "none",                     # remove legend panel
          panel.border=element_rect(colour='black'),
          panel.grid.major=element_line(colour=NA),
          panel.grid.minor=element_line(colour=NA),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = rel(1.5)),
          axis.text.x = element_text(size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)))  # remove grid
  theme(panel.background = element_rect(fill = "white")) 

  SRD_temp_fig <- ggplot() +
    geom_point(data = SRD_dat, aes(x = Temperature, y = SRD_prop), shape=21, colour="black") +
    scale_y_continuous("Occupancy", limits=c(0,1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous("Temperature (°C)",  breaks = seq(4, 15, 2)) +     
    theme_bw() +
    theme(text=element_text(face="bold", size=8),  
          legend.position = "none",                     # remove legend panel
          panel.border=element_rect(colour='black'),
          panel.grid.major=element_line(colour=NA),
          panel.grid.minor=element_line(colour=NA),
          axis.title.y = element_text(size = rel(1.5)),
          axis.title.x = element_text(size = rel(1.5)),
          axis.text.x = element_text(size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)))  # remove grid
  theme(panel.background = element_rect(fill = "white")) 
  
  SRD_area_fig <- ggplot() +
    geom_point(data = SRD_dat, aes(x = Area, y = SRD_prop), shape=21, colour="black") +
    scale_y_continuous("Occupancy", limits=c(0,1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous("Watershed area (km2)",  breaks = seq(0, 1000, 200)) +     
    theme_bw() +
    theme(text=element_text(face="bold", size=8),  
          legend.position = "none",                     # remove legend panel
          panel.border=element_rect(colour='black'),
          panel.grid.major=element_line(colour=NA),
          panel.grid.minor=element_line(colour=NA),
          axis.title.y = element_text(size = rel(1.5)),
          axis.title.x = element_text(size = rel(1.5)),
          axis.text.x = element_text(size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)))  # remove grid
  theme(panel.background = element_rect(fill = "white")) 

  
#'- Combine regression figures
  ggarrange(SRD_HHC_fig, SRD_CRC_fig, SRD_CSR_fig, SRD_area_fig, SRD_agri_fig,  SRD_elv_fig, SRD_temp_fig,
            ncol = 3, nrow = 3) # resolution 900 X 870
 
  