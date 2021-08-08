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
library(spaMM)
library(ggpubr)


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


##### GLMM using package spaMM #####  
#'- Matern correlation function was included to account for spatial autocorrelation

#'## Common Shiner Model

#'- Model
  CMS_mod <- fitme(cbind(CMS_num, SiteNUM-CMS_num)~ scale(HHC_prop) + scale(CRC_prop) + 
                   scale(Area) +  scale(Per_Agriculture) + scale(Elevation) + Matern(1|Lat + Lon), 
                   family= binomial, data= CMS_dat, method="ML")
  
  summary(CMS_mod)
  
#'- Model validation - Pearson Correlation
  cor.test(predict(CMS_mod), CMS_dat$CMS_prop)

# Validation figure    
  cms_val <- ggplot() + 
    geom_point(aes(as.vector(predict(CMS_mod)), CMS_dat$CMS_prop)) + 
    scale_x_continuous("Predicted value", limits =  c(0,1), breaks = seq(0,1,0.2)) +
    scale_y_continuous("Observed value", limits =  c(0,1), breaks = seq(0,1,0.2)) +
    labs(title= "Common shiner") +
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

#'- 95% CI  
    confint(CMS_mod,"(Intercept)")
    confint(CMS_mod,"scale(HHC_prop)")
    confint(CMS_mod,"scale(CRC_prop)")
    confint(CMS_mod,"scale(Area)")
    confint(CMS_mod,"scale(Per_Agriculture)")
    confint(CMS_mod,"scale(Elevation)")
    
#'-  Regression figures for each variable
  CMS_HHC_efct <- plot_effects(CMS_mod,focal_var="HHC_prop", rgb.args = col2rgb("black"))
  CMS_CRC_efct <- plot_effects(CMS_mod,focal_var="CRC_prop", rgb.args = col2rgb("black"))
  CMS_elv_efct <- plot_effects(CMS_mod,focal_var="Elevation", rgb.args = col2rgb("black"))
  
  CMS_HHC_fig <- ggplot() +
    geom_ribbon(data = CMS_HHC_efct, aes(x = focal_var, ymin = low, ymax = up), alpha = 0.2) +
    geom_line(data = CMS_HHC_efct, aes(x = focal_var, y = pointp), size = 1) +
    geom_point(data = CMS_dat, aes(x = HHC_prop, y = CMS_prop), shape=21, colour="black") +
    scale_y_continuous("Occupancy", limits=c(0,1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous("Hornyhead chub",  breaks = seq(0, 1, 0.2)) +     
    labs(title= " (a) Common shiner") +
    theme_bw() +
    theme(text=element_text(face="bold", size=8),
        plot.title=element_text(face="bold", size=13, vjust = 1, hjust = 0),
        legend.position = "none",                     # remove legend panel
        panel.border=element_rect(colour='black'),
        panel.grid.major=element_line(colour=NA),
        panel.grid.minor=element_line(colour=NA),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_blank(),
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
        axis.title.x = element_blank(),
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
        axis.title.x = element_blank(),
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
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)))  # remove grid
    theme(panel.background = element_rect(fill = "white")) 

  CMS_area_fig <- ggplot() +
      geom_point(data = CMS_dat, aes(x = Area, y = CMS_prop), shape=21, colour="black") +
      scale_y_continuous("Occupancy", limits=c(0,1), breaks = seq(0, 1, 0.2)) +
      scale_x_continuous("Watershed area (km2)",  breaks = seq(200, 1000, 200)) +  
      labs(title= " (a) Common shiner") +
      theme_bw() +
      theme(text=element_text(face="bold", size=8), 
          plot.title=element_text(face="bold", size=13, vjust = 1, hjust = 0),
          legend.position = "none",                     # remove legend panel
          panel.border=element_rect(colour='black'),
          panel.grid.major=element_line(colour=NA),
          panel.grid.minor=element_line(colour=NA),
          axis.title.y = element_text(size = rel(1.5)),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)))  # remove grid
    theme(panel.background = element_rect(fill = "white")) 


#'## Southern Redbelly Dace Model

#'- mod model
  SRD_mod <- fitme(cbind(SRD_num, SiteNUM-SRD_num)~ scale(HHC_prop) + scale(CRC_prop) + scale(CSR_prop) + 
                   scale(Area) +  scale(Per_Agriculture) + scale(Elevation) + Matern(1|Lat + Lon),
                   family= binomial, data= SRD_dat, method="ML")
  summary(SRD_mod)

#'- Model validation - Pearson Correlation
  cor.test(predict(SRD_mod), SRD_dat$SRD_prop)
 
# Validation figure   
  srd_val <-ggplot() + 
    geom_point(aes(as.vector(predict(SRD_mod)), SRD_dat$SRD_prop)) + 
    scale_x_continuous("Predicted value", limits =  c(0,1), breaks = seq(0,1,0.2)) +
    scale_y_continuous("Observed value", limits =  c(0,1), breaks = seq(0,1,0.2)) +
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

#'- 95% CI  
  confint(SRD_mod,"(Intercept)")
  confint(SRD_mod,"scale(HHC_prop)")
  confint(SRD_mod,"scale(CRC_prop)")
  confint(SRD_mod,"scale(CSR_prop)")
  confint(SRD_mod,"scale(Area)")
  confint(SRD_mod,"scale(Per_Agriculture)")
  confint(SRD_mod,"scale(Elevation)")
  
#'- Regression figures for each variable
  SRD_CSR_efct <- plot_effects(SRD_mod,focal_var="CSR_prop", rgb.args = col2rgb("black"))
  SRD_agri_efct <- plot_effects(SRD_mod,focal_var="Per_Agriculture", rgb.args = col2rgb("black"))

  SRD_HHC_fig <- ggplot() +
    geom_point(data = SRD_dat, aes(x = HHC_prop, y = SRD_prop), shape=21, colour="black") +
    scale_y_continuous("Occupancy", limits=c(0,1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous("Hornyhead chub",  breaks = seq(0, 1, 0.2)) +
    labs(title= " (b) Southern redbelly dace") +
    theme_bw() +
    theme(text=element_text(face="bold", size=8),
          plot.title=element_text(face="bold", size=13, vjust = 1, hjust = 0),
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

  SRD_area_fig <- ggplot() +
    geom_point(data = SRD_dat, aes(x = Area, y = SRD_prop), shape=21, colour="black") +
    scale_y_continuous("Occupancy", limits=c(0,1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous("Watershed area (km2)",  breaks = seq(0, 1000, 200)) +
    labs(title= " (b) Southern redbelly dace") +
    theme_bw() +
    theme(text=element_text(face="bold", size=8),
          plot.title=element_text(face="bold", size=13, vjust = 1, hjust = 0),
          legend.position = "none",                     # remove legend panel
          panel.border=element_rect(colour='black'),
          panel.grid.major=element_line(colour=NA),
          panel.grid.minor=element_line(colour=NA),
          axis.title.y = element_text(size = rel(1.5)),
          axis.title.x = element_text(size = rel(1.5)),
          axis.text.x = element_text(size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)))  # remove grid
  theme(panel.background = element_rect(fill = "white")) 

  
#'- Combine model validation figures: both species models
  ggarrange(cms_val, srd_val,
            ncol = 1, nrow = 2, align = "hv")  

#'- Combine mutiple figures by biotic and abiotic variables
  
# Biotic variables (metapopulation occupancy of host species)
  ggarrange(CMS_HHC_fig, CMS_CRC_fig, NULL, SRD_HHC_fig, SRD_CRC_fig, SRD_CSR_fig,
                            ncol = 3, nrow = 2, align = "hv") 
  
# Abiotic variables  
  ggarrange(CMS_area_fig, CMS_agri_fig,  CMS_elv_fig, SRD_area_fig, SRD_agri_fig,  SRD_elv_fig,
            ncol = 3, nrow = 2, align = "hv")
  
