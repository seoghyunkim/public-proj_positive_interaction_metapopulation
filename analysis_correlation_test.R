#' ---
#' title: Correlation test
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
library(PerformanceAnalytics)
library(Hmisc)


#' # Read data 
fish_dat <- read_csv("./data_outcome/pos_meta_dat.csv")
fish_dat$X1 <- NULL


#' # Correlation plot 

#' ## Spearman Correlation Table 
rcorr(as.matrix(fish_dat[, c(12:22)]), type = c("spearman"))

#' ## Spearman Correlation figure
chart.Correlation(fish_dat[, c(12:22)], method="spearman", histogram=TRUE, cex = 10) 


