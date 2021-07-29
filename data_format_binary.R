#' ---
#' title: data formatting hornyhead chub 
#' author: A. Terui & S. Kim
#' output:
#'   html_document:
#'     theme: paper
#'     toc: true
#'     toc_float: true
#' ---

#' # Session infomation
sessionInfo()

#' # Load library

# Load library
rm(list=ls(all.names=T)) # clear all data
library(tidyverse)
library(foreign)


#'# Read data 
  
  # (1) fish data
  MN.fish <- read_csv("./data_outcome/fishdata_mn_latest2020-07-27.csv")
  WI.fish <- read_csv("./data_outcome/fishdata_wi_latest2020-07-25.csv") 
  IA.fish <- read_csv("./data_outcome/fishdata_ia_latest2020-07-25.csv") 
  IL.fish <- read_csv("./data_outcome/fishdata_il_latest2020-07-25.csv") 
  
  # Combine all fish data -----------
  Fish_dat <- rbind(MN.fish, WI.fish, IA.fish, IL.fish) # combine all organized data
  #filename1 <- paste0("./data_outcome/fish_dat", Sys.Date(), ".csv")
  #write.csv(Fish_dat, filename1)
  
  n_distinct(Fish_dat$SiteID) # n = 6085

  
  
#'# Data management
    
  # Select unique siteID with date, GPS coordinates
  Fish.gps = Fish_dat %>% 
    filter (!duplicated(SiteID)) %>% # remove duplicate
    select(c(State, SiteID, VisitID, Date, Lat, Lon)) # select columns
  
  
  # Add P/A data for each species ------------
  HHC_dat = Fish_dat %>% 
    mutate(HHC = ifelse(endsWith(Species, "HORNYHEAD_CHUB"), 1, 0)) %>% # nest builder
    mutate(CRC = ifelse(endsWith(Species, "CREEK_CHUB"), 1, 0)) %>% # nest builder
    mutate(CSR = ifelse(endsWith(Species, "CENTRAL_STONEROLLER"), 1, 0)) %>% # nest builder
    mutate(LSR = ifelse(endsWith(Species, "LARGESCALE_STONEROLLER"), 1, 0)) %>% # nest builder
    
    mutate(CMS = ifelse(endsWith(Species, "COMMON_SHINER"), 1, 0)) %>% # nest associate
    mutate(SRD = ifelse(endsWith(Species, "SOUTHERN_REDBELLY_DACE"), 1, 0)) %>% # nest associate
    mutate(CAS = ifelse(endsWith(Species, "CARMINE_SHINER"), 1, 0)) %>% # nest associate
    mutate(RFS = ifelse(endsWith(Species, "ROSYFACE_SHINER"), 1, 0)) %>% # nest associate
    
    mutate(BNM = ifelse(endsWith(Species, "BLUNTNOSE_MINNOW"), 1, 0)) %>% # potential nest associate
    mutate(RAD = ifelse(endsWith(Species, "RAINBOW_DARTER"), 1, 0)) %>% # potential nest associate
    mutate(JOD = ifelse(endsWith(Species, "JOHNNY_DARTER"), 1, 0)) %>% # potential nest associate
    
    mutate(LMB = ifelse(endsWith(Species, "LARGEMOUTH_BASS"), 1, 0)) %>% # predator
    mutate(SMB = ifelse(endsWith(Species, "SMALLMOUTH_BASS"), 1, 0)) %>% # predator
    mutate(NTP = ifelse(endsWith(Species, "NORTHERN_PIKE"), 1, 0)) %>% # predator
    mutate(BKT = ifelse(endsWith(Species, "BROOK_TROUT"), 1, 0)) %>% # predator
    mutate(RBT = ifelse(endsWith(Species, "RAINBOW_TROUT"), 1, 0)) %>% # predator
    mutate(BNT = ifelse(endsWith(Species, "BROWN_TROUT"), 1, 0)) # predator
  
  
  HHC_dat1 <-aggregate(HHC ~ SiteID, data = HHC_dat, "sum") # HHC is hornyhead chub
  CRC_dat1 <-aggregate(CRC ~ SiteID, data = HHC_dat, "sum") # CRC is creek chub
  CSR_dat1 <-aggregate(CSR ~ SiteID, data = HHC_dat, "sum") # CSR is central stoneroller
  LSR_dat1 <-aggregate(LSR ~ SiteID, data = HHC_dat, "sum") # LSR is largescale stronroller
  CMS_dat1 <-aggregate(CMS ~ SiteID, data = HHC_dat, "sum") # CMS is common shiner
  SRD_dat1 <-aggregate(SRD ~ SiteID, data = HHC_dat, "sum") # SRD is southern redbelly dace
  CAS_dat1 <-aggregate(CAS ~ SiteID, data = HHC_dat, "sum") # CAS is carmine shiner
  RFS_dat1 <-aggregate(RFS ~ SiteID, data = HHC_dat, "sum") # RFS is rosyface shiner
  BNM_dat1 <-aggregate(BNM ~ SiteID, data = HHC_dat, "sum") # BNM is bluntnose minnow
  RAD_dat1 <-aggregate(RAD ~ SiteID, data = HHC_dat, "sum") # RAD is rainbow darter
  JOD_dat1 <-aggregate(JOD ~ SiteID, data = HHC_dat, "sum") # JOD is johnny darter
  
  LMB_dat1 <-aggregate(LMB ~ SiteID, data = HHC_dat, "sum") # 
  SMB_dat1 <-aggregate(SMB ~ SiteID, data = HHC_dat, "sum") #
  NTP_dat1 <-aggregate(NTP ~ SiteID, data = HHC_dat, "sum") # 
  BKT_dat1 <-aggregate(BKT ~ SiteID, data = HHC_dat, "sum") # 
  RBT_dat1 <-aggregate(RBT ~ SiteID, data = HHC_dat, "sum") #
  BNT_dat1 <-aggregate(BNT ~ SiteID, data = HHC_dat, "sum") # 
  
  
  
  HHC_dat2 = HHC_dat1 %>% 
    left_join(CRC_dat1, by = "SiteID") %>%
    left_join(CSR_dat1, by = "SiteID") %>%
    left_join(LSR_dat1, by = "SiteID") %>%
    left_join(CMS_dat1, by = "SiteID") %>%
    left_join(SRD_dat1, by = "SiteID") %>%
    left_join(CAS_dat1, by = "SiteID") %>%
    left_join(RFS_dat1, by = "SiteID") %>%
    left_join(BNM_dat1, by = "SiteID") %>%
    left_join(RAD_dat1, by = "SiteID") %>%
    left_join(JOD_dat1, by = "SiteID") %>%
    
    left_join(LMB_dat1, by = "SiteID") %>%
    left_join(SMB_dat1, by = "SiteID") %>%
    left_join(NTP_dat1, by = "SiteID") %>%
    left_join(BKT_dat1, by = "SiteID") %>%
    left_join(RBT_dat1, by = "SiteID") %>%
    left_join(BNT_dat1, by = "SiteID") %>%
    
    left_join(Fish.gps, by = "SiteID") %>%
    select(c(State, SiteID, VisitID, Date, Lat, Lon, HHC, CRC, CSR, LSR, CMS, SRD, CAS, RFS, BNM, RAD, JOD, LMB, SMB, NTP, BKT, RBT, BNT)) # select column
  
  # Check result
  HHC_dat2
  
  n_distinct(HHC_dat2$SiteID) # n = 6085 correct
  
  
  # Export final data into data_output folder
  filename <- paste0("./data_outcome/HHC_dat_", Sys.Date(), ".csv")
  write.csv(HHC_dat2, filename)  

  