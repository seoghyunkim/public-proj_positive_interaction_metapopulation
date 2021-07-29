#' ---
#' title: WI data formatting
#' author: A. Terui & S. Kim
#' output:
#'   html_document:
#'     theme: paper
#'     toc: true
#'     toc_float: true
#'     number_sections: true
#' ---

#' # Session infomation
  sessionInfo()

#' # Load library

  # Load library
  rm(list=ls(all.names=T)) # clear all data
  library(tidyverse)
  source("suppl01_sp-removal.R")

#' # Species removal information
#' This information is included for comparison betwee Terui's and Kim's script

#' # Read data
#' **NOTE: use `readr::read_csv()` instead of `read.csv()` to read data - `read.csv()` treated blanks weiredly in this dataset**
#' 
#' - `wi_station_originalfile.csv` - site date
#' - `wi_fishdata_originalfile.csv` - fish data
#+ message = FALSE, warning = FALSE

  # Read data ---------
  WI.dat_site <- read_csv("./data_raw_fish/data-org_wi-station.csv") %>% rename(SiteSeqNo = MONIT_STATION_SEQ_NO) # change Unique visit ID to match fish data
  WI.dat_fish <- read_csv("./data_raw_fish/data-org_wi-fish.csv", na = c("", "NA", "-") ) # add "-" as NA string
  splist <- str_replace_all(sort(unique(WI.dat_fish$Species)), "\\s","_")
  write.csv(splist, file = "./data_raw_fish/data-org_wi-fish-splist.csv") # for species list
  
  ## check unique sites - check unique combinations
  n_distinct(WI.dat_site$SiteSeqNo) # n = 3596
  n_distinct(WI.dat_site[,c("CALC_LL_LAT_DD_AMT", "CALC_LL_LONG_DD_AMT")]) # n = 3596
  n_distinct(WI.dat_site[,c("SiteSeqNo", "CALC_LL_LAT_DD_AMT", "CALC_LL_LONG_DD_AMT")]) # n = 3596
  
#' # Fish data formatting
#' Remove unidentified or hybrid species. These are identified if `Name2` is either `NA`, `hybrid` or `larvae`

  # Fish data formatting --------

  ## WARNING!!: too many error occurred, thus I just manually remove hybrids and fixed errors ##
  
  ## format Species
  ## replace space with underscore
  ## rename to upppercases
  ## remove '_(AMMOCOETE)' from string
  ## replace 'NORTHERN_HOG_SUCKER' with 'NORTHERN_HOGSUCKER'
  ## replace 'TROUT-PERCH' and 'TROUTPERCH' with 'TROUT_PERCH'
  ## replace 'CHESNUT_LAMPREY' to 'CHESTNUT_LAMPREY'
  ## drop unidentified
  ## remove GOLDFISH and PEARL DACE

  # WARNING to avid potential error, hybrids and unidentified species were manually removed
  WI.dat_fish <- WI.dat_fish %>%
    mutate(Species = str_replace_all(Species, "\\s", "_") ) %>%
    mutate(Species = str_to_upper(Species) ) %>%
    mutate(Species = str_remove(Species, "_\\(AMMOCOETE\\)")) %>% # check if works properly
    mutate(Species = str_replace(Species, "NORTHERN_HOG_SUCKER", "NORTHERN_HOGSUCKER")) %>%
    mutate(Species = str_replace(Species, "TROUT-PERCH", "TROUT_PERCH")) %>%
    mutate(Species = str_replace(Species, "TROUTPERCH", "TROUT_PERCH")) %>% 
    mutate(Species = str_replace(Species, "CHESNUT_LAMPREY", "CHESTNUT_LAMPREY")) %>% 
    drop_na(Species) %>% # remove NA
    filter(!(Species %in% sp_rm) ) -> WI.dat_fish # remove unidentified species and hybrid using sp_rm list

#' # Merge datasets
#' Merge `WI.dat_fish` and `WI.dat_site`
#' 
#' - `WI.dat_fish` as a base dataset because it includes fish data
#' - `WI.dat_site` is joined to obtain GPS location (Lat and Lon)
#'
#' ## Add visit and station data to fish data

  WI.dat = WI.dat_fish %>%
    drop_na(SiteSeqNo) %>% # drop NA SiteSeqNo (unique visit ID)
    select(SiteSeqNo, SampleDate, Species, NumberofFish) %>% # select columns
    group_by(SiteSeqNo, SampleDate, Species) %>% 
    summarise(Abundance = sum(NumberofFish)) %>% # calculate the abundance of each species based on SiteSeqNo (VisitID)
    left_join(WI.dat_site, by = c("SiteSeqNo")) %>% # merge site data
    select(SiteID = SiteSeqNo, Date = SampleDate,
           Lat = CALC_LL_LAT_DD_AMT, Lon = CALC_LL_LONG_DD_AMT, Species, Abundance) %>% # select columns
    mutate(State = "WI", Julian = julian(as.Date(Date, '%m/%d/%Y') ) ) %>% # add State column
    mutate(VisitID = paste0(SiteID, Julian) ) %>%
    filter( !( Lat %in% c("0")|Lon %in% c("0") ) ) %>% # remove sampling sites which do not have GPS (either Lat or Lon)
    drop_na(Lat,Lon) 
    
    WI.dat$SiteID <- paste0("WI_", as.character(WI.dat$SiteID))
  
  ## check result
  n_distinct(WI.dat$SiteID) # n = 1,956
  n_distinct(WI.dat$VisitID) # n = 2,627
  
  ## see the final data including all sampling occasion
  WI.dat # row number = 21,957

#' ## Select the latest sampling occasion by SiteID
  ## unique coordinates for each site
  WI.gps <- WI.dat %>% group_by(SiteID) %>% summarise(Lat = unique(Lat), Lon = unique(Lon) )
  
  ## select the latest sampling occasion in each site
  WI.dat.latest  = WI.dat %>%
    select(SiteID, Date) %>%
    group_by(SiteID) %>% 
    slice(which.max(as.Date(Date, '%m/%d/%Y'))) %>%
    left_join(WI.dat, by = c("SiteID", "Date")) %>%
    group_by(State, SiteID, VisitID, Date, Species) %>%
    summarise(Abundance = sum(Abundance)) %>%
    left_join(WI.gps, by = "SiteID") %>% 
    select(State, SiteID, VisitID, Date, Lat, Lon, Species, Abundance) # re-order column

  # check result
  n_distinct(WI.dat.latest$SiteID) # n = 1,956 correct
  n_distinct(WI.dat.latest$VisitID) # n = 1,956 correct
  
#' Final data only included the latest sampling occasion ------
  WI.dat.latest # row number = 16421

  # Export final data into data_output folder
  filename <- paste0("./data_outcome/fishdata_wi_latest", Sys.Date(), ".csv")
  write.csv(WI.dat.latest, filename)
