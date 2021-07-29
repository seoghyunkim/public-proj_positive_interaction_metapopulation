#' ---
#' title: IA data formatting
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
source("suppl01_sp-removal.R")

#' # Read data
#' **NOTE: use `readr::read_csv()` instead of `read.csv()` to read data - `read.csv()` treated blanks weiredly in this dataset**
#' 
#' - `ia_station_originalfile.csv` - site date
#' - `ia_fishdata_originalfile.csv` - fish data
#+ message = FALSE, warning = FALSE

  # Read data ---------------------------------------------------------------
  IA.dat_site <- read_csv("./data_raw_fish/data-org_ia-station.csv") %>% mutate(SiteID = str_to_upper(SiteID) )
  IA.dat_fish <- read_csv("./data_raw_fish/data-org_ia-fish.csv") %>% mutate(SiteID = str_to_upper(SiteID) )

  
#' # Fish data formatting  

  # Check unidentified or hybrid species
  unique(IA.dat_fish$Species[is.na(IA.dat_fish$Latin)])
  unique(IA.dat_fish$Species[IA.dat_fish$Latin %in% c("hybrid", "larvae", "spp.", "unidentified")])

## format CommonName
## replace space with underscore
## rename to upppercases
## remove '_(AMMOCOETE)' from string
## replace 'NORTHERN_HOG_SUCKER' with 'NORTHERN_HOGSUCKER'
## replace 'TROUT-PERCH' and 'TROUTPERCH' with 'TROUT_PERCH'
## drop unidentified
## remove unidentified and hybrid by list sp_rm

  IA.dat_fish %>%
    mutate(Species = str_replace_all(Species, pattern = "\\s", replacement = "_") ) %>%
    mutate(Species = str_to_upper(Species) ) %>%
    mutate(Species = str_replace(Species, "_\\(AMMOCOETE\\)", "")) %>% # remove '_(AMMOCOETE)' from string
    mutate(Species = str_replace(Species, "NORTHERN_HOG_SUCKER", "NORTHERN_HOGSUCKER")) %>%
    mutate(Species = str_replace(Species, "TROUT-PERCH", "TROUT_PERCH")) %>%
    mutate(Species = str_replace(Species, "TROUTPERCH", "TROUT_PERCH")) %>%
    drop_na(Latin) %>% # remove NA
    filter(!(Species %in% sp_rm) ) -> IA.dat_fish

  ## check difference from Kim's script - why pearl dace removed?
  ## one observation fewer than Kim's original file
  print(unique(IA.dat_fish$Species[IA.dat_fish$Species %in% sp_rm]) )


#' # Merge datasets
#' Merge `IA.dat_fish` and `IA.dat_site`
#' 
#' - `IA.dat_fish` as a base dataset because it includes fish data
#' - `IA.dat_site` is joined to obtain GPS location (Lat and Lon)

  # (1) Add visit and station data to fish data ---------------------------------
  IA.dat = IA.dat_fish %>%
     left_join(IA.dat_site, by = "SiteID") %>% # merge site data
     drop_na(SessionID) %>% # drop NA SessionID (unique visit ID) in dat_site
     mutate(State = "IA") %>% # add State column
     select(c(State, SiteID, SessionID, SampleDate, LatDD.y, LonDD.y, Species, TotalCount) ) %>% # select column
     rename(VisitID = SessionID, Abundance = TotalCount, Lat = LatDD.y, Lon = LonDD.y, Date = SampleDate) # rename column
  IA.dat$SiteID <- paste0("IA_", as.character(IA.dat$SiteID)) # attach State code in SiteID
  
  length(unique(IA.dat$VisitID) ) # n = 1277 correct
  length(unique(IA.dat$SiteID) ) # n = 696 correct

  # see the final data including all sampling occasion
  IA.dat # row number = 19686 correct


 # (2) Select the most recent sampling occasion by SiteID ---------------------------
  IA.dat.recent = IA.dat %>% 
    select(c(SiteID, VisitID, Date)) %>%
    group_by(SiteID) %>%
    slice(which.max(as.Date(Date, '%m/%d/%Y'))) %>% # select the most recent sampling occasion based on date
    right_join(IA.dat, by = "VisitID") %>% # merge the original data (right_join)
    drop_na(SiteID.x) %>% # drop NA SiteID
    select(c(State, SiteID.x, VisitID, Date.x, Lat, Lon, Species, Abundance)) %>% # select column
    rename(SiteID = SiteID.x, VisitID = VisitID, Date = Date.x) # change column name


#' # Final data 
  # Final data only included the latest sampling occasion    
  IA.dat.recent # row number = 10189 correct
  length(unique(IA.dat.recent$SiteID)) # n = 696 correct
  length(unique(IA.dat.recent$VisitID)) # n = 696 correct

 # Final data only included the latest sampling occasion
 IA.dat.recent # row number = 10189 correct

 
 # Export final data into data_output folder
 filename <- paste0("./data_outcome/fishdata_ia_latest", Sys.Date(), ".csv")
 write.csv(IA.dat.recent, filename)
