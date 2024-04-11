#Wendell Challenger's NuSEDs processing and tile plot production
#Adapted by Kaitlyn Manishin

##'Required R Scripts:
##'Prep_CU_tileplots.R #data manipulation
##'data_nuseds.R #reads in NuSEDS and does some cleaning
##'fig_spp_methods_tileplot.R #creates tile plot that show methods
##'fig_spp_pmax_tileplot.R #creates tile plot that shows relative abundance
##'
##'Required Data:---------------------
###'NuSEDs locations data-------------
##'conservation_unit_system_sites.csv 
##'https://open.canada.ca/data/en/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6/resource/0ba6773e-ddf6-3012-87bf-df707eb1ec4c
###'Area Specific NuSEDs csv-----------
##'https://open.canada.ca/data/en/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6
##'NuSEDs read in at line 12 of data_nuseds if you need to adjust file name
##'Don't upload NuSEDs to GitHub, its too big
###'List of streams and species of interest --------------
###'streams.csv
###' Read in here on line 43



#Session set up----------------------------------------------------------------

rm(list=ls())
library(tidyverse)
library(scales)
library(png)
library(jpeg)
library(grid)
library(gridExtra)
library(openxlsx)
library(extrafont)

loadfonts(device = "win")

view.data <- FALSE #Wendell relic

# LOAD DATA:  -----------------------------------------------------
#Read in conservation unit data from nuseds
mdfa_streams <- read.csv("Data/conservation_unit_system_sites.csv",
                         colClasses = c("SPECIES_QUALIFIED" = "factor"))

#read in list of streams identifies with First Nations
pssi <- read.csv("Data/streams.csv")
pssi$WATERBODY <- str_squish(pssi$WATERBODY) %>%str_to_upper()

# Settings ----------------------------------------------------------------
#!!Define your directories here
proj.dir <-  "~/Analyses" #Project directory, where R Project lives
fig.dir  <- "Output/pascale" #where you want you figures to be produced
tab.dir  <- "Output/tables" #where you want you tables to be produced
data.dir <- "Data" #where data lives
script.dir <- "Scripts" #where scripts list

#If table and figure directories to not exist this will create
if (!dir.exists(fig.dir)) dir.create(fig.dir, recursive = TRUE)
if (!dir.exists(tab.dir)) dir.create(tab.dir, recursive = FALSE)




for (spp in c("Chinook", "Coho", "Chum","Sockeye","Steelhead")) {
  # spp = "Chinook"
  message("Processing ", spp,  " CU data")
  source(file.path(script.dir, "Prep_CU_tileplots.R"))
 message(missed,  " data not available in NuSEDs")
  
  
  #--- FIGURE: Species Monitoring Map ---
  
  if (spp == "Pink") {
    spp.grp <- c("Pink (odd)", "Pink (even)")
  } else if (spp == "Sockeye") {
    spp.grp <- c("Sockeye (Lake)", "Sockeye (River)")
  } else {
    spp.grp = spp
  }
  
 
  # --- SPP NuSEDS Overview Figures
  # For the overview (Pmax and methods) tile plot figures River/Lake type
  # sockeye each have their own unique POPID / WATERBODY so we can display
  # in a single figure, but we still need separate Pink (even) Pink (Odd) figures
  if (spp == "Sockeye")   spp.grp <- spp
  
  for (i in seq_along(spp.grp)) {
    message(" - Creating ", spp.grp[i], " NuSEDS tile plots")
    
    spp.name <- str_replace_all(spp.grp[i], "[\\(\\)]","") %>% str_replace_all(pattern = "[:space:]", "_") %>% tolower
    
    # NuSEDS escapement for species of interest
    if (spp == "Pink") {
      spp.nuseds <- pssi.nuseds %>% filter(SppGroup == spp.grp[i])
    } else {
      spp.nuseds <- pssi.nuseds %>% filter(SPECIES == spp)
    }
    
    # FIGURE: Spp pmax escapement plot
    source(file.path(script.dir, "fig_spp_pmax_tileplot.R"))
    
    # FIGURE: spp methods tile plot
    source(file.path(script.dir, "fig_spp_methods_tileplot.R"))
    
    
  }
  
  
  # # NuSEDS escapement for species of interest
  # spp.nuseds <- pssi.nuseds %>% filter(SPECIES == spp)
  # 
  # # Site by epoch summaries
  # spp.sites <- sites.epoch %>% filter(SPECIES == spp)
  # 
  # # TABLE: CU Overview summary table
  # message(" - Creating CU overview summary table.")
  # source(file.path(script.dir, "table_CU_overview_v2.1.R"))
  # 
  # # Table: CU Stream methods/coverage table
  # message(" - Creating stream tables")
  # source(file.path(script.dir, "table_CU_streams_v2.1.R"))
  # 
  # # Compile: Species/group summary  
  # temp <- spp.sites %>% 
  #   group_by(SPECIES, SppGroup, Epoch) %>% 
  #   summarise(
  #     nSite = n(),
  #     nSiteCheck = length(unique(POP_ID)),
  #     Start = min(YearMin, na.rm=TRUE),
  #     End = max(YearMax, na.rm=TRUE),
  #     nMonitored = POP_ID[Status == "Reported Escapement"] %>% unique %>% length,
  #     Effort = sum(nYears, na.rm = TRUE),
  #     .groups="drop"
  #   )
  # results <- bind_rows(results, temp)
  # rm(temp)
  
}
