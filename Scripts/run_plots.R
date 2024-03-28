rm(list=ls())
library(tidyverse)
library(scales)
library(png)
library(jpeg)
library(grid)
library(gridExtra)
library(openxlsx)

library(extrafont)
# font_import(pattern = "Archivo") # Run on new systems to add font to R library
loadfonts(device = "win")

# Settings ----------------------------------------------------------------
view.data <- FALSE

proj.dir <-  "~/Analyses"
fig.dir  <- "Output/figures"
tab.dir  <- "Output/tables"
data.dir <- "Data"
script.dir <- "Scripts"

if (!dir.exists(fig.dir)) dir.create(fig.dir, recursive = TRUE)
if (!dir.exists(tab.dir)) dir.create(tab.dir, recursive = FALSE)


# LOAD DATA:  -----------------------------------------------------
#Read in conservation unit data from nuseds
mdfa_streams <- read.csv("Data/NuSEDs_MDFA.csv",
                         colClasses = c("SPECIES_QUALIFIED" = "factor"))

#read in list of streams identifies with First Nations
pssi <- read.csv("Data/PSSI_streams.csv")
pssi$WATERBODY <- str_squish(pssi$WATERBODY) %>%str_to_upper()


for (spp in c("Chinook", "Coho", "Chum","Sockeye","Steelhead")) {
  spp = "Chinook"
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
