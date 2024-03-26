
nuseds <- read_csv(file = "Data/NuSEDS_20240110.csv")
nuseds$WATERBODY <- str_to_title(nuseds$WATERBODY)
mdfa_streams <- read.csv("Data/NuSEDs_MDFA.csv",
                         colClasses = c("SPECIES_QUALIFIED" = "factor"))
# names(mdfa_streams)[names(mdfa_streams) == 'SPECIES_QUALIFIED'] <- 'SPECIES'

pssi <- read.csv("Data/PSSI_streams.csv")
pssi$WATERBODY <- str_squish(pssi$WATERBODY) %>%str_to_upper()

# mdfa_streams$SPECIES <- fct_recode(mdfa_streams$SPECIES,
#                                    Chum = "CM", Coho ="CO", Pink = "PKE", Chinook = "CK",
#                                    Pink = "PKO", Sockeye = "SEL", Sockeye = "SER")

#all streams in MDFA----------------------------------------------------------
mdfa.sites <- mdfa_streams %>%  
  select(SYSTEM_SITE, Y_LAT, X_LONGT,SPECIES_QUALIFIED, POP_ID, GFE_TYPE, 
         CU_NAME, FULL_CU_IN ,JAZ_ACRO, IS_INDICATOR, WATERSHED_CDE, FWA_WATERSHED_CDE) %>% 
  unique 

#PSSI interest sites---------------------------------------------------------
pssi.sites <-  mdfa.sites %>%  
  filter(SYSTEM_SITE %in% (pssi$WATERBODY))  %>%
  # mutate(CU_INDEX = paste(SPECIES_QUALIFIED, CU_INDEX, sep ="-")) %>% 
  rename(CU_INDEX = FULL_CU_IN) %>% 
  add_column(SPECIES = NA, .before = 'SPECIES_QUALIFIED') %>% 
  add_column(SppGroup = NA, .after = 'SPECIES_QUALIFIED') %>% 
  mutate(
    SPECIES = case_when(
      is.na(SPECIES) & SPECIES_QUALIFIED == "PKE" ~ "Pink",
      is.na(SPECIES) & SPECIES_QUALIFIED == "PKO" ~ "Pink",
      is.na(SPECIES) & SPECIES_QUALIFIED == "CM"  ~ "Chum",
      is.na(SPECIES) & SPECIES_QUALIFIED == "CO"  ~ "Coho",
      is.na(SPECIES) & SPECIES_QUALIFIED == "CK"  ~ "Chinook",
      is.na(SPECIES) & SPECIES_QUALIFIED == "SEL" ~ "Sockeye",
      is.na(SPECIES) & SPECIES_QUALIFIED == "SER" ~ "Sockeye"
    ),
    SppGroup = case_when(
      SPECIES  %in% c("Pink", "Sockeye") == FALSE    ~ SPECIES,
      SPECIES == "Pink" & SPECIES_QUALIFIED == "PKE" ~ "Pink (even)",
      SPECIES == "Pink" & SPECIES_QUALIFIED == "PKO" ~ "Pink (odd)",
      SPECIES == "Sockeye" & SPECIES_QUALIFIED == "SEL" ~ "Sockeye (Lake)",
      SPECIES == "Sockeye" & SPECIES_QUALIFIED == "SER" ~ "Sockeye (River)"
    ),
  ) %>% 
  arrange(SYSTEM_SITE, SppGroup)


# Add indicator status, this cannot be done earlier as we need all the
# unique locations and multiple runs can occur at each
# atlegay.sites <- left_join(
#   x = atlegay.sites,
#   y = nuseds.sites %>% distinct(SPECIES_QUALIFIED, POP_ID, IS_INDICATOR),
#   by = join_by(SPECIES_QUALIFIED, POP_ID)
# )


# if (view.data) {
#   pssi.sites %>% count(SPECIES_QUALIFIED,CU_NAME) %>% spread(SPECIES_QUALIFIED, n) %>% View(title="waterbody pops")
# }

pssi.sites.wide <- pssi.sites %>% 
  count(SYSTEM_SITE,Y_LAT, X_LONGT, WATERSHED_CDE, FWA_WATERSHED_CDE, SPECIES_QUALIFIED, POP_ID) %>% 
  select(-n) %>%
  group_by(SYSTEM_SITE,Y_LAT, X_LONGT,  WATERSHED_CDE, FWA_WATERSHED_CDE, SPECIES_QUALIFIED) %>%
  summarize(POP_ID = paste(POP_ID, collapse = ", "), .groups = "drop")%>% 
  spread(SPECIES_QUALIFIED, POP_ID) 

# if (view.data){
#   pssi.sites.wide %>% select(WATERBODY, CK:SER) %>% View(title = "POP_ID by Waterbody")
# }

# mdfa.sites %>% 
#   distinct(SYSTEM_SITE,Y_LAT, X_LONGT) %>% 
#   write_csv( file = "Output/mdfa-QUERY-sites.csv")

# write_csv(pssi.sites, file = "Output/pssi-waterbodies.csv")
# pssi.sites.wide %>% select(SYSTEM_SITE, CK:SER) %>% 
#   write_csv(file = "Output/atlegay-mainland-waterbody-popid.csv")


# View(mdfa.sites)
# View(pssi.sites)
# View(pssi.sites.wide)

# DEFINE: PSSI NuSEDS -------------------------------------------------
# Updating the filtering method from version 3, 
source('data_nuseds.R')

pssi.nuseds <- nuseds %>% 
  # filter(WATERBODY %in% atlegay.sites$SYSTEM_SITE) %>%
  filter(POP_ID %in% pssi.sites$POP_ID) %>%
  filter(!is.na(Escapement)) %>%
  filter(SPECIES %in% c("Chinook", "Chum", "Coho", "Pink", "Sockeye")) %>% 
  mutate(
    WATERBODY = toupper(WATERBODY), # fix inconsistent capitalization of Clearwater Creek
    Epoch = case_when(
      ANALYSIS_YR  < 1995                       ~ "Historical",
      ANALYSIS_YR >= 1995 & ANALYSIS_YR <= 2004 ~ "Transition",
      ANALYSIS_YR >= 2005                       ~ "Recent"
    ),
    YearType = case_when(
      SPECIES != "Pink"                         ~ "Normal",
      SPECIES == "Pink" & ANALYSIS_YR %% 2 == 0 ~ "Even",
      SPECIES == "Pink" & ANALYSIS_YR %% 2 == 1 ~ "Odd"
    )
  ) %>% 
  # filter(WATERBODY %in% c("PHILLIPS RIVER", "CLEARWATER CREEK")) %>% count(WATERBODY, POP_ID, Epoch) %>% spread(Epoch,n) %>% View
  # # Add PKE/PKO and Species group to identify Even/Odd pinks, but
  # but we also want to group all sockeye types.
  left_join(
    x = .,
    y =  pssi.sites %>% 
      select(POP_ID, SPECIES_QUALIFIED, SppGroup, CU_NAME, CU_INDEX, SYSTEM_SITE, IS_INDICATOR) %>% 
      mutate(
        YearType = case_when(
          SPECIES_QUALIFIED == "PKE" ~ "Even",
          SPECIES_QUALIFIED == "PKO" ~ "Odd",
          TRUE                       ~ "Normal"
        )
      ),
    by = join_by(POP_ID ,YearType)
  ) 


# Add PMax values relative to entire time series
pssi.nuseds <- left_join(
  x = pssi.nuseds,
  y = pssi.nuseds %>% 
    group_by(WATERBODY, POP_ID, SppGroup) %>%  # Adding waterbody should capture clearwater creek, while SppGroup will capture pink even/odd
    summarise(
      MaxEsc = max(Escapement, na.rm=T),
      .groups = "drop"
    ),
  by = join_by(WATERBODY, POP_ID, SppGroup)
) %>% 
  mutate(Pmax = Escapement/MaxEsc)


pssi.nuseds <- left_join(
  x = pssi.nuseds,
  # Max escapement by Epoch
  y = pssi.nuseds %>% 
    group_by(WATERBODY,POP_ID, SppGroup, Epoch) %>% 
    summarise(MaxEsc_Epoch = max(Escapement, na.rm=T), .groups = "drop"),
  by = join_by(WATERBODY,POP_ID, SppGroup, Epoch)
) %>% 
  mutate(Pmax_Epoch = Escapement/MaxEsc_Epoch)




if (view.data){
  pssi.nuseds %>% 
    select(AREA, WATERBODY, ANALYSIS_YR, Epoch, SPECIES, SppGroup, Escapement, MaxEsc, Pmax, MaxEsc_Epoch, Pmax_Epoch) %>% 
    View(title = "pssi.nuseds")
}

