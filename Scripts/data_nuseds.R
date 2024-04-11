
# NuSEDS CU Sites------------------------------------------------------------

nuseds.sites <- read_csv("Data/conservation_unit_system_sites.csv") 

# if (view.data) View(nuseds.sites)

# Compute Escapement ------------------------------------------------------
old.schema <- FALSE
# nuseds <- read_csv(file = file.path(proj.dir, "Data/NuSEDS/20230906/NuSEDS_20230906.csv"))

nuseds <- read_csv("Data/Johnstone Strait and Strait of Georgia NuSEDS_20240110.csv") %>% #WARNING about NATURAL_FEMALES, and _MALES, don't use these cols
  mutate(Name = str_to_title(WATERBODY)) %>%
  arrange(WATERBODY, SPECIES,ANALYSIS_YR) %>% 
  mutate(
    SPAWNERS_RECALC = ifelse(is.na(NATURAL_ADULT_SPAWNERS), 0, NATURAL_ADULT_SPAWNERS) + 
      ifelse(is.na(NATURAL_JACK_SPAWNERS), 0, NATURAL_JACK_SPAWNERS),
    SPAWNERS_RECALC = ifelse(if_all(NATURAL_ADULT_SPAWNERS:NATURAL_JACK_SPAWNERS, is.na), NA, SPAWNERS_RECALC),
    
    # Final spawner value 
    # Spawner type is often broken down, but sometimes it is only available as a total
    # when available by category, use the recalc from categories, otherwise if only the total 
    # is available we will use the total provided by DFO
    SPAWNERS = case_when(
      !is.na(SPAWNERS_RECALC) ~ SPAWNERS_RECALC,
      is.na(SPAWNERS_RECALC) & !is.na(NATURAL_SPAWNERS_TOTAL) ~ NATURAL_SPAWNERS_TOTAL,
      TRUE ~ 0
    ),
    
    BROODSTOCK_RECALC = ifelse(is.na(ADULT_BROODSTOCK_REMOVALS), 0, ADULT_BROODSTOCK_REMOVALS) + 
      ifelse(is.na(JACK_BROODSTOCK_REMOVALS), 0, JACK_BROODSTOCK_REMOVALS),
    BROODSTOCK_RECALC = ifelse(if_all(ADULT_BROODSTOCK_REMOVALS:JACK_BROODSTOCK_REMOVALS, is.na), NA, BROODSTOCK_RECALC),
    
    BROODSTOCK = case_when(
      !is.na(BROODSTOCK_RECALC) ~ BROODSTOCK_RECALC,
      is.na(BROODSTOCK_RECALC) & !is.na(TOTAL_BROODSTOCK_REMOVALS) ~ TOTAL_BROODSTOCK_REMOVALS,
      TRUE ~ 0
    ),
    
    
    TOTAL_RETURN_NEW =  ifelse(is.na(SPAWNERS), 0, SPAWNERS) + 
      ifelse(is.na(BROODSTOCK), 0, BROODSTOCK) +
      ifelse(is.na(OTHER_REMOVALS), 0, OTHER_REMOVALS),
    TOTAL_RETURN_NEW  = ifelse(if_all(NATURAL_ADULT_SPAWNERS:OTHER_REMOVALS, is.na), NA, TOTAL_RETURN_NEW ),
    
    TOTAL_RETURN_RECALC = case_when(
      !is.na(TOTAL_RETURN_NEW) ~ TOTAL_RETURN_NEW,
      is.na(TOTAL_RETURN_NEW) & !is.na(TOTAL_RETURN_TO_RIVER) ~ TOTAL_RETURN_TO_RIVER
    )
  ) %>%
  mutate(
    Escapement = ifelse(rep(old.schema, length(WATERBODY)), MAX_ESTIMATE, TOTAL_RETURN_RECALC),
    Year = ANALYSIS_YR,
    RunType = case_when(
      is.na(RUN_TYPE) & str_detect(toupper(POPULATION), "SPRING" ) ~ "Spring",    # "Spring/Summer",
      is.na(RUN_TYPE) & str_detect(toupper(POPULATION), "SUMMER" ) ~ "Summer",    # "Spring/Summer", 
      toupper(RUN_TYPE) == "SPRING" ~ "Spring",    # "Spring/Summer",
      toupper(RUN_TYPE) == "SUMMER" ~ "Summer",    # "Spring/Summer",
      toupper(RUN_TYPE) == "FALL" ~ "Fall",
      RUN_TYPE == 1 ~ "Fall",
      RUN_TYPE == 2 ~  "Spring", #   "Spring/Summer", # Validate (Amor De COSMOS)   !!! WARNING
      is.na(RUN_TYPE) ~ "Fall"
      # TRUE ~ "FALL"
    )
  ) %>%
  group_by(WATERBODY, SPECIES, RunType) %>%
  mutate(Esc.Scaled = scale(Escapement)[,1]) %>%
  ungroup %>%
  mutate(
    # IQR = p75-p25,
    EscCat = case_when(
      Esc.Scaled > 1 ~ "High",
      Esc.Scaled < -1 ~ "Low",
      !is.na(Esc.Scaled) ~ "Avg"
    ),
    EscCat = factor(EscCat, levels = c("Low", "Avg", "High"), labels = c("Low", "Avg", "High")),
  )

