rm(list=ls())

library(tidyverse)
library(stringr)

nuseds <- read_csv(file = "Data/NuSEDS_20240110.csv")
nuseds$WATERBODY <- str_to_title(nuseds$WATERBODY)
mdfa_streams <- read.csv("Data/NuSEDs_MDFA.csv",
                         colClasses = c("SPECIES_QUALIFIED" = "factor"))
names(mdfa_streams)[names(mdfa_streams) == 'SPECIES_QUALIFIED'] <- 'SPECIES'

pssi <- read.csv("Data/PSSI_streams.csv")
pssi$WATERBODY <- str_squish(pssi$WATERBODY)

mdfa_streams$SPECIES <- fct_recode(mdfa_streams$SPECIES, 
                            Chum = "CM", Coho ="CO", Pink = "PKE", Chinook = "CK",
                            Pink = "PKO", Sockeye = "SEL", Sockeye = "SER")
mdfa_streams <- mdfa_streams %>% filter(SPECIES != "Pink")

nuseds.mdfa <- nuseds %>% left_join(mdfa_streams, by = c('WATERBODY', 'SPECIES'))

old.schema <- ifelse("MAX_ESTIMATE" %in% colnames(nuseds),TRUE, FALSE)

esc.dat <- nuseds.mdfa %>%
  filter(WATERBODY %in% str_squish(pssi$WATERBODY)) %>%
  arrange(WATERBODY, SPECIES,ANALYSIS_YR) %>% 
  mutate(
    # Recalculated total spawners treating 
    SPAWNERS_RECALC = ifelse(is.na(NATURAL_ADULT_SPAWNERS), 0, NATURAL_ADULT_SPAWNERS) + 
      ifelse(is.na(NATURAL_JACK_SPAWNERS), 0, NATURAL_JACK_SPAWNERS),
    SPAWNERS_RECALC = ifelse(if_all(NATURAL_ADULT_SPAWNERS:NATURAL_JACK_SPAWNERS, is.na), NA, SPAWNERS_RECALC),
    
    # Indicated if the recalculated spawners differs from DFO's reported total natural spawners
    SPAWNER_FLAG = case_when(
      is.na(SPAWNERS_RECALC) ~ FALSE,
      is.na(NATURAL_SPAWNERS_TOTAL) ~ FALSE,
      TRUE ~ SPAWNERS_RECALC != NATURAL_SPAWNERS_TOTAL
    ),
    
    # --- COMPUTE: Final Spawner Value ---
    # Spawners numbers can be reported by category or occasionally only as a 
    # total depending on the stock assessment biologist.  When spawning is broken
    # down by category use the Recalculated spawner total, otherwise when use
    # the total spawners provided by DFO as this is the only available number.
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
    # Indicated if the recalculated spawners differs from DFO's reported total natural spawners
    BROODSTOCK_FLAG = case_when(
      is.na( BROODSTOCK_RECALC) ~ FALSE,
      is.na(TOTAL_BROODSTOCK_REMOVALS) ~ FALSE,
      TRUE ~ BROODSTOCK_RECALC != TOTAL_BROODSTOCK_REMOVALS
    ),
    
    TOTAL_RETURN_NEW =  ifelse(is.na(SPAWNERS), 0, SPAWNERS) + 
      ifelse(is.na(BROODSTOCK), 0, BROODSTOCK) +
      ifelse(is.na(OTHER_REMOVALS), 0, OTHER_REMOVALS),
    # set to NA if all subcategories are missing (due to replacing missing values with zeros)
    TOTAL_RETURN_NEW  = ifelse(if_all(NATURAL_ADULT_SPAWNERS:OTHER_REMOVALS, is.na), NA, TOTAL_RETURN_NEW ),
    
    TOTAL_RETURN_RECALC = case_when(
      !is.na(TOTAL_RETURN_NEW) ~ TOTAL_RETURN_NEW,
      is.na(TOTAL_RETURN_NEW) & !is.na(TOTAL_RETURN_TO_RIVER) ~ TOTAL_RETURN_TO_RIVER
    )
  ) %>%
  mutate(
    Escapement = ifelse(rep(old.schema, length(WATERBODY)), MAX_ESTIMATE, TOTAL_RETURN_RECALC),
    Year = ANALYSIS_YR,
    # RunType = ifelse(RUN_TYPE == "FALL", "Fall", "Summer"),
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
    # RunType = factor(RunType, levels = c("Spring", "Summer", "Fall"))
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
  ) %>%
  # Mark priority streams
  mutate(Priority = WATERBODY %in% pssi$WATERBODY) %>%
  mutate(
    EscCat2 = case_when(
      EscCat == "Low" & Priority   ~ "PriorityLow",
      EscCat == "Low" & !Priority  ~ "Low",
      EscCat == "Avg" & Priority   ~ "PriorityAvg",
      EscCat == "Avg" & !Priority  ~ "Avg",
      EscCat == "High" & Priority  ~ "PriorityHigh",
      EscCat == "High" & !Priority ~ "High",
    )
  ) %>%
  filter(SPECIES %in% c("Atlantic", "Pink") == FALSE)


# Substitutions -----------------------------------------------------------



# Removals & Substitutions ----------------------------------------------------------------

# Remove Nanaimo from Cowichan Assessment Unit Reprt
# if (watershed == "Cowichan") {
#   esc.dat <- esc.dat %>%
#     filter(!str_detect( WATERBODY, "Nanaimo")) %>%
#     mutate(RunType = str_replace(RunType, "Summer","Early and Summer"))
# }



# Priority Stream Infill ---------------------------------------------------
# If required we need to add blank records for priority streams not currently
# covered in NuSEDS

# Determine Priority Streams available in data
present.streams <- esc.dat %>% 
  # filter(!is.na(Escapement)) %>%
  select(WATERBODY) %>%
  unique()

miss.priority <- setdiff(str_squish(pssi$WATERBODY), present.streams$WATERBODY)


# Update NuSEDS Escapement 

# Complete any missing priority streams
if (exists('miss.priority')) {
  if (length(miss.priority)>0) {
    
    # warning("Priority infill uses RunType = 'Spring' instead of 'Fall' - Cowichan workaround.")
    
    esc.dat <- esc.dat %>%
      complete(
        WATERBODY = miss.priority,
        SPECIES = "Chinook",
        Year = 2020,
        RunType = "Fall",
        Priority = TRUE
      ) %>%
      mutate(
        RunType = factor(RunType, levels = c("Early and Summer", "Spring", "Summer", "Fall"))
      )
  }
} 


