
# NuSEDS CU Sites------------------------------------------------------------

nuseds.sites <- read_csv("Data/conservation_unit_system_sites.csv") 

# if (view.data) View(nuseds.sites)

# Compute Escapement ------------------------------------------------------
old.schema <- FALSE
# nuseds <- read_csv(file = file.path(proj.dir, "Data/NuSEDS/20230906/NuSEDS_20230906.csv"))

nuseds <- read_csv("Data/NuSEDS_20240110.csv") %>% #WARNING about NATURAL_FEMALES, and _MALES, don't use these cols
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


# FIX: Clearwater Creek ----------------------------------------------------
# Currently Clearwater Creek in the Phillips River system has escapement 
# values in the NuSEDS data base, but the POP_ID, which indicates a unique
# population is the same as Phillips River. These appear to be separte 
# estimates.

# Check if number space to remap clearwater creek popid to will fail. We will 
# simply add a large number to the Phillips popid.
unique.id <- nuseds %>% distinct(POP_ID) %>% unlist
check <- nuseds %>% filter(POP_ID >= 1e6 & POP_ID < 2e6) %>% distinct(POP_ID) %>% unlist
if (length(check) > 0) stop("Clearwater Creek POP_ID mapping failure")


# Search NU
clearwater.phillips.id <- nuseds %>% 
  filter(toupper(WATERBODY) == "CLEARWATER CREEK" & str_detect(POPULATION, "Phillips River")) %>% 
  distinct(POP_ID) %>% 
  unlist

# Create a new POP_ID
clearwater.site <- nuseds.sites %>% 
  filter(POP_ID %in% clearwater.phillips.id) %>% 
  mutate(
    SYSTEM_SITE = "CLEARWATER CREEK",
    POP_ID = POP_ID + 1e6,
    IS_INDICATOR = "N"
  )

nuseds.sites <- nuseds.sites %>% add_row(clearwater.site)
  



# Clearwater Creek fixes
nuseds <- nuseds %>% 
  mutate(
    WATERBODY = case_when(
      WATERBODY == "Clearwater Creek" ~ toupper(WATERBODY), 
      TRUE ~ WATERBODY
    ),
    POP_ID = case_when(
      test =  POP_ID %in% clearwater.phillips.id &   WATERBODY == toupper("Clearwater Creek")  ~ POP_ID + 1e6,
      TRUE ~ POP_ID
    )
  ) 


# nuseds %>% filter(WATERBODY %in% c("PHILLIPS RIVER", "CLEARWATER CREEK")) %>% count(WATERBODY, POP_ID, POPULATION)






# 
# x <- nuseds %>%
#   # filter(str_detect(WATERBODY, "e")) %>% view()
#   mutate(
#     WATERBODY = case_when(
#       WATERBODY == "Clearwater Creek" ~ toupper(WATERBODY),
#       TRUE ~ WATERBODY
#     ),
#     POP_ID = case_when(
#       test =  WATERBODY == "CLEARWATER CREEK" & str_detect(POPULATION, "Phillips River") ~ POP_ID +0.1,
#       TRUE ~ POP_ID
#     )
#   ) %>%
#   filter(WATERBODY == "CLEARWATER CREEK") %>% count(WATERBODY, POP_ID, POPULATION)
# x2 <- nuseds %>%
#   # filter(str_detect(WATERBODY, "e")) %>% view()
#   mutate(
#     WATERBODY = case_when(
#       WATERBODY == "Clearwater Creek" ~ toupper(WATERBODY),
#       TRUE ~ WATERBODY
#     ),
#     POP_ID = case_when(
#       test =  POP_ID %in%  clearwater.id ~ POP_ID +0.1,
#       TRUE ~ POP_ID
#     )
#   ) %>%
#   filter(WATERBODY == "CLEARWATER CREEK") %>% count(WATERBODY, POP_ID, POPULATION)
# 
# identical(x, x2)
# 
# 
# # Testing CU meta join
# left_join(
#   x = x,
#   y = nuseds.sites %>% add_row(clearwater.site) %>% select(POP_ID, SYSTEM_SITE, CU_INDEX)
# )
