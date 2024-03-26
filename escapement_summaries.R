rm(list=ls())

library(openxlsx)
library(tidyverse)

#Read in site data-----------------------------------------------------------
#correct headings with spaces

esc <- read.xlsx("Data/West Coast Vancouver Island NuSEDS_20240110.xlsx", 
                    sheet = "WCVI_NuSEDS", 
                    detectDates = TRUE)
head(esc)

streams <- read.xlsx("Data/NuSEDs_MDFA.xlsx", 
                  sheet = "NuSEDs_MDFA", 
                  detectDates = TRUE)
mdfa.streams <- unique(streams$SYSTEM_SITE)

mdfa.esc <- esc %>% filter(WATERBODY %in% mdfa.streams)

sum5y <-mdfa.esc %>% filter(ANALYSIS_YR >=2019) %>%
  group_by(WATERBODY, SPECIES) %>%
  summarise(mean5y = mean(TOTAL_RETURN_TO_RIVER, na.rm = TRUE),
            n5y = sum(!is.na(TOTAL_RETURN_TO_RIVER))) %>%
  ungroup()

sum15y <-mdfa.esc %>% filter(ANALYSIS_YR >=2009) %>%
  group_by(WATERBODY, SPECIES) %>%
  summarise(mean15y = mean(TOTAL_RETURN_TO_RIVER, na.rm = TRUE),
            n15y = sum(!is.na(TOTAL_RETURN_TO_RIVER))) %>%
  ungroup()

stocks <- mdfa.esc %>% select(AREA, WATERBODY, SPECIES) %>% unique()

stock.sums <- left_join(stocks, sum5y, by = c("WATERBODY", "SPECIES")) %>%
  left_join(sum15y, by = c("WATERBODY", "SPECIES"))
