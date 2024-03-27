rm(list=ls())

library(tidyverse)
library(stringr)

source('Prep_CU_tileplots.R')

# Waterbody Masterlist + Ordering ------------------------------------------
# List of unqiue water bodies that will be displayed in the report which
# we will use to map on axis formatting so that we can highlight the 
# indicator streams. 
#
# We will also re-order the water bodies to be alphabetically
# ordered in each CU allowing us to draw horizontal lines to indicate on the
# figure CU membership


name.master <- pssi.nuseds %>% 
  # mutate(WATERBODY = toupper(WATERBODY)) %>%  # fix inconsistent Clearwater Creek
  group_by(AREA, WATERBODY, GAZETTED_NAME, LOCAL_NAME_1,LOCAL_NAME_2,SYSTEM_SITE,
           RUN_TYPE, POP_ID, CU_INDEX, CU_NAME, SPECIES_QUALIFIED, IS_INDICATOR, SppGroup) %>% 
  summarize(
    EscAvailable = any(Escapement >0),
    .groups = "drop"
  )

dim(name.master)

(check <- name.master %>% count(WATERBODY) %>% filter(n>1))
(check <- name.master %>% count(WATERBODY, RUN_TYPE, SppGroup) %>% count(WATERBODY) %>%  filter(n>1))

name.master %>% filter(WATERBODY != SYSTEM_SITE)

name.master <- name.master %>% 
  mutate(
    DisplayName = case_when(
      WATERBODY %in% check$WATERBODY ~ paste0(str_to_title(WATERBODY), " (Run", RUN_TYPE, SppGroup, ")"),
      TRUE~str_to_title(WATERBODY)
    )
  ) %>%
  distinct(AREA, WATERBODY, SYSTEM_SITE,DisplayName, POP_ID, IS_INDICATOR, 
           CU_INDEX, CU_NAME, EscAvailable, SppGroup) %>% #NOTE: overlapping PINK even and PINK odd?
  arrange(CU_INDEX, DisplayName)  %>% 
  mutate(
    FontFace = ifelse(IS_INDICATOR == "Y", "bold", "plain"),
    LineWidth = ifelse(IS_INDICATOR == "Y", 0.50,0.25),
    LineColor = ifelse(IS_INDICATOR == "Y","grey60","grey92")
  ) %>% 
  mutate(Order = seq_along(WATERBODY) %>% rev) %>% 
  arrange(Order) 


# CU Cuts -----------------------------------------------------------------
# Basede on the the waterbody name ordering and the CU membership determine
# the placement of horizontal lines we will draw on the figure, lines 
# will be placed between tiles so shifted by 0.5 lines.

cu.cuts <- which(duplicated(name.master$CU_INDEX) == FALSE) %>% tail(n=-1) - 0.5


# Create Plot Data --------------------------------------------------------
# Escapement status adds on a the classes of escapement record to the 
# atlegay.nuseds object. We don't modify the original object because we will
# be adding records with the complete statement.



esc.status <- 
  left_join(
  x = pssi.nuseds %>% mutate(WATERBODY = toupper(WATERBODY)),
  y = name.master %>% select(WATERBODY, POP_ID, DisplayName, SppGroup),
  by = join_by(WATERBODY, POP_ID, SppGroup)
) %>%
  mutate(
    StreamName = factor(
      x = DisplayName,
      levels = name.master$DisplayName
    )
  ) %>%
  mutate(
    EscAvail = case_when(
      is.na(Escapement) ~ FALSE,
      !is.na(Escapement) & Escapement == 0 ~ FALSE,
      !is.na(Escapement) & Escapement > 0 ~ TRUE
    ),
    # First Version ---
    # Status = case_when(
    #   !EscAvail & ADULT_PRESENCE %in% c("NOT INSPECTED", "UNKNOWN") ~ "Not Inspected",
    #   !EscAvail & is.na(ADULT_PRESENCE) ~ "Not Inspected",
    #   !EscAvail & ADULT_PRESENCE == "NONE OBSERVED" ~ "No Escapement",
    #   !EscAvail & ADULT_PRESENCE == "PRESENT" ~ "Escapement",
    #   EscAvail & ADULT_PRESENCE == "PRESENT" ~ "Escapement",
    #   EscAvail & ADULT_PRESENCE == "NONE OBSERVED" ~ "Escapement",  # DB Error
    #   TRUE ~ "Other"
    # ),
    Status = case_when(
      !EscAvail & ADULT_PRESENCE %in% c("NOT INSPECTED", "UNKNOWN") ~ "Not Inspected",
      !EscAvail & is.na(ADULT_PRESENCE)                             ~ "Not Inspected",
      !EscAvail & ADULT_PRESENCE == "NONE OBSERVED"                 ~ "No Escapement",
      !EscAvail & Escapement > 0 & ADULT_PRESENCE == "PRESENT"      ~ "Escapement",
      !EscAvail & Escapement == 0 & ADULT_PRESENCE == "PRESENT"     ~ "No Escapement",
      EscAvail & ADULT_PRESENCE == "PRESENT"                        ~ "Escapement",
      EscAvail & ADULT_PRESENCE == "NONE OBSERVED"                  ~ "Escapement",  # DB Error
      TRUE ~ "Other"
    ),
    # Status = factor(Status, levels = c("Not Inspected", "No Escapement", "Escapement")),
    MethodType = case_when(
      str_detect(ESTIMATE_CLASSIFICATION, "TYPE-6") ~ "Presence/Absence",
      str_detect(ESTIMATE_CLASSIFICATION, "TYPE-5") ~ "Relative Abundance",
      str_detect(ESTIMATE_CLASSIFICATION, "TYPE-4") ~ "Relative Abundance",
      str_detect(ESTIMATE_CLASSIFICATION, "TYPE-3") ~ "Relative Abundance",
      str_detect(ESTIMATE_CLASSIFICATION, "TYPE-2") ~ "True Abundance", 
      str_detect(ESTIMATE_CLASSIFICATION, "TYPE-1") ~ "True Abundance",
      str_detect(ESTIMATE_CLASSIFICATION, "RELATIVE") ~ "Relative Abundance",
      str_detect(ESTIMATE_CLASSIFICATION, "UNKNOWN") ~ "Unknown"
    )
  )  %>% 
  complete(SPECIES, ANALYSIS_YR, DisplayName, fill = list(Status = "No Record"))


n.display <- esc.status %>% distinct(StreamName) %>% nrow

if (n.display != nrow(name.master)) stop("Species specific method figure data is missing display streams")

# CU Labels ---------------------------------------------------------------
# Create the CU labels and determine text positions to occur mid way within
# each CU aggregation.


# Prepare CU names and positions for geom_text
cu.labels <- name.master %>% 
  mutate(Duplicated = duplicated(CU_INDEX)) %>% 
  filter(!Duplicated | Order == max(Order)) %>% 
  mutate(
    midpoint = c( (tail(Order, n = -1) + head(Order, n=-1)) / 2 - 0.5, NA)
  ) %>% 
  filter(!is.na(midpoint)) %>% 
  select(CU_INDEX, CU_NAME, midpoint)

buffer <- cu.labels$CU_INDEX %>% nchar %>% max


# CREATE PLOT -------------------------------------------------------------

p.monitoring <-   esc.status %>% 
  mutate(
    StatusMethod = ifelse(
      test = Status == "Escapement" ,
      yes = MethodType,
      no =  Status
    ),
    # StatusMethod = factor(StatusMethod, levels = c("No Record", "Not Inspected", "No Escapement", "Unknown", "Presence/Absence", "Relative Abundance","True Abundance"))
    StatusMethod = factor(
      x=StatusMethod, 
      levels = c("True Abundance",  "Relative Abundance", "Presence/Absence", "Unknown", "No Escapement",  "Not Inspected", "No Record"  ),
      labels =  c("True Abundance",  "Relative Abundance", "Presence/Absence", "Unknown", "No Escapement",  "Not Inspected/No Record", "Not Inspected/No Record"  )
    )
  ) %>% 
  count(StreamName, ANALYSIS_YR, StatusMethod, SPECIES) %>%
  # filter(ANALYSIS_YR > 1954) %>%
  ggplot(aes(x = ANALYSIS_YR, y =StreamName)) +
  geom_tile(aes(fill = StatusMethod)) +
  geom_vline(xintercept = 1953, linetype = "dotted") +
  geom_vline(xintercept = c(1995), linetype = "solid") +
  geom_vline(xintercept = 2005, linetype = "dotdash") +
  geom_hline(yintercept = cu.cuts, linetype = "longdash")+
  # scale_y_discrete(limits=rev) +
  # scale_x_continuous(breaks = pretty_breaks(n=8)) +
  scale_x_continuous(breaks =seq(1930, 2020, by = 10)) +
  # expand_limits(x=2026)+
  scale_fill_manual(
    values = c(
      'Not Inspected/No Record' = rgb(0,0,0,alpha=0),
      'No Escapement'      = "purple",      #"black",
      'Presence/Absence'   = "#FDE725FF",  "#F7FCB9",  #"red", 
      'Relative Abundance' = "#5DC863FF",  "#ADDD8E",  #"orange",
      'True Abundance'     = "#21908CFF",  "#31A354",  #"springgreen4",
      "Unknown"            = "grey80"   #"grey75"
    )
  ) +
  guides(color="none") +
  theme_classic(14)+
  theme(
    plot.margin = unit(c(0.5, buffer, 0.5, 0.5), units = "lines") ,
    axis.title = element_blank(),
    axis.text.y = element_text(size = 9, face = name.master$FontFace),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major.y = element_line(linewidth = name.master$LineWidth , colour =name.master$LineColor)
  )
p.monitoring



# ADD CU LABELS --------------------------------------------------------------
# Allow CU labels to be placed in the plot margins
p.monitoring <-  p.monitoring + coord_cartesian(clip = "off") 


# In the margin the y-axis seems to be +/- the max y-value, even though the
# plot display range is different.  As such we will need to convert the original
# position calculation [0, n] to the new support [-n, n], where 
# n = nrow(name.master)

cu.labels <- cu.labels %>% 
  mutate(
    midpoint_adj = midpoint*2 - nrow(name.master),
    # Nudge calculation
    midpoint_adj = case_when(
      midpoint_adj < 0  ~  midpoint_adj - 0.5,
      midpoint_adj == 0 ~ midpoint_adj,
      midpoint_adj > 0  ~ midpoint_adj + 0.5
    )
  )

for (i in seq_len(nrow(cu.labels))) {
  p.monitoring <-  p.monitoring  + 
    annotation_custom(
      grob = grid::textGrob(cu.labels$CU_INDEX[i], gp = gpar(fontsize = 12, fontface = "bold")), 
      xmin = 2037, 
      ymin = cu.labels$midpoint_adj[i]
    )
}



# Historical Periods ------------------------------------------------------


# p.monitoring + annotate(
#   geom = "segment", 
#   x = 1953, 
#   y = nrow(name.master) +1, 
#   xend = 1994, 
#   yend = nrow(name.master)+1,
#   arrow = arrow(type = "closed", length = unit(0.02, "npc")))


# SAVE --------------------------------------------------------------------
out.file = file.path(fig.dir,paste0(tolower(spp.name), "-methods_tileplot.png"))
ggsave(filename = out.file,  width=10, height= nrow(name.master)*0.2 + 2, plot = p.monitoring)
