
# Waterbody Masterlist + Ordering ------------------------------------------
# List of unqiue water bodies that will be displayed in the report which
# we will use to map on axis formatting so that we can highlight the 
# indicator streams. 
#
# We will also re-order the water bodies to be alphabetically
# ordered in each CU allowing us to draw horizontal lines to indicate on the
# figure CU membership


name.master <- spp.nuseds %>% 
  mutate(WATERBODY = toupper(WATERBODY)) %>%  # fix inconsistent Clearwater Creek
  group_by(AREA, WATERBODY, GAZETTED_NAME, LOCAL_NAME_1,LOCAL_NAME_2, SYSTEM_SITE, RUN_TYPE, POP_ID, CU_INDEX, CU_NAME, SPECIES_QUALIFIED, IS_INDICATOR) %>% 
  summarize(
    EscAvailable = any(Escapement >0),
    .groups = "drop"
  )

dim(name.master)

(check <- name.master %>% count(WATERBODY) %>% filter(n>1))
(check <- name.master %>% count(WATERBODY, RUN_TYPE) %>% count(WATERBODY) %>%  filter(n>1))

name.master %>% filter(WATERBODY != SYSTEM_SITE)

name.master <- name.master %>% 
  mutate(
    DisplayName = case_when(
      str_to_title(WATERBODY) == "Henderson Lake" ~ "Hucuktlis Lake", 
      str_to_title(WATERBODY) == "Little Toquart Creek" ~ "Little Toquaht Creek", 
      str_to_title(WATERBODY) == "Little Toquart Creek" ~ "Little Toquaht Creek", 
      str_to_title(WATERBODY) == "Toquart River" ~ "Toquaht River", 
      str_to_title(WATERBODY) == "Power River" ~ "Hisnit River", 
      str_to_title(WATERBODY) == "Coeur D'alene Creek" ~ "Coeur D'Alene Creek",
      WATERBODY %in% check$WATERBODY ~ paste0(str_to_title(WATERBODY), " (Run", RUN_TYPE, ")"),
      TRUE~str_to_title(WATERBODY)
    )
  ) %>% 
  distinct(AREA, WATERBODY, SYSTEM_SITE, DisplayName, POP_ID, IS_INDICATOR, CU_INDEX, CU_NAME, EscAvailable) %>% #NOTE: overlapping PINK even and PINK odd?
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
# Use the master list in order to determine the display order of the 
# waterbody names so that we can draw the CU membership breaks.
#
# Use a 4 bins to categorize the Pmax values.
#
pmax.dat <- left_join(
  x = spp.nuseds,
  y = name.master %>% select(WATERBODY, POP_ID, DisplayName),
  by = join_by(WATERBODY, POP_ID)
) %>% 
  mutate(
    StreamName = factor(
      x = DisplayName, 
      levels = name.master$DisplayName
    )
  ) %>% 
  mutate(
    PmaxCat = case_when(
      Pmax < 0.25 ~ "Low",
      Pmax < 0.5 ~ "Med-Low",
      Pmax < 0.75  ~ "Med-High",
      Pmax <= 1 ~ "High",
    ),
    PmaxCat = factor(
      x =PmaxCat, 
      levels = c("Low", "Med-Low","Med-High", "High"),
      labels = c("Low [0, 0.25)", "Med/Low [0.25, 0.50)", "Med/High [50, 0.75)", "High [0.75, 1]")
    )
  ) %>% 
  select(AREA:SPECIES, StreamName, CU_INDEX, CU_NAME, PmaxCat)

n.display <- pmax.dat %>% distinct(StreamName) %>% nrow

if (n.display != nrow(name.master)) stop("CU Pmax missing display streams")


# CU Labels ---------------------------------------------------------------
# Create the CU labels and determine text positions to occur mid way within
# each CU aggregation.


# Prepare CU names and positions for geom_text
if(spp == "Sockeye") {
cu.labels <- name.master %>% 
  mutate(Duplicated = duplicated(CU_INDEX)) %>% 
  filter(!Duplicated | Order == max(Order)) %>% 
  mutate(
    midpoint = c( (tail(Order, n = -1) + head(Order, n=-1)) / 2-1, last(x)-.5)
  ) %>% 
  filter(!is.na(midpoint)) %>% 
  select(CU_INDEX, CU_NAME, midpoint)
  }else{
cu.labels <- name.master %>% 
  mutate(Duplicated = duplicated(CU_INDEX)) %>% 
  filter(!Duplicated | Order == max(Order)) %>% 
  mutate(
        midpoint = c( (tail(Order, n = -1) + head(Order, n=-1)) / 2-.5, NA)
  )%>% 
  filter(!is.na(midpoint)) %>% 
  select(CU_INDEX, CU_NAME, midpoint)
}

buffer <- cu.labels$CU_INDEX %>% nchar %>% max


# CREATE PLOT -------------------------------------------------------------

p.pmax <-  ggplot(pmax.dat, aes(x=ANALYSIS_YR, y=StreamName)) +
  geom_tile(aes(fill = PmaxCat)) +
  geom_vline(xintercept = 1953, linetype = "dotted") +
  
  geom_vline(xintercept = c(1995), linetype = "solid") +
  geom_vline(xintercept = 2005, linetype = "dotdash") +
  geom_hline(yintercept = cu.cuts, linetype = "longdash")+
  # scale_y_discrete(limits=rev) +
  # scale_x_continuous(breaks = pretty_breaks(n=8)) +
  scale_x_continuous(breaks =seq(1930, 2020, by = 10)) +
  # expand_limits(x=2026)+
  scale_fill_brewer(palette = "OrRd"  ) +
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
# p.pmax



# ADD CU LABELS --------------------------------------------------------------
# Allow CU labels to be placed in the plot margins
p.pmax <-  p.pmax + coord_cartesian(clip = "off") 


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
  p.pmax <-  p.pmax  + 
    annotation_custom(
      grob = grid::textGrob(cu.labels$CU_INDEX[i], gp = gpar(fontsize = 12, fontface = "bold")), 
      xmin = 2037, 
      ymin = cu.labels$midpoint_adj[i]
    )
}

p.pmax    


# SAVE --------------------------------------------------------------------
out.file = file.path(fig.dir,paste0(tolower(spp.name), "-Pmax_tileplot.png"))
ggsave(filename = out.file,  width=10, height= nrow(name.master)*0.2 + 2, plot = p.pmax)