## CHANGE LOG 
#
# v3.2:
# Broke out the early summer Chinook run and changed position of legend and
# tried a wide and tall layout options.  Added 1995 
#
# v3.1:
#
# Removing distinction between no record and not inspected as it adds too 
# much complexity and doesn't provide any additional information

library(scales)

# COMPUTE: Monitoring Status ---------------------------------------------
esc.dat %>% 
  mutate(
    EscAvail = case_when(
      is.na(Escapement) ~ FALSE,
      !is.na(Escapement) & Escapement == 0 ~ FALSE,
      !is.na(Escapement) & Escapement > 0 ~ TRUE
    ) 
    )%>% 
      count(EscAvail, ADULT_PRESENCE, JACK_PRESENCE)


monitor.status <- esc.dat %>% 
  mutate(
    EscAvail = case_when(
      is.na(Escapement) ~ FALSE,
      !is.na(Escapement) & Escapement == 0 ~ FALSE,
      !is.na(Escapement) & Escapement > 0 ~ TRUE
    ),
    Status = case_when(
      !EscAvail & ADULT_PRESENCE %in% c("NOT INSPECTED", "UNKNOWN") ~ "Not Inspected",
      !EscAvail & is.na(ADULT_PRESENCE)                             ~ "Not Inspected",
      !EscAvail & ADULT_PRESENCE == "NONE OBSERVED"                 ~ "No Escapement", 
      !EscAvail & ADULT_PRESENCE == "PRESENT"                       ~ "Escapement",
      EscAvail & ADULT_PRESENCE == "PRESENT"                        ~ "Escapement",
      EscAvail & ADULT_PRESENCE == "NONE OBSERVED"                  ~ "Escapement",  # DB Error
      TRUE ~ "Other"
    ),
    # When estimates are available designate the category of estimator type
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
   ) %>%
 
  # # Infill any missing year species stream combinations with no record
  # complete(WATERBODY = present.streams, ANALYSIS_YR, SPECIES, fill = list(Status = "No Record")) %>% 
  # Derive a new category variable combining reporting status with method type when escapement is available
  mutate(
    StatusMethod = case_when(
     Status == "Escapement" ~  MethodType,
     Status != "Escapement" ~ Status
    ),
    StatusMethod = factor(
      StatusMethod,
        levels = c("True Abundance",  "Relative Abundance", "Presence/Absence", "Unknown", "No Escapement",  "Not Inspected", "No Record"),
        labels = c("True Abundance",  "Relative Abundance", "Presence/Absence", "Unknown Method", "No Observed Escapement",  "Not Inspected/No Record", "Not Inspected/No Record"),
      )
  ) %>%
  # Summarize multiple runs
  count(WATERBODY, ANALYSIS_YR,  Status, StatusMethod, SPECIES, RunType)

monitor.status %>% filter(n>1)
monitor.status %>% count(Status,StatusMethod)



font.y <- esc.dat %>% 
  count(WATERBODY, Priority) %>%
  mutate(
    Face = ifelse(Priority, "bold", "italic"),
    Color = ifelse(Priority, "black", "grey35"),
    Color.hline = ifelse(Priority, "grey65", "grey95"),
    # Color.hline = ifelse(Priority, "grey65", "white"),
    linetype = ifelse(Priority, "solid", "solid"),
    # Color.hline = ifelse(Priority, "cornflowerblue", "grey92"),
    size.hline = ifelse(Priority, 0.5, 0.25),
  ) 



# FIG: Overview  --------------------------------------------------------


p.monitor <- monitor.status %>% 
  filter(!is.na(RunType)) %>%
  mutate(
    SppLab = case_when(
      SPECIES == "Chinook" ~ paste0(SPECIES, "\n(", RunType, ")"),
      SPECIES != "Chinook" ~ SPECIES
    )
  ) %>%
  # count(StatusMethod)
  # filter(StatusMethod != "No Record/Not Inspected") %>%
ggplot( aes(x = ANALYSIS_YR, y = str_to_title(WATERBODY), fill = StatusMethod)) +
  geom_tile() +
  geom_vline(xintercept = c(1995), linetype = "dashed") +
  # geom_vline(xintercept = c(1995, 2001), linetype = "dashed") +
  geom_vline(xintercept = 2008, linetype =  "dotted") +
  scale_y_discrete(limits=rev) +
  scale_x_continuous(breaks = pretty_breaks(n=8), expand = c(0,0)) +
  facet_wrap(~SppLab) +
  scale_fill_manual(
    values = c(
      'Not Inspected/No Record' = rgb(0,0,0, alpha = 0), #"azure1",   #"azure1",
      # 'Not Inspected'         = "azure2",   #"azure2",
      'No Observed Escapement'  = "purple",      #"black",
      'Presence/Absence'        = "#FDE725FF",  "#F7FCB9",  #"red", 
      'Relative Abundance'      = "#5DC863FF",  "#ADDD8E",  #"orange",
      'True Abundance'          = "#21908CFF",  "#31A354",  #"springgreen4",
      "Unknown Method"          = "grey75"   #"grey75"
    )
  ) +
  theme_bw(14) +
  theme(
    # --- AXIS ---
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    # axis.text.y = element_text(size=6),
    axis.text.y = element_text(
      size = 8, 
      # face = font.y$Face %>% rev, 
      # colour = font.y$Color %>% rev
    ),
    # --- LEGEND ---
    legend.title = element_blank(),
    # legend.position = "bottom",
    legend.position = c(0.95,0.20),
    legend.justification = c(0.95, 0.20),
    legend.key.width = unit(1.5, "lines"),
    legend.key.height = unit(1.5, "lines"),
    # --- panel ---
    panel.spacing = unit(-.01, "lines"),
    panel.border = element_rect( color="black", linewidth = 0.5, fill=NA),
    panel.grid.minor.y = element_blank(),
    # panel.grid.major.y = element_line(linewidth = 0.5, colour = "grey92"),
    panel.grid.major.y = element_line(
      # linewidth = font.y$size.hline %>% rev, 
      # colour = font.y$Color.hline %>% rev, 
      # linetype= font.y$linetype %>% rev
    ),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.major.x = element_line(linewidth = 0.5, colour = "grey92"),
    #--- panel strip ---
    strip.background = element_rect(color="black", linewidth = 0.5, fill="grey85"),
    strip.placement = "inside",
    strip.text = element_text(size=15, face="bold")
  ) +
  labs(
    # title = "NuSEDS Reported Monitoring Coverage"
  )
p.monitor
# out.file = file.path(dev.out.dir, "escapement_monitoring_overview_v3.2.png")
# ggsave(filename = out.file,  width=14, height=9, plot=p.monitor)


# FIG: Overview (portrait) ------------------------------------------------


p.monitor.tall <- p.monitor + facet_wrap(~SppLab, ncol=2) +
  theme(
    legend.position = c(0.90,0.10),
    legend.justification = c(0.90, 0.10),
  )
# out.file = file.path(dev.out.dir, "escapement_monitoring_overview_v3.2_tall.png")
ggsave(filename = "monitor_tall.png",  width=12, height=14, plot=p.monitor.tall)




# FIG: Inspection Trend --------------------------------------------------
n.stream <- monitor.status %>% count(WATERBODY) %>% nrow




p.inspect <- monitor.status %>%
  group_by(SPECIES, ANALYSIS_YR, RunType) %>%
  summarise(
    nRec = sum(!is.na(Status)) ,
    nInspect = sum(Status %in% c("No Escapement", "Escapement")),
    .groups = "drop"
  ) %>%
  filter(!is.na(RunType)) %>%
  mutate(
    SppLab = case_when(
      SPECIES == "Chinook" ~ paste0(SPECIES, "\n(", RunType, ")"),
      SPECIES != "Chinook" ~ SPECIES
    )
  ) %>%
  mutate(InspectPerc = nInspect/n.stream)  %>%
  ggplot(aes(x = ANALYSIS_YR, y=InspectPerc)) +
  geom_vline(xintercept = c(1995), linetype = "dashed") +
  geom_vline(xintercept = 2008, linetype =  "dotted") +
  geom_line(linewidth=1)+
  # geom_bar(stat = "identity") +
  expand_limits(y=c(0,1)) +
  scale_y_continuous(labels = percent, breaks = pretty_breaks(n=5)) +
  scale_x_continuous(breaks = seq(1960, 2020, by=10))+
  facet_wrap(~SppLab) +
  theme_bw(14) +
  theme(
    axis.title.x = element_blank(),
    # --- panel ---
    panel.spacing = unit(-.01, "lines"),
    panel.border = element_rect( color="black", linewidth = 0.5, fill=NA),
    panel.grid.minor.y = element_line(linewidth = 0.25, colour = "grey92"),
    panel.grid.major.y = element_line(linewidth = 0.75, colour = "grey92"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.major.x = element_line(linewidth = 0.5, colour = "grey92"),
    #--- panel strip ---
    strip.background = element_rect(color="black", linewidth = 0.5, fill="grey85"),
    strip.placement = "inside",
    strip.text = element_text(size=15, face="bold")
  ) +
  labs(
    y = "Percent Inspected"
  )
p.inspect
# out.file = file.path(dev.out.dir, "escapement_monitoring_inspection_v3.2.png")
# ggsave(filename = out.file,  width=12, height=7)



# FIG: Inspection (Alt) ---------------------------------------------------
n.pop <- esc.dat %>% distinct(POP_ID) %>% nrow()

p.inspect.alt <- bind_rows(
  monitor.status %>%
    group_by(SPECIES, ANALYSIS_YR, RunType) %>%
    summarise(nInspect = sum(Status %in% c("No Escapement", "Escapement")), .groups = "drop") %>%
    filter(!is.na(RunType)) %>%
    mutate(
      SppLab = case_when(
        SPECIES == "Chinook" ~ paste0(SPECIES, "\n(", RunType, ")"),
        SPECIES != "Chinook" ~ SPECIES
      ),
      InspectPerc = nInspect/n.stream  
    ),
  # --- Total Populations inspected ---
  monitor.status %>%
  group_by(SPECIES, ANALYSIS_YR, RunType) %>%
  summarise(nInspect = sum(Status %in% c("No Escapement", "Escapement")),.groups = "drop") %>%
  group_by(ANALYSIS_YR) %>% 
  summarise(Inspected = sum(nInspect), .groups = "drop") %>% 
  mutate(
    InspectPerc = Inspected/n.pop, 
    SppLab = "Total Coverage"
  )
) %>% 
  ggplot(aes(x = ANALYSIS_YR, y=InspectPerc)) +
  geom_vline(xintercept = c(1995), linetype = "dashed") +
  geom_vline(xintercept = 2008, linetype =  "dotted") +
  # geom_vline(xintercept = c(1995, 2005), linetype = "dashed") +
  geom_line(linewidth=1)+
  # geom_bar(stat = "identity") +
  expand_limits(y=c(0,1)) +
  scale_y_continuous(labels = percent, breaks = pretty_breaks(n=5)) +
  scale_x_continuous(breaks = seq(1960, 2020, by=10))+
  facet_wrap(~SppLab) +
  theme_bw(14) +
  theme(
    axis.title.x = element_blank(),
    # --- panel ---
    panel.spacing = unit(-.01, "lines"),
    panel.border = element_rect( color="black", linewidth = 0.5, fill=NA),
    panel.grid.minor.y = element_line(linewidth = 0.25, colour = "grey92"),
    panel.grid.major.y = element_line(linewidth = 0.75, colour = "grey92"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.major.x = element_line(linewidth = 0.5, colour = "grey92"),
    #--- panel strip ---
    strip.background = element_rect(color="black", linewidth = 0.5, fill="grey85"),
    strip.placement = "inside",
    strip.text = element_text(size=15, face="bold")
  ) +
  labs(
    y = "Percent Inspected"
  )
p.inspect.alt
