# Yolo Bypass Inlet-Outlet Study
# Analysis of Below Liberty Island flows for the Water Balance for the 2017 Yolo Bypass flood event

library(tidyverse)
library(readxl)
library(scales)
library(lubridate)
library(openwaterhg)

# Cache and Miner Slough Flows --------------------------------------------

# Datasets are on SharePoint site for the Open Water Final Report
# Define path on SharePoint site for data
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR Documents - Open Water Final Report - Documents/Technical Appendices/Technical Appendix-B_Inlet-Outlet/Data/Raw"
  )
)  

# Bring in Filtered Flow data for Cache and Miner Sloughs
CacheSl <- 
  read_excel(
    file.path(sharepoint_path, "CacheSl_FilteredFlow_2017.xlsx"), 
    sheet = "Filtered Flow",
    col_names = c("DateTime", "Flow", "Interpolated"),
    col_types = c("date", "skip", "skip", "numeric", "skip", "text"),
    skip = 1
  )

CacheSlDaily <- 
  read_excel(
    file.path(sharepoint_path, "CacheSl_FilteredFlow_2017.xlsx"), 
    sheet = "Daily Average Flow",
    col_names = c("Date", "DailyAvgNetFlow", "Interpolated"),
    col_types = c("date", "numeric", "text"),
    skip = 1
  )

MinerSl <- 
  read_excel(
    file.path(sharepoint_path, "MinerSl_FilteredFlow_2017.xlsx"), 
    sheet = "Filtered Flow",
    col_names = c("DateTime", "Flow", "Interpolated"),
    col_types = c("date", "skip", "skip", "numeric", "skip", "text"),
    skip = 1
  )

MinerSlDaily <- 
  read_excel(
    file.path(sharepoint_path, "MinerSl_FilteredFlow_2017.xlsx"), 
    sheet = "Daily Average Flow",
    col_names = c("Date", "DailyAvgNetFlow", "Interpolated"),
    col_types = c("date", "numeric", "text"),
    skip = 1
  )

# Create plots of the tidally filtered flow data
pdf(file = 'Cache&MinerSl_Flow_2017.pdf', w = 11, h = 8.5)
  # Cache Slough
  CacheSl %>% ggplot(aes(DateTime, Flow, color = Interpolated)) +
    geom_line(aes(group = 1)) +
    scale_x_datetime(
      name = 'Date',
      breaks = breaks_pretty(15),
      labels = label_date_short()
    ) + 
    scale_y_continuous(
      name = 'Tidally Filtered Flow (cfs)',
      labels = label_comma(),
      breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000)
    ) +
    labs(
      title = 'Cache Slough at Ryer Island USGS Station',
      subtitle = "Tidally Filtered Flow"
      )
  
  CacheSlDaily %>% ggplot(aes(Date, DailyAvgNetFlow, color = Interpolated)) +
    geom_line(aes(group = 1)) +
    scale_x_datetime(
      name = 'Date',
      breaks = breaks_pretty(15),
      labels = label_date_short()
    ) + 
    scale_y_continuous(
      name = 'Daily Average Tidally Filtered Flow (cfs)',
      labels = label_comma(),
      breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000)
    ) +
    labs(
      title = 'Cache Slough at Ryer Island USGS Station',
      subtitle = "Tidally Filtered Flow- Daily Averages"
    )
  
  # Miner Slough
  MinerSl %>% ggplot(aes(DateTime, Flow, color = Interpolated)) +
    geom_line(aes(group = 1)) +
    scale_x_datetime(
      name = 'Date',
      breaks = breaks_pretty(15),
      labels = label_date_short()
    ) + 
    scale_y_continuous(
      name = 'Tidally Filtered Flow (cfs)',
      labels = label_comma()
    ) +
    labs(
      title = 'Miner Slough near Sacramento River DWR-NCRO Station',
      subtitle = "Tidally Filtered Flow"
    )
  
  MinerSlDaily %>% ggplot(aes(Date, DailyAvgNetFlow, color = Interpolated)) +
    geom_line(aes(group = 1)) +
    scale_x_datetime(
      name = 'Date',
      breaks = breaks_pretty(15),
      labels = label_date_short()
    ) + 
    scale_y_continuous(
      name = 'Daily Average Tidally Filtered Flow (cfs)',
      labels = label_comma()
    ) +
    labs(
      title = 'Miner Slough near Sacramento River DWR-NCRO Station',
      subtitle = "Tidally Filtered Flow- Daily Averages"
    )

dev.off()


# Comparison with SCHISM Flows --------------------------------------------

# Bring in SCHISM Flow Data
schism <- 
  read_excel(
    file.path(sharepoint_path, "2017_YB_Flood_Flows.xlsx"), 
    sheet = "Output Flows - SCHISM", 
    range = "A2:H11233",
    col_names = c("DateTime", "Flow"),
    col_types = c("date", rep("skip", 6), "numeric")
  ) %>% 
  mutate(Location = "SCHISM")

# Create a df with calculated Below Liberty Island flows
BelowLI <- bind_rows("CacheSl" = CacheSl, "MinerSl" = MinerSl, .id = "Location") %>% 
  pivot_wider(
    id_cols = -Interpolated,
    names_from = Location,
    values_from = Flow
  ) %>% 
  mutate(Flow = CacheSl - MinerSl) %>% 
  select(!ends_with("Sl")) %>% 
  mutate(Location = "BelowLiberty") 

# Combine SCHISM and Below Liberty Island flows into one df for plotting
CombFlows <- bind_rows(schism, BelowLI)

# Subtract 12 hours from DateTime in the BelowLI df to account for travel time lag
BelowLI_12 <- BelowLI %>% mutate(DateTime = DateTime + hours(-12))

# Combine SCHISM and Below Liberty Island flows lagged by 12 hours into one df for plotting
CombFlows_12 <- bind_rows(schism, BelowLI_12)

# Calculate daily averages for both CombFlows and CombFlows_12 df's 
CombFlowsAvg <- CombFlows %>% 
  mutate(Date = date(DateTime)) %>% 
  group_by(Date, Location) %>% 
  summarize(DailyAvgFlow = mean(Flow)) %>% 
  ungroup()

CombFlows_12Avg <- CombFlows_12 %>% 
  mutate(Date = date(DateTime)) %>% 
  group_by(Date, Location) %>% 
  summarize(DailyAvgFlow = mean(Flow)) %>% 
  ungroup()

# Bring in Daily averages of the sum of input flows
InputAvg <- daily_flow_data_all %>% 
  filter(
    Date >= "2017-01-01" & Date <= "2017-05-04",
    LocType == "Inlet"
  ) %>% 
  group_by(Date) %>% 
  summarize(DailyAvgFlow = sum(Flow)) %>% 
  ungroup() %>% 
  mutate(Location = "Sum of Inputs")

# Combine input flows with SCHISM and Below Liberty
AllFlows <- CombFlowsAvg %>% 
  # lag SCHISM and Below Liberty daily average flows by one day
  mutate(Date = Date + days(-1)) %>% 
  bind_rows(InputAvg) %>% 
  # Convert Location variable to a factor to apply plot order
  mutate(
    Location = factor(
      Location,
      levels = c(
        "Sum of Inputs",
        "SCHISM",
        "BelowLiberty"
      )
    )
  )

# Import 2017 Flow data for sampling events
SamplingEvents <- daily_flow_data_se %>% filter(Year == 2017)

# Pull out data for Cache and Miner Sloughs and calculate Below Liberty Island flows
BLibIs <- SamplingEvents %>% 
  filter(LocType == "Below Liberty") %>%
  mutate(StationName = if_else(str_detect(StationName, "^Cache"), "CacheSl", "MinerSl")) %>% 
  pivot_wider(names_from = StationName, values_from = Flow) %>% 
  mutate(Flow = CacheSl - MinerSl) %>% 
  select(-c(CacheSl, MinerSl)) %>% 
  mutate(StationName = "BelowLiberty")

# Combine Below Liberty Island flows back to SamplingEvents df and summarize by location
SamplingEventsSum <- SamplingEvents %>% 
  filter(LocType != "Below Liberty") %>%
  bind_rows(BLibIs) %>% 
  mutate(Location = case_when(
    LocType == "Inlet"  ~ "Sum of Inputs",
    LocType == "Outlet" ~ "SCHISM",
    TRUE                ~ "Below Liberty"
  )
  ) %>% 
  group_by(SamplingEvent, Location) %>% 
  summarize(TotalFlow = sum(Flow)) %>% 
  ungroup() %>% 
  # Convert variables to factor to apply plot order
  conv_fact_samplingevent() %>% 
  mutate(
    Location = factor(
      Location,
      levels = c(
        "Sum of Inputs",
        "SCHISM",
        "Below Liberty"
      )
    )
  )

# Create plots to compare SCHISM flows with the calculated Below Liberty Island flows
pdf(file = 'SCHISM_BelowLI_FlowComparison_2017.pdf', w = 11, h = 8.5)
  # 15-minute data
    # No lag
    CombFlows %>% ggplot(aes(DateTime, Flow)) +
      geom_line(aes(color = Location)) +
      scale_x_datetime(
        name = 'Date',
        breaks = breaks_pretty(15),
        labels = label_date_short()
      ) + 
      scale_y_continuous(
        name = 'Tidally Filtered Flow (cfs)',
        labels = label_comma(),
        breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000)
      ) +
      labs(
        title = 'SCHISM vs. Below Liberty Island Flows- 15 minute data',
        subtitle = "No Lag time included",
        caption = "BelowLiberty = Cache Sl - Miner Sl"
      )
  
    # Below Liberty with 12 hour lag
    CombFlows_12 %>% ggplot(aes(DateTime, Flow)) +
      geom_line(aes(color = Location)) +
      scale_x_datetime(
        name = 'Date',
        breaks = breaks_pretty(15),
        labels = label_date_short()
      ) + 
      scale_y_continuous(
        name = 'Tidally Filtered Flow (cfs)',
        labels = label_comma(),
        breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000)
      ) +
      labs(
        title = 'SCHISM vs. Below Liberty Island Flows- 15 minute data',
        subtitle = "Below Liberty is lagged for 12 hours",
        caption = "BelowLiberty = Cache Sl - Miner Sl"
      )  
  
  # Daily Averages
    # No lag
    CombFlowsAvg %>% ggplot(aes(Date, DailyAvgFlow)) +
      geom_line(aes(color = Location)) +
      scale_x_date(
        name = 'Date',
        breaks = breaks_pretty(15),
        labels = label_date_short()
      ) + 
      scale_y_continuous(
        name = 'Daily Average Tidally Filtered Flow (cfs)',
        labels = label_comma(),
        breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000)
      ) +
      labs(
        title = 'SCHISM vs. Below Liberty Island Flows- Daily Averages',
        subtitle = "No Lag time included",
        caption = "BelowLiberty = Cache Sl - Miner Sl"
      ) 
    
    # Below Liberty with 12 hour lag
    CombFlows_12Avg %>% ggplot(aes(Date, DailyAvgFlow)) +
      geom_line(aes(color = Location)) +
      scale_x_date(
        name = 'Date',
        breaks = breaks_pretty(15),
        labels = label_date_short()
      ) + 
      scale_y_continuous(
        name = 'Daily Average Tidally Filtered Flow (cfs)',
        labels = label_comma(),
        breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000)
      ) +
      labs(
        title = 'SCHISM vs. Below Liberty Island Flows- Daily Averages',
        subtitle = "Below Liberty is lagged for 12 hours",
        caption = "BelowLiberty = Cache Sl - Miner Sl"
      )
  
  # Comparing all flows
  AllFlows %>% ggplot(aes(Date, DailyAvgFlow)) +
    geom_line(aes(color = Location)) +
    scale_x_date(
      name = 'Date',
      breaks = breaks_pretty(15),
      labels = label_date_short()
    ) + 
    scale_y_continuous(
      name = 'Daily Average Flow (cfs)',
      labels = label_comma(),
      breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000)
    ) +
    labs(
      title = 'Sum of Inputs, SCHISM, and Below Liberty Island Flows- Daily Averages',
      subtitle = "SCHISM and Below Liberty are tidally filtered and lagged for 1 day",
      caption = "BelowLiberty = Cache Sl - Miner Sl"
    )
  
  # Compare flows on sampling events
  SamplingEventsSum %>% 
    ggplot(data = .,aes(
      x = SamplingEvent,  #barplot by Sampling Event
      y = TotalFlow,
      fill = Location,  #make each Approach a different fill color
      label = round(TotalFlow, 0)  #label plots with Flow values
    )) +   
    geom_col(position = "dodge") + 
    geom_text(position = position_dodge(width = 0.9), size = 2.5) +
    labs(
      title = "Sampling Event Flows",
      subtitle = "Labels are the flow values",
      x = "Sampling Event",
      y = "Daily Average Flow (cfs)"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  #x-axis labels at 45 degrees

dev.off()

