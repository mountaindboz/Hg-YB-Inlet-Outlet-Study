# Yolo Bypass Inlet-Outlet Study
# Purpose: Calculate the daily averages of the continuous flow data of each station 
# to be used for hydrographs. Pull out flow data for just the sampling events. The 
# resulting .csv files are used to calculate loads and create plots
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(readxl)
library(lubridate)

# 1. Import continuous flow data ---------------------------------------------

# Dataset is on SharePoint site for the Open Water Final Report
# Define path on SharePoint site for data
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR Documents - Open Water Final Report - Documents/Technical Appendices/Technical Appendix-B_Inlet-Outlet/Data/Raw"
  )
)  

# 1.1 2014 Event ----------------------------------------------------------

# Define path for 2014 flow data
flow_data_path14 <- paste0(sharepoint_path, "/2014_YB_Flood_Flows.xlsx")

# Import data
klrc.14 <- 
  read_excel(
    path = flow_data_path14,
    sheet = "KLRC", 
    range = "B2:C2977",
    col_names = c("Date", "Flow"),
    col_types = c("date", "numeric")
  ) %>% 
  mutate(StationName = "Knights Landing Ridge Cut")

ccsb.14 <- 
  read_excel(
    path = flow_data_path14, 
    sheet = "CCSB", 
    range = "B2:E2977",
    col_names = c("Date", "ow.flow", "ow.qual", "lfc.flow"),
    col_types = c("date", "numeric", "text", "numeric")
  ) %>% 
  select(-ow.qual) %>% 
  rename(
    "CCSB- Overflow Weir" = ow.flow,
    "CCSB- Low Flow Channel" = lfc.flow
  ) %>% 
  pivot_longer(
    cols = "CCSB- Overflow Weir":"CCSB- Low Flow Channel", 
    names_to = "StationName", 
    values_to = "Flow"
  )

PutahCk.14 <- 
  read_excel(
    path = flow_data_path14, 
    sheet = "Putah Ck", 
    range = "L2:M31",
    col_names = c("Date", "Flow"),
    col_types = c("date", "numeric")
  ) %>% 
  mutate(StationName = "Putah Creek at Mace Blvd")

schism.14 <- 
  read_excel(
    path = flow_data_path14, 
    sheet = "Toe Drain abv Stairsteps SELFE", 
    range = "B2:C121",
    col_names = c("Date", "Flow"),
    col_types = c("date", "numeric")
  ) %>% 
  mutate(StationName = "Toe Drain at 1/2 Lisbon")

# 1.2 2016 Event ----------------------------------------------------------

# Define path for 2016 flow data
flow_data_path16 <- paste0(sharepoint_path, "/2016_YB_Flood_Flows.xlsx")

# Import data
fw.16 <- 
  read_excel(
    path = flow_data_path16, 
    sheet = "Fremont Weir", 
    range = "B2:C2977",
    col_names = c("Date", "Flow"),
    col_types = c("date", "numeric")
  ) %>% 
  mutate(StationName = "Fremont Weir")

klrc.16 <- 
  read_excel(
    path = flow_data_path16, 
    sheet = "KLRC", 
    range = "B2:C2977",
    col_names = c("Date", "Flow"),
    col_types = c("date", "numeric")
  ) %>% 
  mutate(StationName = "Knights Landing Ridge Cut")

ccsb.16 <- 
  read_excel(
    path = flow_data_path16, 
    sheet = "CCSB", 
    range = "B2:E2977",
    col_names = c("Date", "ow.flow", "ow.qual", "lfc.flow"),
    col_types = c("date", "numeric", "text", "numeric")
  ) %>% 
  select(-ow.qual) %>% 
  rename(
    "CCSB- Overflow Weir" = ow.flow,
    "CCSB- Low Flow Channel" = lfc.flow
  ) %>% 
  pivot_longer(
    cols = "CCSB- Overflow Weir":"CCSB- Low Flow Channel", 
    names_to = "StationName", 
    values_to = "Flow"
  )

PutahCk.16 <- 
  read_excel(
    path = flow_data_path16, 
    sheet = "Putah Ck", 
    range = "L2:M32",
    col_names = c("Date", "Flow"),
    col_types = c("date", "numeric")
  ) %>% 
  mutate(StationName = "Putah Creek at Mace Blvd")

schism.16 <- 
  read_excel(
    path = flow_data_path16, 
    sheet = "SCHISM Output Flows", 
    range = "B2:H4922",
    col_names = c(
      "Date",
      "Toe Drain at 1/2 Lisbon",
      "Liberty Island Breach 3",
      "Liberty Island Breach 2",
      "Liberty Cut below Stairsteps",
      "Liberty Island Breach 1",
      "Shag Slough below Stairsteps"
    ),
    col_types = c(
      "date", 
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    )
  ) %>% 
  pivot_longer(
    cols = "Toe Drain at 1/2 Lisbon":"Shag Slough below Stairsteps", 
    names_to = "StationName", 
    values_to = "Flow"
  )

CacheSl.16 <- 
  read_excel(
    path = flow_data_path16, 
    sheet = "Cache Slough", 
    range = "B2:E2977",
    col_names = c(
      "Date",
      "Flow.raw",
      "Flow.int",
      "Flow"
    ),
    col_types = c(
      "date", 
      "numeric",
      "numeric",
      "numeric"
    )
  ) %>% 
  select(Date, Flow) %>% 
  mutate(StationName = "Cache Slough near Ryer Island")

MinerSl.16 <- 
  read_excel(
    path = flow_data_path16, 
    sheet = "Miner Slough", 
    range = "B2:E2976",
    col_names = c(
      "Date",
      "Flow.raw",
      "Flow.int",
      "Flow"
    ),
    col_types = c(
      "date", 
      "numeric",
      "numeric",
      "numeric"
    )
  ) %>% 
  select(Date, Flow) %>% 
  mutate(StationName = "Miner Slough near Sac River")

# 1.3 2017 Event ----------------------------------------------------------

# Define path for 2016 flow data
flow_data_path17 <- paste0(sharepoint_path, "/2017_YB_Flood_Flows.xlsx")

# Import data
fw.17 <- 
  read_excel(
    path = flow_data_path17, 
    sheet = "Fremont Weir", 
    range = "B2:D12097",
    col_names = c("Date", "Stage", "Flow"),
    col_types = c("date", "numeric", "numeric")
  ) %>% 
  select(-Stage) %>% 
  mutate(StationName = "Fremont Weir")

klrc.17 <- 
  read_excel(
    path = flow_data_path17, 
    sheet = "KLRC", 
    range = "B2:C12001",
    col_names = c("Date", "Flow"),
    col_types = c("date", "numeric")
  ) %>% 
  mutate(StationName = "Knights Landing Ridge Cut")

ccsb.17 <- 
  read_excel(
    path = flow_data_path17, 
    sheet = "CCSB", 
    range = "B2:E11905",
    col_names = c("Date", "ow.flow", "ow.qual", "lfc.flow"),
    col_types = c("date", "numeric", "text", "numeric")
  ) %>% 
  select(-ow.qual) %>% 
  rename(
    "CCSB- Overflow Weir" = ow.flow,
    "CCSB- Low Flow Channel" = lfc.flow
  ) %>% 
  pivot_longer(
    cols = "CCSB- Overflow Weir":"CCSB- Low Flow Channel", 
    names_to = "StationName", 
    values_to = "Flow"
  )

PutahCk.17 <- 
  read_excel(
    path = flow_data_path17, 
    sheet = "Putah Ck", 
    range = "M2:N125",
    col_names = c("Date", "Flow"),
    col_types = c("date", "numeric")
  ) %>% 
  mutate(StationName = "Putah Creek at Mace Blvd")

sw.17 <- 
  read_excel(
    path = flow_data_path17, 
    sheet = "Sacramento Weir", 
    range = "A2:B125",
    col_names = c("Date", "Flow"),
    col_types = c("date", "numeric")
  ) %>% 
  mutate(StationName = "Sac River above the Sacramento Weir")

schism.17 <- 
  read_excel(
    path = flow_data_path17, 
    sheet = "Output Flows - SCHISM", 
    range = "B2:G11233",
    col_names = c(
      "Date",
      "Toe Drain at 1/2 Lisbon",
      "Little Holland",
      "Liberty Cut below Stairsteps",
      "Main Liberty",
      "Shag Slough below Stairsteps"
    ),
    col_types = c(
      "date", 
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    )
  ) %>% 
  pivot_longer(
    cols = "Toe Drain at 1/2 Lisbon":"Shag Slough below Stairsteps", 
    names_to = "StationName", 
    values_to = "Flow"
  )

CacheSl.17 <- 
  read_excel(
    path = flow_data_path17, 
    sheet = "Cache Slough", 
    range = "B2:E12384",
    col_names = c(
      "Date",
      "Flow.raw",
      "Flow.int",
      "Flow"
    ),
    col_types = c(
      "date", 
      "numeric",
      "numeric",
      "numeric"
    )
  ) %>% 
  select(Date, Flow) %>% 
  mutate(StationName = "Cache Slough near Ryer Island")

MinerSl.17 <- 
  read_excel(
    path = flow_data_path17, 
    sheet = "Miner Slough", 
    range = "B2:E12384",
    col_names = c(
      "Date",
      "Flow.raw",
      "Flow.int",
      "Flow"
    ),
    col_types = c(
      "date", 
      "numeric",
      "numeric",
      "numeric"
    )
  ) %>% 
  select(Date, Flow) %>% 
  mutate(StationName = "Miner Slough near Sac River")

# Calculate Daily Average Flows ---------------------------
# Bind all flow data together except for Putah Creek and Sacramento Weir which are already daily averages
flow.all <- 
  bind_rows(
    CacheSl.16,
    CacheSl.17,
    ccsb.14,
    ccsb.16,
    ccsb.17,
    fw.16,
    fw.17,
    klrc.14,
    klrc.16,
    klrc.17,
    MinerSl.16,
    MinerSl.17,
    schism.14,
    schism.16,
    schism.17
  ) %>% 
  # Remove NA values in Flow
  filter(!is.na(Flow)) %>% 
  # Calculate Daily Averages for each Station
  group_by(StationName, Date) %>% 
  summarize(Flow = mean(Flow)) %>% 
  ungroup() %>% 
  # Bind flow data for Putah Creek and Sacramento Weir
  bind_rows(PutahCk.14, PutahCk.16, PutahCk.17, sw.17) %>% 
  # Clean up date formatting; round Flow to nearest whole number; and add a Year variable
  mutate(
    Date = as_date(Date),
    Flow = round(Flow),
    Year = year(Date)
  )

# Create vectors to identify Inlet and Outlet stations
inlet.sta <- c(
  "CCSB- Low Flow Channel",
  "CCSB- Overflow Weir",
  "Fremont Weir",
  "Knights Landing Ridge Cut",
  "Putah Creek at Mace Blvd",           
  "Sac River above the Sacramento Weir"
)

outlet.sta <- c(
  "Liberty Cut below Stairsteps",
  "Liberty Island Breach 1",
  "Liberty Island Breach 2",
  "Liberty Island Breach 3",
  "Little Holland",
  "Main Liberty",
  "Shag Slough below Stairsteps",
  "Toe Drain at 1/2 Lisbon"            
)

# Add a new variable LocType to identify inlet, outlet, and Below Liberty stations
flow.all <- flow.all %>% 
  mutate(
    LocType = case_when(
      StationName %in% inlet.sta ~ "Inlet",
      StationName %in% outlet.sta ~ "Outlet",
      TRUE ~ "Below Liberty"
    )
  ) %>% 
  select(Date, Year, StationName, LocType, Flow)

# Export flow.all
flow.all %>% write_excel_csv("DailyAvgFlows_All.csv")

# This data was added to the "YB_Daily_Avg_Flows.xlsx" spreadsheet in the 
# "All Flows" sheet. This spreadsheet is on the SharePoint site for the Open Water Final Report 
# in the following location: 
# /Technical Appendices/Technical Appendix-B_Inlet-Outlet/Data/Final/YB_Daily_Avg_Flows.xlsx
# A redundant file is in M:\Data\Inlet-Outlet_Final
# This data was also added to the openwaterhg package as daily_flow_data_all

# Pull out daily average flow data for just the sampling events -----------
# Create vectors of the sampling event dates for the inlet and outlet stations
inlet.dates <- c(
  "2014-12-22",
  "2016-03-15",
  "2017-01-11",
  "2017-01-24",
  "2017-01-31",
  "2017-02-14",
  "2017-03-01",
  "2017-03-15",
  "2017-03-28",
  "2017-04-11",
  "2017-04-25"
)

outlet.dates <- c(
  "2014-12-23",
  "2016-03-16",
  "2017-01-12",
  "2017-01-25",
  "2017-02-01",
  "2017-02-15",
  "2017-03-02",
  "2017-03-16",
  "2017-03-29",
  "2017-04-12",
  "2017-04-26"
) 

# Convert Date to character type so it can be filtered correctly
flow.all <- flow.all %>% mutate(Date = as.character(Date))

# Pull out daily averages for the inlet stations
flow.se.in <- flow.all %>% 
  filter(
    LocType == "Inlet",
    Date %in% inlet.dates
  )

# Pull out daily averages for the outlet and Below Liberty stations
flow.se.out <- flow.all %>% 
  filter(
    LocType != "Inlet",
    Date %in% outlet.dates
  )

# Bind flow.se.in and flow.se.out together and add a SamplingEvent variable
flow.se <- bind_rows(flow.se.in, flow.se.out) %>% 
  mutate(
    SamplingEvent = case_when(
      Date %in% c("2014-12-22", "2014-12-23") ~ "Dec 22-23, 2014",
      Date %in% c("2016-03-15", "2016-03-16") ~ "Mar 15-16, 2016",
      Date %in% c("2017-01-11", "2017-01-12") ~ "Jan 11-12, 2017",
      Date %in% c("2017-01-24", "2017-01-25") ~ "Jan 24-25, 2017",
      Date %in% c("2017-01-31", "2017-02-01") ~ "Jan 31-Feb 1, 2017",
      Date %in% c("2017-02-14", "2017-02-15") ~ "Feb 14-15, 2017",
      Date %in% c("2017-03-01", "2017-03-02") ~ "Mar 1-2, 2017",
      Date %in% c("2017-03-15", "2017-03-16") ~ "Mar 15-16, 2017",
      Date %in% c("2017-03-28", "2017-03-29") ~ "Mar 28-29, 2017",
      Date %in% c("2017-04-11", "2017-04-12") ~ "Apr 11-12, 2017",
      Date %in% c("2017-04-25", "2017-04-26") ~ "Apr 25-26, 2017"
    )
  ) %>% 
  select(SamplingEvent, Year, StationName, LocType, Flow)

# Export flow.se
flow.se %>% write_excel_csv("DailyAvgFlows_SE.csv")
  
# This data was added to the "YB_Daily_Avg_Flows.xlsx" spreadsheet in the 
# "Sampling Event Flows" sheet. This spreadsheet is on the SharePoint site for the Open Water Final Report 
# in the following location: 
# /Technical Appendices/Technical Appendix-B_Inlet-Outlet/Data/Final/YB_Daily_Avg_Flows.xlsx
# A redundant file is in M:\Data\Inlet-Outlet_Final
# This data was also added to the openwaterhg package as daily_flow_data_se

