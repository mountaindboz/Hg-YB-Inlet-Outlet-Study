# Wrangle all commonly used data for the YB Mass Balance study to be exported to the openwaterhg package

# Load packages
library(tidyverse)
library(readxl)
library(lubridate)
library(usethis)

# Define path on SharePoint site for data
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR Documents - Open Water Final Report - Documents/Technical Appendices/Technical Appendix-B_Inlet-Outlet/Data/Final"
  )
)

# Import and modify data from DWR's Open Water Final Report SharePoint site
loads_calc <- read_csv(paste0(sharepoint_path, "/All_YB_Loads.csv"))

conc_data <-
  read_csv(
    paste0(sharepoint_path, "/NormalSamples.csv"),
    col_types = "????????????c"
  )

daily_flow_data_se <- read_csv(paste0(sharepoint_path, "/DailyAvgFlows_SE.csv"))

daily_flow_data_all <- read_csv(paste0(sharepoint_path, "/DailyAvgFlows_All.csv"))

loads_flow_cf <-
  read_excel(paste0(sharepoint_path, "/YB_MeHgLoad_and_Flow_2006.xlsx")) %>%
  # Clean up date formatting- extract date from dttm variable
  mutate(SampleDate = as_date(SampleDate))

part_conc_calc <- read_csv(paste0(sharepoint_path, "/Particulate_Conc.csv"))

field_data <-
  read_excel(paste0(sharepoint_path, "/FieldMeasurements.xlsx")) %>%
  # Clean up date and time formatting- extract date and time from dttm variables
  mutate(
    SampleDate = as_date(SampleDate),
    CollectionTimePST = hms::as_hms(CollectionTimePST)
  ) %>%
  # Rename some variables
  rename(
    CollectionTime = CollectionTimePST,
    WaterTempC = "Water Temperature",
    SpCond = "Specific Conductance",
    DissOxy = "Dissolved Oxygen"
  )

comb_param_calc <-read_csv(paste0(sharepoint_path, "/CombinedParameters.csv"))

qa_field_blanks <-
  read_csv(paste0(sharepoint_path, "/BlankSamples.csv")) %>%
  # Rename some variables
  rename(
    AmbSampConc = Result_amb,
    Blank_Amb_ratio = blank_amb_ratio
  )

qa_field_dups <-
  read_csv(
    paste0(sharepoint_path, "/FieldDuplicates.csv"),
    col_types = "??????????????c????"
  )

# Create openwaterhg package data
use_data(
  comb_param_calc,
  conc_data,
  daily_flow_data_all,
  daily_flow_data_se,
  field_data,
  loads_calc,
  loads_flow_cf,
  part_conc_calc,
  qa_field_blanks,
  qa_field_dups,
  overwrite = TRUE
)

