# Import commonly used concentration and flow data for the YB Mass Balance study 
# into the openwaterhg package. To import the data, this script is run from within 
# the openwaterhg project.

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
conc_data <- read_csv(file.path(sharepoint_path, "NormalSamples.csv"))

daily_flow_data_se <- read_excel(
  file.path(sharepoint_path, "YB_Daily_Avg_Flows.xlsx"), 
  sheet = "Sampling Event Flows"
)

daily_flow_data_all <- 
  read_excel(
    file.path(sharepoint_path, "YB_Daily_Avg_Flows.xlsx"), 
    sheet = "All Flows"
  ) %>% 
  mutate(Date = as_date(Date))

part_conc_calc <- read_csv(file.path(sharepoint_path, "Particulate_Conc.csv"))

field_data <-
  read_excel(
    file.path(sharepoint_path, "YB_Inlet-Outlet_FieldMeasurements.xlsx"),
    sheet = "Data"
  ) %>%
  mutate(
    SampleDate = as_date(SampleDate),
    CollectionTime = hms::as_hms(CollectionTime)
  )

comb_param_calc <- 
  read_excel(
    file.path(sharepoint_path, "YB_Inlet-Outlet_CombinedParameters.xlsx"),
    sheet = "Data"
  ) %>% 
  mutate(
    SampleDate = as_date(SampleDate),
    CollectionTime = hms::as_hms(CollectionTime)
  )

qa_field_blanks <- read_csv(file.path(sharepoint_path, "BlankSamples.csv"))

qa_field_dups <-
  read_csv(
    file.path(sharepoint_path, "FieldDuplicates.csv"),
    col_types = "???????????????c????"
  )

# Create openwaterhg package data
use_data(
  comb_param_calc,
  conc_data,
  daily_flow_data_all,
  daily_flow_data_se,
  field_data,
  part_conc_calc,
  qa_field_blanks,
  qa_field_dups,
  overwrite = TRUE
)

