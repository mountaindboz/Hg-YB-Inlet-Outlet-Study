# Import calculated load data for the YB Mass Balance study into the openwaterhg 
# package. To import the data, this script is run from within the openwaterhg project.

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
loads_calc <- read_excel(
  file.path(sharepoint_path, "YB_Inlet-Outlet_Loads.xlsx"), 
  sheet = "Data"
)

loads_flow_cf <-
  read_excel(file.path(sharepoint_path, "YB_MeHgLoad_and_Flow_2006.xlsx")) %>%
  mutate(SampleDate = as_date(SampleDate))

# Create openwaterhg package data
use_data(
  loads_calc,
  loads_flow_cf,
  overwrite = TRUE
)

