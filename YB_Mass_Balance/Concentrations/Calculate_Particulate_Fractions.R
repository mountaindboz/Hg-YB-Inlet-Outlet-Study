# Yolo Bypass Inlet-Outlet Study
# Purpose: Calculate particulate fractions of THg, MeHg, and OC to be used throughout 
# the analysis of this data
# Author: Dave Bosworth

# Load packages
library(tidyverse)

# Define path on SharePoint site for data
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR Documents - Open Water Final Report - Documents/Technical Appendices/Technical Appendix-B_Inlet-Outlet/Data/Final"
  )
)

# Import Concentration Data
conc_orig <- read_csv(file.path(sharepoint_path, "NormalSamples.csv"))

# Calculate the particulate concentrations
conc_part <- conc_orig %>% 
  # Just keep MeHg, THg, TOC, and DOC; remove samples with QualCode "R"
  filter(
    str_detect(Analyte, "^MeHg|^THg|OC$"),
    is.na(QualCode) | !str_detect(QualCode, "^R")
  ) %>% 
  # Create a new variable Conc, which is a numeric version of Result with the MDL and RL for the ND values
  mutate(
    Conc = case_when(
      Result == "< RL"  ~ RL,
      Result == "< MDL" ~ MDL,
      TRUE              ~ as.numeric(Result)
    )
  ) %>% 
  # Keep only necessary variables
  select(
    StationName,
    SampleDate,
    CollectionTime,
    Analyte,
    Conc
  ) %>% 
  # Separate analyte and fraction into 2 individual variables
  mutate(
    Analyte = case_when(
      Analyte == "TOC" ~ "OrgC- total",
      Analyte == "DOC" ~ "OrgC- filtered",
      TRUE ~ Analyte
    )
  ) %>% 
  separate(Analyte, into = c("Analyte", "Fraction"), sep = "- ") %>% 
  # Widen df and rename the Analytes to allow for subtraction
  pivot_wider(names_from = Fraction, values_from = Conc) %>% 
  # Calculate the particulate concentrations by subtraction
  mutate(particulate = total - filtered) %>% 
  # Rename Analytes and add a units variable
  mutate(
    Analyte = case_when(
      Analyte == "MeHg" ~ "MeHg- particulate",
      Analyte == "THg" ~ "THg- particulate",
      Analyte == "OrgC" ~ "POC"
    ),
    Units = if_else(
      str_detect(Analyte, "Hg-"),
      "ng/L",
      "mg/L as C"
    )
  ) %>% 
  # Remove total and filtered variables
  select(-c(total, filtered)) %>% 
  # Rename particulate variable
  rename(Conc = particulate) %>% 
  # Remove NA values in Conc variable
  filter(!is.na(Conc))
  
# Export conc_part df to a .csv file to be used in other scripts/analyses
conc_part %>% write_excel_csv("Particulate_Conc.csv")

# This file was added to the SharePoint site for the Open Water Final Report 
# in the following location: 
# /Technical Appendices/Technical Appendix-B_Inlet-Outlet/Data/Final/Particulate_Conc.csv
# A redundant file is in M:/YB_Inlet-Outlet_Study/Data_Final
# This data was also added to the openwaterhg package as part_conc_calc

