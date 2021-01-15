# Yolo Bypass Inlet-Outlet Study
# Purpose: A script that imports the flood event concentration data and the calculated
# particulate data, binds this data together, and formats it to be used in further calculations.
# Author: Dave Bosworth

# Load packages
library(dplyr)
library(stringr)
library(openwaterhg)

# Clean conc_data
temp_conc_clean <- conc_data %>% 
  # Remove samples with QualCode "R"
  filter(is.na(QualCode) | !str_detect(QualCode, "^R")) %>%
  # Create a new variable Detect to indicate if the result was detected or below the detection limit
  mutate(Detect = if_else(str_detect(Result, "^<"), "Non-detect", "Detect")) %>% 
  # Create a new variable Conc, which is a numeric version of Result with the MDL and RL for the ND values
  add_num_result() %>% 
  # Clean up df
  select(SampleCode:Analyte, Conc, Detect, RL:QualCode)

# Add Detect variable to particulate concentration data frame
temp_part_conc <- part_conc_calc %>% mutate(Detect = "Detect")

# Bind conc_data with calculated data for the particulate fractions of Hg, MeHg, and organic carbon
all_conc <- bind_rows(temp_conc_clean, temp_part_conc)

# Clean up
rm(temp_conc_clean, temp_part_conc)
