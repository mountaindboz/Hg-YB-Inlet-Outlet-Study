# Yolo Bypass Mass Balance Study
# Purpose: Calculate Average Percent recovery of LCS samples for Hg and Organic Carbon
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(readxl)

# Define path on SharePoint site for data
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR Documents - Open Water Final Report - Documents/Technical Appendices/Technical Appendix-B_Inlet-Outlet/Data/Raw"
  )
)
  
# Import LCS QA data from Bryte Lab
lcs_orig <- read_excel(paste0(sharepoint_path, "/LAB_QC_Standards PR 2014 to Present.xlsx"))
glimpse(lcs_orig)

# Clean lcs_orig df
lcs_clean <- lcs_orig %>% 
  # remove duplicates
  distinct(QCS_CODE, AnalyteName, .keep_all = TRUE) %>% 
  # filter QCMeasureName and AnalyteName
  filter(
    QCMeasureName == "LCS - Recovery",
    AnalyteName %in% c("Mercury", "Organic Carbon"),
    !str_detect(MethodName, "200.8")
  )

# Calculate minimum, maximum and average of LCS percent recovery for each analyte
lcs_avg_perc_rec <- lcs_clean %>% 
  group_by(AnalyteName) %>% 
  summarize(
    N = n(),
    min_per_r = min(QCR_PERCENT_RECOVERY) * 100,
    max_per_r = max(QCR_PERCENT_RECOVERY) * 100,
    avg_per_r = round(mean(QCR_PERCENT_RECOVERY) * 100, 1)
  )

# Average LCS Percent Recovery values
# Mercury
lcs_avg_perc_rec %>% 
  filter(AnalyteName == "Mercury") %>% 
  pull(avg_per_r)
  # 97.1% for 352 values

# Organic Carbon
lcs_avg_perc_rec %>% 
  filter(AnalyteName == "Organic Carbon") %>% 
  pull(avg_per_r)
  # 99.5% for 308 values

# Date range for LCS values
summary(lcs_clean$QCB_DATE)
# From 4/21/2014 to 5/9/2019
