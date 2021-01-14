# Yolo Bypass Inlet-Outlet Study
# Purpose: Calculate combined parameters
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
part_conc_orig <- read_csv(file.path(sharepoint_path, "Particulate_Conc.csv"))

# Clean conc_orig
conc_clean <- conc_orig %>% 
  # Remove samples with QualCode "R"
  filter(is.na(QualCode) | !str_detect(QualCode, "^R")) %>%
  # Create a new variable Conc, which is a numeric version of Result with the MDL and RL for the ND values
  mutate(
    Conc = case_when(
      Result == "< RL"  ~ RL,
      Result == "< MDL" ~ MDL,
      TRUE              ~ as.numeric(Result)
    )
  ) %>% 
  # Clean up df
  select(
    StationName,
    SampleDate,
    CollectionTime,
    Analyte,
    Conc
  )

# Bind conc_clean with calculated data for the particulate fractions of Hg, MeHg, and organic carbon
all_conc <- bind_rows(conc_clean, part_conc_orig)

# Calculate all combined parameters
comb_param <- all_conc %>% 
  select(-Units) %>% 
  filter(
    str_detect(Analyte, "^MeHg|^THg") | Analyte %in% c("TSS", "DOC", "TOC", "POC", "Aluminum- total"),
    Conc != 0  # Remove any obs with values of zero
  ) %>% 
  pivot_wider(names_from = Analyte, values_from = Conc) %>% 
  rename(
    fMeHg = "MeHg- filtered",
    tMeHg = "MeHg- total",
    pMeHg = "MeHg- particulate",
    fTHg = "THg- filtered",
    tTHg = "THg- total",
    pTHg = "THg- particulate",
    tAl = "Aluminum- total"
  ) %>% 
  mutate(
    THg_Solids = pTHg/TSS*1e3,
    MeHg_Solids = pMeHg/TSS*1e3,
    THg_Kd = log10(THg_Solids/fTHg*1e3),
    MeHg_Kd = log10(MeHg_Solids/fMeHg*1e3),
    Per_tMeHg_of_tTHg = tMeHg/tTHg*100,
    Per_fMeHg_of_fTHg = fMeHg/fTHg*100,
    Per_pMeHg_of_pTHg = pMeHg/pTHg*100,
    tTHg_norm_TOC = tTHg/TOC,
    fTHg_norm_DOC = fTHg/DOC,
    pTHg_norm_POC = pTHg/POC,
    tMeHg_norm_TOC = tMeHg/TOC,
    fMeHg_norm_DOC = fMeHg/DOC,
    pMeHg_norm_POC = pMeHg/POC,
    TOC_on_Solids = TOC/TSS*1e3,
    POC_on_Solids = POC/TSS*1e3,
    tAl_Solids = tAl/TSS*1e3
  ) %>% 
  select(-c(tMeHg:POC)) %>% 
  pivot_longer(
    cols = THg_Solids:tAl_Solids,
    names_to = "Parameter",
    values_to = "Value"
  ) %>% 
  mutate(Value = signif(Value, 3)) %>% 
  filter(!is.na(Value))

# Rename combined parameters and add a units variable
cpc_names <- sort(unique(comb_param$Parameter))

cpc_key <- tibble(
  Parameter = cpc_names,
  Parameter2 = c(
    "Filtered MeHg normalized by DOC",
    "Filtered THg normalized by DOC",
    "MeHg Partitioning Coefficient (Kd)",
    "MeHg Concentration on Solids",
    "Percent fMeHg Conc of the fTHg Conc",
    "Percent pMeHg Conc of the pTHg Conc",
    "Percent tMeHg Conc of the tTHg Conc",
    "Particulate MeHg normalized by POC",
    "POC Concentration on Solids",
    "Particulate THg normalized by POC",
    "Total Aluminum Conc on Solids",
    "THg Partitioning Coefficient (Kd)",
    "THg Concentration on Solids",
    "Total MeHg normalized by TOC",
    "TOC Concentration on Solids",
    "Total THg normalized by TOC"
  ),
  Units = c(
    "ng/mg C",
    "ng/mg C",
    "log L/kg",
    "ng/g",
    "Percent",
    "Percent",
    "Percent",
    "ng/mg C",
    "mg C/g",
    "ng/mg C",
    "mg/g",
    "log L/kg",
    "ng/g",
    "ng/mg C",
    "mg C/g",
    "ng/mg C"
  )
)

comb_param_clean <- comb_param %>% 
  left_join(cpc_key, by = "Parameter") %>% 
  select(
    StationName,
    SampleDate,
    CollectionTime,
    Parameter2,
    Value,
    Units
  ) %>% 
  rename(Parameter = Parameter2)

# Export comb_param_clean df to a .csv file to be used in other scripts/analyses
comb_param_clean %>% write_excel_csv("YB_Inlet-Outlet_CombinedParameters.csv")

# This file was added to the SharePoint site for the Open Water Final Report 
# in the following location: 
# /Technical Appendices/Technical Appendix-B_Inlet-Outlet/Data/Final/YB_Inlet-Outlet_CombinedParameters.xlsx
# A redundant file is in M:\Data\Inlet-Outlet_Final
# This data was also added to the openwaterhg package as comb_param_calc

