# Yolo Bypass Inlet-Outlet Study
# Purpose: Compile, combine, and clean all of the concentration data collected
# during sampling events to characterize Yolo Bypass flood events 
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(readxl)
library(lubridate)


# 1.0 Contract Lab Data -------------------------------------------------------

# Define paths for data files
mlml_path <- "M:/Data/Lab/MLML/Open_Water/YB_In-Out_Study"
pnnl_path <- "M:/Data/Lab/PNNL"
  
# Create character vectors of all data files
mlml_files <- dir(mlml_path, pattern = "\\.xls$", recursive = T, full.names = T)
pnnl_files <- dir(pnnl_path, pattern = "SWAMP.xlsx$", full.names = T)
  
# Remove some of the files from the vectors
# 2014 file is in the wrong format, 2016 file has data unrelated to flood event sampling
mlml_files <- mlml_files[!str_detect(mlml_files, "2014|2016-Dec20")] 
# Remove QA data file
pnnl_files <- pnnl_files[!str_detect(pnnl_files, "QA_Samples")] 
  
# Combine all of the data
mlml_data_orig <- map_dfr(mlml_files, read_excel)
pnnl_data_orig <- map_dfr(pnnl_files, read_excel)
 
# Clean up the datasets
  # MLML
  mlml_data_clean <- mlml_data_orig %>%
    # Convert some of the variables to numeric
    mutate(
      Result = as.numeric(Result),
      MDL = as.numeric(MDL),
      RL = as.numeric(RL),
      ExpectedValue = as.numeric(ExpectedValue)
    ) %>% 
    # Only keep Grab data, Field Blanks, and Filter Blanks
    filter(SampleTypeCode %in% c("Grab", "FieldBlank", "FilterBlank", "EquipBlank")) %>%
    # Remove * Stations
    filter(StationName != "*") %>%
    # Remove a couple of MS, MSD samples with SampleTypeCode of "Grab"
    filter(!str_detect(LabSampleID, "ms")) %>% 
    # Round the Result variable
    mutate(Result = signif(Result, 3))
  
  # PNNL Data
  pnnl_data_clean <- pnnl_data_orig %>% 
    # Filter out QA data
    filter(StationCode != "LABQA", StationName != "LABQA") %>%
    filter(!str_detect(SampleTypeCode, "MS") | is.na(SampleTypeCode)) %>% 
    # Round the Result variable
    mutate(Result = if_else(Result < 1000, signif(Result, 3), round(Result)))
  
# Import one MLML dataset with different format
mlml2 <- read_excel(path = "M:/Data/Lab/MLML/Open_Water/YB_In-Out_Study/MeHg/2014-Dec22_23_YB_InOutStudy_MMHg_data_forR.xlsx")

# Combine MLML and PNNL Data
contract_data <- bind_rows(mlml_data_clean, pnnl_data_clean, mlml2)
  
# Clean up contract_data df
  # ONLY NECESSARY ONCE- JUST KEPT CODE FOR FUTURE REFERENCE
    # Create a list of unique StationNames
    #StationNames <- count(ContractData, StationName)
    # Export list to a .csv file
    #write_excel_csv(StationNames, "StationNameKey_ContractLabs.csv")
    
  #Import StationName Standarized Key
  std_station_cl <- read_csv("YB_Mass_Balance/Concentrations/StationNameKey_ContractLabs.csv")

  contract_data_clean <- contract_data %>%
    # Make a new variable "Analyte" that combines AnalyteName and FractionName
    mutate(
      Analyte = case_when(
        AnalyteName == "Boron"                                                        ~ "Boron- total",
        AnalyteName == "Methylmercury" & FractionName %in% c("Total", "raw")          ~ "MeHg- total",
        AnalyteName == "Methylmercury" & FractionName %in% c("Dissolved", "filtered") ~ "MeHg- filtered",
        AnalyteName == "Iron"                                                         ~ "Iron- filtered",
        AnalyteName == "Managnese"                                                    ~ "Manganese- filtered"
      )
    ) %>% 
    # Create a new variable ResultQual to indicate <MDL, DNQ, and Detect values
    mutate(
      ResultQual = case_when(
        ResQualCode == "ND" ~  "< MDL",
        Result > MDL & Result < RL ~ "DNQ",
        TRUE ~ "Detect"
      ),
      # Convert values less than MDL as equal to their MDL
      Result = if_else(ResultQual == "< MDL", MDL, Result)
    ) %>% 
    # Clean up date and time formatting
    mutate(
      SampleDate = as_date(SampleDate), 
      AnalysisDate = as_date(AnalysisDate),
      CollectionTime = hms::as_hms(CollectionTime)
    ) %>% 
    # Standardize StationNames
    left_join(std_station_cl) %>%
    # Keep necessary variables
    select(
      StationCode,
      StationNameStd,
      SampleDate,
      CollectionTime,
      LabBatch,
      AnalysisDate,
      Analyte,
      UnitName,
      Result,
      ResultQual,
      MDL,
      RL,
      LabResultComments
    ) %>% 
    # Rename some of the variables (New Name = Old Name)
    rename(
      SampleCode = StationCode,
      StationName = StationNameStd,
      Units = UnitName,
      LabComments = LabResultComments
    ) %>% 
    # Add "C" to the end of the SampleCodes to be consistent with Bryte reporting
    mutate(SampleCode = if_else(SampleDate > "2014-12-31", paste0(SampleCode, "C"), SampleCode)) %>% 
    # remove data earlier than first sampling event on Dec 22-23, 2014
    filter(SampleDate >= "2014-12-22")
  
# Change StationNames of two boron samples that were switched
contract_data_temp <- contract_data_clean %>% 
  filter(
    SampleCode %in% c("EH0317B0545C", "EH0317B0547C"),
    Analyte == "Boron- total"
  )

contract_data_switch <- contract_data_temp %>% 
  mutate(
    StationName = if_else(
      StationName == "Field Blank", 
      "Field Duplicate Sample",
      "Field Blank"
    ),
    MME_Comments = if_else(
      StationName == "Field Blank", 
      "Field Blank originally labeled as Field Duplicate on data sheet, most likely a blank sample after looking at data from sampling event",
      "Field Duplicate originally labeled as Field Blank on data sheet, most likely an ambient sample after looking at data from sampling event"
    )
  )

contract_data_clean <- 
  anti_join(contract_data_clean, contract_data_temp) %>% 
  bind_rows(contract_data_switch)

# Create a vector of analytes for the contract labs to be used later
contract_ana <- sort(unique(contract_data_clean$Analyte))
    
# Clean up
df_keep <- c("contract_data_clean", "contract_ana", "df_keep")
rm(list= ls()[!(ls() %in% df_keep)])


# 2.0 Bryte Lab Data ----------------------------------------------------------

# Define path for data files
bryte_path <- "M:/Data/Lab/Bryte_Lab/Open_Water/WDL_Downloads"

# Create a character vector of all data files
bryte_files <- dir(bryte_path, full.names = T)

# Remove some of the files from the vector- these have data unrelated to flood event sampling
bryte_files <- bryte_files[!str_detect(bryte_files, "E-W_Transect|Pasture")]

# Combine all of the data
bryte_data_orig <- map_dfr(
  bryte_files, 
  read_excel, 
  col_types = c(
    rep("guess", 7),
    "text",
    rep("guess", 6),
    "text",
    rep("guess", 2)
  )
)

# Import additional Bryte Lab data - total Iron, which was provided separately 
bryte_data_tFe_orig <- read_excel(path = "M:/Data/Lab/Bryte_Lab/Open_Water/YB_Inlet-Outlet_Data_Bryte_tFe.xlsx")
 
# Clean up bryte_data_orig and bryte_data_tFe_orig df's
  # Clean up variable names
  names(bryte_data_orig) <- str_replace_all(names(bryte_data_orig), "[:space:]", "")
  names(bryte_data_tFe_orig) <- str_replace_all(names(bryte_data_tFe_orig), "[:space:]", "")
  bryte_data_tFe_orig <- bryte_data_tFe_orig %>% 
    rename(StationNameStd = StationName)
  
  #Import StationName Standarized Key
  std_station_b <- read_csv("YB_Mass_Balance/Concentrations/StationNameKey_Bryte.csv")
  
  # Import Analyte name Standarized Key
  std_analyte_b <- read_csv("YB_Mass_Balance/Concentrations/AnalyteKey_Bryte.csv")
  
  bryte_data_clean <- bryte_data_orig %>% 
    # Remove a some Hg samples analyzed by an unwanted method
    filter(Method != "EPA 200.8 (Hg Total) [1]*") %>% 
    # Standardize StationNames
    left_join(std_station_b) %>% 
    # bind tFe data from 2017
    bind_rows(bryte_data_tFe_orig) %>% 
    # Standardize Analyte Names
    left_join(std_analyte_b) %>% 
    # Separate Collection Date into 2 variables- one for date, other for time
    mutate(
      SampleDate = as_date(CollectionDate), 
      CollectionTime = hms::as_hms(CollectionDate)
    ) %>%
    # Indicate Field Duplicates and Companion Grab Samples in the StationNameStd variable
    mutate(
      StationNameStd = case_when(
        Description == "Field Duplicate Sample" ~ "Field Duplicate Sample",
        Description == "Companion Grab Sample"  ~ "Companion Grab Sample",
        TRUE                                    ~ StationNameStd
      )
    ) %>%
    # Clean up Result variable
    mutate(
      # Create a new variable ResultQual to indicate <RL and Detect values
      ResultQual = if_else(str_detect(Result, "^<"), "< RL", "Detect"),
      # Convert Result variable to numeric and convert values less than RL as equal to their RL
      Result = if_else(str_detect(Result, "^<"), RptLimit, as.numeric(Result)),
      # Round Result variable to specified number of digits in n_round
      Result = round(Result, n_round)
    ) %>% 
    # Keep necessary variables
    select(
      SampleCode,
      StationNameStd,
      SampleDate,
      CollectionTime,
      AnalyteStd,
      Result,
      ResultQual,
      RptLimit,
      Units,
      Method,
      ParentSample,
      n_round
    ) %>% 
    # Rename some variables (New Name = Old Name)
    rename(
      StationName = StationNameStd,
      Analyte = AnalyteStd,
      RL = RptLimit
    )
    
# Clean up 
df_keep <- append(df_keep, "bryte_data_clean")
rm(list= ls()[!(ls() %in% df_keep)])
  

# 3.0 All Data ----------------------------------------------------------------

# Combine Contract and Bryte Lab Data
all_data <- bind_rows(contract_data_clean, bryte_data_clean)


# 4.0 Lab Replicates ----------------------------------------------------------

# Create a df of all Lab Replicates
lab_reps <- all_data %>% 
  count(SampleCode, Analyte) %>% 
  filter(n == 2) %>% 
  select(-n)
  
# Pull out a df of all Lab Replicates only including variables that are unique in Rep1 and Rep2
lab_reps_u <- inner_join(all_data, lab_reps) %>% 
  select(SampleCode, Analyte, Result, ResultQual)

# Pull out a df of all Lab Replicates only including variables that are identical in Rep1 and Rep2
lab_reps_i <- inner_join(all_data, lab_reps) %>% 
  select(-c(Result, ResultQual, LabComments)) %>% 
  # Remove duplicate rows
  distinct()

# Pull out a df of all data not including Lab Replicates
no_lab_reps <- anti_join(all_data, lab_reps)

# Create two different df to spread out unique variables
  # Result
  S_Result <- lab_reps_u %>% 
    select(-ResultQual) %>%
    group_by(SampleCode, Analyte) %>% 
    mutate(Rep = paste0("Result", row_number())) %>% 
    ungroup() %>% 
    pivot_wider(names_from = Rep, values_from = Result)
    
  # ResultQual
  S_ResultQual <- lab_reps_u %>% 
    select(-Result) %>%
    group_by(SampleCode, Analyte) %>% 
    mutate(Rep = paste0("ResultQual", row_number())) %>% 
    ungroup() %>% 
    pivot_wider(names_from = Rep, values_from = ResultQual)
  
# Join all of the df back together
lab_rep_data <- reduce(list(S_Result, S_ResultQual, lab_reps_i), left_join)

# Modify the lab_rep_data df to be exported
lab_rep_data_exp <- lab_rep_data %>% 
  mutate(
    # Calculate RPD values for each replicate pair and flag if necessary
    RPD = if_else(
      str_detect(ResultQual1, "^<") | str_detect(ResultQual2, "^<"),
      NA_real_,
      round(abs(Result1 - Result2)/((Result1 + Result2)/2), 3)
    ),
    Flag = case_when(
      Analyte %in% contract_ana & RPD > 0.25 & (Result1 > 10 * MDL | Result2 > 10 * MDL) ~ "RPD",
      !Analyte %in% contract_ana & RPD > 0.25 & (Result1 > 10 * RL | Result2 > 10 * RL) ~ "RPD",
      TRUE ~ NA_character_
    ),
    # Convert Result variables to character indicating <MDL and <RL values as such
    Result1 = case_when(
      ResultQual1 == "< RL" ~ "< RL",
      ResultQual1 == "< MDL" ~ "< MDL",
      TRUE ~ as.character(Result1)
    ),
    Result2 = case_when(
      ResultQual2 == "< RL" ~ "< RL",
      ResultQual2 == "< MDL" ~ "< MDL",
      TRUE ~ as.character(Result2)
    )
  ) %>% 
  # Only keep "DNQ" in ResultQual variables
  mutate(across(starts_with("ResultQual"), ~if_else(.x == "DNQ", "DNQ", NA_character_))) %>% 
  # Select variables to keep for export
  select(
    SampleCode,
    StationName,
    SampleDate,
    CollectionTime,
    Analyte,
    LabBatch,
    Result1,
    Result2,
    ResultQual1,
    ResultQual2,
    RPD,
    RL,
    MDL,
    Units,
    MME_Comments,
    Flag
  )

# Export lab_rep_data_exp to .csv file
lab_rep_data_exp %>% write_excel_csv("LabReplicates.csv", na = "")
# This file was added to the SharePoint site for the Open Water Final Report 
# in the following location: 
# /Technical Appendices/Technical Appendix-B_Inlet-Outlet/Data/Final/LabReplicates.csv
# This data is also in the "Lab Replicates" sheet in the "YB_Inlet-Outlet_Conc_QA_Data.xlsx"
# spreadsheet in the same location on the SharePoint site
# Redundant files are in M:/YB_Inlet-Outlet_Study/Data_Final

# Modify the lab_rep_data df to be added back to no lab reps
lab_rep_data_mod <- lab_rep_data %>% 
  mutate(
    # Average Concentration values
    Result = (Result1 + Result2)/2,
    # Consolidate ResultQual variables (all replicates have the same Qual code)
    ResultQual = ResultQual1,
    # Add comment about using the average of lab replicates
    MME_Comments = case_when(
      is.na(MME_Comments) & ResultQual != "< RL" ~ "Average of Lab Replicates",
      is.na(MME_Comments) & ResultQual == "< RL" ~ "Both Lab Replicates are < RL",
      TRUE ~ paste("Average of Lab Replicates", MME_Comments, sep = "; ")
    )
  ) %>% 
  # Delete a few variables
  select(!ends_with(c("1", "2")))
  
# Bind the lab_rep_data_mod df back with the no_lab_reps df
all_data1 <- bind_rows(no_lab_reps, lab_rep_data_mod)
  
# Remove df that are no longer necessary
df_keep <- append(df_keep, c("all_data", "all_data1", "lab_rep_data"))
rm(list= ls()[!(ls() %in% df_keep)])


# 5.0 Blanks and QA Information -----------------------------------------------

# Create a .csv file that summarizes the Lab Methods used for each analyte
all_data1 %>%
  count(Analyte, Method) %>%
  select(-n) %>%
  write_excel_csv("AnalyteMethods.csv", na = "")
# This data was added to the "YB_Inlet-Outlet_Conc_QA_Data.xlsx" spreadsheet in the 
# "Lab Methods" sheet. This spreadsheet is on the SharePoint site for the Open Water Final Report 
# in the following location: 
# /Technical Appendices/Technical Appendix-B_Inlet-Outlet/Data/Final/YB_Inlet-Outlet_Conc_QA_Data.xlsx
# A redundant file is in M:/YB_Inlet-Outlet_Study/Data_Final
  
# Export a .csv file for Lab QA batches
all_data1 %>%
  select(LabBatch, SampleCode, Analyte) %>%
  write_excel_csv("Lab_QA_Batch.csv", na = "")
# This data was added to the "YB_Inlet-Outlet_Conc_QA_Data.xlsx" spreadsheet in the 
# "Lab QA Batches" sheet. This spreadsheet is on the SharePoint site for the Open Water Final Report 
# in the following location: 
# /Technical Appendices/Technical Appendix-B_Inlet-Outlet/Data/Final/YB_Inlet-Outlet_Conc_QA_Data.xlsx
# A redundant file is in M:/YB_Inlet-Outlet_Study/Data_Final

# Export a .csv file for Analysis dates for Lab QA batches
all_data1 %>%
  select(LabBatch, AnalysisDate) %>%
  distinct() %>%
  # removing Lab Batch: MPSL-DFG_20160427_W_MeHg with AnalysisDate of 4/28/2016, since all
  # but one sample in this batch were analyzed on 4/27/2016
  filter(!(LabBatch == "MPSL-DFG_20160427_W_MeHg" & AnalysisDate == "2016-04-28")) %>%
  write_excel_csv("Lab_QA_Batch_AnaDate.csv", na = "")
# This data was added to the "YB_Inlet-Outlet_Conc_QA_Data.xlsx" spreadsheet in the 
# "Analysis Dates" sheet. This spreadsheet is on the SharePoint site for the Open Water Final Report 
# in the following location: 
# /Technical Appendices/Technical Appendix-B_Inlet-Outlet/Data/Final/YB_Inlet-Outlet_Conc_QA_Data.xlsx
# A redundant file is in M:/YB_Inlet-Outlet_Study/Data_Final

# Remove a few QA related variables
all_data1 <- all_data1 %>% select(-c(LabBatch, AnalysisDate, Method))

# Pull out Blank Samples and save for QA validation
blank_samples <- all_data1 %>% 
  filter(str_detect(StationName, "Blank$")) %>% 
  # Round Result variable
  mutate(
    Result = case_when(
      str_detect(Analyte, "^Boron|^MeHg") ~ round(Result, 3),
      Analyte %in% c("Iron- filtered", "Manganese- filtered") ~ signif(Result, 3),
      TRUE ~ round(Result, n_round)
    )
  ) %>% 
  select(-c(ParentSample, n_round))

# Remove Blank samples from all_data1 df
all_data2 <- all_data1 %>% filter(!str_detect(StationName, "Blank$"))


# 6.0 Field Duplicates --------------------------------------------------------

# Make a new df with the Field Duplicate Samples
field_dups <- filter(all_data2, StationName == "Field Duplicate Sample")
  
# Remove Field Duplicate Samples from all_data2 df
no_field_dups <- filter(all_data2, StationName != "Field Duplicate Sample")

# Create a df with all of the parent sample codes
parent_samples <- field_dups %>% 
  count(ParentSample) %>% 
  select(-n) %>% 
  filter(!is.na(ParentSample))
  
# Create a df with all Station-Date combos for the Field Duplicate pairs
fd_station_dates <- 
  inner_join(no_field_dups, parent_samples, by = c("SampleCode" = "ParentSample")) %>% 
  count(StationName, SampleDate) %>% 
  select(-n)
  
# Inner join fd_station_dates df to no_field_dups df to pull out all Field Duplicate pairs
field_dup_pairs <- inner_join(no_field_dups, fd_station_dates)

# Remove field_dup_pairs from no_field_dups df
no_field_dups <- anti_join(no_field_dups, field_dup_pairs, by = c("StationName", "SampleDate"))
  
# Clean up
rm(parent_samples, fd_station_dates)

# Join parent samples and field dups together using suffixes
field_dups_all <- 
  left_join(
    field_dup_pairs,
    field_dups,
    by = c("SampleDate", "Analyte"),
    suffix = c("_PS", "_FD")
  ) %>% 
  # Calculate RPD values for each replicate pair and flag if necessary
  mutate(
    RPD = if_else(
      str_detect(ResultQual_PS, "^<") | str_detect(ResultQual_FD, "^<"),
      NA_real_,
      round(abs(Result_PS - Result_FD)/((Result_PS + Result_FD)/2), 3)
    ),
    Flag = case_when(
      Analyte %in% contract_ana & RPD > 0.25 & (Result_PS > 10 * MDL_PS | Result_FD > 10 * MDL_FD) ~ "FV",
      (!Analyte %in% contract_ana & !Analyte %in% c("DOC", "TOC", "VSS")) & RPD > 0.25 & (Result_PS > 10 * RL_PS | Result_FD > 10 * RL_FD) ~ "FV",
      Analyte %in% c("DOC", "TOC", "VSS") & RPD > 0.3 & (Result_PS > 10 * RL_PS | Result_FD > 10 * RL_FD) ~ "FV",
      TRUE ~ NA_character_
    )
  ) %>% 
  # Select variables to keep and rename a few
  select(
    starts_with("SampleCode"),
    StationName = StationName_PS,
    SampleDate,
    starts_with("CollectionTime"),
    Analyte,
    starts_with("Result_"),
    starts_with("ResultQual"),
    RPD,
    RL = RL_PS,
    MDL = MDL_PS,
    Units = Units_PS,
    starts_with("LabComments"),
    starts_with("MME_Comments"),
    Flag,
    n_round = n_round_PS
  )

# Modify field dup df to be exported
field_dups_all_exp <- field_dups_all %>% 
  # Convert Result variables to character indicating <MDL and <RL values as such
  mutate(
    Result_PS = case_when(
      ResultQual_PS == "< RL" ~ "< RL",
      ResultQual_PS == "< MDL" ~ "< MDL",
      TRUE ~ as.character(Result_PS)
    ),
    Result_FD = case_when(
      ResultQual_FD == "< RL" ~ "< RL",
      ResultQual_FD == "< MDL" ~ "< MDL",
      TRUE ~ as.character(Result_FD)
    )
  ) %>% 
  # Only keep "DNQ" in ResultQual variables
  mutate(across(starts_with("ResultQual"), ~if_else(.x == "DNQ", "DNQ", NA_character_))) %>% 
  # Remove n_round variable for export
  select(-n_round)

# Export field_dups_all_exp to .csv file
field_dups_all_exp %>%  write_excel_csv("FieldDuplicates.csv", na = "")
# This file was added to the SharePoint site for the Open Water Final Report 
# in the following location: 
# /Technical Appendices/Technical Appendix-B_Inlet-Outlet/Data/Final/FieldDuplicates.csv
# This data is also in the "Field Duplicates" sheet in the "YB_Inlet-Outlet_Conc_QA_Data.xlsx"
# spreadsheet in the same location on the SharePoint site
# Redundant files are in M:/YB_Inlet-Outlet_Study/Data_Final
# This data was also added to the openwaterhg package as qa_field_dups
  
# Modify the field_dups_all df to be added back to no field dups
field_dups_all_mod <- field_dups_all %>% 
  # Average Concentration values
  mutate(
    Result = (Result_PS + Result_FD)/2,
    # Add comment about using the average of field duplicates
    MME_Comments = case_when(
      ResultQual_PS == "< MDL" &  ResultQual_FD == "< MDL" ~ "Both Field Duplicates are < MDL",
      ResultQual_PS == "< MDL" &  ResultQual_FD == "Detect" ~ "Average of Field Duplicates, used the MDL value for the Parent Sample",
      is.na(MME_Comments_PS) & is.na(MME_Comments_FD) ~ "Average of Field Duplicates",
      (!is.na(MME_Comments_PS) | !is.na(MME_Comments_FD)) & str_detect(MME_Comments_FD, "Field") ~ "Average of Field Duplicates and Lab Replicates; Field Duplicate originally labeled as Field Blank on data sheet, most likely an ambient sample after looking at data from sampling event",
      TRUE ~ "Average of Field Duplicates and Lab Replicates"
    ),
    # Consolidate Lab Comments
    LabComments = case_when(
      !is.na(LabComments_PS) & !is.na(LabComments_FD) ~ paste0("Parent Sample: ", LabComments_PS, "; Field Dup: ", LabComments_FD),
      !is.na(LabComments_PS) & is.na(LabComments_FD) ~ paste0("Parent Sample: ", LabComments_PS),
      is.na(LabComments_PS) & !is.na(LabComments_FD) ~ paste0("Field Dup: ", LabComments_FD)
    ),
    # Consolidate ResultQual variables
    ResultQual = case_when(
      ResultQual_PS == "DNQ" & ResultQual_FD == "DNQ" ~ "DNQ (both Field Duplicate pairs)",
      ResultQual_PS == "Detect" & ResultQual_FD == "DNQ" ~ "DNQ (Field Duplicate only)",
      ResultQual_PS == "< MDL" & ResultQual_FD == "Detect" ~ "Detect",
      TRUE ~ ResultQual_PS
    )
  ) %>% 
  # Rename some variables
  rename(
    SampleCode = SampleCode_PS,
    CollectionTime = CollectionTime_PS
  ) %>% 
  # Delete a few variables
  select(-c(ends_with(c("_PS", "_FD")), RPD, Flag))

# Bind the field_dups_all_mod df back with the no_field_dups df
all_data3 <- bind_rows(no_field_dups, field_dups_all_mod) %>% 
  # Round Result variable
  mutate(
    Result = case_when(
      str_detect(Analyte, "^Boron|^MeHg") ~ round(Result, 3),
      Analyte %in% c("Iron- filtered", "Manganese- filtered") ~ signif(Result, 3),
      TRUE ~ round(Result, n_round)
    )
  ) %>% 
  # remove n_round variable since it is no longer necessary
  select(-n_round)

# Remove df that are no longer necessary
df_keep <- append(df_keep, c("all_data2", "all_data3", "blank_samples", "field_dups_all"))
rm(list= ls()[!(ls() %in% df_keep)])


# 7.0 Companion Grab Samples --------------------------------------------------

# Make a new df with the Companion Grab Samples
comp_grab <- filter(all_data3, StationName == "Companion Grab Sample")

# Remove Companion Samples from AllData df
no_comp_grab <- filter(all_data3, StationName != "Companion Grab Sample")

# Create a df with all of the parent sample codes
parent_samples <- comp_grab %>% 
  count(ParentSample) %>% 
  select(-n) %>% 
  filter(!is.na(ParentSample))

# Create a df with all Station-Date combos for the Companion Grab pairs
cg_station_dates <- inner_join(no_comp_grab, parent_samples, by = c("SampleCode" = "ParentSample")) %>% 
  count(StationName, SampleDate) %>% 
  select(-n)

# Inner join cg_station_dates df to no_comp_grab df to pull out all Companion Grab pairs
comp_grab_pairs <- inner_join(no_comp_grab, cg_station_dates)

# Remove one sample from comp_grab_pairs since it doesn't have a Companion Grab pair associated with it
comp_grab_pairs <- filter(comp_grab_pairs, !(SampleDate == "2017-02-01" & Analyte == "Boron- total"))

# Remove comp_grab_pairs from no_comp_grab df
no_comp_grab <- anti_join(no_comp_grab, comp_grab_pairs, by = c("StationName", "SampleDate", "Analyte"))

# Clean up
rm(cg_station_dates, parent_samples)

# Join parent samples and companion grabs together using suffixes
comp_grab_all <- 
  left_join(
    comp_grab_pairs,
    comp_grab,
    by = c("SampleDate", "Analyte"),
    suffix = c("_PS", "_CG")
  ) %>% 
  # Select variables to keep and rename a few
  select(
    starts_with("SampleCode"),
    StationName = StationName_PS,
    SampleDate,
    starts_with("CollectionTime"),
    Analyte,
    starts_with("Result_"),
    starts_with("ResultQual"),
    RL = RL_PS,
    MDL = MDL_PS,
    Units = Units_PS,
    starts_with("LabComments"),
    starts_with("MME_Comments")
  )

# Modify comp grab df to be exported
comp_grab_all_exp <- comp_grab_all %>% 
  # Add comments about some questionable companion grab data
  mutate(
    MME_Comments_CG = case_when(
      SampleDate == "2017-04-26" & Analyte == "MeHg- filtered" ~ "Sample appears to be unfiltered",
      SampleDate == "2017-04-26" & Analyte == "THg- total" ~ "Value appears to be biased low possibly because of inadequate mixing of the bulk sample",
      TRUE ~ MME_Comments_CG
    )
  ) %>% 
  # Only keep "DNQ" in ResultQual variables
  mutate(across(starts_with("ResultQual"), ~if_else(.x == "DNQ", "DNQ", NA_character_)))

# Export comp_grab_all_exp to .csv file
comp_grab_all_exp %>% write_excel_csv("CompanionGrabSamples.csv", na = "")
# This file was added to the SharePoint site for the Open Water Final Report 
# in the following location: 
# /Technical Appendices/Technical Appendix-B_Inlet-Outlet/Data/Final/CompanionGrabSamples.csv
# This data is also in the "Companion Grab Samples" sheet in the "YB_Inlet-Outlet_Conc_QA_Data.xlsx"
# spreadsheet in the same location on the SharePoint site
# Redundant files are in M:/YB_Inlet-Outlet_Study/Data_Final

# Decided to use Companion Grab samples to represent the actual values since they were 
# collected by hand which is how all other samples were collected from the boat. The 
# parent sample pairs were collected with a bucket sampler. Only the parent samples from 
# the 4/26/2017 event were used to represent the actual values since some of the companion
# grab values were questionable during this event.

# Modify the comp_grab_all df to switch the CGS and normal samples, except for the 4/26/2017 event
comp_grab_all_mod1 <- comp_grab_all %>% 
  filter(SampleDate != "2017-04-26") %>% 
  # Remove the variables for the parent samples
  select(-ends_with("_PS")) %>% 
  # Add a comment explaining that the CGS and Normal samples are switched
  mutate(
    MME_Comments_CG = if_else(
      is.na(MME_Comments_CG),
      "This sample was entered as a Companion Grab Sample in FLIMS and was collected by hand",
      paste0("This sample was entered as a Companion Grab Sample in FLIMS and was collected by hand; ", MME_Comments_CG)
    )
  ) %>% 
  # Remove the _CG suffix from variable names
  rename_with(~str_remove(.x, "_CG$"), ends_with("_CG"))
  
# Modify the comp_grab_all df keeping the normal samples for the 4/26/2017 event
comp_grab_all_mod2 <- comp_grab_all %>% 
  filter(SampleDate == "2017-04-26") %>% 
  # Remove the variables for the companion grab samples
  select(-ends_with("_CG")) %>% 
  # Add a comment explaining that the Normal samples were used since some of the CGS were questionable
  mutate(
    MME_Comments_PS = "This sample was entered as a normal sample in FLIMS and was collected with a bucket sampler; used these values for this station on this date since the some of the data for the CGS were questionable (the fMeHg sample appeared to be unfiltered and the tTHg value appeared to be biased low mabye due to inadequate mixing of the bulk sample)"
  ) %>% 
  # Remove the _PS suffix from variable names
  rename_with(~str_remove(.x, "_PS$"), ends_with("_PS"))

# Bind the comp_grab_all_mod1, comp_grab_all_mod2 and no_comp_grab df's
all_data4 <- bind_rows(comp_grab_all_mod1, comp_grab_all_mod2, no_comp_grab) %>% 
  # Delete ParentSample variable
  select(-ParentSample)

# Remove df that are no longer necessary
df_keep <- append(df_keep, "all_data4")
rm(list= ls()[!(ls() %in% df_keep)])


# 8.0 QA Checks ---------------------------------------------------------------

# 8.1 Qualify Detected Blank Samples --------------------------------------

# Import df with Locations where Field Blanks were collected for each SampleDate
field_blanks_loc <- read_csv("YB_Mass_Balance/Concentrations/Field_Blank_collection_loc.csv")

# Find Field Blanks with detected values
field_blanks_det <- blank_samples %>% 
  filter(
    !str_detect(ResultQual, "^<"),
    str_detect(StationName, "^Field")
  ) %>% 
  # Add StationNames where each Field Blank was collected
  left_join(field_blanks_loc, by = "SampleDate", suffix = c("_qa", "_amb"))

# Add concentration values for the ambient samples
field_blanks_det <- all_data4 %>% 
  select(StationName, SampleDate, Analyte, Result) %>% 
  right_join(
    field_blanks_det,
    by = c("StationName" = "StationName_amb", "SampleDate", "Analyte"),
    suffix = c("_amb", "_qa")
  ) %>% 
  # Calculate the Blank Conc:Ambient Conc ratio, and flag any that are greater than 0.2
  mutate(
    Blank_Amb_ratio = round(Result_qa/Result_amb, 3),
    Flag = if_else(Blank_Amb_ratio >= 0.2, "BD", NA_character_)
  ) %>% 
  # Clean up df
  select(-StationName) %>% 
  rename(
    StationName = StationName_qa,
    Result = Result_qa
  )

# Find Filter Blanks with detected values
filter_blanks_det <- blank_samples %>% 
  filter(
    !str_detect(ResultQual, "^<"),
    str_detect(StationName, "^Filter")
  )
  
# Create a new filter_blanks_det df with a new variable with SampleDate subtracted 
# by 1 to match to the ambient data
filter_blanks_det1 <- filter_blanks_det %>% 
  mutate(SampleDate = SampleDate - 1) %>% 
  select(SampleDate, Analyte)
  
# Pull out all ambient data associated with the detected filter blanks
amb_samples <- all_data4 %>% 
  inner_join(filter_blanks_det1) %>% 
  # Calculate average concentrations
  group_by(SampleDate, Analyte) %>% 
  summarize(Result_amb = mean(Result)) %>% 
  ungroup() %>% 
  # Round averages to proper number of digits
  mutate(
    Result_amb = if_else(
      str_detect(Analyte, "^MeHg"), 
      round(Result_amb, 3),
      round(Result_amb, 1)
    )
  )

# Add ambient data to filter_blanks_det df
filter_blanks_det <- filter_blanks_det %>% 
  # add a new SampleDate variable to use in join
  mutate(SampleDate_mod = SampleDate - 1) %>%
  left_join(amb_samples, by = c("SampleDate_mod" = "SampleDate", "Analyte")) %>% 
  # Calculate the Blank Conc:Ambient Conc ratio, and flag any that are greater than 0.2
  mutate(
    Blank_Amb_ratio = round(Result/Result_amb, 3),
    Flag = if_else(Blank_Amb_ratio >= 0.2, "BD", NA_character_)
  ) %>% 
  # Clean up df
  select(-SampleDate_mod)

# Bind all blank sample data back together
blank_samples <- blank_samples %>% 
  filter(str_detect(ResultQual, "^<")) %>% 
  bind_rows(field_blanks_det, filter_blanks_det) %>% 
  # Convert Result variable to character indicating <MDL and <RL values as such
  mutate(
    Result = case_when(
      ResultQual == "< RL" ~ "< RL",
      ResultQual == "< MDL" ~ "< MDL",
      TRUE ~ as.character(Result)
    )
  ) %>% 
  # Only keep "DNQ" in ResultQual variables
  mutate(across(starts_with("ResultQual"), ~if_else(.x == "DNQ", "DNQ", NA_character_))) %>%
  # Select variables to keep for export
  select(
    SampleCode:Analyte,
    Result,
    ResultQual,
    RL,
    MDL,
    Units,
    LabComments,
    MME_Comments,
    AmbSampConc = Result_amb,
    Blank_Amb_ratio,
    Flag
  )

# Export blank_samples to .csv file
blank_samples %>% write_excel_csv("BlankSamples.csv", na = "")
# This file was added to the SharePoint site for the Open Water Final Report 
# in the following location: 
# /Technical Appendices/Technical Appendix-B_Inlet-Outlet/Data/Final/BlankSamples.csv
# This data is also in the "Field and Filter Blanks" sheet in the "YB_Inlet-Outlet_Conc_QA_Data.xlsx"
# spreadsheet in the same location on the SharePoint site
# Redundant files are in M:/YB_Inlet-Outlet_Study/Data_Final
# This data was also added to the openwaterhg package as qa_field_blanks

# Clean up
rm(field_blanks_det, field_blanks_loc, filter_blanks_det, filter_blanks_det1, amb_samples)

# Add QualCodes to ambient samples with associated blank samples with a "BD" flag
# Pull out Blanks with "BD" flag
blank_samples_flag <- blank_samples %>% 
  filter(Flag == "BD") %>% 
  select(SampleDate, Analyte) %>% 
  # Subtract one day from SampleDate for the THg- filtered blank
  mutate(SampleDate = if_else(Analyte == "THg- filtered", SampleDate - 1, SampleDate))

# Pull out matching ambient samples and add QualCode variable
all_data4_flag_bd <- all_data4 %>% 
  inner_join(blank_samples_flag) %>% 
  mutate(QualCode = "J- BD")

# Bind data back together
all_data5 <- all_data4 %>% 
  anti_join(blank_samples_flag) %>% 
  bind_rows(all_data4_flag_bd)

# Clean up
rm(blank_samples_flag, all_data4_flag_bd)


# 8.2 Qualify samples with high Field Variability -------------------------

# Pull out Field Duplicates with "FV" flag
field_dup_flag <- field_dups_all %>% 
  filter(Flag == "FV") %>% 
  select(SampleDate, Analyte)

# Pull out matching ambient samples and add comment to QualCode variable
all_data5_flag_fv <- all_data5 %>% 
  inner_join(field_dup_flag) %>% 
  mutate(
    QualCode = case_when(
      Analyte == "Potassium- total" ~ "R- FV",
      QualCode == "J- BD" ~ "J- BD, FV",
      TRUE ~ "J- FV"
    )
  )

# Bind data back together
all_data6 <- all_data5 %>% 
  anti_join(field_dup_flag) %>% 
  bind_rows(all_data5_flag_fv)

# Clean up
rm(field_dup_flag, all_data5_flag_fv)


# 8.3 Check for filtered values that are greater than the total values --------

# Pull out all analytes with associated filtered and total measurements and find filtered
# that are greater than the total values
filt_g_total <- all_data6 %>% 
  select(SampleCode, Analyte, Result) %>% 
  filter(str_detect(Analyte, "^MeHg|^THg|OC$")) %>% 
  # Separate analyte and fraction into 2 individual variables
  mutate(
    Analyte = case_when(
      Analyte == "TOC" ~ "OrgC- total",
      Analyte == "DOC" ~ "OrgC- filtered",
      TRUE ~ Analyte
    )
  ) %>% 
  separate(Analyte, into = c("Analyte", "Fraction"), sep = "- ") %>% 
  pivot_wider(names_from = Fraction, values_from = Result) %>% 
  # Look for filtered > total
  filter(filtered > total) %>% 
  # Restructure dataframe to use in a join
  select(-c(total, filtered)) %>% 
  mutate(Analyte = if_else(Analyte == "MeHg", "MeHg- total/MeHg- filtered", "TOC/DOC")) %>% 
  separate_rows(Analyte, sep = "/")

# Pull out samples and add comment to QualCode variable
all_data6_flag_fgt <- all_data6 %>% 
  inner_join(filt_g_total) %>% 
  mutate(QualCode = "R- FGT")

# Bind data back together
all_data7 <- all_data6 %>% 
  anti_join(filt_g_total) %>% 
  bind_rows(all_data6_flag_fgt)

# Clean up
rm(filt_g_total, all_data6_flag_fgt)


# 8.4 Flag any other data to exclude from analyses ------------------------

# CCSB low flow channel on 3/15/2016 - 
# Sample was collected inside of the CCSB, which is not representative of this station
# Pull out these samples
nr_sample <- all_data7 %>% 
  filter(
    SampleDate == "2016-03-15",
    StationName == "CCSB- Low Flow Channel"
  )

# Add Qual Codes and Comments
all_data7_flag_nrs <- nr_sample %>% 
  mutate(
    QualCode = if_else(is.na(QualCode), "R- NRS", "R- NRS, FV"),
    MME_Comments = if_else(
      is.na(MME_Comments),
      "Sample was collected inside of the CCSB- not representative of this station",
      paste0("Sample was collected inside of the CCSB- not representative of this station; ", MME_Comments)
    )
  )

# Bind data back together
all_data8 <- all_data7 %>% 
  anti_join(nr_sample) %>% 
  bind_rows(all_data7_flag_nrs)

# Clean up
rm(nr_sample, all_data7_flag_nrs)

# Export all_data8 to .csv file
all_data8 %>% 
  mutate(
    # Convert Result variable to character indicating <MDL and <RL values as such
    Result = case_when(
      ResultQual == "< RL" ~ "< RL",
      ResultQual == "< MDL" ~ "< MDL",
      TRUE ~ as.character(Result)
    ),
    # Move DNQ flag to QualCode
    QualCode = case_when(
      is.na(QualCode) & str_detect(ResultQual, "^DNQ") ~ paste0("J- ", ResultQual),
      !is.na(QualCode) & str_detect(ResultQual, "^DNQ") ~ paste(QualCode, ResultQual, sep = ", "),
      TRUE ~ QualCode
    )
  ) %>% 
  # Remove ResultQual
  select(-ResultQual) %>% 
  write_excel_csv("NormalSamples.csv", na = "")

# This final copy of the Lab concentration data for the Yolo Bypass Inlet-Outlet Study
# was added to the SharePoint site for the Open Water Final Report in the following location: 
# /Technical Appendices/Technical Appendix-B_Inlet-Outlet/Data/Final/NormalSamples.csv
# A redundant file is in M:/YB_Inlet-Outlet_Study/Data_Final
# This data was also added to the openwaterhg package as conc_data

