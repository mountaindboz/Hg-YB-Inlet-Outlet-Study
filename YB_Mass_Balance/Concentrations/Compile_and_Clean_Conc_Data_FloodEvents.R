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
    # Convert Result variable to character data type
    mutate(
      Result = if_else(ResQualCode == "ND", "< MDL", as.character(Result)),
      # Create a new variable ResQual where ND = 1 and Detect = 0
      ResQual = if_else(ResQualCode == "ND", 1, 0)
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
      ResQual,
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
    # Keep necessary variables
    select(
      StationNameStd,
      SampleCode,
      SampleDate,
      CollectionTime,
      AnalyteStd,
      Result,
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
    ) %>% 
    # Clean up Result variable
    mutate(
      # Create a new variable ResQual where ND = 1 and Detect = 0
      ResQual = if_else(str_detect(Result, "^<"), 1, 0),
      # Convert Result variable to numeric
      Result = if_else(ResQual == 1, RL, as.numeric(Result)),
      # Round Result variable to specified number of digits in n_round
      Result = round(Result, n_round),
      # Convert Result variable back to character
      Result = if_else(ResQual == 1, "< RL", as.character(Result))
    )
    
# Clean up 
df_keep <- append(df_keep, "bryte_data_clean")
rm(list= ls()[!(ls() %in% df_keep)])
  

# 3.0 All Data ----------------------------------------------------------------

# Combine Contract and Bryte Lab Data
all_data <- bind_rows(contract_data_clean, bryte_data_clean) %>% 
  # Create a new variable Conc, which is a numeric version of Result with the MDL and RL for the ND values
  mutate(
    Conc = case_when(
      Result == "< RL"  ~ RL,
      Result == "< MDL" ~ MDL,
      TRUE              ~ as.numeric(Result)
    )
  )


# 4.0 Lab Replicates ----------------------------------------------------------

# Create a df of all Lab Replicates
lab_reps <- all_data %>% 
  count(SampleCode, Analyte) %>% 
  filter(n == 2) %>% 
  select(-n)
  
# Pull out a df of all Lab Replicates only including variables that are unique in Rep1 and Rep2
lab_reps_u <- inner_join(all_data, lab_reps) %>% 
  select(SampleCode, Analyte, Result, Conc)

# Pull out a df of all Lab Replicates only including variables that are identical in Rep1 and Rep2
lab_reps_i <- inner_join(all_data, lab_reps) %>% 
  select(-c(Result, LabComments, Conc)) %>% 
  # Average ResQual variable for each Replicate group
  group_by(SampleCode, Analyte) %>% 
  mutate(ResQual = mean(ResQual)) %>% 
  ungroup() %>% 
  # Remove duplicate rows
  distinct()
  
# Pull out a df of all data not including Lab Replicates
no_lab_reps <- anti_join(all_data, lab_reps)

# Create two different df to spread out unique variables
  # Result
  S_Result <- lab_reps_u %>% 
    select(-Conc) %>%
    group_by(SampleCode, Analyte) %>% 
    mutate(Rep = paste0("Result", row_number())) %>% 
    ungroup() %>% 
    pivot_wider(names_from = Rep, values_from = Result)
    
  # Conc
  S_Conc <- lab_reps_u %>% 
    select(-Result) %>%
    group_by(SampleCode, Analyte) %>% 
    mutate(Rep = paste0("Conc", row_number())) %>% 
    ungroup() %>% 
    pivot_wider(names_from = Rep, values_from = Conc)
  
# Join all of the df back together
lab_rep_data <-
  reduce(list(S_Conc, S_Result, lab_reps_i), left_join) %>% 
  # Calculate RPD values for each replicate pair and flag if necessary
  mutate(
    RPD = round(abs(Conc1 - Conc2)/((Conc1 + Conc2)/2), 3),
    Flag = case_when(
      Analyte %in% contract_ana & RPD > 0.25 & (Conc1 > 10 * MDL | Conc2 > 10 * MDL) ~ "y",
      !Analyte %in% contract_ana & RPD > 0.25 & (Conc1 > 10 * RL | Conc2 > 10 * RL) ~ "y",
      TRUE ~ NA_character_
    )
  )

# Export lab_rep_data to .csv file- only needed once
# lab_rep_data %>%
#   select(
#     SampleCode,
#     StationName,
#     SampleDate,
#     CollectionTime,
#     Analyte,
#     LabBatch,
#     Result1,
#     Result2,
#     RPD,
#     ResQual,
#     RL,
#     MDL,
#     Units,
#     MME_Comments,
#     Flag
#   ) %>%
#   write_excel_csv("LabReplicates.csv", na = "")

# Modify the lab_rep_data df
lab_rep_data_mod <- lab_rep_data %>% 
  mutate(
    # Average Concentration values
    Conc = (Conc1 + Conc2)/2,
    # Add Result variable back to df
    Result = case_when(
      ResQual == 1 & Analyte %in% contract_ana ~ "< MDL",
      ResQual == 1 & !Analyte %in% contract_ana ~ "< RL",
      TRUE ~ as.character(Conc)
    ),
    # Add comment about using the average of lab replicates
    MME_Comments = if_else(
      is.na(MME_Comments),
      "Average of Lab Replicates",
      paste("Average of Lab Replicates", MME_Comments, sep = "; ")
    )
  ) %>% 
  # Delete a few variables
  select(
    -c(
      Conc1, 
      Conc2,
      Result1, 
      Result2,
      rpd,
      Flag
    )
  )
  
# Bind the lab_rep_data_mod df back with the no_lab_reps df
all_data1 <- bind_rows(no_lab_reps, lab_rep_data_mod)
  
# Remove df that are no longer necessary
df_keep <- append(df_keep, c("all_data", "all_data1", "lab_rep_data"))
rm(list= ls()[!(ls() %in% df_keep)])


# 5.0 Blanks and QA Information -----------------------------------------------

# Create a .csv file that summarizes the Lab Methods used for each analyte- only needed once
# all_data1 %>%
#   count(Analyte, Method) %>%
#   select(-n) %>%
#   write_excel_csv("AnalyteMethods.csv", na = "")
  
# Export a .csv file for Lab QA batches- only needed once
# all_data1 %>%
#   select(LabBatch, SampleCode, Analyte) %>%
#   write_excel_csv("Lab_QA_Batch.csv", na = "")

# Export a .csv file for Analysis dates for Lab QA batches- only needed once
# all_data1 %>%
#   select(LabBatch, AnalysisDate) %>%
#   distinct() %>%
#   # removing Lab Batch: MPSL-DFG_20160427_W_MeHg with AnalysisDate of 4/28/2016, since all
#   # but one sample in this batch were analyzed on 4/27/2016
#   filter(!(LabBatch == "MPSL-DFG_20160427_W_MeHg" & AnalysisDate == "2016-04-28")) %>%
#   write_excel_csv("Lab_QA_Batch_AnaDate.csv", na = "")

# Remove a few QA related variables
all_data1 <- all_data1 %>% 
  select(-c(LabBatch, AnalysisDate, Method))
  
# Pull out Blank Samples and save for QA validation
blank_samples <- all_data1 %>% 
  filter(str_detect(StationName, "Blank$")) %>% 
  # Round Conc variable and update Result variable with rounded values
  mutate(
    Conc = case_when(
      str_detect(Analyte, "^Boron|^MeHg") ~ round(Conc, 3),
      Analyte %in% c("Iron- filtered", "Manganese- filtered") ~ signif(Conc, 3),
      TRUE ~ round(Conc, n_round)
    ),
    Result = case_when(
      ResQual == 1 & Analyte %in% contract_ana ~ "< MDL",
      ResQual == 1 & !Analyte %in% contract_ana ~ "< RL",
      TRUE ~ as.character(Conc)
    )
  ) %>% 
  select(-c(ResQual, ParentSample:Conc))

# Remove Blank samples from all_data df
all_data2 <- anti_join(all_data1, blank_samples, by = c("SampleCode", "Analyte"))


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
  
# Bind field_dups and field_dup_pairs together
field_dups_all <- bind_rows(field_dups, field_dup_pairs)
  
# Clean up
rm(parent_samples, fd_station_dates, field_dups, field_dup_pairs)
  
# Separate field dups from parent samples, then join together using suffixes
field_dups_fd <- field_dups_all %>% 
  filter(StationName == "Field Duplicate Sample")

field_dups_ps <- field_dups_all %>% 
  filter(StationName != "Field Duplicate Sample")
    
field_dups_all <- 
  left_join(
    field_dups_ps,
    field_dups_fd,
    by = c("SampleDate", "Analyte"),
    suffix = c("_PS", "_FD")
  ) %>% 
  select(
    SampleCode_PS,
    SampleCode_FD,
    StationName_PS,
    SampleDate,
    CollectionTime_PS,
    CollectionTime_FD,
    Analyte,
    Result_PS,
    Result_FD,
    Conc_PS,
    Conc_FD,
    ResQual_PS,
    ResQual_FD,
    RL_PS,
    MDL_PS,
    Units_PS,
    LabComments_PS,
    LabComments_FD,
    MME_Comments_PS,
    MME_Comments_FD,
    n_round_PS
  ) %>% 
  rename(
    StationName = StationName_PS,
    RL = RL_PS,
    MDL = MDL_PS,
    Units = Units_PS,
    n_round = n_round_PS
  )

# Clean up 
rm(field_dups_fd, field_dups_ps)

# Modify field dup df
field_dup_data <- field_dups_all %>% 
  # Calculate RPD values for each replicate pair and flag if necessary
  mutate(
    RPD = round(abs(Conc_PS - Conc_FD)/((Conc_PS + Conc_FD)/2), 3),
    Flag = case_when(
      Analyte %in% contract_ana & RPD > 0.25 & (Conc_PS > 10 * MDL | Conc_FD > 10 * MDL) ~ "FV",
      (!Analyte %in% contract_ana & !Analyte %in% c("DOC", "TOC", "VSS")) & RPD > 0.25 & (Conc_PS > 10 * RL | Conc_FD > 10 * RL) ~ "FV",
      Analyte %in% c("DOC", "TOC", "VSS") & RPD > 0.3 & (Conc_PS > 10 * RL | Conc_FD > 10 * RL) ~ "FV",
      TRUE ~ "n"
    ),
    # Average ResQual values
    ResQual = (ResQual_PS + ResQual_FD)/2,
    # Add one comment regarding using the MDL value to calculate the RPD of one of the duplicate pairs
    MME_Comments_PS = if_else(
      ResQual == 0.5, 
      "Used the MDL value to calculate RPD of the Duplicates",
      MME_Comments_PS
    )
  ) %>% 
  select(-c(ResQual_PS, ResQual_FD))

# Export field_dup_data to .csv file- added to Final data spreadsheet, and kept as a separate
# .csv file to be added as a dataset to the openwaterhg package
field_dup_data %>%
  select(
    SampleCode_PS:Result_FD,
    RPD,
    ResQual,
    RL:MME_Comments_FD,
    Flag
  ) %>%
  write_excel_csv("FieldDuplicates.csv", na = "")  # moved to SharePoint site
  
# Modify the field_dup_data df
field_dup_data_mod <- field_dup_data %>% 
  # Average Concentration values
  mutate(
    Conc = (Conc_PS + Conc_FD)/2,
    # Add comment about using the average of field duplicates
    MME_Comments = case_when(
      ResQual == 1 ~ "Average of Field Duplicates, both values were < MDL",
      is.na(MME_Comments_PS) & is.na(MME_Comments_FD) ~ "Average of Field Duplicates",
      ResQual == 0.5 ~ "Average of Field Duplicates, used the MDL value for the Parent Sample",
      ResQual == 0 & (!is.na(MME_Comments_PS) | !is.na(MME_Comments_FD)) ~ "Average of Field Duplicates and Lab Replicates"
    ),
    # Consolidate Lab Comments
    LabComments = case_when(
      !is.na(LabComments_PS) & !is.na(LabComments_FD) ~ paste0("Parent Sample: ", LabComments_PS, "; Field Dup: ", LabComments_FD),
      !is.na(LabComments_PS) & is.na(LabComments_FD) ~ paste0("Parent Sample: ", LabComments_PS),
      is.na(LabComments_PS) & !is.na(LabComments_FD) ~ paste0("Field Dup: ", LabComments_FD)
    )
  ) %>% 
  # Rename some variables
  rename(
    SampleCode = SampleCode_PS,
    CollectionTime = CollectionTime_PS
  ) %>% 
  # Delete a few variables
  select(
    -c(
      ends_with("_PS"),
      ends_with("_FD"),
      RPD,
      Flag
    )
  )

# Bind the field_dup_data_mod df back with the no_field_dups df
all_data3 <- bind_rows(no_field_dups, field_dup_data_mod) %>% 
  # Round Conc variable and update Result variable with rounded values
  mutate(
    Conc = case_when(
      str_detect(Analyte, "^Boron|^MeHg") ~ round(Conc, 3),
      Analyte %in% c("Iron- filtered", "Manganese- filtered") ~ signif(Conc, 3),
      TRUE ~ round(Conc, n_round)
    ),
    Result = case_when(
      ResQual == 1 & Analyte %in% contract_ana ~ "< MDL",
      ResQual == 1 & !Analyte %in% contract_ana ~ "< RL",
      TRUE ~ as.character(Conc)
    )
  ) %>% 
  # remove Conc and n_round variables
  select(-c(Conc, n_round))

# Remove df that are no longer necessary
df_keep <- append(df_keep, c("all_data2", "all_data3", "blank_samples", "field_dup_data"))
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

# Bind comp_grap and comp_grab_pairs together
comp_grab_all <- bind_rows(comp_grab, comp_grab_pairs)

# Clean up
rm(cg_station_dates, comp_grab, comp_grab_pairs, parent_samples)

# Separate companion grabs from parent samples, then join together using suffixes
comp_grab_cg <- comp_grab_all %>% 
  filter(StationName == "Companion Grab Sample")

comp_grab_ps <- comp_grab_all %>% 
  filter(StationName != "Companion Grab Sample")

comp_grab_all <- 
  left_join(
    comp_grab_ps,
    comp_grab_cg,
    by = c("SampleDate", "Analyte"),
    suffix = c("_PS", "_CG")
  ) %>% 
  select(
    SampleCode_PS,
    SampleCode_CG,
    StationName_PS,
    SampleDate,
    CollectionTime_PS,
    CollectionTime_CG,
    Analyte,
    Result_PS,
    Result_CG,
    ResQual_PS,
    ResQual_CG,
    RL_PS,
    MDL_PS,
    Units_PS,
    LabComments_PS,
    LabComments_CG,
    MME_Comments_PS,
    MME_Comments_CG
  ) %>% 
  rename(
    StationName = StationName_PS,
    RL = RL_PS,
    MDL = MDL_PS,
    Units = Units_PS
  )

# Clean up 
rm(comp_grab_cg, comp_grab_ps)

# Export CompanionGrabData to .csv file- only needed once
# comp_grab_all %>%
#   # Add comment about some questionable companion grab data
#   mutate(
#     MME_Comments_CG = case_when(
#       SampleDate == "2017-04-26" & Analyte == "MeHg- filtered" ~ "Sample appears to be unfiltered",
#       SampleDate == "2017-04-26" & Analyte == "THg- total" ~ "Value appears to be biased low possibly because of inadequate mixing of the bulk sample",
#       TRUE ~ MME_Comments_CG
#     )
#   ) %>%
#   write_excel_csv("CompanionGrabSamples.csv", na = "")

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
  )
  
# Rename a few variables of comp_grab_all_mod1
names(comp_grab_all_mod1) <- str_replace_all(names(comp_grab_all_mod1), "_CG$", "")

# Modify the comp_grab_all df keeping the normal samples for the 4/26/2017 event
comp_grab_all_mod2 <- comp_grab_all %>% 
  filter(SampleDate == "2017-04-26") %>% 
  # Remove the variables for the companion grab samples
  select(-ends_with("_CG")) %>% 
  # Add a comment explaining that the Normal samples were used since some of the CGS were questionable
  mutate(
    MME_Comments_PS = "This sample was entered as a normal sample in FLIMS and was collected with a bucket sampler; used these values for this station on this date since the some of the data for the CGS were questionable (the fMeHg sample appeared to be unfiltered and the tTHg value appeared to be biased low mabye due to inadequate mixing of the bulk sample)"
  )

# Rename a few variables of comp_grab_all_mod2
names(comp_grab_all_mod2) <- str_replace_all(names(comp_grab_all_mod2), "_PS$", "")
  
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
    !str_detect(Result, "^<"),
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
    Result_amb = as.numeric(Result_amb),
    Result_qa = as.numeric(Result_qa),
    blank_amb_ratio = round(Result_qa/Result_amb, 3),
    Flag = if_else(blank_amb_ratio >= 0.2, "BD", "n")
  ) %>% 
  # Clean up df
  select(-StationName) %>% 
  rename(
    StationName = StationName_qa,
    Result = Result_qa
  ) %>% 
  mutate(Result = as.character(Result))

# Find Filter Blanks with detected values
filter_blanks_det <- blank_samples %>% 
  filter(
    !str_detect(Result, "^<"),
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
  # Convert Result to numeric
  mutate(Result = as.numeric(Result)) %>% 
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
    Result = as.numeric(Result),
    blank_amb_ratio = round(Result/Result_amb, 3),
    Flag = if_else(blank_amb_ratio >= 0.2, "BD", "n")
  ) %>% 
  # Clean up df
  select(-SampleDate_mod) %>% 
  mutate(Result = as.character(Result))

# Bind all blank sample data back together
blank_samples <- blank_samples %>% 
  filter(str_detect(Result, "^<")) %>% 
  bind_rows(field_blanks_det, filter_blanks_det) %>% 
  select(SampleCode:Analyte, Result, RL, MDL, Units, LabComments:Flag)

# Export blank_samples to .csv file- added to Final data spreadsheet, and kept as a separate
# .csv file to be added as a dataset to the openwaterhg package
blank_samples %>% write_excel_csv("BlankSamples.csv", na = "")  # moved to SharePoint site

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
all_data4 <- all_data4 %>% 
  anti_join(blank_samples_flag) %>% 
  bind_rows(all_data4_flag_bd)

# Clean up
rm(blank_samples_flag, all_data4_flag_bd)


# 8.2 Qualify samples with high Field Variability -------------------------

# Pull out Field Duplicates with "FV" flag
field_dup_flag <- field_dup_data %>% 
  filter(Flag == "FV") %>% 
  select(SampleDate, Analyte)

# Pull out matching ambient samples and add comment to QualCode variable
all_data4_flag_fv <- all_data4 %>% 
  inner_join(field_dup_flag) %>% 
  mutate(
    QualCode = case_when(
      Analyte == "Potassium- total" ~ "R- FV",
      QualCode == "J- BD" ~ "J- BD, FV",
      TRUE ~ "J- FV"
    )
  )

# Bind data back together
all_data4 <- all_data4 %>% 
  anti_join(field_dup_flag) %>% 
  bind_rows(all_data4_flag_fv)

# Clean up
rm(field_dup_flag, all_data4_flag_fv)


# 8.3 Check for filtered values that are greater than the total values --------

# Pull out all analytes with associated filtered and total measurements and find filtered
# that are greater than the total values
filt_g_total <- all_data4 %>% 
  mutate(
    Conc = case_when(
      Result == "< RL"  ~ RL,
      Result == "< MDL" ~ MDL,
      TRUE              ~ as.numeric(Result)
    )
  ) %>% 
  select(SampleCode, Analyte, Conc) %>% 
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
  pivot_wider(names_from = Fraction, values_from = Conc) %>% 
  # Look for filtered > total
  filter(filtered > total) %>% 
  # Restructure dataframe to use in a join
  select(-c(total, filtered)) %>% 
  mutate(Analyte = if_else(Analyte == "MeHg", "MeHg- total/MeHg- filtered", "TOC/DOC")) %>% 
  separate_rows(Analyte, sep = "/")

# Pull out samples and add comment to QualCode variable
all_data4_flag_fgt <- all_data4 %>% 
  inner_join(filt_g_total) %>% 
  mutate(QualCode = "R- FGT")

# Bind data back together
all_data4 <- all_data4 %>% 
  anti_join(filt_g_total) %>% 
  bind_rows(all_data4_flag_fgt)

# Clean up
rm(filt_g_total, all_data4_flag_fgt)


# 8.4 Flag any other data to exclude from analyses ------------------------

# CCSB low flow channel on 3/15/2016 - 
# Sample was collected inside of the CCSB, which is not representative of this station
# Pull out these samples
nr_sample <- all_data4 %>% 
  filter(
    SampleDate == "2016-03-15",
    StationName == "CCSB- Low Flow Channel"
  )

# Add Qual Codes and Comments
all_data4_flag_nrs <- nr_sample %>% 
  mutate(
    QualCode = if_else(is.na(QualCode), "R- NRS", "R- NRS, FV"),
    MME_Comments = if_else(
      is.na(MME_Comments),
      "Sample was collected inside of the CCSB- not representative of this station",
      paste0("Sample was collected inside of the CCSB- not representative of this station; ", MME_Comments)
    )
  )

# Bind data back together
all_data4 <- all_data4 %>% 
  anti_join(nr_sample) %>% 
  bind_rows(all_data4_flag_nrs)

# Clean up
rm(nr_sample, all_data4_flag_nrs)

# Export all_data4 to .csv file- added to Final data spreadsheet, and kept as a separate
# .csv file to be added as a dataset to the openwaterhg package
all_data4 %>% write_excel_csv("NormalSamples.csv", na = "")  # moved to SharePoint site

# The final copy of the Lab concentration data for the Yolo Bypass Inlet-Outlet Study is 
# located here: M:\Data\Lab_Final\YB_Inlet-Outlet_Conc_Data.xlsx

