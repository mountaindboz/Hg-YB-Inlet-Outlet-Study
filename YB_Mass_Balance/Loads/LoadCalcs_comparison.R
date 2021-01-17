# Yolo Bypass Inlet-Outlet Study
# Purpose: Compare two different approaches to calculate loads for:
  # Outlet- SCHISM vs. Balanced (Outlet flow = sum of Inlet flows)
  # Below Liberty Island- raw vs. scaled (adjusted using the sum of Inlet flows)
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(lubridate)
library(openwaterhg)

# 1. Concentration Data ------------------------------------------------------

# 1.1 Import and Clean data -----------------------------------------------

# Clean conc_data
conc_data_clean <- conc_data %>% 
  # Remove samples with QualCode "R"
  filter(is.na(QualCode) | !str_detect(QualCode, "^R")) %>%
  # Create a new variable Conc, which is a numeric version of Result with the MDL and RL for the below detection values
  add_num_result() %>% 
  # Clean up df
  select(SampleCode:Analyte, Conc, RL:QualCode)

# Bind conc_data with calculated data for the particulate fractions of Hg, MeHg, and organic carbon
all_conc <- bind_rows(conc_data_clean, part_conc_calc)

# Create a vector of all stations to include
Stations <- c(
  "Cache Slough near Ryer Island",
  "CCSB- Low Flow Channel",
  "CCSB Overflow Weir- North",
  "CCSB Overflow Weir- South",
  "Fremont Weir- East Side",
  "Fremont Weir- Middle",
  "Fremont Weir- West Side",
  "Knights Landing Ridge Cut",
  "Liberty Cut below Stairsteps",
  "Miner Slough near Sac River",
  "Putah Creek at Mace Blvd",           
  "Sac River above the Sacramento Weir",
  "Shag Slough below Stairsteps",
  "Toe Drain at 1/2 Lisbon"            
)
  
# Create a vector of all analytes to include
Analytes <- c(
  "Chloride- filtered",
  "DOC",
  "Iron- filtered",
  "Manganese- filtered",
  "MeHg- filtered",
  "MeHg- particulate",
  "MeHg- total",
  "POC",
  "THg- filtered",
  "THg- particulate",
  "THg- total",
  "TOC",
  "TSS",
  "VSS"
)

# Structure all_conc dataframe to be used for load calculations
all_conc_clean <- all_conc %>% 
  # Keep only necessary data
  filter(
    StationName %in% Stations,
    Analyte %in% Analytes
  ) %>% 
  # Create a standardized variable to identify each unique sampling event
  add_samplingevent() %>% 
  mutate(
    # Add a variable for the year
    Year = year(SampleDate),
    # Calculate the number of significant digits in the Conc values
    digits = case_when(
      Analyte %in% c("Iron- filtered", "Manganese- filtered") ~ 3,
      str_detect(Analyte, "^MeHg") & Conc < 0.01 ~ 1,
      str_detect(Analyte, "^MeHg") & Conc < 0.1 ~ 2,
      str_detect(Analyte, "^MeHg") ~ 3,
      Analyte %in% c("Chloride- filtered", "TSS", "VSS") & Conc < 10 ~ 1,
      Analyte %in% c("Chloride- filtered", "TSS", "VSS") & Conc < 100 ~ 2,
      Analyte %in% c("Chloride- filtered", "TSS", "VSS") ~ 3,
      str_detect(Analyte, "^THg|OC$") & Conc < 1 ~ 1,
      str_detect(Analyte, "^THg|OC$") & Conc < 10 ~ 2,
      str_detect(Analyte, "^THg|OC$") & Conc ~ 3
    )
  ) %>%
  # Keep only necessary variables
  select(
    SamplingEvent,
    Year,
    StationName,
    Analyte,
    Conc,
    Units,
    digits
  )


# 1.2 Average Concentration data for a few of the Input stations ------------
# CCSB Overflow Weir- North and South
CCSB <- all_conc_clean %>% 
  filter(str_detect(StationName, "Overflow")) %>% 
  # Group and summarize to average the North and South stations
  group_by(SamplingEvent, Year, Analyte, Units) %>% 
  summarize(
    "CCSB- Overflow Weir" = mean(Conc),
    digits = min(digits)
  ) %>%
  ungroup() %>% 
  # Restructure dataframe back into long format
  pivot_longer(
    cols = "CCSB- Overflow Weir", 
    names_to = "StationName", 
    values_to = "Conc"
  )

# Fremont Weir
Fremont <- all_conc_clean %>% 
  # Filter out Fremont Weir stations
  filter(str_detect(StationName, "^Fremont Weir")) %>% 
  # Group and summarize to average the Fremont Weir stations
  group_by(SamplingEvent, Year, Analyte, Units) %>% 
  summarize(
    "Fremont Weir" = mean(Conc),
    digits = min(digits),
  ) %>%
  ungroup() %>% 
  # Restructure dataframe back into long format
  pivot_longer(
    cols = "Fremont Weir", 
    names_to = "StationName", 
    values_to = "Conc"
  )

# Add back CCSB and Fremont Weir dataframes to all_conc_clean dataframe
all_conc_clean1 <- all_conc_clean %>% 
  filter(!str_detect(StationName, "Overflow|^Fremont")) %>% 
  bind_rows(CCSB, Fremont)

# Clean up
rm(CCSB, Fremont)


# 1.3 Estimate some Organic Carbon data -----------------------------------
# The DOC concentration was greater than the TOC concentration at Liberty Cut on 2/16/2017
# Need to estimate these values in order to calculate loads by using the averages of
# the Toe Drain at 1/2 Lisbon and Shag Slough stations
oc_out_feb16 <- all_conc_clean1 %>% 
  filter(
    StationName %in% c("Toe Drain at 1/2 Lisbon", "Shag Slough below Stairsteps"),
    Analyte %in% c("TOC", "DOC", "POC"),
    SamplingEvent == "Feb 14-15, 2017"
  ) %>% 
  group_by(SamplingEvent, Year, Analyte, Units) %>% 
  summarize(
    Conc = mean(Conc),
    digits = min(digits)
  ) %>% 
  ungroup() %>% 
  mutate(StationName = "Liberty Cut below Stairsteps")

all_conc_clean2 <- bind_rows(all_conc_clean1, oc_out_feb16)

# Clean up
rm(oc_out_feb16)


# 1.4 Create some additional outlet sampling locations ----------------------
# This is necessary to match the flow data from the SCHISM model

# Define concentration values for the outlets for the 2016 sampling event
OutSta_2016_c <- all_conc_clean2 %>%
  # Filter out the outlet stations
  filter(
    Year == 2016,
    StationName %in% c(
      "Liberty Cut below Stairsteps",
      "Shag Slough below Stairsteps",
      "Toe Drain at 1/2 Lisbon"
    )
  ) %>%
  # Restructure the dataframe
  pivot_wider(
    id_cols = -digits,
    names_from = StationName,
    values_from = Conc
  ) %>%
  rename(
    Liberty = "Liberty Cut below Stairsteps",
    Shag = "Shag Slough below Stairsteps",
    Toe = "Toe Drain at 1/2 Lisbon"
  ) %>%
  # Create new variables for Liberty Island Breach locations
  mutate(
    "Liberty Island Breach 1" = Shag,  #this breach is closest to the Shag Slough site
    "Liberty Island Breach 2" = Liberty,  #this breach is closest to the Liberty Cut site
    "Liberty Island Breach 3" = Toe  #this breach is closest to the Toe Drain at 1/2 Lisbon site
  ) %>%
  # Remove Liberty, Shag, and Toe since they are no longer necessary
  select(-c(Toe, Liberty, Shag)) %>%
  # Pivot dataframe back into long format
  pivot_longer(
    cols = c("Liberty Island Breach 1", "Liberty Island Breach 2", "Liberty Island Breach 3"),
    names_to = "StationName",
    values_to = "Conc"
  )

# Define significant digits for each new station
OutSta_2016_d <- all_conc_clean2 %>%
  # Filter out the outlet stations
  filter(
    Year == 2016,
    StationName %in% c(
      "Liberty Cut below Stairsteps",
      "Shag Slough below Stairsteps",
      "Toe Drain at 1/2 Lisbon"
    )
  ) %>%
  # Restructure the dataframe
  pivot_wider(
    id_cols = -Conc,
    names_from = StationName,
    values_from = digits
  ) %>%
  rename(
    Liberty = "Liberty Cut below Stairsteps",
    Shag = "Shag Slough below Stairsteps",
    Toe = "Toe Drain at 1/2 Lisbon"
  ) %>%
  # Create new variables for Liberty Island Breach locations
  mutate(
    "Liberty Island Breach 1" = Shag,  #this breach is closest to the Shag Slough site
    "Liberty Island Breach 2" = Liberty,  #this breach is closest to the Liberty Cut site
    "Liberty Island Breach 3" = Toe  #this breach is closest to the Toe Drain at 1/2 Lisbon site
  ) %>%
  # Remove Liberty, Shag, and Toe since they are no longer necessary
  select(-c(Toe, Liberty, Shag)) %>%
  # Pivot dataframe back into long format
  pivot_longer(
    cols = c("Liberty Island Breach 1", "Liberty Island Breach 2", "Liberty Island Breach 3"),
    names_to = "StationName",
    values_to = "digits"
  )

# Join dataframes with concentrations and significant digits together
OutSta_2016 <- left_join(OutSta_2016_c, OutSta_2016_d)

# Define concentration values for the outlets for the 2017 sampling events
OutSta_2017_c <- all_conc_clean2 %>%
  # Filter out the outlet stations
  filter(
    Year == 2017,
    StationName %in% c(
      "Liberty Cut below Stairsteps",
      "Shag Slough below Stairsteps",
      "Toe Drain at 1/2 Lisbon"
    )
  ) %>%
  # Restructure the dataframe
  pivot_wider(
    id_cols = -digits,
    names_from = StationName,
    values_from = Conc
  ) %>%
  rename(
    Liberty = "Liberty Cut below Stairsteps",
    Shag = "Shag Slough below Stairsteps",
    Toe = "Toe Drain at 1/2 Lisbon"
  ) %>%
  # Create a new variable for Little Holland and assign values
  mutate(
    "Little Holland" = if_else(
      SamplingEvent != "Apr 11-12, 2017",
      (Toe + Liberty)/2,
      NULL  #only sampled Toe Drain at 1/2 Lisbon on April 12, 2017
    )
  ) %>%
  # Create a new variable for Main Liberty and assign values
  mutate(
    "Main Liberty" = case_when(
      # For sampling events with good mixing across the Bypass use the average of Liberty Cut and Shag Slough
      SamplingEvent %in% c("Jan 11-12, 2017", "Jan 31-Feb 1, 2017", "Feb 14-15, 2017", "Mar 1-2, 2017", "Mar 15-16, 2017") ~ (Liberty + Shag)/2,
      # For sampling events when Shag Slough was much different use the concentrations from Liberty Cut
      SamplingEvent %in% c("Jan 24-25, 2017", "Mar 28-29, 2017", "Apr 25-26, 2017") ~ Liberty
      # Only sampled Toe Drain on April 12, so not necessary to assign value for this event
    )
  ) %>%
  # Remove Liberty, Shag, and Toe since they are no longer necessary
  select(-c(Shag, Liberty, Toe)) %>%
  # Pivot dataframe back into long format
  pivot_longer(
    cols = c("Little Holland", "Main Liberty"),
    names_to = "StationName",
    values_to = "Conc"
  ) %>%
  # Remove NA values
  filter(!is.na(Conc))

# Define significant digits for each new station
# Toe Drain at 1/2 Lisbon and Liberty Cut combination
OutSta_2017_d_ToeLib <- all_conc_clean2 %>%
  # Filter out the outlet stations
  filter(
    Year == 2017,
    StationName %in% c(
      "Liberty Cut below Stairsteps",
      "Toe Drain at 1/2 Lisbon"
    )
  ) %>%
  group_by(SamplingEvent, Analyte) %>%
  summarize(digits = min(digits)) %>%
  ungroup()

# Shag Slough and Liberty Cut combination
OutSta_2017_d_ShagLib <- all_conc_clean2 %>%
  # Filter out the outlet stations
  filter(
    Year == 2017,
    StationName %in% c(
      "Liberty Cut below Stairsteps",
      "Shag Slough below Stairsteps"
    )
  ) %>%
  group_by(SamplingEvent, Analyte) %>%
  summarize(digits = min(digits)) %>%
  ungroup()

# Just Liberty Cut
OutSta_2017_d_Lib <- all_conc_clean2 %>%
  # Filter out the outlet stations
  filter(
    Year == 2017,
    StationName == "Liberty Cut below Stairsteps"
  ) %>%
  select(SamplingEvent, Analyte, digits)

# Join digits dataframes to the concentration dataframe based on calculation method
# Little Holland
little_holland <- OutSta_2017_c %>%
  filter(StationName == "Little Holland") %>%
  left_join(OutSta_2017_d_ToeLib)

# Main Liberty- good mixing events
main_lib_good_mix <- OutSta_2017_c %>%
  filter(
    StationName == "Main Liberty",
    SamplingEvent %in% c(
      "Jan 11-12, 2017",
      "Jan 31-Feb 1, 2017",
      "Feb 14-15, 2017",
      "Mar 1-2, 2017",
      "Mar 15-16, 2017"
    )
  ) %>%
  left_join(OutSta_2017_d_ShagLib)

# Main Liberty- Shag Slough much different
main_lib_shag_diff <- OutSta_2017_c %>%
  filter(
    StationName == "Main Liberty",
    SamplingEvent %in% c(
      "Jan 24-25, 2017",
      "Mar 28-29, 2017",
      "Apr 25-26, 2017"
    )
  ) %>%
  left_join(OutSta_2017_d_Lib)
  
# Bind all df's together
all_conc_clean_3 <-
  bind_rows(
    all_conc_clean2,
    OutSta_2016,
    little_holland,
    main_lib_good_mix,
    main_lib_shag_diff
  )

# Create vectors to identify Inlet and Outlet stations
inlet_sta <- c(
  "CCSB- Low Flow Channel",
  "CCSB- Overflow Weir",
  "Fremont Weir",
  "Knights Landing Ridge Cut",
  "Putah Creek at Mace Blvd",           
  "Sac River above the Sacramento Weir"
)

outlet_sta <- c(
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
all_conc_clean_f <- all_conc_clean_3 %>% 
  mutate(
    LocType = case_when(
      StationName %in% inlet_sta ~ "Inlet",
      StationName %in% outlet_sta ~ "Outlet",
      TRUE ~ "BelowLiberty"
    )
  )

# Clean up
rm(
  all_conc,
  all_conc_clean,
  all_conc_clean1,
  all_conc_clean2,
  all_conc_clean_3,
  conc_data_clean,
  little_holland,
  main_lib_good_mix,
  main_lib_shag_diff,
  OutSta_2016,
  OutSta_2016_c,
  OutSta_2016_d,
  OutSta_2017_c,
  OutSta_2017_d_Lib,
  OutSta_2017_d_ShagLib,
  OutSta_2017_d_ToeLib
)


# 2. Flow Data ------------------------------------------------------------

# Sum the CCSB flows for the sampling events when we didn't collect samples at the Low Flow Channel
ccsb_flow <- daily_flow_data_se %>% 
  # Filter out CCSB stations
  filter(
    str_detect(StationName, "^CCSB") &
    !SamplingEvent %in% c("Mar 28-29, 2017", "Apr 11-12, 2017", "Apr 25-26, 2017")
  ) %>% 
  # Group and summarize to sum the flows of the LFC and Overflow Weir stations
  group_by(SamplingEvent, Year, LocType) %>% 
  summarize("CCSB- Overflow Weir" = sum(Flow)) %>%
  ungroup() %>% 
  # Pivot dataframe back into long format
  pivot_longer(
    cols = "CCSB- Overflow Weir",
    names_to = "StationName",
    values_to = "Flow"
  )

# Add the CCSB flow data back to the daily flow data dataframe
daily_flow_data1 <- daily_flow_data_se %>% 
  # Remove the CCSB Stations to prevent duplicates
  filter(
    !(
      str_detect(StationName, "^CCSB") &
      !SamplingEvent %in% c("Mar 28-29, 2017", "Apr 11-12, 2017", "Apr 25-26, 2017")
    )
  ) %>%
  # Add summed CCSB flows
  bind_rows(ccsb_flow)

# Clean up
rm(ccsb_flow)

# Sum flows for all outlet stations for April 11-12 sampling event and assign to 1/2 Lisbon station
OutFlow_Apr12 <- daily_flow_data1 %>% filter(SamplingEvent == "Apr 11-12, 2017", LocType == "Outlet")
Flow_Lis_Apr12 <- sum(OutFlow_Apr12$Flow)
OutFlow_Apr12 <- OutFlow_Apr12 %>% filter(StationName == "Toe Drain at 1/2 Lisbon")
OutFlow_Apr12$Flow <- Flow_Lis_Apr12

# Add the April 12 outflow data back to the flow data
daily_flow_data_f <- daily_flow_data1 %>% 
  # Remove the Outlet stations for the April 11-12 sampling event in FlowData df to prevent duplicates
  filter(!(SamplingEvent == "Apr 11-12, 2017" & LocType == "Outlet")) %>%
  # Bind all df back together
  bind_rows(OutFlow_Apr12)

# Clean up
rm(daily_flow_data1, OutFlow_Apr12, Flow_Lis_Apr12)


# 2.1 Create a new Flow df for the balanced flows approach ----------------
# Make a new df that summarizes the total input and output flows for each sampling event
flow_summ <- daily_flow_data_f %>%
  filter(LocType != "Below Liberty") %>% 
  group_by(SamplingEvent, Year, LocType) %>% 
  summarize(TotalFlow = sum(Flow)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = LocType, values_from = TotalFlow)

OutFlow <- daily_flow_data_f %>% 
  # Pull out the flow data for outlet locations
  filter(LocType == "Outlet") %>% 
  # Join summary df
  left_join(flow_summ) %>% 
  # Create a new variable for the adjusted flow values: (flow at site/total outflow) * total inflow
  mutate(FlowB = (Flow/Outlet) * Inlet) %>% 
  # Remove a few variables
  select(-c(Flow, Inlet, Outlet)) %>% 
  # Rename FlowB
  rename(Flow = FlowB)

# Add the adjusted outflow data back to the inflow data
flow_data_bal <- daily_flow_data_f %>% 
  # Remove the flow data for outlet locations to prevent duplicates
  filter(LocType != "Outlet") %>%
  # Bind OutFlow to FlowData
  bind_rows(OutFlow)

# Clean up
rm(OutFlow)


# 3. Calculate Loads ---------------------------------------------------------
# Remove concentration data for a few samples since they won't be used in load calculations
# I decided to not calculate loads for Cache and Miner Sloughs for the 2016 sampling event since
# the Below Liberty flows during this flood event were much lower than the sum of the input flows

all_conc_clean_f <- all_conc_clean_f %>%
  filter(
    !(
      Year == 2016 &
        StationName %in% c(
          "Cache Slough near Ryer Island",
          "Miner Slough near Sac River"
        )
    )
  )

# Split concentration data into 3 df based on LocType to calculate loads
all_conc_clean_split <- all_conc_clean_f %>% split(.$LocType)

# Calculate loads
loads <- 
  list(
    Inlet = all_conc_clean_split$Inlet,
    Outlet.SCHISM = all_conc_clean_split$Outlet,
    Outlet.Bal = all_conc_clean_split$Outlet,
    BelowLib = all_conc_clean_split$BelowLiberty
  ) %>% 
  map(~select(.x, -LocType)) %>% 
  map_at(
    c("Inlet", "Outlet.SCHISM", "BelowLib"), 
    ~left_join(.x, daily_flow_data_f)
  ) %>% 
  map_at(
    c("Outlet.Bal"), 
    ~left_join(.x, flow_data_bal)
  ) %>% 
  bind_rows(.id = "Calc.type") %>% 
  # Create a new variable to calculate loads
  mutate(
    Load = Conc * Flow * 28.317*60*60*24/1e9,  #The same conversion factor is used for all calculations
    LoadUnits = case_when(
      str_detect(Units, "mg/L") ~ "1,000 kg/day",
      Units == "ug/L"           ~ "kg/day",
      Units == "ng/L"           ~ "g/day"
    )
  )

# Resolve significant digits for Cache and Miner Sloughs
conc_digits_bl <- all_conc_clean_f %>% 
  filter(str_detect(StationName, "^Cache|^Miner")) %>% 
  group_by(SamplingEvent, Analyte) %>% 
  summarize(digits = min(digits)) %>% 
  ungroup() %>% 
  mutate(StationName = "Below Liberty Island")

# Calculate Below Liberty Island loads using subtraction (Cache Sl - Miner Sl)
loads_bl <- loads %>% 
  filter(LocType == "Below Liberty") %>% 
  select(-c(Conc, Units, Flow, digits)) %>% 
  pivot_wider(names_from = StationName, values_from = Load) %>% 
  rename(
    Cache = "Cache Slough near Ryer Island",
    Miner = "Miner Slough near Sac River"
  ) %>% 
  mutate(
    Load = Cache - Miner,
    StationName = "Below Liberty Island"
  ) %>% 
  select(-c(Cache, Miner)) %>% 
  # add variable with significant digits
  left_join(conc_digits_bl)

# Calculate Below Liberty Island loads using a balanced flows approach (Below Liberty flow = sum of input flows)
# Make a new df that summarizes the Below Liberty Island flows for each sampling event
flow_summ_bl <- daily_flow_data_f %>% 
  filter(LocType == "Below Liberty") %>% 
  select(-LocType) %>% 
  pivot_wider(names_from = StationName, values_from = Flow) %>% 
  rename(
    Cache = "Cache Slough near Ryer Island",
    Miner = "Miner Slough near Sac River"
  ) %>% 
  mutate(BelowLibertyFlow = Cache - Miner) %>% 
  select(-c(Cache, Miner))

# Calculate loads for Below Liberty Island that are scaled to the sum of the input flows
loads_bl_bal <- flow_summ %>% 
  select(-Outlet) %>% 
  #Join the Below Liberty Island and Input flows for each sampling event
  right_join(flow_summ_bl) %>% 
  #Join the Below Liberty Island loads for each sampling event
  right_join(loads_bl) %>%  
  mutate(ScaledLoad = Load * (Inlet/BelowLibertyFlow)) %>% 
  select(-c(BelowLibertyFlow, Inlet, Load)) %>% 
  rename(Load = ScaledLoad)

# Bind df's for each of the load calculation approaches into a list
loads_list <- loads %>% split(.$Calc.type) 
loads_list[["BelowLib"]] <- NULL
loads_list <- loads_list %>% 
  append(
    list(
      BelowLib = loads_bl,
      BelowLib.Bal = loads_bl_bal
    )
  ) %>% 
  # Round loads to appropriate number of significant figures
  map(~mutate(.x, Load = signif(Load, digits = digits)))

# Clean up
rm(all_conc_clean_split, flow_summ_bl, loads, loads_bl, loads_bl_bal, conc_digits_bl)


# 4. Compare the two load calculation approaches -----------------------------

# 4.1 Outlet Flows --------------------------------------------------------
# using either the SCHISM flows or the Balanced flows approach

# Summarize the outlet flows for the two approaches- just 2017 events
  # SCHISM
  flowSCHISM_summ <- flow_summ %>% 
    filter(Year == 2017) %>%
    # Remove Inlet flows
    select(-Inlet) %>% 
    rename(OutletFlow = Outlet) %>% 
    # create a new variable to identify which approach was used
    mutate(Approach = "SCHISM")
  
  # Balanced
  flow_bal_summ <- flow_data_bal %>% 
    filter(
      Year == 2017,
      LocType == "Outlet"
    ) %>%
    group_by(SamplingEvent, Year, LocType) %>% 
    summarize(OutletFlow = sum(Flow)) %>% 
    ungroup() %>% 
    select(-LocType) %>% 
    # create a new variable to identify which approach was used
    mutate(Approach = "Balanced")
  
# Create a summary df by binding the two summary dfs together
flow_summ_all <- 
  bind_rows(flowSCHISM_summ, flow_bal_summ) %>%
  # Pivot by Approach
  pivot_wider(names_from = Approach, values_from = OutletFlow) %>% 
  # Create new variables for the differences and the % differences
  mutate(
    Difference = Balanced - SCHISM,
    PerDiff = round((Balanced - SCHISM)/SCHISM * 100, 1)
  ) %>% 
  # Pivot back to long format
  pivot_longer(
    cols = SCHISM:Difference,
    names_to = "Approach",
    values_to = "OutletFlow"
  ) %>% 
  # Modify PerDiff variable
  mutate(PerDiff = as.character(paste0(PerDiff, "%"))) %>%  #convert PerDiff to character data type
  mutate(PerDiff = if_else(Approach == "SCHISM", PerDiff, NULL)) %>% 
  # Convert variables to factor to apply plot order
  conv_fact_samplingevent() %>% 
  mutate(
    Approach = factor(
      Approach,
      levels = c(
        "SCHISM",
        "Balanced",
        "Difference"
      )
    )
  )

# Plot Outlet flow comparison
pdf(file = '2017_SE_Output_Flow_comparison.pdf', w = 11, h = 8.5)
  flow_summ_all %>%
    ggplot(
      aes(
        x = SamplingEvent,  #barplot by Sampling Event
        y = OutletFlow,
        fill = Approach,  #make each Approach a different fill color
        label = PerDiff  #label plots with Percent Differences
      )
    ) +   
    geom_col(position = "dodge") + 
    geom_text(position = position_dodge(width = 0), size = 3, vjust = -1) +
    labs(
      title = "Comparison Barplot for Outlet Flows used in two different load calculation approaches",
      subtitle = "Labels are the percent differences between the outlet flows used in the two load calculation approaches",
      caption = "Difference is Balanced - SCHISM; Percent Difference is (Balanced-SCHISM)/SCHISM",
      x = "Sampling Event",
      y = "Daily Average Flow (cfs)"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  #x-axis labels at 45 degrees
dev.off()

# Clean up
rm(daily_flow_data_f, flow_data_bal, flow_summ, flow_summ_all, flow_bal_summ, flowSCHISM_summ)


# 4.2 Outlet Loads --------------------------------------------------------
# using either the SCHISM flows or the Balanced flows approach for the outlet loads

# Summarize Loads for each approach
loads_summ <- loads_list %>% 
  # Only look at 2017
  map(~filter(.x, Year == 2017)) %>% 
  # summarize by SamplingEvent and Analyte, then sum(Load)
  map(~group_by(.x, LocType, SamplingEvent, Analyte, LoadUnits)) %>% 
  map(~summarize(.x, TotalLoad = round(sum(Load), 1))) %>%   #round to the tenth's place
  map(ungroup) 

# Create a df used to plot comparisons between the two load calculation approaches
loads_comp_in_out <- 
  list(
    SCHISM = bind_rows(loads_summ$Inlet, loads_summ$Outlet.SCHISM),
    Balanced = bind_rows(loads_summ$Inlet, loads_summ$Outlet.Bal)
  ) %>% 
  # Pivot by LocType and calculate net loads for each sampling event
  map(~pivot_wider(.x, names_from = LocType, values_from = TotalLoad)) %>% 
  map(~mutate(.x, NetLoad = Outlet - Inlet)) %>% 
  # Pivot back to long format
  map(
    ~pivot_longer(
      .x,
      cols = Inlet:NetLoad,
      names_to = "LoadType",
      values_to = "Load" 
    )
  ) %>% 
  # bind two elements together into a df
  bind_rows(.id = "Approach") %>% 
  # Convert variables to factor to apply plot order
  conv_fact_samplingevent() %>% 
  mutate(
    LoadType = factor(
      LoadType,
      levels = c(
        "Inlet",
        "Outlet",
        "NetLoad"
      )
    ),
    Approach = factor(
      Approach,
      levels = c(
        "SCHISM",
        "Balanced"
      )
    )
  )

# Create a new df with calcuated RPD's between approaches
loads_rpd <- loads_comp_in_out %>%  
  pivot_wider(names_from = Approach, values_from = Load) %>% 
  mutate(RPD = round(abs(SCHISM - Balanced)/((abs(SCHISM) + abs(Balanced))/2), 3) * 100) %>% 
  mutate(RPD = as.character(paste0(RPD, "%"))) %>%  #convert RPD to character data type
  mutate(RPD = if_else(LoadType != "Inlet", RPD, NULL)) %>% 
  mutate(RPD = if_else(RPD != "NaN%", RPD, NULL)) %>%   #remove one NaN value
  select(-c(LoadUnits, SCHISM, Balanced))

# Join loads_rpd df to loads_comp_in_out df to include RPD calculations in the plots
loads_comp_in_out <- left_join(loads_comp_in_out, loads_rpd) %>% 
  # Remove the RPD values for the Balanced approach rows to prevent duplicate labels in the plot
  mutate(RPD = if_else(Approach == "SCHISM", RPD, NULL))

# Plot the two approaches together for each analyte- RPD's as labels
pdf(file = '2017_Output_Load_Calc_comparison.pdf', w = 11, h = 8.5)
  # Comparison Barplots for each Analyte separated by LoadType
  loads_comp_in_out %>% group_by(Analyte) %>% do(plot={
    print(.$Analyte[1])
    p = ggplot(
      data = .,
      aes(
        x = SamplingEvent,  #barplot by Sampling Event
        y = Load,
        fill = Approach,  #make each Approach a different fill color
        label = RPD  #label plots with RPD's
      )) +   
      geom_col(position = "dodge") + 
      geom_text(position = position_dodge(0)) +
      facet_grid(
        rows = vars(LoadType),  #horizontal facets for each LoadType (Output and NetLoad)
        #Rename facet labels
        labeller = labeller(LoadType = c(   
          "Inlet" = "Input Loads",
          "Outlet" = "Output Loads", 
          "NetLoad" = "Net Loads"
        ))  
      ) +
      labs(
        title = paste0("Comparison Barplots for ", .$Analyte[1], " Loads"),
        subtitle = "Labels are RPD's between the two load calculation approaches",
        x = "Sampling Event",
        y = paste0("Load (", .$LoadUnits[1], ")")
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  #x-axis labels at 45 degrees
    print(p)
  })
dev.off()

# Plot the two approaches together for each analyte- loads as labels
pdf(file = '2017_Output_Load_Calc_comparison2.pdf', w = 11, h = 8.5)
  # Comparison Barplots for each Analyte separated by LoadType
  loads_comp_in_out %>% group_by(Analyte) %>% do(plot={
    print(.$Analyte[1])
    p = ggplot(
      data = .,
      aes(
        x = SamplingEvent,  #barplot by Sampling Event
        y = Load,
        fill = Approach,  #make each Approach a different fill color
        label = round(Load, 0)  #label plots with Load values
      )) +   
      geom_col(position = "dodge") + 
      geom_text(position = position_dodge(width = 0.9), size = 3) +
      facet_grid(
        rows = vars(LoadType),  #horizontal facets for each LoadType (Output and NetLoad)
        #Rename facet labels
        labeller = labeller(
          LoadType = c(   
            "Inlet" = "Input Loads",
            "Outlet" = "Output Loads", 
            "NetLoad" = "Net Loads"
          )
        )  
      ) +
      labs(
        title = paste0("Comparison Barplots for ", .$Analyte[1], " Loads"),
        subtitle = "Labels are load values for the two calculation approaches",
        x = "Sampling Event",
        y = paste0("Load (", .$LoadUnits[1], ")")
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  #x-axis labels at 45 degrees
    print(p)
  })
dev.off()

# Plot just the net loads for the two approaches and their difference
# Add a few items to the LoadsSummary df for these plots
net_loads_comp_in_out <- loads_comp_in_out %>% 
  # Just keep the Net Load Calculations
  filter(LoadType == "NetLoad") %>% 
  # Remove a couple of unnecessary variables
  select(-c(LoadType, RPD)) %>% 
  # Pivot by Approach in order to calculate differences between them
  pivot_wider(names_from = Approach, values_from = Load) %>% 
  # Create a new variable for the differences in the net flows
  mutate(Difference = Balanced - SCHISM) %>% 
  # Pivot to put the df back to long format
  pivot_longer(
    cols = SCHISM:Difference,
    names_to = "Approach",
    values_to = "Load"
  ) %>% 
  mutate(
    Approach = factor(
      Approach,
      levels = c(
        "SCHISM",
        "Balanced",
        "Difference"
      )
    )
  ) %>% 
  # Create Analyte groups for more efficient plots
  mutate(
    AnalyteGroup = case_when(
      str_detect(Analyte, "^THg")                 ~ "THg",
      str_detect(Analyte, "^MeHg")                ~ "MeHg",
      Analyte %in% c("TOC", "DOC", "POC")         ~ "Organic Carbon",
      Analyte %in% c("VSS", "Chloride- filtered") ~ "VSS and Chloride",
      Analyte == "TSS"                            ~ "TSS",
      Analyte == "Iron- filtered"                 ~ "Filtered Fe",
      Analyte == "Manganese- filtered"            ~ "Filtered Mn"
    )
  )

# Create Load plots
pdf(file = "2017_Net_Loads_comparison.pdf", w = 11, h = 8.5)
  # Comparison Barplots of the Net Loads for each AnalyteGroup 
  net_loads_comp_in_out %>% group_by(AnalyteGroup) %>% do(plot={
    print(.$AnalyteGroup[1])
    p = ggplot(
      data = .,
      aes(
        x = SamplingEvent,  #barplot by Sampling Event
        y = Load,
        fill = Approach,  #make each Approach a different fill color
        label = round(Load, 0)  #label plots with Load values
      )) +   
      geom_col(position = "dodge") + 
      geom_text(position = position_dodge(width = 0.9), size = 2.5) +
      facet_grid(rows = vars(Analyte)) +  #horizontal facets for each Analyte within group
      labs(
        title = paste0("Comparison Barplots of the Net Loads for ", .$AnalyteGroup[1]),
        subtitle = "Labels are net load values and their differences for the two calculation approaches",
        caption = "Difference is Balanced - SCHISM",
        x = "Sampling Event",
        y = paste0("Net Load (", .$LoadUnits[1], ")")
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  #x-axis labels at 45 degrees
    print(p)
  })
dev.off()

# Clean up
rm(loads_comp_in_out, net_loads_comp_in_out, loads_rpd)

# *****************
# Outlet loads:
  # Since the overall water balance between the sum of the input flows and the SCHISM flows for the 2017 flood
  # from Jan-May was close to balanced, the technical team decided that the Balanced approach is the best way
  # to calculate outlet loads for the 2017 sampling event
  # The water balances for the 2014 and 2016 floods were incomplete and unbalanced, but I decided to use
  # the Balanced approach to calculate outlet loads for these events as well to be consistent
# *****************


# 4.3 Below Liberty Island Loads ----------------------------------------------
# using raw vs. scaled (adjusted using the sum of Inlet flows) loads for Below Liberty Island
  
# Calculate net loads (Below Liberty - Outlet) for both load calculation approaches
loads_comp_bl <-
  list(
    Raw = bind_rows(loads_summ$Outlet.Bal, loads_summ$BelowLib),
    Balanced = bind_rows(loads_summ$Outlet.Bal, loads_summ$BelowLib.Bal)
  ) %>% 
  # Remove one event when we didn't collect samples at Cache and Miner Sloughs
  map(~filter(.x, SamplingEvent != "Apr 11-12")) %>% 
  # Pivot by LocType and calculate net loads for each sampling event
  map(~pivot_wider(.x, names_from = LocType, values_from = TotalLoad)) %>% 
  map(~rename(.x, BelowLiberty = "Below Liberty")) %>% 
  map(~mutate(.x, NetLoad = BelowLiberty - Outlet)) %>% 
  # bind two elements together into a df
  bind_rows(.id = "Approach") %>% 
  # Convert variables to factor to apply plot order
  conv_fact_samplingevent() %>% 
  filter(!is.na(NetLoad))

# Plot the net loads for the two approaches
pdf(file = "BelowLiberty_Net_Loads_comparison.pdf", w = 11, h = 8.5)
  loads_comp_bl %>% group_by(Analyte) %>% do(plot={
    print(.$Analyte[1])
    p = ggplot(
      data = .,
      aes(
        x = SamplingEvent,  #barplot by Sampling Event
        y = NetLoad,
        fill = Approach  #make each Approach a different fill color
      )) +   
      geom_col(position = "dodge") + 
      labs(
        title = paste0("Comparison Barplots of the Net Loads for ", .$Analyte[1]),
        subtitle = "Net Load = Below Liberty - Yolo Bypass Output",
        x = "Sampling Event",
        y = paste0("Net Load (", .$LoadUnits[1], ")")
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  #x-axis labels at 45 degrees
    print(p)
  })
dev.off()

# Clean up
rm(loads_comp_bl)

# *****************
# Below Liberty Island loads:
  # Since the overall water balance between the sum of the input flows and the Below Liberty Island flows 
  # for the 2017 flood from Jan-May was very close to balanced, I decided that the Balanced or scaled approach 
  # is the best way to calculate the Below Liberty loads
# *****************

