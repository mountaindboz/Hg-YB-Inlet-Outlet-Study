# Yolo Bypass Inlet-Outlet Study
# Create plots and summarize combined parameters for all sampling events

library(tidyverse)
library(lubridate)
library(openwaterhg)


# 1. Prepare Data ----------------------------------------------------------

# Create a tibble with shortened parameter names
cpc_key <- tibble(
  Parameter = c(
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
  Parameter_Short = c(
    "fMeHg/DOC",
    "fTHg/DOC",
    "MeHg Kd",
    "MeHg Conc on Solids",
    "% fMeHg/fTHg",
    "% pMeHg/pTHg",
    "% tMeHg/tTHg",
    "pMeHg/POC",
    "POC Conc on Solids",
    "pTHg/POC",
    "tAl Conc on Solids",
    "THg Kd",
    "THg Conc on Solids",
    "tMeHg/TOC",
    "TOC Conc on Solids",
    "tTHg/TOC"
  )
)

# Prepare combined parameter data
comb_param_clean <- comb_param_calc %>% 
  # Create some new variables
  mutate(Year = year(SampleDate)) %>% 
  add_samplingevent() %>% 
  # Shorten StationNames and Parameter
  add_short_sta_names() %>% 
  left_join(cpc_key) %>% 
  # Reorder variables
  select(
    StationName,
    ShortName,
    SampleDate,
    CollectionTime,
    Year,
    SamplingEvent,
    Parameter,
    Parameter_Short,
    Value,
    Units
  )

# Setup plotting order
params_long <- sort(unique(comb_param_clean$Parameter))
params_long_order <-  params_long[c(11,3,12,4,9,7,8,16,2,6,15,1,5,13,10,14)]

params_short <- sort(unique(comb_param_clean$Parameter_Short))
params_short_order <-  params_short[c(12,6,13,7,3,1,2,16,5,10,14,4,8,15,9,11)]

comb_param_clean <- comb_param_clean %>% 
  conv_fact_samplingevent() %>% 
  conv_fact_long_sta_names() %>% 
  conv_fact_short_sta_names() %>% 
  mutate(
    Parameter = factor(Parameter, levels = params_long_order),
    Parameter_Short = factor(Parameter_Short, levels = params_short_order)
  )

# Create a df of just the 2017 concentration data
comb_param_clean_17 <- comb_param_clean %>% 
  filter(Year == 2017) %>% 
  mutate(SamplingEvent = fct_drop(SamplingEvent))

# Clean up
rm(cpc_key, params_long, params_long_order, params_short, params_short_order)


# 2. Create Plots ---------------------------------------------------------

# 2.1 All combined parameters ----------------------------------------

# Grouped by station
pdf(file = "CombinedParameter_Plots_byStation.pdf", w=15, h=8.5)
  # All sampling events
  comb_param_clean %>% 
    group_by(StationName) %>% 
    do(plot = {
      print(.$StationName[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = SamplingEvent, 
            y = Value
          )
        ) +
        geom_col() +
        scale_x_discrete(drop = FALSE) +
        facet_wrap(
          vars(Parameter_Short, Units),
          scales = "free_y"
        ) +
        labs(
          title = paste0("Combined Parameters at ", .$StationName[1]),
          subtitle = "All Sampling Events",
          x = "Sampling Event",
          y = NULL
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x-axis labels at 90 degrees
      
      print(p)
    })
  
  # Just 2017 sampling events
  comb_param_clean_17 %>% 
    group_by(StationName) %>% 
    do(plot = {
      print(.$StationName[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = SamplingEvent, 
            y = Value
          )
        ) +
        geom_line(aes(group = 1)) +
        geom_point() +
        scale_x_discrete(drop = FALSE) +
        facet_wrap(
          vars(Parameter_Short, Units),
          scales = "free_y"
        ) +
        labs(
          title = paste0("Combined Parameters at ", .$StationName[1]),
          subtitle = "Just 2017 Sampling Events",
          x = "Sampling Event",
          y = NULL
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x-axis labels at 90 degrees
      
      print(p)
      
    })
  
dev.off()

# Grouped by Parameter
pdf(file = "CombinedParameter_Plots_byParameter.pdf", w=15, h=8.5)
  # All sampling events
  comb_param_clean %>% 
    group_by(Parameter) %>% 
    do(plot = {
      print(.$Parameter[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = SamplingEvent, 
            y = Value 
          )
        ) +
        geom_col() +
        facet_wrap(vars(ShortName)) +
        labs(
          title = .$Parameter[1],
          subtitle = "All Sampling Events",
          x = "Sampling Event",
          y = .$Units[1]
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x-axis labels at 90 degrees
      
      print(p)
    })
  
  # Just 2017 sampling events
  comb_param_clean_17 %>% 
    group_by(Parameter) %>% 
    do(plot = {
      print(.$Parameter[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = SamplingEvent, 
            y = Value
          )
        ) +
        geom_line(aes(group = 1)) +
        geom_point() +
        facet_wrap(vars(ShortName)) +
        labs(
          title = .$Parameter[1],
          subtitle = "Just 2017 Sampling Events",
          x = "Sampling Event",
          y = .$Units[1]
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x-axis labels at 90 degrees
      
      print(p)
      
    })
  
dev.off()
    
# Boxplots
pdf(file = "CombinedParameter_Boxplots.pdf", w=15, h=8.5)
  comb_param_clean %>% 
    mutate(Year = as.character(Year)) %>% 
    group_by(Parameter) %>% 
    do(plot = {
      print(.$Parameter[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = ShortName, 
            y = Value
          )
        ) +
        geom_boxplot(outlier.shape = NA) +
        geom_jitter(
          width = 0.25,
          aes(color = Year)
        ) +
        labs(
          title = paste0("Boxplots of ", .$Parameter[1]),
          subtitle = "All Sampling Events",
          x = "Station",
          y = .$Units[1]
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x-axis labels at 90 degrees
      
      print(p)
      
    })

dev.off()


# 2.2 Plot the Combined parameters for the Toe Drain Transect -------------

comb_param_clean_tdt <- comb_param_clean %>% 
  filter(
    StationName %in% c(
      "Toe Drain at County Road 22",
      "Toe Drain at Interstate 80",
      "Toe Drain at Lisbon Weir",
      "Toe Drain at 1/2 Lisbon",
      "Prospect Slough"      
    )
  )

# Create plots
pdf(file = "CombinedParam_ToeDrTransect_Plots.pdf", w=11, h=8.5)  
  # Facet by sampling event, grouped by combined parameter
  comb_param_clean_tdt %>% 
    group_by(Parameter) %>% 
    do(plot = {
      print(.$Parameter[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = ShortName, 
            y = Value
          )
        ) +
        geom_line(aes(group = 1)) +
        geom_point() +
        facet_wrap(vars(SamplingEvent)) +
        labs(
          title = "Toe Drain Transect",
          subtitle = .$Parameter[1],
          x = "Station",
          y = .$Units[1]
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x-axis labels at 90 degrees
      
      print(p)
      
    })
  
  # Facet by combined parameter, grouped by sampling event
  comb_param_clean_tdt %>% 
    group_by(SamplingEvent) %>% 
    do(plot = {
      print(.$SamplingEvent[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = ShortName, 
            y = Value
          )
        ) +
        geom_line(aes(group = 1)) +
        geom_point() +
        facet_wrap(
          vars(Parameter_Short, Units),
          scales = "free_y"
        ) +
        labs(
          title = "Toe Drain Transect",
          subtitle = .$SamplingEvent[1],
          x = "Station",
          y = NULL
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x-axis labels at 90 degrees
      
      print(p)
      
    })
  
dev.off()

# 3. Calculate Summary Statistics -----------------------------------------

# Summarize combined parameters by Station
comb_param_clean_list <- 
  list(
    AllEvents = comb_param_clean,
    Just2017 = comb_param_clean_17
  )

comb_param_clean_summ <- map(comb_param_clean_list, ~summ_stat(.x, Value, Parameter, StationName))

# Add Units to the Summary Statistics
unit_key <- comb_param_clean %>% 
  count(Parameter, Units) %>% 
  select(-n)

comb_param_clean_summ <- comb_param_clean_summ %>% 
  map(~left_join(.x, unit_key))

# Export Summary Statistics
comb_param_clean_summ$AllEvents %>% write_excel_csv("CombinedParam_SummaryStats_all.csv")
comb_param_clean_summ$Just2017 %>% write_excel_csv("CombinedParam_SummaryStats_2017.csv")

# Export Combined Parameters in a wide format
comb_param_clean %>% 
  select(
    ShortName,
    SamplingEvent,
    Parameter_Short,
    Value
  ) %>% 
  pivot_wider(names_from = ShortName, values_from = Value) %>% 
  write_excel_csv("CombinedParameters_wide.csv", na = "")

