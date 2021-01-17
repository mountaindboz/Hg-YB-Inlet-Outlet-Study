# Yolo Bypass Inlet-Outlet Study
# Create Concentration vs. Flow scatterplots

library(tidyverse)
library(readxl)
library(lubridate)
library(ggforce)
library(corrr)
library(broom)
library(patchwork)
library(openwaterhg)


# 1. Import and Clean Data --------------------------------------------------

# 1.1 Concentration Data -------------------------------------------------

# Import concentration data
source("YB_Mass_Balance/Concentrations/Import_Conc_Data.R")

# Clean MeHg and THg on solids data
hg_solids_clean <- comb_param_calc %>% 
  filter(Parameter %in% c("THg Concentration on Solids", "MeHg Concentration on Solids")) %>% 
  rename(
    Analyte = Parameter,
    Conc = Value
  )

# Bind all concentration data
all_conc <- bind_rows(all_conc, hg_solids_clean)

# Filter and Clean all Conc Data
all_conc_clean <- all_conc %>% 
  # Keep only necessary data
  filter(
    !str_detect(StationName, "Low Flow|^Sac|^Cache|^Miner"),
    Analyte %in% c(
      "DOC",
      "MeHg- filtered",
      "MeHg- particulate",
      "MeHg- total",
      "MeHg Concentration on Solids",
      "POC",
      "THg- filtered",
      "THg- particulate",
      "THg- total",
      "THg Concentration on Solids",
      "TOC",
      "TSS"
    )
  ) %>%
  # Add a year variable
  mutate(Year = year(SampleDate)) %>%
  # Add variable for SamplingEvent
  add_samplingevent() %>% 
  # Keep only necessary variables
  select(
    StationName,
    SamplingEvent,
    Year,
    Analyte,
    Conc,
    Units
  )

# Add averages of CCSB Overflow Weir and Fremont Weir stations to the concentration df
  # CCSB Overflow Weir stations
  conc_ccsb_avg <- all_conc_clean %>% 
    filter(str_detect(StationName, "^CCSB")) %>% 
    group_by(SamplingEvent, Year, Analyte, Units) %>% 
    summarize(Conc = signif(mean(Conc), 3)) %>% 
    ungroup() %>% 
    mutate(StationName = "CCSB Overflow Weir- Average")

  # Fremont Weir stations
  conc_fre_avg <- all_conc_clean %>% 
    filter(str_detect(StationName, "^Fremont")) %>% 
    group_by(SamplingEvent, Year, Analyte, Units) %>% 
    summarize(Conc = signif(mean(Conc), 3)) %>% 
    ungroup() %>% 
    mutate(StationName = "Fremont Weir- Average")

  # Add average concentrations to all_conc_clean df
  all_conc_clean <- bind_rows(all_conc_clean, conc_ccsb_avg, conc_fre_avg)

# Clean up
rm(conc_ccsb_avg, conc_fre_avg)

# 1.2 Flow Data ----------------------------------------------------------

# Create a df for the individual inlet flows
flow_inlet <- daily_flow_data_se %>% 
  filter(
    LocType == "Inlet",
    !StationName %in% c("CCSB- Low Flow Channel", "Sac River above the Sacramento Weir")
  ) %>% 
  select(-c(LocType, Year))

# Create a df for the total inlet flows for each sampling event
flow_inlet_total <- daily_flow_data_se %>% 
  filter(LocType == "Inlet") %>% 
  group_by(SamplingEvent) %>% 
  summarize(Flow = sum(Flow)) %>% 
  ungroup()


# 2. Join Conc and Flow data and prepare for plotting --------------------

# Create Conc vs Flow dataframes 
  # Input stations
  conc_flow_inlet <- all_conc_clean %>% 
    filter(str_detect(StationName, "^Fremont|^CCSB|^Knights|^Putah")) %>% 
    # Create a variable used to join flow_inlet df
    mutate(
      StationName_flow = case_when(
        str_detect(StationName, "^CCSB") ~ "CCSB- Overflow Weir",
        str_detect(StationName, "^Fremont") ~ "Fremont Weir",
        TRUE ~ StationName
      )
    ) %>%
    # Join flow_inlet df
    left_join(flow_inlet, by = c("StationName_flow" = "StationName", "SamplingEvent")) %>% 
    select(-StationName_flow)

  # All other stations
  conc_flow_other <- all_conc_clean %>%
    filter(!str_detect(StationName, "^Fremont|^CCSB|^Knights|^Putah")) %>% 
    # Join flow_inlet_total df
    left_join(flow_inlet_total)

  # Join two dataframes together
  conc_flow_all <- bind_rows(Inlet = conc_flow_inlet, Other = conc_flow_other, .id = "LocType")
  
  # Filter the conc_flow_all df to just include 2017 sampling events
  conc_flow_all_17 <- filter(conc_flow_all, Year == 2017)
 
  
# 3. Create Plotting Functions --------------------------------------------

# Create a function for the Conc vs Flow scatterplots
plot_conc_flow <- function(df, PlotTitle, Rsq, pval, LocType) {
  p <- 
    ggplot(
      data = df,
      aes(
        x = Flow,
        y = Conc
      )
    ) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = PlotTitle,
      subtitle = paste0("R Squared = ", Rsq, "%\np-value = ", pval),
      y = paste0("Conc (", df$Units[1], ")")
    )
  
  # Use a different x-axis label depending upon LocType argument 
  if (LocType == "Inlet") {
    p <- p + xlab("Daily Average Flow (cfs)")
    
  } else if (LocType == "Other") {
    p <- p + xlab("Daily Average Input Flow (cfs)")
  }
  
  return(p)
}  

# Create functions for grouping Conc vs. Flow scatterplots
# Group by Station
group_plots_by_sta <- function(df, Group, PlotSubT) {
  # Group plots together
  p <- df %>% pull(plot_ana) %>% wrap_plots(ncol = 4)
  
  # Add Plot Title
  p <- p +  
    plot_annotation(
      title = paste0("Concentration vs. Flow Scatterplots\nGrouped by Station:  ", Group),
      subtitle = PlotSubT
    )
  
  return(p)
}

# Group by Analyte
group_plots_by_ana <- function(df, Group, PlotSubT_Loc, PlotSubT_SE_Type) {
  # Define custom number of columns for plots
  ncol_cust <- if_else(PlotSubT_Loc == "Outlet Stations", 2, 3)
  
  # Group plots together
  p <- df %>% pull(plot_sta) %>% wrap_plots(ncol = ncol_cust)
  
  # Add Plot Title
  p <- p +  
    plot_annotation(
      title = paste0("Concentration vs. Flow Scatterplots\nGrouped by Parameter:  ", Group),
      subtitle = paste0(PlotSubT_Loc, "\n", PlotSubT_SE_Type)
    )
  
  return(p)
}


# 4. Create Conc vs. Flow plots ------------------------------

# Create a nested dataframe and create Concentration vs Flow scatterplots
conc_flow_plots <- 
  list(
    "All Sampling Events" = conc_flow_all, 
    "Just 2017 Sampling Events" = conc_flow_all_17
  ) %>% 
  map(~group_nest(.x, StationName, Analyte, LocType)) %>%
  bind_rows(.id = "SE_Type") %>% 
  mutate(
    model = map(data, ~lm(Conc ~ Flow, data = .x)),
    Rsq = signif(map_dbl(model, ~glance(.x)$r.squared * 100), 3),
    p_value = signif(map_dbl(model, ~glance(.x)$p.value), 2),
    plot_sta = pmap(list(data, StationName, Rsq, p_value, LocType), .f = plot_conc_flow),
    plot_ana = pmap(list(data, Analyte, Rsq, p_value, LocType), .f = plot_conc_flow)
  )

# Define plotting order
conc_flow_plots <- conc_flow_plots %>% 
  mutate(
    StationName = factor(
      StationName, 
      levels = c(
        "Fremont Weir- East Side",
        "Fremont Weir- Middle",
        "Fremont Weir- West Side",
        "Fremont Weir- Average",
        "CCSB Overflow Weir- North",
        "CCSB Overflow Weir- South",
        "CCSB Overflow Weir- Average",
        "Knights Landing Ridge Cut",
        "Putah Creek at Mace Blvd",
        "Toe Drain at County Road 22",
        "Toe Drain at Interstate 80",
        "Toe Drain at Lisbon Weir",
        "Toe Drain at 1/2 Lisbon",
        "Prospect Slough",
        "Liberty Cut below Stairsteps",
        "Shag Slough below Stairsteps"
      )
    ),
    Analyte = factor(
      Analyte, 
      levels = c(
        "THg- total",
        "THg- filtered",
        "THg- particulate",
        "THg Concentration on Solids",
        "MeHg- total",
        "MeHg- filtered",
        "MeHg- particulate",
        "MeHg Concentration on Solids",
        "TOC",
        "DOC",
        "POC",        
        "TSS"
      )
    )
  )

# Create grouped plots by Station
conc_flow_plots_by_sta <- conc_flow_plots %>% 
  select(
    SE_Type,
    StationName,
    Analyte,
    plot_ana
  ) %>% 
  arrange(Analyte) %>% 
  group_nest(SE_Type, StationName) %>% 
  arrange(StationName) %>% 
  mutate(group_plot = pmap(list(data, StationName, SE_Type), .f = group_plots_by_sta))

# Create grouped plots by Analyte
  # Split up conc_flow_plots df by Location Type
    # Inlet stations
    conc_flow_plots_in <- conc_flow_plots %>% 
      filter(LocType == "Inlet") %>% 
      mutate(
        LocType = "Inlet Stations",
        StationName = fct_drop(StationName)
      )

    # Toe Drain Transect stations
    conc_flow_plots_tdt <- conc_flow_plots %>% 
      filter(str_detect(StationName, "^Toe|^Prospect")) %>% 
      mutate(
        LocType = "Toe Drain Transect Stations",
        StationName = fct_drop(StationName)
      )
    
    # Outlet stations
    conc_flow_plots_out <- conc_flow_plots %>% 
      filter(
        StationName %in% c(
          "Toe Drain at 1/2 Lisbon",
          "Liberty Cut below Stairsteps",
          "Shag Slough below Stairsteps"
        )
      ) %>% 
      mutate(
        LocType = "Outlet Stations",
        StationName = fct_drop(StationName)
      )
    
  # Combine these three dataframes and group plots
  conc_flow_plots_by_ana <- 
    list(
      inlet = conc_flow_plots_in,
      tdt = conc_flow_plots_tdt,
      outlet = conc_flow_plots_out
    ) %>% 
    map(~select(.x, SE_Type:LocType, plot_sta)) %>% 
    map(~arrange(.x, StationName)) %>% 
    map(~group_nest(.x, SE_Type, Analyte, LocType)) %>% 
    map(~mutate(.x, group_plot = pmap(list(data, Analyte, LocType, SE_Type), .f = group_plots_by_ana))) %>% 
    bind_rows(.id = "Location") %>% 
    select(
      Analyte,
      Location,
      SE_Type,
      group_plot
    ) %>% 
    mutate(Location = factor(Location, levels = c("inlet", "tdt", "outlet"))) %>% 
    arrange(Analyte, Location, SE_Type)

# Print Conc vs. Flow plots to .pdf files
  # Plots Grouped by Station
  pdf(file = "Conc_vs_Flow_Plots_byStation.pdf", w = 13, h = 8.5)
    for (p in conc_flow_plots_by_sta$group_plot) {
      print(p)
    }
  dev.off()

  # Plots Grouped by Analyte
  pdf(file = "Conc_vs_Flow_Plots_byAnalyte.pdf", w = 13, h = 8.5)
    for (p in conc_flow_plots_by_ana$group_plot) {
      print(p)
    }
  dev.off()


