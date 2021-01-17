# Yolo Bypass Inlet-Outlet Study
# Create Concentration vs. Time Inundated scatterplots

library(tidyverse)
library(readxl)
library(lubridate)
library(ggforce)
library(corrr)
library(broom)
library(patchwork)
library(openwaterhg)


# 1. Import and Clean Concentration Data --------------------------------------------------

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
  # Add a year variable for filtering
  mutate(Year = year(SampleDate)) %>%
  # Keep only necessary data
  filter(
    Year == 2017,
    !str_detect(StationName, "Low Flow|^Sac|^YB|^Cache|^Miner"),
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
  # Keep only necessary variables
  select(-Year)

# Add averages of CCSB Overflow Weir and Fremont Weir stations to the concentration df
  # CCSB Overflow Weir stations
  conc_ccsb_avg <- all_conc_clean %>% 
    filter(str_detect(StationName, "^CCSB")) %>% 
    group_by(SampleDate, Analyte, Units) %>% 
    summarize(Conc = signif(mean(Conc), 3)) %>% 
    ungroup() %>% 
    mutate(StationName = "CCSB Overflow Weir- Average")

  # Fremont Weir stations
  conc_fre_avg <- all_conc_clean %>% 
    filter(str_detect(StationName, "^Fremont")) %>% 
    group_by(SampleDate, Analyte, Units) %>% 
    summarize(Conc = signif(mean(Conc), 3)) %>% 
    ungroup() %>% 
    mutate(StationName = "Fremont Weir- Average")

  # Add average concentrations to all_conc_clean df
  all_conc_clean <- bind_rows(all_conc_clean, conc_ccsb_avg, conc_fre_avg)
  
# Add a variable for weeks flooded; the YB started to flood on 1/9/2017
all_conc_clean <- mutate(all_conc_clean, WeeksFlood = (as.numeric(SampleDate - as_date("2017-01-09"))/7))
  
# Clean up
rm(conc_ccsb_avg, conc_fre_avg)


# 2. Create Plotting Functions --------------------------------------------

# Create a function for the Conc vs. Time Inundated scatterplots
plot_conc_weeks_flood <- function(df, Group, Rsq, pval) {
  p <- 
    ggplot(
      data = df,
      aes(
        x = WeeksFlood,
        y = Conc
      )
    ) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = Group,
      subtitle = paste0("R Squared = ", Rsq, "%\np-value = ", pval),
      x = "Weeks since start of 2017 flood",
      y = paste0("Conc (", df$Units[1], ")")
    )
  
  return(p)
}  

# Create functions for grouping Conc vs. Time Inundated scatterplots
# Group by Station
group_plots_by_sta <- function(df, Group) {
  # Group plots together
  p <- df %>% pull(plot_ana) %>% wrap_plots(ncol = 4)
  
  # Add Plot Title
  p <- p +  
    plot_annotation(
      title = paste0("Concentration vs. Time Inundated Scatterplots\nGrouped by Station:  ", Group),
      subtitle = "2017 Flood Event"
    )
  
  return(p)
}

# Group by Analyte
group_plots_by_ana <- function(df, Group, PlotSubT) {
  # Define custom number of columns for plots
  ncol_cust <- if_else(PlotSubT == "Outlet Stations", 2, 3)
  
  # Group plots together
  p <- df %>% pull(plot_sta) %>% wrap_plots(ncol = ncol_cust)
  
  # Add Plot Title
  p <- p +  
    plot_annotation(
      title = paste0("Concentration vs. Time Inundated Scatterplots\nGrouped by Parameter:  ", Group),
      subtitle = paste0(PlotSubT, "\n2017 Flood Event")
    )
  
  return(p)
}


# 3. Create Conc vs. Time Inundated plots ------------------------------

# Create a nested dataframe and create Concentration vs Time Inundated scatterplots
conc_weeks_flood_plots <- all_conc_clean %>% 
  group_nest(StationName, Analyte) %>%
  mutate(
    model = map(data, ~lm(Conc ~ WeeksFlood, data = .x)),
    Rsq = signif(map_dbl(model, ~glance(.x)$r.squared * 100), 3),
    p_value = signif(map_dbl(model, ~glance(.x)$p.value), 2),
    plot_sta = pmap(list(data, StationName, Rsq, p_value), .f = plot_conc_weeks_flood),
    plot_ana = pmap(list(data, Analyte, Rsq, p_value), .f = plot_conc_weeks_flood)
  )

# Define plotting order
conc_weeks_flood_plots <- conc_weeks_flood_plots %>% 
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
conc_weeks_flood_plots_by_sta <- conc_weeks_flood_plots %>% 
  select(
    StationName,
    Analyte,
    plot_ana
  ) %>% 
  arrange(Analyte) %>% 
  group_nest(StationName) %>% 
  arrange(StationName) %>% 
  mutate(group_plot = map2(data, StationName, .f = group_plots_by_sta))

# Create grouped plots by Analyte
  # Split up conc_weeks_flood_plots df by Location Type
    # Inlet stations
    conc_weeks_flood_plots_in <- conc_weeks_flood_plots %>% 
      filter(str_detect(StationName, "^Fremont|^CCSB|^Knights|^Putah")) %>% 
      mutate(
        LocType = "Inlet Stations",
        StationName = fct_drop(StationName)
      )

    # Toe Drain Transect stations
    conc_weeks_flood_plots_tdt <- conc_weeks_flood_plots %>% 
      filter(str_detect(StationName, "^Toe|^Prospect")) %>% 
      mutate(
        LocType = "Toe Drain Transect Stations",
        StationName = fct_drop(StationName)
      )
    
    # Outlet stations
    conc_weeks_flood_plots_out <- conc_weeks_flood_plots %>% 
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
  conc_weeks_flood_plots_by_ana <- 
    list(
      inlet = conc_weeks_flood_plots_in,
      tdt = conc_weeks_flood_plots_tdt,
      outlet = conc_weeks_flood_plots_out
    ) %>% 
    map(~select(.x, StationName, Analyte, LocType, plot_sta)) %>% 
    map(~arrange(.x, StationName)) %>% 
    map(~group_nest(.x, Analyte, LocType)) %>% 
    map(~mutate(.x, group_plot = pmap(list(data, Analyte, LocType), .f = group_plots_by_ana))) %>% 
    bind_rows(.id = "Location") %>% 
    select(
      Analyte,
      Location,
      group_plot
    ) %>% 
    mutate(Location = factor(Location, levels = c("inlet", "tdt", "outlet"))) %>% 
    arrange(Analyte, Location)

# Print Conc vs. Time Inundated plots to .pdf files
  # Plots Grouped by Station
  pdf(file = "Conc_vs_WeeksFlood_Plots_byStation.pdf", w = 13, h = 8.5)
    for (p in conc_weeks_flood_plots_by_sta$group_plot) {
      print(p)
    }
  dev.off()

  # Plots Grouped by Analyte
  pdf(file = "Conc_vs_WeeksFlood_Plots_byAnalyte.pdf", w = 13, h = 8.5)
    for (p in conc_weeks_flood_plots_by_ana$group_plot) {
      print(p)
    }
  dev.off()


