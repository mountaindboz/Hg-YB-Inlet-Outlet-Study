# Yolo Bypass Inlet-Outlet Study
# Create plots and summarize concentration data for all sampling events

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(openwaterhg)


# 1. Prepare Data -------------------------------------------------------------

# Import concentration data
source("YB_Mass_Balance/Concentrations/Import_Conc_Data.R")

# Filter and clean concentration data
all_conc_clean <- all_conc %>% 
  # Create some new variables
  mutate(Year = year(SampleDate)) %>% 
  add_samplingevent() %>% 
  add_short_sta_names() %>% 
  # Convert some variables to factors to apply plotting order
  conv_fact_long_sta_names() %>% 
  conv_fact_short_sta_names() %>% 
  conv_fact_samplingevent() %>% 
  # Keep only necessary variables
  select(
    StationName,
    ShortName,
    SampleDate,
    CollectionTime,
    Year,
    SamplingEvent,
    Analyte,
    Conc,
    Detect,
    Units,
    MME_Comments,
    QualCode
  )

# Create a df of just the 2017 concentration data
all_conc_clean_17 <- all_conc_clean %>% 
  filter(Year == 2017) %>% 
  mutate(SamplingEvent = fct_drop(SamplingEvent))

# Create a df for stacked barplots of the filtered and particulate fractions of MeHg, THg, and OC
all_conc_clean_frac <- all_conc_clean %>% 
  filter(
    Analyte %in% c(
      "DOC",
      "POC",
      "MeHg- filtered",
      "MeHg- particulate",
      "THg- filtered",
      "THg- particulate"
    )
  ) %>% 
  mutate(
    AnalyteGroup = case_when(
      Analyte %in% c("DOC", "POC") ~ "Organic Carbon",
      Analyte %in% c("MeHg- filtered", "MeHg- particulate") ~ "MeHg",
      Analyte %in% c("THg- filtered", "THg- particulate") ~ "THg"
    )
  )

# Calculate the percentage of each fraction for THg and MeHg
all_conc_clean_frac_per <- all_conc_clean %>% 
  filter(
    Analyte %in% c(
      "MeHg- total",
      "MeHg- filtered",
      "MeHg- particulate",
      "THg- total",
      "THg- filtered",
      "THg- particulate"
    )
  ) %>%
  separate(Analyte, into = c("AnalyteGroup", "Fraction"), sep = "- ") %>% 
  select(
    StationName,
    ShortName,
    SamplingEvent,
    Year,
    AnalyteGroup,
    Fraction,
    Conc
  ) %>% 
  pivot_wider(names_from = Fraction, values_from = Conc) %>% 
  mutate(
    Filt_per = filtered/total,
    Part_per = particulate/total
  ) %>% 
  select(-c(filtered:particulate)) %>%
  rename(
    Filtered = Filt_per,
    Particulate = Part_per
  ) %>% 
  pivot_longer(
    cols = Filtered:Particulate,
    names_to = "Fraction",
    values_to = "Percent"
  ) %>% 
  filter(!is.na(Percent))


# 2. Create Plots ---------------------------------------------------------

# Create a vector of all analytes to include in main plots
ana_main <- c(
  "Chloride- filtered",
  "DOC",
  "Iron- filtered",
  "Manganese- filtered",
  "MeHg- filtered",
  "MeHg- particulate",
  "MeHg- total",
  "POC",
  "Sulfate- filtered",
  "THg- filtered",
  "THg- particulate",
  "THg- total",
  "TOC",
  "TSS",
  "UVA 254",
  "VSS"
)

# Create df's that contain analytes for the main plots
all_conc_clean_main <- all_conc_clean %>% filter(Analyte %in% ana_main)
all_conc_clean_17_main <- all_conc_clean_17 %>% filter(Analyte %in% ana_main)


# 2.1 Plot all parameters -------------------------------------------------

# Grouped by station
pdf(file = "Conc_Plots_byStation.pdf", w=15, h=8.5)
  # All sampling events
  all_conc_clean_main %>% 
    group_by(StationName) %>% 
    do(plot = {
      print(.$StationName[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = SamplingEvent, 
            y = Conc, 
            color = Detect,
            fill = Detect
          )
        ) +
        geom_col() +
        scale_color_manual(
          name = NULL,
          values = c("grey35", "red"),
          aesthetics = c("color", "fill")
        ) +
        scale_x_discrete(drop = FALSE) +
        facet_wrap(
          vars(Analyte, Units),
          scales = "free_y"
        ) +
        labs(
          title = paste0("Concentrations at ", .$StationName[1]),
          subtitle = "All Sampling Events",
          x = "Sampling Event",
          y = "Concentration"
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x-axis labels at 90 degrees
      
      print(p)
    })

  # Just 2017 sampling events
  all_conc_clean_17_main %>% 
    group_by(StationName) %>% 
    do(plot = {
      print(.$StationName[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = SamplingEvent, 
            y = Conc
          )
        ) +
        geom_line(aes(group = 1)) +
        geom_point(
          aes(
            shape = Detect,
            color = Detect
          )
        ) +
        scale_shape_manual(
          name = NULL,
          values = c(16,15)
        ) +
        scale_color_manual(
          name = NULL,
          values = c("black", "red")
        ) +
        scale_x_discrete(drop = FALSE) +
        facet_wrap(
          vars(Analyte, Units),
          scales = "free_y"
        ) +
        labs(
          title = paste0("Concentrations at ", .$StationName[1]),
          subtitle = "Just 2017 Sampling Events",
          x = "Sampling Event",
          y = "Concentration"
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x-axis labels at 90 degrees
      
      print(p)
      
    })
  
dev.off()
  
# Grouped by analyte
pdf(file = "Conc_Plots_byAnalyte.pdf", w=15, h=8.5)
  # All sampling events
  all_conc_clean_main %>% 
    group_by(Analyte) %>% 
    do(plot = {
      print(.$Analyte[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = SamplingEvent, 
            y = Conc, 
            color = Detect,
            fill = Detect
          )
        ) +
        geom_col() +
        scale_color_manual(
          name = NULL,
          values = c("grey35", "red"),
          aesthetics = c("color", "fill")
        ) +
        facet_wrap(vars(ShortName)) +
        labs(
          title = paste0("Concentrations of ", .$Analyte[1]),
          subtitle = "All Sampling Events",
          x = "Sampling Event",
          y = paste0("Concentration (", .$Units[1], ")")
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x-axis labels at 90 degrees
      
      print(p)
    })
  
  # Just 2017 sampling events
  all_conc_clean_17_main %>% 
    group_by(Analyte) %>% 
    do(plot = {
      print(.$Analyte[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = SamplingEvent, 
            y = Conc
          )
        ) +
        geom_line(aes(group = 1)) +
        geom_point(
          aes(
            shape = Detect,
            color = Detect
          )
        ) +
        scale_shape_manual(
          name = NULL,
          values = c(16,15)
        ) +
        scale_color_manual(
          name = NULL,
          values = c("black", "red")
        ) +
        facet_wrap(vars(ShortName)) +
        labs(
          title = paste0("Concentrations of ", .$Analyte[1]),
          subtitle = "Just 2017 Sampling Events",
          x = "Sampling Event",
          y = paste0("Concentration (", .$Units[1], ")")
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x-axis labels at 90 degrees
      
      print(p)
      
    })
  
dev.off()
    
# Boxplots
pdf(file = "Conc_Boxplots.pdf", w=15, h=8.5)
  all_conc_clean_main %>% 
    mutate(Year = as.character(Year)) %>% 
    group_by(Analyte) %>% 
    do(plot = {
      print(.$Analyte[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = ShortName, 
            y = Conc
          )
        ) +
        geom_boxplot(outlier.shape = NA) +
        geom_jitter(
          width = 0.25,
          aes(color = Year)
        ) +
        labs(
          title = paste0("Boxplots of ", .$Analyte[1], " Concentrations"),
          subtitle = "All Sampling Events",
          x = "Station",
          y = paste0("Concentration (", .$Units[1], ")")
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x-axis labels at 90 degrees
      
      print(p)
      
    })
    
dev.off()


# 2.2 Plot Analyte Fractions ----------------------------------------------

pdf(file = "Conc_Fraction_Plots.pdf", w=15, h=8.5)
  # Plot fractions of MeHg, THg, and OC for each sampling event faceted by station
  all_conc_clean_frac %>% 
    group_by(AnalyteGroup) %>% 
    do(plot = {
      print(.$AnalyteGroup[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = SamplingEvent, 
            y = Conc, 
            fill = Analyte
          )
        ) +
        geom_col() +
        facet_wrap(vars(ShortName)) +
        labs(
          title = paste0("Concentrations of ", .$AnalyteGroup[1], " Fractions"),
          x = "Sampling Event",
          y = paste0("Concentration (", .$Units[1], ")")
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))  #x-axis labels at 90 degrees
      
      print(p)
      
    })
  
  # Plot Percent fraction of MeHg and THg for each sampling event faceted by station
  all_conc_clean_frac_per %>% 
    group_by(AnalyteGroup) %>% 
    do(plot = {
      print(.$AnalyteGroup[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = SamplingEvent, 
            y = Percent, 
            fill = Fraction
          )
        ) +
        geom_col() +
        facet_wrap(vars(ShortName)) +
        labs(
          title = paste0("Percentage of each ", .$AnalyteGroup[1], " Fraction"),
          x = "Sampling Event",
          y = "Fraction Percentage"
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  #x-axis labels at 90 degrees
        scale_y_continuous(labels = percent_format())
      
      print(p)
      
    })
  
  # Plot fractions of MeHg, THg, and OC for each station faceted by sampling event
  all_conc_clean_frac %>% 
    group_by(AnalyteGroup) %>% 
    do(plot = {
      print(.$AnalyteGroup[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = ShortName, 
            y = Conc, 
            fill = Analyte
          )
        ) +
        geom_col() +
        facet_wrap(vars(SamplingEvent)) +
        labs(
          title = paste0("Concentrations of ", .$AnalyteGroup[1], " Fractions"),
          x = "Station",
          y = paste0("Concentration (", .$Units[1], ")")
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))  #x-axis labels at 90 degrees
      
      print(p)
      
    })
  
  # Plot Percent fraction of MeHg and THg for each station faceted by sampling event
  all_conc_clean_frac_per %>% 
    group_by(AnalyteGroup) %>% 
    do(plot = {
      print(.$AnalyteGroup[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = ShortName, 
            y = Percent, 
            fill = Fraction
          )
        ) +
        geom_col() +
        facet_wrap(vars(SamplingEvent)) +
        labs(
          title = paste0("Percentage of each ", .$AnalyteGroup[1], " Fraction"),
          x = "Station",
          y = "Fraction Percentage"
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  #x-axis labels at 90 degrees
        scale_y_continuous(labels = percent_format())
      
      print(p)
      
    })

dev.off()


# 2.3 Plot the Toe Drain Transect Concentrations --------------------------

all_conc_clean_tdt <- 
  list(
    MainAnalytes = all_conc_clean_main,
    FracAnalytes = all_conc_clean_frac,
    PercentFrac = all_conc_clean_frac_per
  ) %>% 
  map(
    ~filter(
      .x,
      StationName %in% c(
        "Toe Drain at County Road 22",
        "Toe Drain at Interstate 80",
        "Toe Drain at Lisbon Weir",
        "Toe Drain at 1/2 Lisbon",
        "Prospect Slough"      
      )
    )
  )

# Create plots
pdf(file = "Conc_ToeDrainTransect_Plots.pdf", w=11, h=8.5)  
  # Facet by sampling event, grouped by parameter
  all_conc_clean_tdt$MainAnalytes %>% 
    group_by(Analyte) %>% 
    do(plot = {
      print(.$Analyte[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = ShortName, 
            y = Conc
          )
        ) +
        geom_line(aes(group = 1)) +
        geom_point() +
        facet_wrap(vars(SamplingEvent)) +
        labs(
          title = "Toe Drain Transect Concentrations",
          subtitle = .$Analyte[1],
          x = "Station",
          y = paste0("Concentration (", .$Units[1], ")")
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x-axis labels at 90 degrees
      
      print(p)
      
    })
  
  # Facet by parameter, grouped by sampling event
  all_conc_clean_tdt$MainAnalytes %>% 
    group_by(SamplingEvent) %>% 
    do(plot = {
      print(.$SamplingEvent[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = ShortName, 
            y = Conc
          )
        ) +
        geom_line(aes(group = 1)) +
        geom_point() +
        facet_wrap(
          vars(Analyte, Units),
          scales = "free_y"
        ) +
        labs(
          title = "Toe Drain Transect Concentrations",
          subtitle = .$SamplingEvent[1],
          x = "Station",
          y = "Concentration"
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x-axis labels at 90 degrees
      
      print(p)
      
    })
  
  # Fractions of MeHg, THg, and OC faceted by sampling event
  all_conc_clean_tdt$FracAnalytes %>% 
    group_by(AnalyteGroup) %>% 
    do(plot = {
      print(.$AnalyteGroup[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = ShortName, 
            y = Conc, 
            fill = Analyte
          )
        ) +
        geom_col() +
        facet_wrap(vars(SamplingEvent)) +
        labs(
          title = paste0("Concentrations of ", .$AnalyteGroup[1], " Fractions"),
          subtitle = "Toe Drain Transect",
          x = "Station",
          y = paste0("Concentration (", .$Units[1], ")")
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))  #x-axis labels at 90 degrees
      
      print(p)
      
    })
  
  # Percent Fraction of MeHg and THg faceted by sampling event
  all_conc_clean_tdt$PercentFrac %>% 
    group_by(AnalyteGroup) %>% 
    do(plot = {
      print(.$AnalyteGroup[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = ShortName, 
            y = Percent, 
            fill = Fraction
          )
        ) +
        geom_col() +
        facet_wrap(vars(SamplingEvent)) +
        labs(
          title = paste0("Percentage of each ", .$AnalyteGroup[1], " Fraction"),
          subtitle = "Toe Drain Transect",
          x = "Station",
          y = "Fraction Percentage"
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  #x-axis labels at 90 degrees
        scale_y_continuous(labels = percent_format())
      
      print(p)
      
    })

dev.off()


# 2.4 Plot SpConc, ICP, and ICP-MS results --------------------------------

# Clean spcond_orig
spcond_clean <- field_data %>% 
  select(
    StationName,
    SampleDate,
    Conc = SpCond
  ) %>% 
  # Create some new variables
  mutate(
    Year = year(SampleDate),
    Detect = "Detect",
    Units = "uS/cm",
    Analyte = "Specific Conductance"
  ) %>% 
  add_samplingevent() %>% 
  add_short_sta_names() %>% 
  # Convert some variables to factors to apply plotting order
  conv_fact_samplingevent() %>% 
  conv_fact_short_sta_names() %>% 
  conv_fact_long_sta_names() %>% 
  # Select only necessary variables
  select(
    StationName,
    ShortName,
    Year,
    SamplingEvent,
    Analyte,
    Conc,
    Units,
    Detect
  ) %>% 
  # Remove some unnecessary data
  filter(
    !SamplingEvent %in% c("Dec 22-23, 2014", "Apr 11-12, 2017"),
    !ShortName %in% c("Cache Sl", "Miner Sl")
  )
  
# Create a df with the ICP and ICP-MS analytes
all_conc_clean_icp <- all_conc_clean %>%
  filter(
    !Analyte %in% ana_main,
    !Analyte %in% c("Iron- total", "Potassium- total")
  ) %>% 
  select(
    StationName,
    ShortName,
    Year,
    SamplingEvent,
    Analyte,
    Conc,
    Units,
    Detect
  ) %>% 
  # Bind Specific Conductance data
  bind_rows(spcond_clean) %>% 
  # Remove a few Stations from the plots
  filter(
    !ShortName %in% c(
      "Toe Dr at Rd 22", 
      "Toe Dr at I-80", 
      "Toe Dr at Lisbon", 
      "Prospect Sl"
    )
  ) %>% 
  # Drop unused factors
  mutate(
    StationName = fct_drop(StationName),
    ShortName = fct_drop(ShortName),
    SamplingEvent = fct_drop(SamplingEvent)
  )

# Create Plots
pdf(file = "Conc_ICP_Plots.pdf", w=15, h=8.5)
# Facet by parameter, grouped by sampling event
all_conc_clean_icp %>% 
  group_by(SamplingEvent) %>% 
  do(plot = {
    print(.$SamplingEvent[1])
    p <- 
      ggplot(
        data = .,
        aes(
          x = ShortName, 
          y = Conc,
          color = Detect,
          fill = Detect
        )
      ) +
      geom_col() +
      scale_color_manual(
        name = NULL,
        values = c("grey35", "red"),
        aesthetics = c("color", "fill")
      ) +
      facet_wrap(
        vars(Analyte, Units),
        scales = "free_y"
      ) +
      labs(
        title = "Concentrations of ICP and ICP-MS Analytes",
        subtitle = .$SamplingEvent[1],
        x = "Station",
        y = "Concentration"
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  #x-axis labels at 90 degrees
    
    print(p)
    
  })

dev.off()


# 3. Calculate Summary Statistics -----------------------------------------

# Summarize Conc data by Station and Analyte
  all_conc_clean_list <- 
    list(
      AllEvents = all_conc_clean,
      Just2017 = all_conc_clean_17
    )
  
  all_conc_clean_summ <- map(all_conc_clean_list, ~summ_stat(.x, Conc, StationName, Analyte))
  
  # Count number of Non-detects per Station-Analyte combo
  all_conc_clean_nd <- all_conc_clean_list %>% 
    map(~filter(.x, Detect == "Non-detect")) %>% 
    map(~count(.x, StationName, Analyte)) %>% 
    map(~rename(.x, N_nd = n))
  
  # Add number of Non-detects to ConcData.Summ df
  all_conc_clean_summ <- all_conc_clean_summ %>% 
    map_at("AllEvents", ~left_join(.x, all_conc_clean_nd$AllEvents)) %>% 
    map_at("Just2017", ~left_join(.x, all_conc_clean_nd$Just2017)) %>% 
    map(~replace_na(.x, list(N_nd = 0)))
  
  # Add Units to the Summary Statistics
  unit_key <- all_conc_clean %>% 
    count(Analyte, Units) %>% 
    select(-n)
  
  all_conc_clean_summ <- all_conc_clean_summ %>% 
    map(~left_join(.x, unit_key))
  
  # Export Summary Statistics
  all_conc_clean_summ$AllEvents %>% write_excel_csv("ConcData_SummaryStats_all.csv")
  all_conc_clean_summ$Just2017 %>% write_excel_csv("ConcData_SummaryStats_2017.csv")

# Summarize Percent Fraction data by Station and Analyte
  frac_per <- all_conc_clean_frac_per %>% 
    unite(AnalyteGroup, Fraction, col = "Analyte", sep = "- ")
  
  frac_per_list <- 
    list(
      AllEvents = frac_per,
      Just2017 = filter(frac_per, Year == 2017)
    )
  
  frac_per_summ <- frac_per_list %>% 
    map(~summ_stat(.x, Percent, StationName, Analyte)) %>% 
    map(~separate(.x, Analyte, into = c("AnalyteGroup", "Fraction"), sep = "- "))
  
  # Export Summary Statistics
  frac_per_summ$AllEvents %>% write_excel_csv("FracPer_SummaryStats_all.csv")
  frac_per_summ$Just2017 %>% write_excel_csv("FracPer_SummaryStats_2017.csv")
  
  