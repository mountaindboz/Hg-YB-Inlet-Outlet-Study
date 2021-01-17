# Yolo Bypass Inlet-Outlet Study
# EDA plots of the Field Measurements collected in 2017 during the Yolo Bypass flood event

library(tidyverse)
library(lubridate)
library(openwaterhg)
library(scales)

# Create a data frame of Parameter names for plots and their units
param_key <- tibble(
  Parameter = c(
    "WaterTempC", 
    "SpCond", 
    "DissOxy",
    "Turbidity"
  ),
  Parameter_plot = c(
    "Water Temperature", 
    "Specific Conductance", 
    "Dissolved Oxygen",
    "Turbidity"
  ),
  Units = c(
    "Degrees C",
    "uS/cm",
    "mg/L",
    "NTU"
  )
)

# Prepare Field Measurement data for plots
field_data_clean <- field_data %>% 
  pivot_longer(
    cols = WaterTempC:Turbidity,
    names_to = "Parameter",
    values_to = "Value"
  ) %>% 
  # Add parameter names for plots and units variable
  left_join(param_key) %>% 
  # Add variable with shortened station names for plots and apply plotting order
  add_short_sta_names() %>% 
  conv_fact_short_sta_names() %>% 
  # Add a variable for year
  mutate(Year = year(SampleDate)) %>% 
  # Remove NA values
  filter(!is.na(Value)) %>% 
  # Select variables to keep
  select(
    ShortName,
    SampleDate,
    Year,
    Parameter_plot,
    Value,
    Units
  )

# Create Time Series plots by parameter for just 2017 sampling events
pdf(file = "2017_FieldMeas_TS.pdf", w = 11, h = 8.5)
  field_data_clean %>%
    filter(Year == 2017) %>% 
    group_by(Parameter_plot) %>% 
    do(plot = {
      print(.$Parameter_plot[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = SampleDate, 
            y = Value
          )
        ) + 
        geom_point() +
        geom_line() +
        facet_wrap(vars(ShortName)) +
        labs(
          title = paste0('Time Series Plots by Station for ', .$Parameter_plot[1]),
          subtitle = "2017 Sampling Events",
          y = paste0('Value (', .$Units[1], ')')
        ) +
        scale_x_date(
          name = "Sampling Date",
          breaks = breaks_pretty(5),
          labels = label_date_short()
        )
      
      print(p)
    })
dev.off()

# Create Boxplots by parameter for all sampling events
pdf(file = "FieldMeas_Boxplots.pdf", w = 11, h = 8.5)
  field_data_clean %>% 
    mutate(Year = as.character(Year)) %>%
    group_by(Parameter_plot) %>% 
    do(plot = {
      print(.$Parameter_plot[1])
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
          title = paste0('Boxplot for ', .$Parameter_plot[1]),
          subtitle = "All Sampling Events",
          x = 'Station',
          y = paste0('Value (', .$Units[1], ')')
          ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))  #vertical x-axis labels
      
      print(p)
    })
dev.off()

# Create Histograms for just 2017 sampling events
pdf(file = "2017_FieldMeas_HistPlots.pdf", w = 11, h = 8.5)
  field_data_clean %>% 
    filter(Year == 2017) %>% 
    group_by(Parameter_plot) %>% 
    do(plot = {
      print(.$Parameter_plot[1])
      p <- 
        ggplot(
          data = .,
          aes(x = Value)
        ) + 
        geom_histogram(bins = 5) +
        facet_wrap(vars(ShortName)) +
        labs(
          title = paste0('Histograms for ', .$Parameter_plot[1]),
          subtitle = "Just 2017 Sampling Events",
          x = paste0('Value (', .$Units[1], ')'),
          y = 'Count'
        )
      
      print(p)
    })
dev.off()

