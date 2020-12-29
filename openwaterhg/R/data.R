#' @title Combined Parameter Data
#' @description Contains all combined parameter data for the Yolo Bypass
#'     Mass Balance Study. The calculations for this dataset are in the
#'     "YB_Mass_Balance/Concentrations/CombinedCalcs_Plots_and_SummaryStats.R"
#'     script file. The combined parameters include:
#' \describe{
#' \item{THg and MeHg Concentrations on Solids}{Provide estimates of the
#'     amount of total mercury or methylmercury bound to the suspended
#'     sediment. An increasing or decreasing trend in these parameters
#'     indicate either an enrichment or reduction of Hg or MeHg on suspended
#'     particles.}
#' \item{THg and MeHg Partitioning Coefficients (Kd)}{An increasing trend in
#'     these parameters indicates greater partitioning of Hg or MeHg to the
#'     solid-bound phase, and a decreasing trend indicates greater partitioning
#'     to the filtered or dissolved phase.}
#' \item{Percent MeHg Concentration of the THg Concentration}{The percent of
#'     THg that is MeHg. An increasing trend indicates more of the total mercury
#'     is methylmercury. Values for all three fractions (filtered, particulate,
#'     and unfiltered) are provided.}
#' \item{THg or MeHg normalized by OC}{Either THg or MeHg concentrations
#'     divided by organic carbon concentrations. Values for all three fractions
#'     (filtered, particulate, and unfiltered) are provided.}
#' \item{TOC or POC Concentrations on solids}{Either TOC or POC concentrations
#'     divided by the TSS concentration multiplied by 1,000.}
#' \item{Total Aluminum Concentration on Solids}{The total aluminum
#'     concentration divided by the TSS concentration multiplied by 1,000.}
#' }
#'
#' @format data frame with 2,477 rows and 6 columns
#' \describe{
#' \item{StationName}{The station name}
#' \item{SampleDate}{The sampling date}
#' \item{CollectionTime}{The collection time in PST}
#' \item{Parameter}{The name of the calculated parameter}
#' \item{Value}{Calculated value}
#' \item{Units}{The units of the value}
#' }
#'
"comb_param_calc"


#' @title Concentration Data in Water
#' @description Contains all water concentration data collected for the Yolo Bypass
#'     Mass Balance Study. The raw data for this dataset were integrated,
#'     cleaned, and QA'ed in the "Compile_and_Clean_Conc_Data_FloodEvents.R" script file.
#'
#' @format data frame with 3,398 rows and 13 columns
#' \describe{
#' \item{SampleCode}{The unique identifier for each sample produced by Bryte Lab}
#' \item{StationName}{The station name where the water sample was collected}
#' \item{SampleDate}{The sampling date}
#' \item{CollectionTime}{The sample collection time in PST}
#' \item{Analyte}{The analytical parameter measured}
#' \item{Result}{The analytical result}
#' \item{ResQual}{A numeric variable that represents whether the result was below
#'     the detection limit (RL or MDL). A value of "0" indicates that the result
#'     was detected, a value of "1" indicates that the result was below the
#'     detection limit, and a value of "0.5" indicates that either the replicate
#'     or parent sample was below the detection limit.}
#' \item{RL}{The analytical reporting limit of the sample}
#' \item{MDL}{The analytical method detection limit of the sample}
#' \item{Units}{The units of the result}
#' \item{LabComments}{Comments provided by the analytical laboratory}
#' \item{MME_Comments}{Comments provided by staff from the Mercury Monitoring and
#'     Evaluation section}
#' \item{QualCode}{Qualification code(s) for the sample. "J" indicates that there
#'     was an issue with the sample, but it is still being used in analyses. "R"
#'     indicates that there was an issue with the sample, and it is not being used
#'     in analyses. "BD" indicates that there was a detection in a blank sample
#'     associated with the sample. "FV" indicates that there was a RPD value greater
#'     than its acceptable limit in a field duplicate pair associated with the
#'     sample, which could be due to variability in the sampling method or the
#'     water being sampled. "FGT" indicates that the value of the filtered sample
#'     was greater than its associated total sample. "NRS" indicates that the sample
#'     may not have been representative of waterbody. The reason is explained in
#'     the MME Comments.}
#' }
#'
"conc_data"


#' @title Daily Average Flow Data for the 2014, 2016, and 2017 Floods
#' @description Contains daily average flow data for all sampling locations
#'     for the 2014, 2016, and 2017 flood events in the Yolo Bypass. The raw data
#'     for this dataset were integrated, cleaned, and averaged in the
#'     "YB_Mass_Balance/Flows/Process_Flow_Data.R" script file.
#'
#' @format data frame with 1,962 rows and 5 columns
#' \describe{
#' \item{Date}{The date for the daily average flow data}
#' \item{Year}{The year of the flood}
#' \item{StationName}{The station name where the flow measurements were collected}
#' \item{LocType}{A categorical variable indicating whether the station was an
#'     inlet, outlet, or Below Liberty Island location}
#' \item{Flow}{The daily average flow value in cubic feet per second}
#' }
#'
"daily_flow_data_all"


#' @title Daily Average Flow Data for the Sampling Events
#' @description Contains a subset of the \code{daily_flow_data_all} dataset with
#'     daily average flow data for the 11 sampling events conducted for the
#'     Yolo Bypass Mass Balance study. These flow values were used in the load
#'     calculations for these 11 sampling events. The raw data for this dataset
#'     were integrated, cleaned, and averaged in the
#'     "YB_Mass_Balance/Flows/Process_Flow_Data.R" script file.
#'
#' @format data frame with 134 rows and 5 columns
#' \describe{
#' \item{SamplingEvent}{The sampling event for the daily average flow data}
#' \item{Year}{The year of the flood}
#' \item{StationName}{The station name where the flow measurements were collected}
#' \item{LocType}{A categorical variable indicating whether the station was an
#'     inlet, outlet, or Below Liberty Island location}
#' \item{Flow}{The daily average flow value in cubic feet per second}
#' }
#'
"daily_flow_data_se"


#' @title Field Measurements
#' @description Contains field measurements collected for the Yolo Bypass
#'     Mass Balance Study. Measurements include Water Temperature, Specific
#'     Conductance, Dissolved Oxygen, and Turbidity.
#'
#' @format data frame with 165 rows and 7 columns
#' \describe{
#' \item{StationName}{The station name where the measurement was collected}
#' \item{SampleDate}{The sampling date of the field measurement}
#' \item{CollectionTime}{The collection time of the field measurement in PST}
#' \item{WaterTempC}{The water temperature in degrees Celcius}
#' \item{SpCond}{The specific conductance in uS/cm}
#' \item{DissOxy}{The dissolved oxygen concentration in mg/L}
#' \item{Turbidity}{The turbidity in NTU}
#' }
#'
"field_data"


#' @title Calculated Mass Loads
#' @description Contains calculated mass loads for the Yolo Bypass Mass Balance
#'     Study. The calculations for this dataset are in the
#'     "YB_Mass_Balance/Loads/All_LoadCalcs.R" script file.
#'
#' @format data frame with 1,460 rows and 10 columns
#' \describe{
#' \item{SamplingEvent}{The sampling event for the mass load value}
#' \item{Year}{The year of the flood}
#' \item{StationName}{The station name}
#' \item{LocType}{A categorical variable indicating whether the station was an
#'     inlet, outlet, or Below Liberty Island location}
#' \item{Analyte}{The analytical parameter for the mass load value}
#' \item{Load}{The calculated mass load value}
#' \item{LoadUnits}{The units of the load value}
#' \item{digits}{The number of sigificant figures for the calculated mass load value}
#' }
#'
"loads_calc"


#' @title Flow and Mass Load Data from the CALFED 2008 report
#' @description Contains total daily inflow and inlet, outlet, and net methylmercury
#'     load data from the 2006 flood event in the Yolo Bypass as presented in the
#'     CALFED 2008 report. This dataset is used as a comparison to the MeHg loads
#'     from the Yolo Bypass Mass Balance study.
#'
#' @format data frame with 14 rows and 7 columns
#' \describe{
#' \item{SampleDate}{The date of the total inflow and net load values}
#' \item{Year}{The year of the flood}
#' \item{Analyte}{The analytical parameter for the net load value}
#' \item{Inlet_Load}{The total inlet load value in g/day}
#' \item{Outlet_Load}{The total outlet load value in g/day}
#' \item{Net_Load}{The net load value in g/day}
#' \item{Total_Inflow}{The total daily inflow value in cubic feet per second}
#' }
#'
"loads_flow_cf"


#' @title Calculated Particulate Concentrations
#' @description Contains calculated particulate concentration data for mercury,
#'     methylmercury, and organic carbon for the Yolo Bypass Mass Balance Study.
#'     Particulate concentrations were calculated as the difference between the
#'     unfiltered and filtered concentrations. The calculations for this dataset
#'     are in the "YB_Mass_Balance/Concentrations/Calculate_Particulate_Fractions.R"
#'     script file.
#'
#' @format data frame with 488 rows and 6 columns
#' \describe{
#' \item{StationName}{The station name}
#' \item{SampleDate}{The sampling date}
#' \item{CollectionTime}{The sample collection time in PST}
#' \item{Analyte}{The name of the calculated particulate parameter}
#' \item{Conc}{The calculated particulate concentration}
#' \item{Units}{The units of the \code{Conc} variable}
#' }
#'
"part_conc_calc"


#' @title Field and Filter Blanks Collected in Association with \code{conc_data}
#' @description Contains all field and filter blank data collected for the Yolo
#'     Bypass Mass Balance Study. Field and filter blanks were collected each
#'     day water samples were collected to test for possible contamination from
#'     sampling techniques, sample bottles, or filtering equipment. These blanks
#'     were separated from the raw data files in the "Compile_and_Clean_Conc_Data.R"
#'     script file.
#'
#' @format data frame with 460 rows and 14 columns
#' \describe{
#' \item{SampleCode}{The unique identifier for each field or filter blank sample
#'     produced by Bryte Lab}
#' \item{StationName}{The name of the blank sample, either Field Blank or Filter
#'     Blank}
#' \item{SampleDate}{The collection date of the blank sample}
#' \item{CollectionTime}{The collection time of the blank sample in PST}
#' \item{Analyte}{The analytical parameter measured}
#' \item{Result}{The analytical result}
#' \item{RL}{The analytical reporting limit of the sample}
#' \item{MDL}{The analytical method detection limit of the sample}
#' \item{Units}{The units of the result}
#' \item{LabComments}{Comments provided by the analytical laboratory}
#' \item{MME_Comments}{Comments provided by staff from the Mercury Monitoring and
#'     Evaluation section}
#' \item{AmbSampConc}{Either the concentration of the ambient or environmental sample
#'     collected at the same station and date as the field blank sample, or the 
#'     average concentration of all ambient samples associated with the filter blank 
#'     sample. This value is only provided for blanks that had a detected value.}
#' \item{Blank_Amb_ratio}{The ratio of the blank sample concentration to the
#'     \code{AmbSampConc} value expressed as a percent. Samples with percentages
#'     of 20% and below were not flagged as a blank detection. If this percentage
#'     was greater than 20%, all samples collected on the same day were flagged as
#'     "BD" in \code{Flag}.}
#' \item{Flag}{Flag for the sample. "BD" indicates that there was a detection in
#'     the blank sample.}
#' }
#'
"qa_field_blanks"


#' @title Field Duplicates Collected in Association with \code{conc_data}
#' @description Contains all field duplicate data collected for the Yolo Bypass
#'     Mass Balance Study. Field duplicates were collected each day water samples
#'     were collected to test for sample reproducibility and field variability.
#'     The duplicates and their associated parent samples were separated from
#'     the raw data files in the "Compile_and_Clean_Conc_Data.R" script file.
#'
#' @format data frame with 460 rows and 19 columns
#' \describe{
#' \item{SampleCode_PS}{The unique identifier for the associated parent sample
#'     produced by Bryte Lab}
#' \item{SampleCode_FD}{The unique identifier for the field duplicate sample
#'     produced by Bryte Lab}
#' \item{StationName}{The name of the station where the field duplicate was
#'     collected}
#' \item{SampleDate}{The collection date of the field duplicate and parent
#'     samples}
#' \item{CollectionTime_PS}{The collection time of the associated parent
#'     sample in PST}
#' \item{CollectionTime_FD}{The collection time of the field duplicate
#'     sample in PST}
#' \item{Analyte}{The analytical parameter measured}
#' \item{Result_PS}{The analytical result of the associated parent sample}
#' \item{Result_FD}{The analytical result of the field duplicate sample}
#' \item{RPD}{The relative percent difference between the field duplicate sample
#'     and the associated parent sample. This was calculated as the absolute
#'     difference between the two values divided by their average and expressed as
#'     a percent. RPD's greater than their Measurement Quality Objectives were
#'     flagged as "FV" in \code{Flag}. This only applied to duplicate pairs
#'     where at least one value was greater than ten times the Reporting Limit.}
#' \item{ResQual}{A numeric variable that represents whether either of the results
#'     were below the detection limit (RL or MDL). A value of "0" indicates that
#'     the results of both samples were detected, a value of "1" indicates that
#'     both samples were below the detection limit, and a value of "0.5" indicates
#'     that either the field duplicate or parent sample was below the detection
#'     limit.}
#' \item{RL}{The analytical reporting limit of the sample}
#' \item{MDL}{The analytical method detection limit of the sample}
#' \item{Units}{The units of the result}
#' \item{LabComments_PS}{Comments provided by the analytical laboratory for the
#'     associated parent sample}
#' \item{LabComments_FD}{Comments provided by the analytical laboratory for the
#'     field duplicate sample}
#' \item{MME_Comments_PS}{Comments provided by staff from the Mercury Monitoring and
#'     Evaluation section for the associated parent sample}
#' \item{MME_Comments_FD}{Comments provided by staff from the Mercury Monitoring and
#'     Evaluation section for the field duplicate sample}
#' \item{Flag}{Flag for the sample pair. "FV" indicates that the pair has a RPD value
#'     greater than its acceptable limit.}
#' }
#'
"qa_field_dups"
