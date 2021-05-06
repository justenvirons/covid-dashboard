# Author: C. Scott Smith
# Updated 11/17/2020
# Download and process COVID-19 test, cases and deaths data by ZCTA in Chicago
# activate packages

library(csv)
library(dplyr)
library(tidyverse)
library(sf)
library(ggplot2)
library(clipr)
library(readxl)
library(scales)
library(zoo)
library(lubridate)

# Functions used for processing, transforming data tables (used below)
fx_asnum <- function(x) as.numeric(gsub(",","",x)) # convert to number
fx_pctpop <- function(x) (x/(table$Total)*100) # percent of total population
fx_pctcas <- function(x) (x/(table$Cases)*100) # percent of total cases
fx_pctdth <- function(x) (x/(table$Deaths)*100) # percent of total deaths
fx_pcthos <- function(x) (x/(table$Hosp)*100) # percent of total hospitalizations
fx_cumsum <- function(x) cumsum(x) # cumulative sum using Date field
fx_rollmn <- function(x) rollmean(x, k=7, fill=0, align="right") # rolling 7-day average calculation using date column
fx_lagwk <- function(x) lag(x, k=7, fill=0, align="right") # 7-day lag value using date column
fx_rollsm <- function(x) rollsum(x, k=7, fill=0, align="right") # rolling 7-day sum using date column
fx_pfxpct <- function(x) paste0('pct',x) # prefix "pct" added to column names with percent values
fx_pfxtot <- function(x) paste0('tot',x) # prefix "tot" added to column names with total values

# Import, rename and process citywide demographic data from Chicago Data Portal 
# data.frame(colnames(DemoChars_Chicago)) # use to identify column names
DemoChars_Chicago <- read.csv("https://data.cityofchicago.org/api/views/85cm-7uqa/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
DemoChars_Chicago <- DemoChars_Chicago %>% select(-1, -Record.ID) %>% 
  filter(Geography=="Chicago",
         Year==2019) %>% 
  rename("Total"="Population...Total", 
         "Age0_17"="Population...Age.0.17", 
         "Age18_29"="Population...Age.18.29", 
         "Age30_39"="Population...Age.30.39", 
         "Age40_49"="Population...Age.40.49", 
         "Age50_59"="Population...Age.50.59", 
         "Age60_69"="Population...Age.60.69", 
         "Age70_79"="Population...Age.70.79", 
         "Age80Pl"="Population...Age.80.", 
         "Female"="Population...Female", 
         "Male"="Population...Male", 
         "Latinx"="Population...Latinx", 
         "AsianNL"="Population...Asian.Non.Latinx", 
         "BlackNL"="Population...Black.Non.Latinx", 
         "WhiteNL"="Population...White.Non.Latinx", 
         "OthRaceNL"="Population...Other.Race.Non.Latinx") %>%
  select(-contains("Population"))
  
table <- DemoChars_Chicago
table <- table %>% mutate_at(vars(3:18), fx_asnum)
DemoChars_Chicago_pct <- table %>% mutate_at(vars(4:18), fx_pctpop)
rm(table)

# Import, rename and process daily COVID-19 cases, deaths, hospitalizations data from Chicago Data Portal
# data.frame(colnames(Chicago_COVID19_CasesDeathsHospitalizations)) # use to identify column names
Chicago_COVID19_CasesDeathsHospitalizations <- read_csv("https://data.cityofchicago.org/api/views/naz8-j4nc/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
Chicago_COVID19_CasesDeathsHospitalizations <- Chicago_COVID19_CasesDeathsHospitalizations %>% 
  rename("Cases"="Cases - Total",
         "Deaths"="Deaths - Total",
         "Hosp"="Hospitalizations - Total",
         "Cases_Age0_17"="Cases - Age 0-17",
         "Cases_Age18_29"="Cases - Age 18-29",
         "Cases_Age30_39"="Cases - Age 30-39",
         "Cases_Age40_49"="Cases - Age 40-49",
         "Cases_Age50_59"="Cases - Age 50-59",
         "Cases_Age60_69"="Cases - Age 60-69",
         "Cases_Age70_79"="Cases - Age 70-79",
         "Cases_Age80Pl"="Cases -  Age 80+",
         "Cases_UnkAge"="Cases - Age Unknown",
         "Cases_Female"="Cases - Female",
         "Cases_Male"="Cases - Male",
         "Cases_UnkGend"="Cases - Unknown Gender",
         "Cases_Latinx"="Cases - Latinx",
         "Cases_AsianNL"="Cases - Asian Non-Latinx",
         "Cases_BlackNL"="Cases - Black Non-Latinx",
         "Cases_WhiteNL"="Cases - White Non-Latinx",
         "Cases_OthRaceNL"="Cases - Other Race Non-Latinx",
         "Cases_UnkRace"="Cases - Unknown Race/Ethnicity",
         "Deaths_Age0_17"="Deaths - Age 0-17",
         "Deaths_Age18_29"="Deaths - Age 18-29",
         "Deaths_Age30_39"="Deaths - Age 30-39",
         "Deaths_Age40_49"="Deaths - Age 40-49",
         "Deaths_Age50_59"="Deaths - Age 50-59",
         "Deaths_Age60_69"="Deaths - Age 60-69",
         "Deaths_Age70_79"="Deaths - Age 70-79",
         "Deaths_Age80Pl"="Deaths - Age 80+",
         "Deaths_UnkAge"="Deaths - Age Unknown",
         "Deaths_Female"="Deaths - Female",
         "Deaths_Male"="Deaths - Male",
         "Deaths_UnkGend"="Deaths - Unknown Gender",
         "Deaths_Latinx"="Deaths - Latinx",
         "Deaths_AsianNL"="Deaths - Asian Non-Latinx",
         "Deaths_BlackNL"="Deaths - Black Non-Latinx",
         "Deaths_WhiteNL"="Deaths - White Non-Latinx",
         "Deaths_OthRaceNL"="Deaths - Other Race Non-Latinx",
         "Deaths_UnkRace"="Deaths - Unknown Race/Ethnicity",
         "Hosp_Age0_17"="Hospitalizations - Age 0-17",
         "Hosp_Age18_29"="Hospitalizations - Age 18-29",
         "Hosp_Age30_39"="Hospitalizations - Age 30-39",
         "Hosp_Age40_49"="Hospitalizations - Age 40-49",
         "Hosp_Age50_59"="Hospitalizations - Age 50-59",
         "Hosp_Age60_69"="Hospitalizations - Age 60-69",
         "Hosp_Age70_79"="Hospitalizations - Age 70-79",
         "Hosp_Age80Pl"="Hospitalizations - Age 80+",
         "Hosp_UnkAge"="Hospitalizations - Age Unknown",
         "Hosp_Female"="Hospitalizations - Female",
         "Hosp_Male"="Hospitalizations - Male",
         "Hosp_UnkGend"="Hospitalizations - Unknown Gender",
         "Hosp_Latinx"="Hospitalizations - Latinx",
         "Hosp_AsianNL"="Hospitalizations - Asian Non-Latinx",
         "Hosp_BlackNL"="Hospitalizations - Black Non-Latinx",
         "Hosp_WhiteNL"="Hospitalizations - White Non-Latinx",
         "Hosp_OthRaceNL"="Hospitalizations - Other Race Non-Latinx",
         "Hosp_UnkRace"="Hospitalizations - Unknown Race/Ethnicity")

Chicago_COVID19_CasesDeathsHospitalizations$Date <- as.Date(Chicago_COVID19_CasesDeathsHospitalizations$Date, "%m/%d/%Y") # reformat date text to date data type
Chicago_COVID19_CasesDeathsHospitalizations$Geography <- "Chicago"
table <- Chicago_COVID19_CasesDeathsHospitalizations 
table <- table %>% arrange(Date) %>% mutate_at(vars(2:58), fx_cumsum)
Chicago_COVID19_CasesDeathsHospitalizations_pct <- table %>% 
  arrange(Date) %>% 
  mutate_at(vars(5:22), fx_pctcas) %>% 
  mutate_at(vars(23:40), fx_pctdth) %>% 
  mutate_at(vars(41:58), fx_pcthos) %>% 
  mutate_if(is.numeric,list(~replace_na(.,0))) %>% 
  ungroup()

rm(table) # remove temporary table

# Reformat variables, add layer and label fields for time series data that power serial charts, selector tools used in disparity dashboard
temp1 <- Chicago_COVID19_CasesDeathsHospitalizations_pct %>% gather("Category", "Rate_Cat", 5:13) %>% select(Date,Category,Rate_Cat) %>% mutate(Layer="Cases", Label="Age")
temp2 <- Chicago_COVID19_CasesDeathsHospitalizations_pct %>% gather("Category", "Rate_Cat", 14:16) %>% select(Date,Category,Rate_Cat) %>% mutate(Layer="Cases", Label="Gender")
temp3 <- Chicago_COVID19_CasesDeathsHospitalizations_pct %>% gather("Category", "Rate_Cat", 17:22) %>% select(Date,Category,Rate_Cat) %>% mutate(Layer="Cases", Label="Race")
temp4 <- Chicago_COVID19_CasesDeathsHospitalizations_pct %>% gather("Category", "Rate_Cat", 23:31) %>% select(Date,Category,Rate_Cat) %>% mutate(Layer="Deaths", Label="Age")
temp5 <- Chicago_COVID19_CasesDeathsHospitalizations_pct %>% gather("Category", "Rate_Cat", 32:34) %>% select(Date,Category,Rate_Cat) %>% mutate(Layer="Deaths", Label="Gender")
temp6 <- Chicago_COVID19_CasesDeathsHospitalizations_pct %>% gather("Category", "Rate_Cat", 35:40) %>% select(Date,Category,Rate_Cat) %>% mutate(Layer="Deaths", Label="Race")
temp7 <- Chicago_COVID19_CasesDeathsHospitalizations_pct %>% gather("Category", "Rate_Cat", 41:49) %>% select(Date,Category,Rate_Cat) %>% mutate(Layer="Hospitalizations", Label="Age")
temp8 <- Chicago_COVID19_CasesDeathsHospitalizations_pct %>% gather("Category", "Rate_Cat", 50:52) %>% select(Date,Category,Rate_Cat) %>% mutate(Layer="Hospitalizations", Label="Gender")
temp9 <- Chicago_COVID19_CasesDeathsHospitalizations_pct %>% gather("Category", "Rate_Cat", 53:58) %>% select(Date,Category,Rate_Cat) %>% mutate(Layer="Hospitalizations", Label="Race")

# bind rows remove suffixes from category column 
Chicago_COVID19_CasesDeathsHospitalizations_pct_long <- rbind(temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9) %>% mutate(Category = str_replace(Category, "Cases_", "")) %>% mutate(Category = str_replace(Category, "Deaths_", "")) %>% mutate(Category = str_replace(Category, "Hosp_", ""))
# Chicago_COVID19_CasesDeathsHospitalizations_pct_long <- Chicago_COVID19_CasesDeathsHospitalizations_pct_long %>% mutate(WeekNo = as.numeric(format(Date,"%W"))+1, EndDate = ceiling_date(Date,unit="week")) %>% mutate(WeekNo = ifelse(format(Date,"%Y")==2021,WeekNo+53,WeekNo))
Chicago_COVID19_CasesDeathsHospitalizations_pct_long <- Chicago_COVID19_CasesDeathsHospitalizations_pct_long %>% 
  mutate(WeekNo = epiweek(Date), 
         EndDate = ceiling_date(Date,unit="week")-1,
         WeekNo = if_else(epiyear(Date)==2021,WeekNo+52,WeekNo)-1)

rm(list=ls(pattern="temp")) # remove all temporary tables

# Transform demographic data table, convert from short to long-form
DemoChars_Chicago_pct_long <- DemoChars_Chicago_pct %>% 
  gather("Category", "Rate_Ref", 4:18) %>% 
  select(Category,Rate_Ref)

# Reorder label values so that they appear in order within dashboard serial charts
DemoChars_Chicago_pct_long <- DemoChars_Chicago_pct_long %>% 
  mutate(Order=row_number())

# Combine demographics to COVID-19 time series data
Chicago_COVID19_CasesDeathsHospitalizations_pct_long <- Chicago_COVID19_CasesDeathsHospitalizations_pct_long %>% 
  drop_na(Date) %>% 
  replace(is.na(.), 0) #remove NAs in Date column, replace NAs with 0s in all other
Chicago_COVID19_CasesDeathsHospitalizations_pct_long <- Chicago_COVID19_CasesDeathsHospitalizations_pct_long %>% 
  left_join(DemoChars_Chicago_pct_long, by="Category") 
Chicago_COVID19_CasesDeathsHospitalizations_pct_long_wk <- Chicago_COVID19_CasesDeathsHospitalizations_pct_long %>% 
  arrange(Date, Order) %>%
  drop_na(Date) %>% 
  replace(is.na(.), 0) %>%
  filter(Date==EndDate)

# Table to be imported into ArcGIS online 
write.csv(Chicago_COVID19_CasesDeathsHospitalizations_pct_long_wk,"layers/Disparity_timeseries_metrics.csv")
