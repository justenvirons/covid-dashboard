## ---------------------------
##
## Script name: ArcGISOnline_Vaccinations.R
# Purpose of script: import and process COVID-19 vaccine data from City of
# Chicago data portal for DePaul COVID-19 dashboard
## Author: C. Scott Smith, PhD AICP
## Date Created: 2021-01-15
## Date Last Updated: 2021-03-09
## Email: c.scott.smith@depaul.edu
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

# activate installed packages
library(dplyr) # https://cran.r-project.org/web/packages/dplyr/dplyr.pdf
library(tidyverse) # https://cran.r-project.org/web/packages/tidyverse/tidyverse.pdf
library(sf) # https://cran.r-project.org/web/packages/sf/sf.pdf
library(lubridate)
library(zoo)


# Download census geographies
ZCTA_select = st_read("layers/ZCTA_Select.shp")

# Download vacciations by zip code data for Chicago
Chicago_COVID19Vaccinations_ByZCTA <- read_csv(file="https://data.cityofchicago.org/api/views/553k-3xzc/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
Chicago_COVID19Vaccinations_ByZCTA <- Chicago_COVID19Vaccinations_ByZCTA %>% rename("zcta"="Zip Code", "date"="Date", "doses_daily"="Total Doses - Daily", "doses_cum"="Total Doses - Cumulative", "fdose_daily"="1st Dose - Daily", "fdose_cum"="1st Dose - Cumulative", "fdose_pctpop"="1st Dose - Percent Population", "com_daily"="Vaccine Series Completed - Daily", "com_cum"="Vaccine Series Completed - Cumulative", "com_pctpop"="Vaccine Series Completed  - Percent Population", "pop"="Population", "geometry"="ZIP Code Location")
Chicago_COVID19Vaccinations_ByZCTA$date <- as.Date(Chicago_COVID19Vaccinations_ByZCTA$date, "%m/%d/%Y") # reformat date text to date data type
Chicago_COVID19Vaccinations_ByZCTA <- Chicago_COVID19Vaccinations_ByZCTA %>% 
  mutate(WeekNo = epiweek(date),
         WeekNo = if_else(epiyear(date)==2021,WeekNo+52,WeekNo)-1)

Chicago_COVID19Vaccinations_ByZCTA$date <- Chicago_COVID19Vaccinations_ByZCTA$date + 1 # add one day to date for ArcGIS online
Chicago_COVID19Vaccinations_ByZCTA$StartDate <- floor_date(Chicago_COVID19Vaccinations_ByZCTA$date,unit="week")+1
Chicago_COVID19Vaccinations_ByZCTA$EndDate <- ceiling_date(Chicago_COVID19Vaccinations_ByZCTA$date,unit="week")+1

# Create summaries for welcome page daily time series
Chicago_COVID19Vaccinations_ByZCTA_Sum <- Chicago_COVID19Vaccinations_ByZCTA %>% filter(pop>0) %>%group_by(date) %>% arrange(date) %>% summarise(doses = sum(doses_daily), dosescm = sum(doses_cum), fdoses = sum(fdose_daily),fdosescm = sum(fdose_cum), sdoses = sum(com_daily), sdosescm = sum(com_cum),pop=max(pop))
Chicago_COVID19Vaccinations_ByZCTA_Sum[is.na(Chicago_COVID19Vaccinations_ByZCTA_Sum)] <- 0

Chicago_COVID19Vaccinations_ByZCTA_Sum <- Chicago_COVID19Vaccinations_ByZCTA_Sum %>% mutate(doses7d = rollmean(x=doses, k=7, fill=0, align="right"), dosert = dosescm/pop*100, fdoses7d = rollmean(x=fdoses, k=7, fill=0, align="right"), fdosert = fdosescm/pop*100, sdoses7d = rollmean(x=sdoses, k=7, fill=0, align="right"), sdosert = sdosescm/pop*100)

# Create latest vaccinations shapefile for Welcome dashboard
Chicago_COVID19Vaccinations_ByZCTA <- Chicago_COVID19Vaccinations_ByZCTA %>% mutate(zcta_char = as.character(zcta))
Chicago_COVID19Vaccinations_ByZCTA_GEOM <- Chicago_COVID19Vaccinations_ByZCTA %>% select(-geometry) %>% left_join(ZCTA_select, by=c("zcta_char"="ZCTA5"))
# Chicago_COVID19Vaccinations_ByZCTA_GEOM_latest <- Chicago_COVID19Vaccinations_ByZCTA_GEOM %>% filter(date==max(date)) %>% drop_na(ZCTA5No) %>% mutate(geometry= st_centroid(geometry))
Chicago_COVID19Vaccinations_ByZCTA_GEOM_latest <- Chicago_COVID19Vaccinations_ByZCTA_GEOM %>% filter(date==max(date)) %>% drop_na(ZCTA5No)

# Export summary and latest vaccinations data to CSV and shapefile
st_write(Chicago_COVID19Vaccinations_ByZCTA_GEOM_latest,"layers/Chicago_COVID19Vaccinations_ByZCTA_GEOM_latest.shp", append=FALSE)
write.csv(Chicago_COVID19Vaccinations_ByZCTA_Sum,"layers/Vaccinations_Summary.csv")

adate <- as.character(Sys.Date())

Chicago_COVID19TestingLocations <- read_csv(file="https://data.cityofchicago.org/api/views/thdn-3grx/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
Chicago_COVID19VaccinationLocations <- read_csv(file="https://data.cityofchicago.org/api/views/6q3z-9maq/rows.csv?accessType=DOWNLOAD")
write.csv(Chicago_COVID19VaccinationLocations, paste0("archive/Chicago_COVID19VaccinationLocations_",adate,".csv"))
write.csv(Chicago_COVID19TestingLocations, paste0("archive/Chicago_COVID19TestingLocations_",adate,".csv"))
