## ---------------------------
##
## Script name: 
## Purpose of script:
## Author: C. Scott Smith, PhD AICP
## Date Created: 2020-11-15
## Date Last Updated: 2021-03-09
## Email: c.scott.smith@depaul.edu
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

library(csv)
library(dplyr)
library(tidyverse)
library(clipr)
library(sf)
library(scales)
library(zoo)
library(gtools)
library(zip)

# Download latest cases, deaths US data from WHO
# https://covid19.who.int/WHO-COVID-19-global-data.csv
# create nationwide table
covidCasesDataByCountry_WHO_raw <- read.csv(file = "https://covid19.who.int/WHO-COVID-19-global-data.csv")
# covidCasesDataByCountry_WHO_raw_rename <- covidCasesDataByCountry_WHO_raw %>% rename_at(1,~"Date") %>% rename(Deaths=New_deaths, Cases=New_cases, Deathscm=Cumulative_deaths, Casescm=Cumulative_cases, Geography=Country_code) %>% drop_na(Date)
covidCasesDataByCountry_WHO_raw_rename <- covidCasesDataByCountry_WHO_raw %>% rename_at(1,~"Date") %>% rename(Deaths=New_deaths, Cases=New_cases, Deathscm=Cumulative_deaths, Casescm=Cumulative_cases, Geography=Country_code)
covidCasesDataByCountry_WHO_raw_rename$Date <- as.Date(covidCasesDataByCountry_WHO_raw_rename$Date, "%Y-%m-%d") # reformat date text to date data type
covidCasesDataByCountry_WHO_US <- covidCasesDataByCountry_WHO_raw_rename %>% filter(Geography=="US") %>% arrange(Date) %>%  select(Date, Geography, Cases, Deaths, Casescm, Deathscm) %>% mutate(Cases7d = rollmean(x=Cases, k=7, fill=0, align="right"),Deaths7d = rollmean(x=Deaths, k=7, fill=0, align="right"))
covidCasesDataByCountry_WHO_US <- covidCasesDataByCountry_WHO_US %>% gather("layer","value",3:8)

# Download latest cases, deaths US data from Johns Hopkins University (JHU)
# https://coronavirus.jhu.edu/map.html
# create statewide tables
covidCasesDataByCounty_JH_raw <- read.csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
covidDeathsDataByCounty_JH_raw <- read.csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

# Reformat, subset JH case data; rename select columns; 
anumcolumns <- ncol(covidCasesDataByCounty_JH_raw)-1
covidCasesDataByCounty_JH_long <- covidCasesDataByCounty_JH_raw %>% gather(Date, Cases, 12:anumcolumns) # reformat data from short to long form
covidCasesDataByCounty_JH_long$Date <- sub("X", "",covidCasesDataByCounty_JH_long$Date) # remove "X"s from date text column 
covidCasesDataByCounty_JH_long$Date <- as.Date(covidCasesDataByCounty_JH_long$Date, "%m.%d.%y") # reformat date text to date data type
covidCasesDataByCounty_JH_long_IL <- covidCasesDataByCounty_JH_long %>% filter(Province_State=="Illinois", FIPS != 80017, FIPS != 90017) %>% select(Date, Geography=Province_State, Cases) %>% group_by(Date, Geography) %>% summarize(Casescm=sum(Cases)) %>% ungroup()
covidCasesDataByCounty_JH_long_IL <- covidCasesDataByCounty_JH_long_IL %>% arrange(Date) %>% mutate(Cases = Casescm - lag(Casescm, 1, order_by = Date), Cases=replace_na(Cases,0), Cases7d = rollmean(x=Cases, k=7, fill=0, align="right"))
covidCasesDataByCounty_JH_long_IL <- covidCasesDataByCounty_JH_long_IL %>% gather("layer","value",Casescm:Cases7d)

anumcolumns <- ncol(covidDeathsDataByCounty_JH_raw)-1
covidDeathsDataByCounty_JH_long <- covidDeathsDataByCounty_JH_raw %>% gather(Date, Deaths, 12:anumcolumns) # reformat data from short to long form
covidDeathsDataByCounty_JH_long$Date <- sub("X", "",covidDeathsDataByCounty_JH_long$Date) # remove "X"s from date text column 
covidDeathsDataByCounty_JH_long$Date <- as.Date(covidDeathsDataByCounty_JH_long$Date, "%m.%d.%y") # reformat date text to date data type
covidDeathsDataByCounty_JH_long_IL <- covidDeathsDataByCounty_JH_long %>% filter(Province_State=="Illinois", FIPS != 80017, FIPS != 90017) %>% select(Date, Geography=Province_State, Deaths) %>% group_by(Date, Geography) %>% summarize(Deathscm=sum(Deaths)) %>% ungroup()
covidDeathsDataByCounty_JH_long_IL <- covidDeathsDataByCounty_JH_long_IL %>% arrange(Date) %>% mutate(Deaths = Deathscm - lag(Deathscm, 1, order_by = Date), Deaths=replace_na(Deaths,0), Deaths7d = rollmean(x=Deaths, k=7, fill=0, align="right"))
covidDeathsDataByCounty_JH_long_IL <- covidDeathsDataByCounty_JH_long_IL %>% gather("layer","value",Deathscm:Deaths7d)

# create Cook county tables
# Reformat, subset JH case data; rename select columns; 
covidCasesDataByCounty_JH_long_Cook <- covidCasesDataByCounty_JH_long %>% filter(Province_State=="Illinois", Admin2=="Cook") %>% select(Date, State=Province_State, Geography=Admin2, Cases)
covidCasesDataByCounty_JH_long_Cook <- covidCasesDataByCounty_JH_long_Cook %>% filter(State=="Illinois" & Geography=="Cook") %>% select(Date, Geography, Cases) %>% group_by(Date, Geography) %>% summarize(Casescm=sum(Cases)) %>% ungroup()
covidCasesDataByCounty_JH_long_Cook <- covidCasesDataByCounty_JH_long_Cook %>% arrange(Date) %>% mutate(Cases = Casescm - lag(Casescm, 1, order_by = Date), Cases=replace_na(Cases,0), Cases7d = rollmean(x=Cases, k=7, fill=0, align="right"))
covidCasesDataByCounty_JH_long_Cook <- covidCasesDataByCounty_JH_long_Cook %>% gather("layer","value",Casescm:Cases7d)

covidDeathsDataByCounty_JH_long_Cook <- covidDeathsDataByCounty_JH_long %>% filter(Province_State=="Illinois", Admin2=="Cook") %>% select(Date, State=Province_State, Geography=Admin2, Deaths)
covidDeathsDataByCounty_JH_long_Cook <- covidDeathsDataByCounty_JH_long_Cook %>% filter(State=="Illinois" & Geography=="Cook") %>% select(Date, Geography, Deaths) %>% group_by(Date, Geography) %>% summarize(Deathscm=sum(Deaths)) %>% ungroup()
covidDeathsDataByCounty_JH_long_Cook <- covidDeathsDataByCounty_JH_long_Cook %>% arrange(Date) %>% mutate(Deaths = Deathscm - lag(Deathscm, 1, order_by = Date), Deaths=replace_na(Deaths,0), Deaths7d = rollmean(x=Deaths, k=7, fill=0, align="right"))
covidDeathsDataByCounty_JH_long_Cook <- covidDeathsDataByCounty_JH_long_Cook %>% gather("layer","value",Deathscm:Deaths7d)

# Download and import data from Chicago Data Portal
# create citywide table
Chicago_COVID19_CasesDeathsHospitalizations <- read_csv("https://data.cityofchicago.org/api/views/naz8-j4nc/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
Chicago_COVID19_CasesDeathsHospitalizations <- Chicago_COVID19_CasesDeathsHospitalizations %>% rename("Cases"="Cases - Total","Deaths"="Deaths - Total")
Chicago_COVID19_CasesDeathsHospitalizations$Date <- as.Date(Chicago_COVID19_CasesDeathsHospitalizations$Date, "%m/%d/%Y") # reformat date text to date data type
Chicago_COVID19_Summary <- Chicago_COVID19_CasesDeathsHospitalizations %>% drop_na(Cases)
Chicago_COVID19_Summary <- Chicago_COVID19_Summary %>% mutate_if(is.numeric,list(~replace_na(.,0))) %>% ungroup()
Chicago_COVID19_Summary <- Chicago_COVID19_Summary %>% arrange(Date) %>% select(Date, Cases, Deaths) %>% mutate(Cases7d = rollmean(x=Cases, k=7, fill=0, align="right"), Deaths7d = rollmean(x=Deaths, k=7, fill=0, align="right"), Casescm = cumsum(Cases), Deathscm = cumsum(Deaths), Geography="Chicago")
Chicago_COVID19_Summary <- Chicago_COVID19_Summary %>% gather("layer","value",Cases:Deathscm)
Welcome_metrics <- Welcome_metrics %>% mutate(Date = Date +1)

# rbind tables together
Welcome_metrics <- Chicago_COVID19_Summary %>% rbind(covidDeathsDataByCounty_JH_long_Cook,covidCasesDataByCounty_JH_long_Cook,covidDeathsDataByCounty_JH_long_IL,covidCasesDataByCounty_JH_long_IL,covidCasesDataByCountry_WHO_US) %>% drop_na(Date) %>% mutate(value=if_else(value<0,0,value)) %>% filter(Date>="2020-03-14")
Welcome_metrics_spread <- Welcome_metrics %>% spread(layer,value)

# add one day to date because of strange ArcGIS Online bug
write.csv(Welcome_metrics,"C:/Users/scott/OneDrive - DePaul University/PROJECTS/COVID/Dashboard/Layers/GeoSummary_metrics.csv")
write.csv(Welcome_metrics_spread,"C:/Users/scott/OneDrive - DePaul University/PROJECTS/COVID/Dashboard/Layers/GeoSummary_metrics_spread.csv")

ZCTA_select = st_read("C:/Users/scott/OneDrive - DePaul University/PROJECTS/COVID/Dashboard/Layers/ZCTA_Select.shp")

Chicago_COVID19_ByZCTA <- read_csv("https://data.cityofchicago.org/api/views/yhhz-zm2v/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
Chicago_COVID19_ByZCTA <- Chicago_COVID19_ByZCTA %>% rename("ZCTA5"="ZIP Code", "WeekNo"="Week Number","StartDate"="Week Start","EndDate"="Week End","CasesWk"="Cases - Weekly","CasesCm"="Cases - Cumulative","CasesWkRt"="Case Rate - Weekly","CasesCmRt"="Case Rate - Cumulative","TestsWk"="Tests - Weekly","TestsCm"="Tests - Cumulative","TestsWkRt"="Test Rate - Weekly","TestsCmRt"="Test Rate - Cumulative","DeathsWk"="Deaths - Weekly","DeathsCm"="Deaths - Cumulative","DeathsWkRt"="Death Rate - Weekly","DeathsCmRt"="Death Rate - Cumulative", "PctPosWk"="Percent Tested Positive - Weekly", "PctPosCum"="Percent Tested Positive - Cumulative","TotPop"="Population","RowID"="Row ID","GEOM"="ZIP Code Location") %>% mutate(PctPosCum = CasesCm/TestsCm*100, PctPosWk = CasesWk/TestsWk*100)
Chicago_COVID19_ByZCTA$ZCTA5 <- as.character(Chicago_COVID19_ByZCTA$ZCTA5)
Chicago_COVID19_ByZCTA$GEOM <- NULL
Chicago_COVID19_ByZCTA$StartDate <- as.Date(Chicago_COVID19_ByZCTA$StartDate, "%m/%d/%Y")+1 # reformat date text to date data type
Chicago_COVID19_ByZCTA$EndDate <- as.Date(Chicago_COVID19_ByZCTA$EndDate, "%m/%d/%Y")+1 # reformat date text to date data type

Chicago_COVID19_ByZCTA_geom <- left_join(Chicago_COVID19_ByZCTA, ZCTA_select, by = c("ZCTA5"="ZCTA5"))
Chicago_COVID19_ByZCTA_geom <- Chicago_COVID19_ByZCTA_geom %>% filter(StartDate>=as.Date("3/15/2020","%m/%d/%Y"))
Chicago_COVID19_ByZCTA_geom <- Chicago_COVID19_ByZCTA_geom[, !duplicated(colnames(Chicago_COVID19_ByZCTA_geom))]
Chicago_COVID19_ByZCTA_geom$TotPop <- NULL

Chicago_COVID19_ByZCTA_geom_latest <- Chicago_COVID19_ByZCTA_geom %>% filter(EndDate==max(EndDate))

# data.frame(colnames(Chicago_COVID19_ByZCTA_geom))
fx_ntile <- function(x) quantcut(x,q=seq(0,1,by=0.2),labels=FALSE)
fx_pfxnt <- function(x) paste0('nt',x) # prefix "pct" added to column names with percent values

Chicago_COVID19_ByZCTA_geom_ntile <- Chicago_COVID19_ByZCTA_geom %>% filter(ZCTA5 != "Unknown" | ZCTA5 == "60666") %>% mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% mutate_at(vars(5:14,16,18), fx_ntile) %>% rename_at(vars(5:18), fx_pfxnt)
Chicago_COVID19_ByZCTA_geom_ntile_gather <- Chicago_COVID19_ByZCTA_geom_ntile %>% gather("ntLayer","ntile",5:18) %>% select(ntLayer, ntile)
Chicago_COVID19_ByZCTA_geom <- Chicago_COVID19_ByZCTA_geom %>% filter(ZCTA5 != "Unknown" | ZCTA5 == "60666") %>% mutate(geometry=st_centroid(geometry)) %>% gather("Layer","Value",5:18) %>% mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% cbind(Chicago_COVID19_ByZCTA_geom_ntile_gather) %>% select(-RowID,-TOTPOP)
Chicago_COVID19_ByZCTA_geom$WeekNo <- Chicago_COVID19_ByZCTA_geom$WeekNo-3

# Used for disparity maps (death and case counts all weeks) 
st_write(Chicago_COVID19_ByZCTA_geom, "layers/Disparity_counts.shp", append=FALSE)
# zip("layers/Disparity_counts.zip",files=c("layers/Disparity_counts.shp","layers/Disparity_counts.dbf","layers/Disparity_counts.shx","layers/Disparity_counts.prj"))
shell.exec(file.path(paste0(getwd(), "/zipfiles.bat")))

# Used for welcome maps (latest death and case counts) 
st_write(Chicago_COVID19_ByZCTA_geom_latest, "layers/ZCTA_map_latestdate.shp", append=FALSE)

rm(list=ls(pattern="_JH"))
rm(list=ls(pattern="_WHO"))
rm(list=ls(pattern="_Cases"))
rm(list=ls(pattern="_Summary"))
rm(list=ls(pattern="_ntile"))
rm(Chicago_COVID19_ByZCTA,ZCTA_select)
