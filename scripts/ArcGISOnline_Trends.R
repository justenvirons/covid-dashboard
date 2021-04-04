# Author: C. Scott Smith
# Updated: 11/23/2020

# Activate libraries
library(csv)
library(dplyr)
library(tidyverse)
library(sf)
library(readxl)
library(scales)
library(gtools)
library(classInt)
library(clipr)
library(purrr)
library(sp)
library(spdep)
library(rgdal)
library(rgeos)
library(spgwr)
library(grid)
library(gridExtra)
library(data.table)

# import clipped zip code geographic file
ZCTA_select = st_read("layers/ZCTA_Select.shp")

# Analyses for hardship dashboard
# download and import data from Chicago Data Portal

Chicago_COVID19_ByZCTA <- read_csv("https://data.cityofchicago.org/api/views/yhhz-zm2v/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
Chicago_COVID19_ByZCTA <- Chicago_COVID19_ByZCTA %>% rename("ZCTA5"="ZIP Code", "WeekNo"="Week Number","StartDate"="Week Start","EndDate"="Week End","CasesWk"="Cases - Weekly","CasesCm"="Cases - Cumulative","CasesWkRt"="Case Rate - Weekly","CasesCmRt"="Case Rate - Cumulative","TestsWk"="Tests - Weekly","TestsCm"="Tests - Cumulative","TestsWkRt"="Test Rate - Weekly","TestsCmRt"="Test Rate - Cumulative","DeathsWk"="Deaths - Weekly","DeathsCm"="Deaths - Cumulative","DeathsWkRt"="Death Rate - Weekly","DeathsCmRt"="Death Rate - Cumulative", "PctPosWk"="Percent Tested Positive - Weekly", "PctPosCum"="Percent Tested Positive - Cumulative","TotPop"="Population","RowID"="Row ID","GEOM"="ZIP Code Location")
Chicago_COVID19_ByZCTA$ZCTA5 <- as.character(Chicago_COVID19_ByZCTA$ZCTA5)
Chicago_COVID19_ByZCTA$GEOM <- NULL
Chicago_COVID19_ByZCTA$StartDate <- as.Date(Chicago_COVID19_ByZCTA$StartDate, "%m/%d/%Y")+1 # reformat date text to date data type
Chicago_COVID19_ByZCTA$EndDate <- as.Date(Chicago_COVID19_ByZCTA$EndDate, "%m/%d/%Y")+1 # reformat date text to date data type
Chicago_COVID19_ByZCTA <- Chicago_COVID19_ByZCTA %>% 
  mutate(WeekNo = ifelse(format(StartDate,"%Y")==2021,
                         WeekNo+53,
                         WeekNo))

Chicago_COVID19_ByZCTA_geom <- left_join(Chicago_COVID19_ByZCTA, ZCTA_select, by = c("ZCTA5"="ZCTA5"))
Chicago_COVID19_ByZCTA_geom <- Chicago_COVID19_ByZCTA_geom %>% filter(StartDate>=as.Date("3/14/2020","%m/%d/%Y"))
Chicago_COVID19_ByZCTA_geom <- Chicago_COVID19_ByZCTA_geom[, !duplicated(colnames(Chicago_COVID19_ByZCTA_geom))]
Chicago_COVID19_ByZCTA_geom$TotPop <- NULL

# Replace NAs with 0s to allow for subsequent calculations
Chicago_COVID19_ByZCTA_geom[c("CasesWk","CasesCm","CasesWkRt","CasesCmRt","TestsWk","TestsCm","TestsWkRt","TestsCmRt")][is.na(Chicago_COVID19_ByZCTA_geom[c("CasesWk","CasesCm","CasesWkRt","CasesCmRt","TestsWk","TestsCm","TestsWkRt","TestsCmRt")])] <- 0

# Add/transform columns to create additional variables
Chicago_COVID19_ByZCTA_geom$SurvCm <- Chicago_COVID19_ByZCTA_geom$TOTPOP-Chicago_COVID19_ByZCTA_geom$DeathsCm
Chicago_COVID19_ByZCTA_geom$NotTestedCm <- Chicago_COVID19_ByZCTA_geom$TOTPOP-Chicago_COVID19_ByZCTA_geom$TestsCm
Chicago_COVID19_ByZCTA_geom$NotInfCm <- Chicago_COVID19_ByZCTA_geom$TOTPOP-Chicago_COVID19_ByZCTA_geom$CasesCm
Chicago_COVID19_ByZCTA_geom$PctPosCum <- Chicago_COVID19_ByZCTA_geom$CasesCm/Chicago_COVID19_ByZCTA_geom$TestsCm*100
Chicago_COVID19_ByZCTA_geom$PctPosWk <- Chicago_COVID19_ByZCTA_geom$CasesWk/Chicago_COVID19_ByZCTA_geom$TestsWk*100

Chicago_COVID19_ByZCTA_geom[c("SurvCm","NotTestedCm","NotInfCm","PctPosCum","PctPosWk")][is.na(Chicago_COVID19_ByZCTA_geom[c("SurvCm","NotTestedCm","NotInfCm","PctPosCum","PctPosWk")])] <- 0

# Create hardship index
# Create lag variables
Chicago_COVID19_ByZCTA_geom  <- Chicago_COVID19_ByZCTA_geom %>% group_by(ZCTA5) %>% arrange(EndDate) %>% mutate(CasesWkRtLag1wk = lag(CasesWkRt, 1, order_by=EndDate), DeathsWkRtLag1wk = lag(DeathsWkRt, 1, order_by=EndDate),PctPosWkLag1wk = lag(PctPosWk, 1, order_by=EndDate))
Chicago_COVID19_ByZCTA_geom[c("CasesWkRtLag1wk","DeathsWkRtLag1wk","PctPosWkLag1wk")][is.na(Chicago_COVID19_ByZCTA_geom[c("CasesWkRtLag1wk","DeathsWkRtLag1wk","PctPosWkLag1wk")])] <- 0

# Rescale indicator variables
attach(Chicago_COVID19_ByZCTA_geom)
Chicago_COVID19_ByZCTA_geom <- Chicago_COVID19_ByZCTA_geom %>% group_by(WeekNo) %>% mutate(PctPosWk_adj = rescale(PctPosWk, to=c(0,100)), CasesWkRt_adj = rescale(CasesWkRt, to=c(0,100)),DeathsWkRt_adj = rescale(DeathsWkRt, to=c(0,100)), PctPosWk_trd = rescale((PctPosWk-PctPosWkLag1wk), to=c(0,100)), CasesWkRt_trd = rescale((CasesWkRt-CasesWkRtLag1wk), to=c(0,100)), DeathsWkRt_trd = rescale((DeathsWkRt-DeathsWkRtLag1wk), to=c(0,100)))
detach(Chicago_COVID19_ByZCTA_geom)

# Weight and combine indicator variables
attach(Chicago_COVID19_ByZCTA_geom)
Chicago_COVID19_ByZCTA_geom$Hardship = rescale(PctPosWk_adj*0.25 + CasesWkRt_adj*0.25 + DeathsWkRt_adj*0.25 + rescale(PctPosWk_trd*0.33+CasesWkRt_trd*0.33+DeathsWkRt_trd*0.33, to=c(0,100))*0.25,to=c(0,100))
Chicago_COVID19_ByZCTA_geom[c("Hardship")][is.na(Chicago_COVID19_ByZCTA_geom[c("Hardship")])] <- 0
Chicago_COVID19_ByZCTA_geom$Hardship[Chicago_COVID19_ByZCTA_geom$Hardship == Inf] <- 0
Chicago_COVID19_ByZCTA_geom$Hardship[Chicago_COVID19_ByZCTA_geom$Hardship == -Inf] <- 0
detach(Chicago_COVID19_ByZCTA_geom)

# Create quintiles
# quintiles over entire study period
attach(Chicago_COVID19_ByZCTA_geom)
Chicago_COVID19_ByZCTA_geom$CasesWkRt_qa <- quantcut(CasesWkRt,q=seq(0,1,by=0.20),labels=FALSE)
Chicago_COVID19_ByZCTA_geom$PctPosWk_qa <- quantcut(PctPosWk,q=seq(0,1,by=0.20),labels=FALSE)
Chicago_COVID19_ByZCTA_geom$Hardship_qa <- quantcut(Hardship,q=seq(0,1,by=0.20),labels=FALSE)
Chicago_COVID19_ByZCTA_geom <- Chicago_COVID19_ByZCTA_geom %>% mutate(color=case_when(Hardship_qa == 1 ~ "#0070ff",Hardship_qa == 2 ~ "#bed2ff",Hardship_qa == 3 ~ "#ffebaf",Hardship_qa == 4 ~ "#ffaa00",Hardship_qa == 5 ~ "#ff0000"))
detach(Chicago_COVID19_ByZCTA_geom)

# abreaks <- classIntervals(DeathsWkRt, n=5, style="jenks")
Chicago_COVID19_ByZCTA_geom <- Chicago_COVID19_ByZCTA_geom %>% mutate(DeathsWkRt_qa=case_when(DeathsWkRt <= 0 ~ 1, DeathsWkRt > 0 & DeathsWkRt <= 2.6 ~ 2, DeathsWkRt > 2.6 & DeathsWkRt <= 8 ~ 3, DeathsWkRt > 8 & DeathsWkRt <= 15.8 ~ 4, DeathsWkRt > 15.8 ~ 5))

# Select variables for ZCTA_profile CSV
sumProfileZCTA <- Chicago_COVID19_ByZCTA_geom %>% filter(ZCTA5!="Unknown" & ZCTA5!="60666") %>% select(EndDate, ZCTA5, CasesCm, DeathsCm, TestsCm) %>% group_by(ZCTA5) %>% summarise(CasCm = max(CasesCm), DthCm = max(DeathsCm), TstCm = max(TestsCm), PosRt = format(round(CasCm/TstCm*100, 1))) %>% arrange(ZCTA5) 
sumProfilePkCasZCTA <-  Chicago_COVID19_ByZCTA_geom %>% filter(ZCTA5!="Unknown" & ZCTA5!="60666") %>% select(EndDate, ZCTA5, CasesWk, CasesWkRt) %>% group_by(ZCTA5) %>% slice(which.max(CasesWk)) %>% mutate(EndDate = as.character(format(EndDate, "%D"))) %>% rename(PkCasWk = WeekNo, PkCasDat = EndDate, PkCas=CasesWk, PkCasRt = CasesWkRt) %>% arrange(ZCTA5) 
sumProfilePkDthZCTA <-  Chicago_COVID19_ByZCTA_geom %>% filter(ZCTA5!="Unknown" & ZCTA5!="60666") %>% select(EndDate, ZCTA5, DeathsWk, DeathsWkRt) %>% group_by(ZCTA5) %>% slice(which.max(DeathsWk)) %>% mutate(EndDate = as.character(format(EndDate, "%D"))) %>% rename(PkDthWk = WeekNo, PkDthDat = EndDate, PkDth=DeathsWk, PkDthRt = DeathsWkRt) %>% arrange(ZCTA5) 
sumProfilePkPosZCTA <-  Chicago_COVID19_ByZCTA_geom %>% filter(ZCTA5!="Unknown" & ZCTA5!="60666") %>% select(EndDate, ZCTA5, PctPosWk) %>% group_by(ZCTA5) %>% slice(which.max(PctPosWk)) %>% mutate(EndDate = as.character(format(EndDate, "%D"))) %>% rename(PkPosWk = WeekNo, PkPosDat = EndDate, PkPosRt=PctPosWk) %>% arrange(ZCTA5)
sumProfileRegZCTA <- Chicago_COVID19_ByZCTA_geom %>% filter(ZCTA5!="Unknown" & ZCTA5!="60666") %>% select(EndDate, ZCTA5, REGION) %>% group_by(ZCTA5, REGION) %>% summarize(EndDate = max(EndDate), EndDateCh = as.character(format(max(EndDate),format="%B %d, %Y"))) %>% ungroup()

ZCTA_select <- ZCTA_select %>% arrange(ZCTA5No)
ZCTA_profile <- cbind(ZCTA_select, sumProfileZCTA,sumProfilePkCasZCTA,sumProfilePkDthZCTA,sumProfilePkPosZCTA,sumProfileRegZCTA)
ZCTA_profile <- ZCTA_profile %>% select(-contains(".")) %>% st_drop_geometry()
ZCTA_profile <- ZCTA_profile %>% mutate(CasRt_c = CasCm/TOTPOP*100000, DthRt_c = DthCm/TOTPOP*100000,PosRt_c = CasCm/TstCm*100, CasRtRnk = row_number(-CasCm/TOTPOP),DthRtRnk = row_number(-DthCm/TOTPOP),PosRtRnk = row_number(-CasCm/TstCm))

# Select variables for ZCTA_metrics CSV
ZCTA_metrics <- Chicago_COVID19_ByZCTA_geom %>% filter(ZCTA5!="Unknown" & ZCTA5!="60666") %>% select(-geometry) %>% mutate(CasesWk_c = sum(CasesWk),DeathsWk_c = sum(DeathsWk),CasesWkRt_c = CasesWk_c/sum(TOTPOP)*100000,DeathsWkRt_c = DeathsWkRt/sum(TOTPOP)*100000, PctPosWk_c = sum(CasesWk)/sum(TestsWk)*100,CasesRtDiff = CasesWkRt-CasesWkRt_c, DeathsRtDiff = DeathsWkRt - DeathsWkRt_c, PctPosDiff = PctPosWk - PctPosWk_c) %>% ungroup()
ZCTA_select$ZCTA5No <- as.integer(ZCTA_select$ZCTA5)

# Select and rename variables for ZCTA_map layer
ZCTA_map <- Chicago_COVID19_ByZCTA_geom %>% filter(ZCTA5!="Unknown" & ZCTA5!="60666") %>% select(ZCTA5, REGION, WeekNo, "SrtDate" = StartDate, EndDate, "PosWkqa" = PctPosWk_qa, "CasWkRtqa" = CasesWkRt_qa, "Hdshpqa" = Hardship_qa, "DthWkRqa" = DeathsWkRt_qa, "PosWk" = PctPosWk, "CasWkRt" = CasesWkRt, "DthWkRt" = DeathsWkRt, "Hdshp" = Hardship, "geom" = geometry)
ZCTA_map$WeekNo <- ZCTA_map$WeekNo-3
ZCTA_map$ZCTA5 <- as.character(ZCTA_map$ZCTA5)
ZCTA_map$ZCTA5No <- as.integer(ZCTA_map$ZCTA5) 
ZCTA_map <- ZCTA_map %>% gather("layer", "ntile", 6:9)
ZCTA_map <- ZCTA_map %>% mutate(label=case_when(ntile == 1 ~ "Lowest",ntile == 2 ~ "Low",ntile == 3 ~ "Moderate",ntile == 4 ~ "High",ntile == 5 ~ "Highest"))
ZCTA_map <- ZCTA_map %>% mutate(color=case_when(ntile == 1 ~ "#0070ff",ntile == 2 ~ "#bed2ff",ntile == 3 ~ "#ffebaf",ntile == 4 ~ "#ffaa00",ntile == 5 ~ "#ff0000"))
ZCTA_map <- ZCTA_map %>% mutate(layer_d=case_when(layer == "PosWkqa" ~ "Positivity Rate", layer == "Hdshpqa" ~ "Hardship Index (0-100)", layer == "CasWkRtqa" ~ "Infection Rate (per 100K)", layer == "DthWkRqa" ~ "Death Rate (per 100K)"))
ZCTA_map <- ZCTA_map %>% mutate(value=case_when(layer == "PosWkqa"  ~ PosWk, layer == "CasWkRtqa" ~ CasWkRt, layer =="DthWkRqa" ~ DthWkRt, layer=="Hdshpqa" ~ Hdshp)) %>% select(-PosWk,-CasWkRt,-DthWkRt,-Hdshp)
ZCTA_map <- ZCTA_map %>% ungroup()
ZCTA_map %>% group_by(layer_d, ntile, label, color) %>% summarise(n(), min(value), max(value))
ZCTA_map %>% group_by(layer_d, ntile, label, color) %>% summarise(n(), min(value), max(value))

# Run LISA clustering for all variables and weeks
ZCTA_map_layers <- ZCTA_map %>% group_by(layer) %>% summarise(n()) %>% select(layer)
ZCTA_map_dates <- ZCTA_map %>% group_by(EndDate) %>% summarise(n()) %>% select(EndDate)

# develop data frame template for subsequent analyses
for(layerrow in 1:1){
  for (daterow in 1:1) {
    alayer <- ZCTA_map_layers[[layerrow,"layer"]]
    adate <- ZCTA_map_dates[[daterow,"EndDate"]]
    alayername = paste("cluster",as.character(layerrow),as.character(daterow),sep = "_")
    ZCTA_map_latest <- ZCTA_map %>% filter(EndDate==adate, layer== alayer)
    ZCTA_map_latest$value[ZCTA_map_latest$value == Inf] <- 0
    ZCTA_map_latest$value[ZCTA_map_latest$value == -Inf] <- 0
    ZCTA_map_latest_sf <- ZCTA_map_latest %>% st_as_sf() # simple feature
    ZCTA_map_latest_sp <- as_Spatial(ZCTA_map_latest_sf) # spatial point dataset
    ZCTA_map_latest_nb <- poly2nb(ZCTA_map_latest_sp) # neighbors, queen
    ZCTA_map_latest_ww <- nb2listw(ZCTA_map_latest_nb, style='B') # spatial weights object
    ZCTA_map_latest_mi <- moran(ZCTA_map_latest_sp$value, ZCTA_map_latest_ww, n=length(ZCTA_map_latest_ww$neighbours), S0=Szero(ZCTA_map_latest_ww)) # Moran's I
    ZCTA_map_latest_lm <- localmoran(ZCTA_map_latest_sp$value, ZCTA_map_latest_ww) # LISA scores
    ZCTA_map_latest_lm <- cbind(ZCTA_map_latest_sf, ZCTA_map_latest_lm)
    ZCTA_map_latest_lm <- ZCTA_map_latest_lm[, !duplicated(colnames(ZCTA_map_latest_lm))]
    ZCTA_map_latest_lm <- ZCTA_map_latest_lm %>% select(-contains(".1")) %>% rename(Pr.Z = Pr.z...0.)
    ZCTA_map_latest_lm <- ZCTA_map_latest_lm %>% mutate(value_s = scale(value, scale=FALSE), Ii_s = scale(Ii, scale=FALSE), Gi_bin = ifelse(value_s >0 & Ii_s>0, 1, ifelse(value_s < 0 & Ii_s>0 ,2,ifelse(value_s < 0 & Ii_s<0 ,3,ifelse(value_s > 0 & Ii_s < 0 ,4,0)))))
  }
}

# ZCTA_map_latest_lm_bind %>% group_by(layer) %>% summarize(n())
# ZCTA_map %>% group_by(layer) %>% summarize(n())

signif <- 0.1 # significance threshold for LISA values
ZCTA_map_latest_lm_bind <- ZCTA_map_latest_lm[0,]

for(layerrow in 1:nrow(ZCTA_map_layers)){
  for (daterow in 1:nrow(ZCTA_map_dates)) {
    alayer <- ZCTA_map_layers[[layerrow,"layer"]]
    adate <- ZCTA_map_dates[[daterow,"EndDate"]]
    
    alayername = paste("cluster",as.character(layerrow),as.character(daterow),sep = "_")
    
    ZCTA_map_latest <- ZCTA_map %>% filter(EndDate==adate, layer== alayer)
    ZCTA_map_latest$value[ZCTA_map_latest$value == Inf] <- 0
    ZCTA_map_latest$value[ZCTA_map_latest$value == -Inf] <- 0
    ZCTA_map_latest_sf <- ZCTA_map_latest %>% st_as_sf() # simple feature
    ZCTA_map_latest_sp <- as_Spatial(ZCTA_map_latest_sf) # spatial point dataset
    ZCTA_map_latest_nb <- poly2nb(ZCTA_map_latest_sp) # neighbors, queen
    ZCTA_map_latest_ww <- nb2listw(ZCTA_map_latest_nb, style='B') # spatial weights object
    ZCTA_map_latest_mi <- moran(ZCTA_map_latest_sp$value, ZCTA_map_latest_ww, n=length(ZCTA_map_latest_ww$neighbours), S0=Szero(ZCTA_map_latest_ww)) # Moran's I
    ZCTA_map_latest_lm <- localmoran(ZCTA_map_latest_sp$value, ZCTA_map_latest_ww) # LISA scores
    ZCTA_map_latest_lm <- cbind(ZCTA_map_latest_sf, ZCTA_map_latest_lm)
    ZCTA_map_latest_lm <- ZCTA_map_latest_lm[, !duplicated(colnames(ZCTA_map_latest_lm))]
    ZCTA_map_latest_lm <- ZCTA_map_latest_lm %>% select(-contains(".1")) %>% rename(Pr.Z = Pr.z...0.)
    ZCTA_map_latest_lm <- ZCTA_map_latest_lm %>% mutate(value_s = scale(value, scale=FALSE), Ii_s = scale(Ii, scale=FALSE), Gi_bin = ifelse(value_s >0 & Ii_s>0, 1, ifelse(value_s < 0 & Ii_s>0 ,2,ifelse(value_s < 0 & Ii_s<0 ,3,ifelse(value_s > 0 & Ii_s < 0 ,4,0))))) %>% mutate(Gi_bin = ifelse(Pr.Z > signif, 0, Gi_bin))
    alayer <- assign(alayername,ZCTA_map_latest_lm) # assign custom name to layer- and date-specific dataframe
    ZCTA_map_latest_lm_bind <- ZCTA_map_latest_lm_bind %>% rbind(alayer)
  }
}

ZCTA_map_latest_lm_bind[is.na(ZCTA_map_latest_lm_bind)] <- 0

rm(list=ls(pattern="cluster_"))
rm(list=ls(pattern="sumProfile"))
rm(alayer,Chicago_COVID19_ByZCTA, Chicago_COVID19_ByZCTA_geom, ZCTA_map,ZCTA_map_dates,ZCTA_map_latest,ZCTA_map_latest_lm,ZCTA_map_latest_mi,ZCTA_map_latest_nb,ZCTA_map_latest_sf,ZCTA_map_latest_sp,ZCTA_map_latest_ww,ZCTA_map_layers,ZCTA_select)

# write data frames to layers folder
st_write(ZCTA_map_latest_lm_bind, "layers/Trends_map.shp", append=FALSE)
write.csv(ZCTA_profile,"layers/ZCTA_profile.csv")
write.csv(ZCTA_metrics,"layers/ZCTA_metrics.csv")
