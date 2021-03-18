library(jsonlite)
library(dplyr)
library(readxl)
library(tidyverse)
library(sf)
library(tigris)
library("ggmap")

# Download and format testing site information from IDPH
IDPH_covidtestingsites <- fromJSON("https://dph.illinois.gov/sitefiles/COVIDTestingSites.json",flatten=TRUE)
IDPH_covidtestingsites <- IDPH_covidtestingsites$features
IDPH_covidtestingsites[,1:2]<-NULL # remove unnecessary fields
names(IDPH_covidtestingsites) <- gsub("geometry.", "", names(IDPH_covidtestingsites))
names(IDPH_covidtestingsites) <- gsub("properties.", "", names(IDPH_covidtestingsites))
IDPH_covidtestingsites_coords <- IDPH_covidtestingsites[,1[1]] %>% unlist(use.names = FALSE)
IDPH_covidtestingsites_coords <- as.data.frame(IDPH_covidtestingsites_coords)
IDPH_covidtestingsites$coordinates <- NULL
IDPH_covidtestingsites$longitude <- IDPH_covidtestingsites_coords[c(TRUE,FALSE),]
IDPH_covidtestingsites$latitude <- IDPH_covidtestingsites_coords[c(FALSE,TRUE),]

rm(IDPH_covidtestingsites_coords)

# Download and format testing site information from City of Chicago
CDPH_covidtestingsites <- fromJSON("https://data.cityofchicago.org/api/geospatial/thdn-3grx?method=export&format=GeoJSON")
CDPH_covidtestingsites <- CDPH_covidtestingsites$features
CDPH_covidtestingsites <- CDPH_covidtestingsites %>% filter(CDPH_covidtestingsites$geometry$coordinates != "NULL")
CDPH_covidtestingsites_properties <- CDPH_covidtestingsites$properties
CDPH_covidtestingsites_geometry <- CDPH_covidtestingsites$geometry
names(CDPH_covidtestingsites_geometry) <- gsub("geometry.", "", names(CDPH_covidtestingsites_geometry))
names(CDPH_covidtestingsites_properties) <- gsub("properties.", "", names(CDPH_covidtestingsites_properties))
CDPH_covidtestingsites_coordinates <- CDPH_covidtestingsites_geometry$coordinates
CDPH_covidtestingsites_coordinates <- CDPH_covidtestingsites_coordinates %>% unlist(use.names = FALSE)
CDPH_covidtestingsites_coordinates <- as.data.frame(CDPH_covidtestingsites_coordinates)
CDPH_covidtestingsites_properties$longitude <- CDPH_covidtestingsites_coordinates[c(TRUE,FALSE),]
CDPH_covidtestingsites_properties$latitude <- CDPH_covidtestingsites_coordinates[c(FALSE,TRUE),]
CDPH_covidtestingsites <- CDPH_covidtestingsites_properties %>% select(address, name=facility,longitude,latitude,website=web_site,phone)
rm(CDPH_covidtestingsites_coordinates,CDPH_covidtestingsites_geometry,CDPH_covidtestingsites_properties)

# Geocode community-based testing sites
CDPH_popupsites <- read_excel("C:/Users/scott/OneDrive - DePaul University/PROJECTS/COVID/Dashboard/Layers/COVID-19_TestingFacilities_ArcGISOnline.xlsx", sheet = "ComtyBasedTestingSites_CDPH")
CDPH_popupsites <- CDPH_popupsites %>% filter(Status=="Active")
register_google(key="AIzaSyCrN9Rd0lIz5I55yPdrDtGY4943dyKGm2s")
getOption("ggmap")
addresses = CDPH_popupsites$address
length(addresses) # check number of addresses
locations <- geocode(addresses, key="AIzaSyCrN9Rd0lIz5I55yPdrDtGY4943dyKGm2s")
CDPH_covidtestingpopupsites <- cbind(CDPH_popupsites,locations)
rm(locations,addresses,CDPH_popupsites)
CDPH_covidtestingpopupsites$longitude <- CDPH_covidtestingpopupsites$lon
CDPH_covidtestingpopupsites$latitude <- CDPH_covidtestingpopupsites$lat
CDPH_covidtestingpopupsites$lon <- NULL
CDPH_covidtestingpopupsites$lat <- NULL

IL_Places_cb <- places(state = "IL", cb=TRUE, year=2018, class="sf")
IL_Places_Chicago_cb <- IL_Places_cb %>% filter(NAME=="Chicago")
IL_Places_Chicago_cb <- st_transform(IL_Places_Chicago_cb, crs = 26916)

# convert data frames to simple features (sf)
CDPH_covidtestingsites_sf <- st_as_sf(x = CDPH_covidtestingsites, coords = c("longitude", "latitude"), crs = 4326,remove = FALSE)
CDPH_covidtestingsites_sf <- st_transform(CDPH_covidtestingsites_sf, crs = 26916)
IDPH_covidtestingsites_sf <- st_as_sf(x = IDPH_covidtestingsites, coords = c("longitude", "latitude"), crs = 4326,remove = FALSE)
IDPH_covidtestingsites_sf <- st_transform(IDPH_covidtestingsites_sf, crs = 26916)
IDPH_covidtestingsites_sf <- IDPH_covidtestingsites_sf %>% filter(city=="Chicago")
CDPH_covidtestingpopupsites_sf <- st_as_sf(x = CDPH_covidtestingpopupsites, coords = c("longitude", "latitude"), crs = 4326,remove = FALSE)
CDPH_covidtestingpopupsites_sf <- st_transform(CDPH_covidtestingpopupsites_sf, crs = 26916)

# download hospitals, FQHCs
FQHCs <- st_read("https://opendata.arcgis.com/datasets/09a11bbc96c04f16bcb196dd2c521b71_0.geojson") %>% filter(ARC_State=="IL")
Hospitals <- st_read("https://opendata.arcgis.com/datasets/53b8031b906e43c4a4dbcf2250022ca0_0.geojson") %>% filter(State_1=="IL")
Hospitals_Chicago <- fromJSON("https://api.chicagohealthatlas.org/api/v1/hospitals")

# write to shapefiles
st_write(CDPH_covidtestingsites_sf, "Maps/CDPH_covidtestingsites_sf.shp", append=FALSE)
st_write(IDPH_covidtestingsites_sf, "Maps/IDPH_covidtestingsites_sf.shp", append=FALSE)
st_write(CDPH_covidtestingpopupsites_sf, "Maps/CDPH_covidtestingpopupsites_sf.shp", append=FALSE)
st_write(IL_Places_Chicago_cb, "Maps/ChicagoBoundary_sf.shp", append=FALSE)
