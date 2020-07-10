###############################################################################
# NY Time style maps: drop low density census tracts
# Will Doyle
# Init: 2020-07-10
###############################################################################

library(tidyverse)
library(tigris)
library(tidycensus)
library(sf)

## Simple County Maps

counties<- counties(cb=TRUE)

names(counties)<-tolower(names(counties))

## Population of each county



## Simple Census Tract Maps



## Population of each census tract
## per: https://walker-data.com/2017/05/tidycensus-every-tract/

us <- unique(fips_codes$state_code)[1:51]



tracts <- reduce(
  map(us, function(x) {
  get_acs(geography = "tract", variables = "B01003_001", 
          state = x,geometry = TRUE)
}),
rbind)

save(tracts,file="../data/tracts.Rda")

v2010<-load_variables(year = 2010,dataset = "sf1")

##
