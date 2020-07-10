###############################################################################
# NY Time style maps: drop low density census tracts
# Inspired by: https://twitter.com/abhishekn/status/1278545394993991685
# Will Doyle
# Init: 2020-07-10
###############################################################################

library(tidyverse)
library(tigris)
library(tidycensus)
library(sf)
library(here)

## Simple County Maps

counties<- counties(cb=TRUE)

names(counties)<-tolower(names(counties))

## Simple Census Tract Maps

## List of state fips
us <- unique(fips_codes$state_code)[1:51]

## Get shapefiles for all census tracts, includes land area
## per: https://walker-data.com/2017/05/tidycensus-every-tract/

if(!file.exists(here("data", "tract_maps.Rda"))){

tract_maps<-reduce(
  map(us,function(x){
  tracts(state=x,cb=TRUE,class="sf")    
  }),
  rbind
  )
  
save(tract_maps,file=here("data", "tract_maps.Rda"))

} else {
  load(here("data","tract_maps.Rda"))
}
names(tract_maps)<-tolower(names(tract_maps))

## Population of each census tract
## per: https://walker-data.com/2017/05/tidycensus-every-tract/


if(!file.exists(here("data","tract_maps.Rda"))) {

tracts <-
  map_df(us, function(x) {
  get_acs(geography = "tract", variables = "B01003_001", 
          state = x)
  })

save(tracts,file=here("data","tracts.Rda"))

} else {
 load(here("data","tracts.Rda") )
}

names(tracts)<-str_to_lower(names(tracts))

tracts<-tracts%>%
  select(geoid,estimate)%>%
  rename(population=estimate)

## Get data for counties

tract_maps<-left_join(tract_maps,tracts,by="geoid")

tract_maps<-tract_maps%>%
  mutate(sq_mile=aland/2.59e06)%>%
  mutate(sq_mile=ifelse(sq_mile==0,NA,sq_mile))%>%
  mutate(pop_sq=population/sq_mile)


tract_map_ri<-tract_maps%>%filter(statefp=="47")

#%>%filter(pop_sq<10)

plot(tract_map_ri["pop_sq"])

pal_fun <- colorNumeric("YlOrRd",domain=NULL)


leaflet(tract_map_ri) %>%
  addPolygons(
    stroke = FALSE, # remove polygon borders
    fillColor = ~pal_fun(population), # set fill color with function from above and value
    fillOpacity = 0.8, 
    smoothFactor = 0.5)

## Spread, so that each level of education gets its own column
educ_vars<-educ_vars%>%
  select(GEOID,NAME,variable,estimate)%>%
  spread(key=variable,value = estimate)

## rename to be all lower case 
names(educ_vars)<-str_to_lower(names(educ_vars))

## Calculate prop with at least bachelor's for every county
educ_vars<-educ_vars%>%xxx
  mutate(prop_bach=(b15003_022+
                      b15003_023+
                      b15003_024+
                      b15003_025)/b15003_001)

## simplify to just proportion
educ_vars<-educ_vars%>%
  select(geoid,name,prop_bach,geometry)


gg1<-ggplot(tract_maps,aes(fill=pop_sq))
gg1<-gg1+geom_sf(color=NA)
#gg1<-gg1+coord_sf(crs=26911)
gg1<- gg1+ scale_fill_viridis_c(option = "viridis") 

gg1