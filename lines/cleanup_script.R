library(sf)
library(osmextract)
library(purrr)
library(dplyr)
library(stringr)


## running for Netherlands 
## reads gpkg file (already downloaded osm.pbf)

path = "C:/Users/samia/OneDrive/Documents/GitHub/DataViz/30 day map challenge 2022/lines/"

get_lines<-function(file)
{
  a<-oe_read(
    file_path = file,
    download_directory = path, 
    quiet = FALSE
  )
  
  assign(str_remove_all(file,paste0(path,"|-latest.gpkg")), a, envir = .GlobalEnv)
}

list.files("~/GitHub/DataViz/30 day map challenge 2022/lines", full.names=TRUE) %>%
  str_subset(".latest.gpkg") %>%
  map(get_lines)


## filter bicycle tags
## criteria:
### highway = cycleway 
### highway is not in categories not meant for cycling 
### other_tags includes yes or designated for bicycle
### other_tags includes cycleway = yes and access is not private/no
### other_tags includes lcn, rcn, icn, ncn (cycle routes) and access is not private/no
### service is not private no

filter_bicycle<-function(dataset)
{
  a <- dataset %>% 
    mutate(
      bicycle = case_when(
        str_detect(other_tags,"\"bicycle\"=>\"yes\"") ~ "yes",
        str_detect(other_tags,"\"bicycle\"=>\"no\"") ~ "no",
        str_detect(other_tags,"\"bicycle\"=>\"use_sidepath\"") ~ "use_sidepath",
        str_detect(other_tags,"\"bicycle\"=>\"private\"") ~ "private",
        str_detect(other_tags,"\"bicycle\"=>\"restricted\"") ~ "restricted",
        str_detect(other_tags,"\"bicycle\"=>\"designated\"") ~ "designated",
        TRUE ~ NA_character_),
      
      network = case_when(
        str_detect(other_tags,"\"rcn\"=>\"yes\"|\"rcn_ref\"") ~ "rcn",
        str_detect(other_tags,"\"lcn\"=>\"yes\"|\"lcn_ref\"") ~ "lcn",
        str_detect(other_tags,"\"icn\"=>\"yes\"|\"icn_ref\"") ~ "icn",
        str_detect(other_tags,"\"ncn\"=>\"yes\"|\"ncn_ref\"") ~ "ncn",
        TRUE ~ NA_character_),
      
      cycleway = case_when(
        str_detect(other_tags,"\"cycleway\"=>\"no\"") ~ "no",
        str_detect(other_tags,"\"cycleway:both\"=>\"no\"") ~ "no",
        str_detect(other_tags,"\"cycleway", negate=TRUE) ~ NA_character_,
        TRUE ~ "yes"),
      
      access = case_when(
        str_detect(other_tags,"\"access\"=>\"private\"") ~ "private",
        str_detect(other_tags,"\"access\"=>\"no\"") ~ "no",
        str_detect(other_tags,"\"access", negate=TRUE) ~ NA_character_,
        TRUE ~ "yes"),
      
      service = case_when(
        str_detect(other_tags,"\"service\"=>\"private") ~ "private",
        TRUE ~ NA_character_
      )) %>%
    filter(!is.na(highway),
           !highway %in% c("abandoned", "bus_guideway", "byway", "construction", "corridor", "elevator", "fixme", "escalator", "gallop", "historic", "no", "planned", "platform", "proposed", "raceway", "steps"),
           is.na(service),
           !bicycle %in% c("no","use_sidepath","private","restricted"),
           (highway == "cycleway" & !access %in% c("private","no")) | (cycleway == "yes" & !access %in% c("private","no")) | bicycle %in% c("yes","designated") | (network %in% c("icn","rcn","ncn","lcn") & !access %in% c("private","no")))
  return(a)
}

## cut into smaller datasets and join to run function
netherlands$group<-cut(1:nrow(netherlands), 20, labels=FALSE)

netherlands_split<-netherlands %>% group_split(group)

netherlands_bike_cleaned<-netherlands_split %>% map(filter_bicycle) %>% bind_rows()

## mutate more variables for categorizing (not used in map)
netherlands_bike<-netherlands_bike_cleaned %>%
mutate(network = case_when(
  !(network %in% c("icn","lcn","rcn","ncn")) & (highway == "cycleway"|cycleway =="yes") ~ "cycleway",
  is.na(network) & highway!= "cycleway" | cycleway!="yes" ~  "other cycle route",
  TRUE ~ network),
  
    highway_type = case_when(
      str_detect(highway,"primary") ~ "primary",
      str_detect(highway,"secondary") ~ "secondary",
      str_detect(highway,"tertiary") ~ "tertiary",
      str_detect(highway,"trunk") ~ "trunk",
      TRUE ~ highway
    ))

st_write(netherlands_bike, paste0(path,"netherlands_bike.shp"), append=FALSE)
