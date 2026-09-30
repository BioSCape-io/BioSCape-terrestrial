## set working directory
setwd("/Users/awcardos/Library/CloudStorage/GoogleDrive-anabellecardoso@gmail.com/Shared drives/BioSCape_Admin/GIS files/Rscripts_anabelle")

## directory whjere all GIS files are found
gis.dir="/Users/awcardos/Library/CloudStorage/GoogleDrive-anabellecardoso@gmail.com/Shared drives/BioSCape_Admin/GIS files"

## packages 
library(terra)
library(sf)
library(tidyverse)
library(rgeos)
library(dplyr)
library(units)


#### PART 1: Pulling out elevation and cloud values for the flight boxes ####


## download cloud and elevation data
options(timeout=1000)

download.file("http://data.earthenv.org/cloud/MODCF_monthlymean_10.tif",
              destfile="webdata/cloud_10.tif") #october
download.file("http://data.earthenv.org/cloud/MODCF_monthlymean_11.tif",
              destfile="webdata/cloud_11.tif") #november
download.file("https://data.earthenv.org/topography/elevation_1KMma_SRTM.tif",
              destfile="webdata/earthenv_maxelev_1km.tif") #1km elevation from SRTM

cloud10=rast("webdata/cloud_10.tif")
cloud11=rast("webdata/cloud_11.tif")
elev=rast("webdata/earthenv_maxelev_1km.tif")

## read in gpkg of flight boxes 
boxdir="/Users/awcardos/Library/CloudStorage/GoogleDrive-anabellecardoso@gmail.com/Shared drives/BioSCape_Admin/GIS files"
boxfile= "20230913_G3_AVIRIS_PRISM_elev_boxes.gpkg"
outboxfile= "/Rscripts_anabelle/20230913_G3_AVIRIS_PRISM_boxes_elev_check.gpkg"

## reproject flight boxes into crs of cloud layers
boxes1=st_read(file.path(boxdir,boxfile)) %>% 
  st_transform(st_crs(cloud10))

## extract cloud and elevation stats
boxes1$cloud10=terra::extract(cloud10,boxes1,fun=mean)[,2]/100
boxes1$cloud11=terra::extract(cloud11,boxes1,fun=mean)[,2]/100
boxes1$maxmaxelev=terra::extract(elev,boxes1,fun=max)[,2]
boxes1$meanmaxelev=terra::extract(elev,boxes1,fun=mean)[,2]

## pull out mean cloud values for each box 
# this is average cloud in october and november for each box
# high average cloud means high risk for that box (less likely to be clear on any given day)
boxes <- 
  boxes1%>%
  mutate(cloudmean=(cloud10+cloud11)/2)%>% #mean oct/nov cloud
  st_transform(9221) #transform back to original CRS (epsg9221)

## export boxes with added cloud info 
st_write(boxes,dsn = file.path(boxdir,outboxfile),append=F) 

## have a look at some plots 
plot(boxes[,c("cloudmean","meanmaxelev","maxmaxelev")])
plot(boxes$cloud10,boxes$cloud11)
plot(boxes$maxmaxelev,boxes$meanmaxelev);abline(0,1)

## need to do: plot histogram of each boxes elevation (mean and max) to check there aren't any big mountains driving box elevation 
#eg. 
#hist(lidar_dem,
#main = "Distribution of surface elevation values",
#xlab = "Elevation (meters)", ylab = "Frequency",
#col = "springgreen")
#except clipped onto each box


#### PART 2: Working out how to prioritise boxes to be flown ####


## read in flight boxes and team region of interest (ROI) polygons 
boxes.dir= "20230905_G5_LVIS_boxes.gpkg"
rois.dir="20230825_Team ROIs.gpkg"

boxes <- st_read(file.path(gis.dir,boxes.dir))
rois <- st_read(file.path(gis.dir,rois.dir))

## fix and clean up geometries
boxes <- st_make_valid(boxes)
rois <- st_make_valid(rois)

## sum up area each PI requested (total)
roi_areas <- rois %>% 
  mutate(area = st_area(.)) %>% 
  group_by(team_PI) %>% 
  summarize(pi_area_tot = sum(area) )

## add column for area already acquired (in m^2)
team_PI <- c("Adler", "Cawse-Nicholson", "Cho", "Clark", "Guild", 
             "Rossi", "Slingsby", "Stovall", "Van Aardt", "Wu", "van Niekerk")

pi_area_acquired <- c("0", #Adler
                      "0", #Cawse-Nicholson
                      "0", #Cho
                      "0", #Clark
                      "0", #Guild
                      "0", #Rossi
                      "0", #Slingsby
                      "0", #Stovall
                      "0", #van Aardt
                      "0", #Wu
                      "0" #van Niekerk
                      )

area_acquired <- data.frame(team_PI, pi_area_acquired)
area_acquired$pi_area_acquired = as.numeric(area_acquired$pi_area_acquired)
area_acquired$pi_area_acquired = set_units(area_acquired$pi_area_acquired, m^2)
area_acquired$pi_area_acquired = as_units(area_acquired$pi_area_acquired)

# clip ROIs to flight boxes, dissolve, and calculate areas (in m^2)
areas_pi_flightbox <- 
  st_intersection(rois, boxes) %>% 
  mutate(polygon_area = st_area(.)) %>% 
  group_by(box_nr, team_PI, priority) %>% 
  summarize(polygon_area = sum(polygon_area) ) %>% 
  left_join(st_set_geometry(pi_tot_area,NULL),by="team_PI") %>% 
  left_join(area_acquired, by="team_PI") %>%
  mutate(area_remaining=(pi_total_area-pi_area_acquired)/pi_total_area) %>% 
  mutate(priority_assigned=case_when(
    priority=="high" ~ 3,
    priority=="medium" ~ 2,
    priority=="low" ~ 1
  ))

## calculate area-based priority index by box

area_priority_by_box <- areas_pi_flightbox %>% 
  mutate(pi_area_in_box = polygon_area/pi_total_area) %>% 
  mutate (polygon_area_priority = pi_area_in_box * priority_assigned * area_remaining)  %>% #penalised if you already have an acquisition
  group_by (box_nr) %>% 
  summarize(area_based_box_priority = sum(polygon_area_priority) ) 
  
## pull in a cloud risk value for each box 

cloud_box_values <- st_set_geometry(boxes,NULL) #remove geometry from boxes, coerce to dataframe
cloud_box_values <- cloud_box_values [, c("box_nr", "cloudmean")] # pull out box_nr and cloud mean

## add in cloud values and calculate new combined priority metric
cloudXarea_box <- area_priority_by_box %>% 
  left_join(cloud_box_values, by="box_nr") %>%
  mutate (areaXcloud_priority = (cloudmean/100) * area_based_box_priority) # make cloud percentage into a ratio 

## problems: 
# need to fairly weight area and priority,we dont want PIs to be incentivised to make everything high priority or give us huge ROIs. 
# need to include some kind of metric that helps us pick up when a team isn't going to get any acquisitions. 
# need to include wind... 


## add corner coords to box for margot 
st_read("/Users/awcardos/Library/CloudStorage/GoogleDrive-anabellecardoso@gmail.com/Shared drives/BioSCape_Admin/GIS files/20230912_BioSCape_Flight_Boxes.gpkg") %>% 
  st_transform(4326)%>% 
  st_coordinates()       
