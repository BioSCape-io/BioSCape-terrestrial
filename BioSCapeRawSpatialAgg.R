########################################################
# BioSCapeRawSpatialAgg.R
#
# Purpose: Aggregate GIS data from field botanist into single uniform 
# layers. Note that due to non-uniformity in how the data is labelled 
# and ongoing duplication of uploads
# this will be a file-by-file process to ensure quality and accuracy.
#
# TL/DR: This code is clunky
# Date: October 2023
# Author(s): Henry Frye
########################################################

# Clean out global environment
rm(list = ls())

# Set working directory
if(Sys.info()['user']=='henryfrye') setwd('/Users/henryfrye/Dropbox/Intellectual_Endeavours/Wisconsin/BioSCapeTownsend/BioSCapeSpatialData/RawDataFromBotanist')


# Read in libraries
library(sf)
library(tidyverse)

#### Read in spatial data ####

# Read in data for North Cederberg (Ross)
GroundNCederPoint <- st_read('NorthCederbergRoss/Blue Point_Points.shp')
GroundNCederPoly <- st_read('NorthCederbergRoss/Blue Polygon_Polygons.shp')
ParkingNCeder <- st_read('NorthCederbergRoss/Green Point_Points.shp')

# Read in data Cape Point Peninsula (Ross)
GroundCapePenPoint <- st_read('CapePointRoss/Red Point_Points.shp')
GroundCapePenPoly <- st_read('CapePointRoss/Yellow Polygon_Polygons.shp')
ParkingCapePen <- st_read('CapePointRoss/Green Point_Points.shp')

# Read in Swartberg, Groot Winterhoek, De Hoop, and Boland (Ross)
GroundSwart2BolandPoint <- st_read('GrootBolandDeHoopSwartGardenRoss/Blue Point_Points.shp')
GroundSwart2BolandPoly <- st_read('GrootBolandDeHoopSwartGardenRoss/Blue Polygon_Polygons.shp')
GroundSwart2BolandParking <- st_read('GrootBolandDeHoopSwartGardenRoss/Green Point_Points.shp')

# Read in Peninsula, Hottentots, and Kogelberg (Doug)
PenHotKogPoint <- st_read('PeninKogelHotsDoug/Blue Point_Points.shp')
PenHotKogPoly <- st_read('PeninKogelHotsDoug/Blue Polygon_Polygons.shp')

# Doug's points are combined by parking and plot need to split those up first
PenHotKogPoint <- PenHotKogPoint %>% mutate(ParkPlot = str_extract(Name,'parking'))
PenHotKogParking <- PenHotKogPoint %>% dplyr::filter(ParkPlot == 'parking') %>% dplyr::select(-ParkPlot)
PenHotKogPoint <- PenHotKogPoint %>% dplyr::filter(is.na(ParkPlot) == TRUE) %>% 
  dplyr::select(-ParkPlot)

# Read in sites from West Coast, Vrolijkheid, Bontebok, Demond (Agulhas), 
#and Waenhuiskrans (Agulhas).
WestAlPoint <- st_read('WestVroiBonAlUnsure/Blue Point_Points.shp')
WestAlPoly <- st_read('WestVroiBonAlUnsure/Blue Polygon_Polygons.shp')
WestAlParking <- st_read('WestVroiBonAlUnsure/Green Point_Points.shp')



#### Process data for joining ####

###### North Cederberg into uniform format for later joins ######

# Plot centers
# Clean up plot ID for uniform BioSCape plots
GroundNCederPoint <- GroundNCederPoint %>%
  mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(GroundNCederPoint$Name, "\\d+"), sep = "")))  %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Add region variable
GroundNCederPoint$Region <- rep('Cederberg', length(rownames(GroundNCederPoint)))
# Rearrange columns
GroundNCederPoint <- GroundNCederPoint %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
GroundNCederPoint <- GroundNCederPoint %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)

# Plot polygons
# Clean up plot ID for uniform BioSCape plots
GroundNCederPoly <- GroundNCederPoly %>%
  mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(GroundNCederPoly$Name, "\\d+"), sep = ""))) %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Clean up region column
GroundNCederPoly$Region <- rep('Cederberg', length(rownames(GroundNCederPoly)))
# Rearrange columns
GroundNCederPoly <- GroundNCederPoly %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
GroundNCederPoly <- GroundNCederPoly %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)

# Parking
# Clean up plot ID for uniform BioSCape plots
ParkingNCeder <- ParkingNCeder %>% mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(ParkingNCeder$Name, "\\d+"), sep = ""))) %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Add region variable
ParkingNCeder$Region <- rep('Cederberg', length(rownames(ParkingNCeder)))
# Rearrange columns
ParkingNCeder <- ParkingNCeder %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
ParkingNCeder <- ParkingNCeder %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)


###### Cape Point into uniform format for later joins ######

# Plot centers
#Clean up plot ID for uniform BioSCape plots and region labels
GroundCapePenPoint <- GroundCapePenPoint %>%
  mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(GroundCapePenPoint$Name, "\\d+"), sep = "")),
         Region = str_replace_all(GroundCapePenPoint$Name, "[^A-Za-z]", "")) %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Clean up lingering region label
GroundCapePenPoint$Region[1] <- 'CapePeninsula'
# Rearrange columns
GroundCapePenPoint <- GroundCapePenPoint %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
GroundCapePenPoint <- GroundCapePenPoint %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)

# Plot polygons 
# Clean up plot ID for uniform BioSCape plots
GroundCapePenPoly <- GroundCapePenPoly %>%
  mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(GroundCapePenPoly$Name, "\\d+"), sep = ""))) %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Clean up region column
GroundCapePenPoly <- GroundCapePenPoly %>% mutate(Region = word(Name, 1, sep = "_"))
# Rearrange columns
GroundCapePenPoly <- GroundCapePenPoly %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
GroundCapePenPoly <- GroundCapePenPoly %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)

#Parking
# Clean up plot ID for uniform BioSCape plots
ParkingCapePen <- ParkingCapePen %>% mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(ParkingCapePen$Name, "\\d+"), sep = ""))) %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Add region variable
ParkingCapePen <- ParkingCapePen %>% mutate(Region = word(Name, 1, sep = "_"))
# Rearrange columns
ParkingCapePen <- ParkingCapePen %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
ParkingCapePen <- ParkingCapePen %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)

###### Swartberg to Boland plots into uniform format for later joins ######

#Plot centers
#Clean up plot ID for uniform BioSCape plots and region labels
GroundSwart2BolandPoint <- GroundSwart2BolandPoint %>%
  mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(GroundSwart2BolandPoint$Name, "\\d+"), sep = "")),
         Region = word(Name, 1, sep = "_"))  %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
GroundSwart2BolandPoint <- GroundSwart2BolandPoint %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
GroundSwart2BolandPoint <- GroundSwart2BolandPoint %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)

#Polygons
# Clean up plot ID for uniform BioSCape plots
GroundSwart2BolandPoly <- GroundSwart2BolandPoly %>%
  mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(GroundSwart2BolandPoly$Name, "\\d+"), sep = ""))) %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Clean up region column
GroundSwart2BolandPoly <- GroundSwart2BolandPoly %>% mutate(Region = word(Name, 1, sep = "_"))
# Rearrange columns
GroundSwart2BolandPoly <- GroundSwart2BolandPoly %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
GroundSwart2BolandPoly <- GroundSwart2BolandPoly %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)


#Parking
# Clean up plot ID for uniform BioSCape plots
GroundSwart2BolandParking <- GroundSwart2BolandParking %>% mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(GroundSwart2BolandParking$Name, "\\d+"), sep = ""))) %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Clean up region column
GroundSwart2BolandParking <- GroundSwart2BolandParking %>% mutate(Region = word(Name, 1, sep = "_"))
# Rearrange columns
GroundSwart2BolandParking <- GroundSwart2BolandParking %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
GroundSwart2BolandParking <- GroundSwart2BolandParking %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)


###### Peninsula, Kogelberg, Hottentots (Doug) plots into uniform format for later joins ######

# Plot centers
# Clean up plot ID for uniform BioSCape plots and region labels
PenHotKogPoint <- PenHotKogPoint %>%
  mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(PenHotKogPoint$Name, "\\d+"), sep = "")),
         Region = word(Name, 1, sep = "_"))  %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
PenHotKogPoint <- PenHotKogPoint %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
PenHotKogPoint <- PenHotKogPoint %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)

# Plot polygons
# Clean up plot ID for uniform BioSCape plots
PenHotKogPoly <- PenHotKogPoly %>%
  mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(PenHotKogPoly$Name, "\\d+"), sep = "")),
         Region = word(Name, 1, sep = "_")) %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
PenHotKogPoly <- PenHotKogPoly%>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
PenHotKogPoly <- PenHotKogPoly %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)

# Parking
# Clean up plot ID for uniform BioSCape plots
PenHotKogParking <- PenHotKogParking %>% mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(PenHotKogParking$Name, "\\d+"), sep = "")),
                                                                  Region = word(Name, 1, sep = "_")) %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
PenHotKogParking <- PenHotKogParking %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
PenHotKogParking <- PenHotKogParking %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)


###### West Coast, Vrolijkheid, Bontebok plots into uniform format for later joins ######

# Plot centers
# Clean up plot ID for uniform BioSCape plots and region labels
WestAlPoint <- WestAlPoint %>%
  mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(WestAlPoint$Name, "\\d+"), sep = "")),
         Region = word(Name, 1, sep = "_"))  %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
WestAlPoint <- WestAlPoint %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
WestAlPoint <- WestAlPoint %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)

# Plot polygons
# Clean up plot ID for uniform BioSCape plots
WestAlPoly <- WestAlPoly %>%
  mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(WestAlPoly$Name, "\\d+"), sep = "")),
         Region = word(Name, 1, sep = "_")) %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
WestAlPoly <- WestAlPoly %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
WestAlPoly <- WestAlPoly %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)

#Parking
# Clean up plot ID for uniform BioSCape plots
WestAlParking <- WestAlParking %>% mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(WestAlParking$Name, "\\d+"), sep = "")),
                                                Region = word(Name, 1, sep = "_")) %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
WestAlParking <- WestAlParking %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
WestAlParking <- WestAlParking %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)



#### Join Data ####
CFRGroundPoints <- rbind(GroundCapePenPoint, GroundNCederPoint, GroundSwart2BolandPoint, PenHotKogPoint, WestAlPoint)
CFRPolys <-  rbind(GroundNCederPoly,GroundCapePenPoly, GroundSwart2BolandPoly, PenHotKogPoly, WestAlPoly )
CFRParking <-  rbind(ParkingNCeder,ParkingCapePen, GroundSwart2BolandParking, PenHotKogParking, WestAlParking)



#### Write data ####
current_datetime <- format(Sys.time(), "%Y_%m%_%d")
path <-  "../IntermediateSpatialData/"
publicpath <- '../BioSCape-terrestrial/VegSpatialProducts/'

#Write shape and kmls
#need to patch the kmls, they put things in wrong order.
st_write(CFRGroundPoints, paste0(publicpath,"BioSCapeVegCenters",current_datetime, ".shp"), 
         append= FALSE)
st_write(CFRGroundPoints, paste0(path,"BioSCapeVegCenters",current_datetime, ".kml"), driver = "KML",
         append = FALSE)
#st_write(CFRGroundPoints, paste0(path,"BioSCapeVegCenters",current_datetime, ".gpx"), driver = "GPX",
#         append = FALSE) #fix later



st_write(CFRPolys, paste0(publicpath,"BioSCapeVegPolys",current_datetime, ".shp"),
         append= FALSE)
st_write(CFRPolys, paste0(path,"BioSCapeVegPolys",current_datetime, ".kml"), driver = "KML",
         append = FALSE)

st_write(CFRParking, paste0(publicpath,"BioSCapeVegParking",current_datetime, ".shp"),
         append= FALSE)
st_write(CFRParking, paste0(path,"BioSCapeVegParking",current_datetime, ".kml"), driver = "KML",
         append = FALSE)


#write as geopackage file
# Write the second sf object to the GeoPackage file, with `update = TRUE` to append to the same file
st_write(CFRGroundPoints, paste0(publicpath,"BioSCapeVegData",current_datetime, ".gpkg"), layer = "PlotCenters")
st_write(CFRPolys, paste0(publicpath,"BioSCapeVegData",current_datetime, ".gpkg"), layer = "PlotPolygons", append = TRUE)
st_write(CFRParking, paste0(publicpath,"BioSCapeVegData",current_datetime, ".gpkg"), layer = "PlotParking", append = TRUE)
         


