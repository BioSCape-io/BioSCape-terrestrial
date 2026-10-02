########################################################
# BioSCapeRawSpatialAgg.R
#
# Purpose: Aggregate GIS data from field botanist into single uniform 
# layers. Note that due to non-uniformity in how the data is labelled 
# and ongoing duplication of uploads
# this will be a file-by-file process to ensure quality and accuracy.
#
# Date Created: October 2023
# Most recent modification: November 2024
# Author(s): Henry Frye
########################################################

# Clean out global environment
rm(list = ls())

# Set working directory
if(Sys.info()['user']=='henryfrye') setwd('/Users/henryfrye/Dropbox/Intellectual_Endeavours/Wisconsin/BioSCapeTownsend/BioSCapeTeamSpatialData/RawDataFromBotanist')


# Read in libraries
library(sf)
library(tidyverse)
library(lubridate)

#### Read in spatial data ####

# Read in data for North Cederberg (Ross)
GroundNCederPoint <- st_read('NorthCederbergRoss/Blue Point_Points.shp')
GroundNCederPoly <- st_read('NorthCederbergRoss/Blue Polygon_Polygons.shp')
ParkingNCeder <- st_read('NorthCederbergRoss/Green Point_Points.shp')

# Read in data Cape Point Peninsula (Ross)
GroundCapePenPoint <- st_read('CapePointRoss/Red Point_Points.shp')
GroundCapePenPoly <- st_read('CapePointRoss/CapePointRossPolyFixed.shp')
ParkingCapePen <- st_read('CapePointRoss/Green Point_Points.shp')

# Read in Swartberg, Groot Winterhoek, De Hoop, and Boland (Ross)
GroundSwart2BolandPoint <- st_read('GrootBolandDeHoopSwartGardenRoss/Blue Point_Points.shp')
GroundSwart2BolandPoly <- st_read('GrootBolandDeHoopSwartGardenRoss/Swart2BoldandRossPolysFixed.shp')
GroundSwart2BolandParking <- st_read('GrootBolandDeHoopSwartGardenRoss/Green Point_Points.shp')

# Read in Peninsula, Hottentots, and Kogelberg (Doug)
PenHotKogPoint <- st_read('PeninKogelHotsDoug/Blue Point_Points.shp')
PenHotKogPoly <- st_read('PeninKogelHotsDoug/PeninKogPolysDougFixed.shp')

# Doug's points are combined by parking and plot need to split those up first
PenHotKogPoint <- PenHotKogPoint %>% mutate(ParkPlot = str_extract(Name,'parking'))
PenHotKogParking <- PenHotKogPoint %>% dplyr::filter(ParkPlot == 'parking') %>% dplyr::select(-ParkPlot)
PenHotKogPoint <- PenHotKogPoint %>% dplyr::filter(is.na(ParkPlot) == TRUE) %>% 
  dplyr::select(-ParkPlot)

# Read in sites from West Coast, Vrolijkheid, Bontebok, Demond (Agulhas), 
# and Waenhuiskrans (Agulhas).
WestAlPoint <- st_read('WestVroiBonAlUnsure/Blue Point_Points.shp')
WestAlPoly <- st_read('WestVroiBonAlUnsure/WestAlPolysFixed.shp')
WestAlParking <- st_read('WestVroiBonAlUnsure/Green Point_Points.shp')

# Read in plots from the Rooiberg region
RooPoint <- st_read('Rooiberg/RooibergPlotCenters.shp')
RooPoly <- st_read('Rooiberg/RooibergPolysFixed.shp')
RooParking <- st_read('Rooiberg/RooibergParking.shp')

# Read in plots based on Oct 17 2023 update that includes West Coast, Rocherpan,
# Agulhas, Garden Route and a few sites in between.
West2GardOct17Point <- sf::st_read('WestAlGardenOct17/WesttoGardenOct17VegCenter.shp')
West2GardOct17Poly <- sf::st_read('WestAlGardenOct17/WesttoGardenOct17VegPoly.shp')
West2GardOct17Parking <- sf::st_read('WestAlGardenOct17/WesttoGardenOct17VegParking.shp')

# Read in plots from South Cedeberg
SouthCedNov2Point <- sf::st_read('SouthCederbergDougNov2/SouthCederberg.shp')
SouthCedNov2Poly <- sf::st_read('SouthCederbergDougNov2/SouthCederbergPolysFixed.shp')

# Read in additional Agulhas data
ExtraAgulOct25Point <- sf::st_read('ExtraAgulhasCapensisOct25/AdditionalAgulhasCenters.shp')
ExtraAgulOct25Poly <- sf::st_read('ExtraAgulhasCapensisOct25/ExtraAgulhasPolysFixed.shp')
ExtraAgulOct25Parking <- sf::st_read('ExtraAgulhasCapensisOct25/ExtraAgulhasParking.shp')

# Read in Baviaanskloof data
BavsNov2Point <- sf::st_read('BaviaanskloofRossNov2/BaviaanskloofCenter.shp')
BavsNov2Poly <- sf::st_read('BaviaanskloofRossNov2/BaviaanskloofPolysFixed.shp')
BavsNov2Parking <- sf::st_read('BaviaanskloofRossNov2/BaviaanskloofParking.shp')

# Read in Anysberg data
AnysNov8Point <- sf::st_read('AnysbergEmmsNov8/AnysbergPlotCenters.shp')
AnysNov8Poly <- sf::st_read('AnysbergEmmsNov8/AnysbergPatchesFixed.shp')
AnysNov8Parking <- sf::st_read('AnysbergEmmsNov8/AnysbergParking.shp')

# Read in Townsend bioscape plot data
TownsendPoints <- st_read('/Users/henryfrye/Dropbox/Intellectual_Endeavours/Wisconsin/BioSCapeTownsend/TownsendTraitFieldData/SpatialData/TownsendBioSCapePlotsGPSCleanV1.geojson')

# Read plot quality annotations from Townsend team
QualityAssess <- read.csv('/Users/henryfrye/Dropbox/Intellectual_Endeavours/Wisconsin/BioSCapeTownsend/BioSCapeTeamSpatialData/Merged_Plot_Note_Issues.csv')

# Read in associations between Townsend team plots and BioSCape plots
PlotAssoc <- read.csv('/Users/henryfrye/Dropbox/Intellectual_Endeavours/Wisconsin/BioSCapeTownsend/TownsendTraitFieldData/SpatialData/PlotAssociation.csv')



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
# Input surveying botanist name (need to do before aggregation because of calibration plots, e.g.,
# same plot, multiple botanists)
GroundNCederPoint$Botanist <- rep('Ross Turner',nrow(GroundNCederPoint))
GroundNCederPoint <- GroundNCederPoint %>% dplyr::select(BioScapePlotID, Region, Botanist, Name:geometry)


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
# Input surveying botanist name (need to do before aggregation because of calibration plots, e.g.,
# same plot, multiple botanists)
GroundNCederPoly$Botanist <- rep('Ross Turner',nrow(GroundNCederPoly))
GroundNCederPoly <- GroundNCederPoly %>% dplyr::select(BioScapePlotID, Region, Botanist, Name:geometry)

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
# Input surveying botanist name 
GroundCapePenPoint$Botanist <- rep('Ross Turner',nrow(GroundCapePenPoint))
GroundCapePenPoint <- GroundCapePenPoint %>% dplyr::select(BioScapePlotID, Region, Botanist, Name:geometry)

# Remove T089 from Ross (there is no veg survey data associated with it)
GroundCapePenPoint <- GroundCapePenPoint %>% filter(BioScapePlotID != "T089")


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
# Input surveying botanist name 
GroundCapePenPoly$Botanist <- rep('Ross Turner',nrow(GroundCapePenPoly))
GroundCapePenPoly <- GroundCapePenPoly %>% dplyr::select(BioScapePlotID, Region, Botanist, Name:geometry)

# Remove Ross's T089 patch since there is not veg survey data associated
GroundCapePenPoly <- GroundCapePenPoly %>% filter(BioScapePlotID != "T089")

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
# Input surveying botanist name 
GroundSwart2BolandPoint$Botanist <- rep('Ross Turner',nrow(GroundSwart2BolandPoint))
GroundSwart2BolandPoint <- GroundSwart2BolandPoint %>% dplyr::select(BioScapePlotID, Region, Botanist, Name:geometry)


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
# Input surveying botanist name 
GroundSwart2BolandPoly$Botanist <- rep('Ross Turner',nrow(GroundSwart2BolandPoly))
GroundSwart2BolandPoly <- GroundSwart2BolandPoly %>% dplyr::select(BioScapePlotID, Region, Botanist, Name:geometry)


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
# Input surveying botanist name 
PenHotKogPoint$Botanist <- rep('Doug Euston-Brown',nrow(PenHotKogPoint))
PenHotKogPoint <- PenHotKogPoint %>% dplyr::select(BioScapePlotID, Region, Botanist, Name:geometry)



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
# Input surveying botanist name 
PenHotKogPoly$Botanist <- rep('Doug Euston-Brown',nrow(PenHotKogPoly))
PenHotKogPoly <- PenHotKogPoly %>% dplyr::select(BioScapePlotID, Region, Botanist, Name:geometry)


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
# Input surveying botanist name (the capensis files had a mix of botanist [and for some reason different plots for center
# and patches] so this is a bit more involved)
MoltenWest <- c('T188','T187','T095', 'T184', 'T282','T172', 'T087', 'T253', 'T002', 'T138','T264', 'T078',
                'T183', 'T063', 'T001', 'T019','T023')
HallWest <- c('T083', 'T180')
EmmsWest <- c('T084')
WestAlPoint <- WestAlPoint %>% mutate(Botanist = case_when(BioScapePlotID %in% HallWest  ~ 'Stuart Hall',
                                                           BioScapePlotID %in% MoltenWest ~ 'Steven Molteno',
                                                           BioScapePlotID %in% EmmsWest ~ 'Paul Emms'))
WestAlPoint <- WestAlPoint %>% dplyr::select(BioScapePlotID, Region, Botanist, Name:geometry)

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
# Input surveying botanist name
WestAlPoly <- WestAlPoly %>% mutate(Botanist = case_when(BioScapePlotID %in% HallWest  ~ 'Stuart Hall',
                                                           BioScapePlotID %in% MoltenWest ~ 'Steven Molteno',
                                                         BioScapePlotID %in% EmmsWest ~ 'Paul Emms'))
WestAlPoly <- WestAlPoly %>% dplyr::select(BioScapePlotID, Region, Botanist, Name:geometry)

#Parking
# Clean up plot ID for uniform BioSCape plots
WestAlParking <- WestAlParking %>% mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(WestAlParking$Name, "\\d+"), sep = "")),
                                                Region = word(Name, 1, sep = "_")) %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
WestAlParking <- WestAlParking %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
WestAlParking <- WestAlParking %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)

###### Rooiberg region plots into uniform format for later joins ######

# Plot centers
# Clean up plot ID for uniform BioSCape plots and region labels
RooPoint <- RooPoint %>%
  mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(RooPoint$Name, "\\d+"), sep = "")),
         Region = word(Name, 1, sep = "_"))  %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
RooPoint <- RooPoint %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
RooPoint <- RooPoint %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)
# Input surveying botanist name 
RooPoint <- RooPoint %>% mutate(Botanist = 'Ross Turner')
RooPoint <- RooPoint %>% dplyr::select(BioScapePlotID, Region, Botanist, Name:geometry)


# Plot polygons
# Clean up plot ID for uniform BioSCape plots
RooPoly <- RooPoly %>%
  mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(RooPoly$Name, "\\d+"), sep = "")),
         Region = word(Name, 1, sep = "_")) %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
RooPoly <- RooPoly %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
RooPoly <- RooPoly %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)
# Input surveying botanist name 
RooPoly <- RooPoly %>% mutate(Botanist = 'Ross Turner')
RooPoly <- RooPoly %>% dplyr::select(BioScapePlotID, Region, Botanist, Name:geometry)

#Parking
# Clean up plot ID for uniform BioSCape plots
RooParking <- RooParking %>% mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(RooParking$Name, "\\d+"), sep = "")),
                                          Region = word(Name, 1, sep = "_")) %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
RooParking <- RooParking %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
RooParking <- RooParking %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)


###### Oct 17 West Coast, Rocherpan, Agulhas, Garden Routeregion plots into uniform format for later joins ######

# Plot centers
# Clean up plot ID for uniform BioSCape plots and region labels
West2GardOct17Point <- West2GardOct17Point %>%
  mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(West2GardOct17Point$Name, "\\d+"), sep = "")),
         Region = word(Name, 1, sep = "_"))  %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
West2GardOct17Point <- West2GardOct17Point %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
West2GardOct17Point <- West2GardOct17Point %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)
#Input surveying botanist
HallWRAG <- c('T270', 'T181', 'T178', 'T175', 'T176', 'T182', 'T277', 'T164','T281', 'T066', 'T067',
              'T009', 'T018', 'T017', 'T022')
LabusWRAG <-  c('T194', 'T053', 'T139', 'T275', 'T051')
EmmsWRAG <- c('T235', 'T088', 'T091', 'T173', 'T177')
West2GardOct17Point <-West2GardOct17Point %>% mutate(Botanist = case_when(BioScapePlotID %in% HallWRAG  ~ 'Stuart Hall',
                                                           BioScapePlotID %in% LabusWRAG ~ 'Adam Labuschagne',
                                                           BioScapePlotID %in% EmmsWRAG ~ 'Paul Emms'))
West2GardOct17Point <-West2GardOct17Point %>% dplyr::select(BioScapePlotID, Region, Botanist, Name:geometry)


# Plot polygons
# Clean up plot ID for uniform BioSCape plots
West2GardOct17Poly <- West2GardOct17Poly%>%
  mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(West2GardOct17Poly$Name, "\\d+"), sep = "")),
         Region = word(Name, 1, sep = "_")) %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
West2GardOct17Poly <- West2GardOct17Poly %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
West2GardOct17Poly <- West2GardOct17Poly %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)
# Input surveying botanist
West2GardOct17Poly <-West2GardOct17Poly %>% mutate(Botanist = case_when(BioScapePlotID %in% HallWRAG  ~ 'Stuart Hall',
                                                                          BioScapePlotID %in% LabusWRAG ~ 'Adam Labuschagne',
                                                                          BioScapePlotID %in% EmmsWRAG ~ 'Paul Emms'))
West2GardOct17Poly <-West2GardOct17Poly %>% dplyr::select(BioScapePlotID, Region, Botanist, Name:geometry)



#Parking
# Clean up plot ID for uniform BioSCape plots
West2GardOct17Parking <- West2GardOct17Parking %>% mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(West2GardOct17Parking$Name, "\\d+"), sep = "")),
                                    Region = word(Name, 1, sep = "_")) %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
West2GardOct17Parking <- West2GardOct17Parking %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
West2GardOct17Parking <- West2GardOct17Parking %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)

###### South Cederberg Doug ######

# Plot centers
# Clean up plot ID for uniform BioSCape plots and region labels
SouthCedNov2Point <- SouthCedNov2Point %>%
  mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(SouthCedNov2Point$Name, "\\d+"), sep = "")),
         Region = word(Name, 1, sep = "_"))  %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
SouthCedNov2Point <- SouthCedNov2Point %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
SouthCedNov2Point <- SouthCedNov2Point %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)
# Input surveying botanist name 
SouthCedNov2Point <- SouthCedNov2Point %>% mutate(Botanist = 'Doug Euston-Brown')
SouthCedNov2Point <- SouthCedNov2Point %>% dplyr::select(BioScapePlotID, Region, Botanist, Name:geometry)

# Plot polygons
# Clean up plot ID for uniform BioSCape plots
SouthCedNov2Poly <- SouthCedNov2Poly %>%
  mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(SouthCedNov2Poly$Name, "\\d+"), sep = "")),
         Region = word(Name, 1, sep = "_")) %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
SouthCedNov2Poly <- SouthCedNov2Poly %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
SouthCedNov2Poly <- SouthCedNov2Poly %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)
# Input surveying botanist name 
SouthCedNov2Poly <- SouthCedNov2Poly %>% mutate(Botanist = 'Doug Euston-Brown')
SouthCedNov2Poly <- SouthCedNov2Poly %>% dplyr::select(BioScapePlotID, Region, Botanist, Name:geometry)

# no parking layer

###### Extra Agulhas capensis Oct 25 ######

# Plot centers
# Clean up plot ID for uniform BioSCape plots and region labels
ExtraAgulOct25Point <- ExtraAgulOct25Point %>%
  mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(ExtraAgulOct25Point$Name, "\\d+"), sep = "")),
         Region = word(Name, 1, sep = "_"))  %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
ExtraAgulOct25Point <- ExtraAgulOct25Point %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
ExtraAgulOct25Point <- ExtraAgulOct25Point %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)
# Input surveying botanist name 
ExtraAgulOct25Point <- ExtraAgulOct25Point %>% mutate(Botanist = 'Steven Molteno')
ExtraAgulOct25Point <- ExtraAgulOct25Point %>% dplyr::select(BioScapePlotID, Region, Botanist, Name:geometry)



# Plot polygons
# Clean up plot ID for uniform BioSCape plots
ExtraAgulOct25Poly <- ExtraAgulOct25Poly %>%
  mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(ExtraAgulOct25Poly$Name, "\\d+"), sep = "")),
         Region = word(Name, 1, sep = "_")) %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
ExtraAgulOct25Poly <- ExtraAgulOct25Poly %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
ExtraAgulOct25Poly <- ExtraAgulOct25Poly %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)
# Input surveying botanist name 
ExtraAgulOct25Poly <- ExtraAgulOct25Poly %>% mutate(Botanist = 'Steven Molteno')
ExtraAgulOct25Poly <- ExtraAgulOct25Poly %>% dplyr::select(BioScapePlotID, Region, Botanist, Name:geometry)

#Parking
# Clean up plot ID for uniform BioSCape plots
ExtraAgulOct25Parking <- ExtraAgulOct25Parking %>% mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(ExtraAgulOct25Parking$Name, "\\d+"), sep = "")),
                                                          Region = word(Name, 1, sep = "_")) %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
ExtraAgulOct25Parking <- ExtraAgulOct25Parking %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
ExtraAgulOct25Parking <- ExtraAgulOct25Parking %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)

###### Baviaanskloof ######

# Plot centers
# Clean up plot ID for uniform BioSCape plots and region labels
BavsNov2Point <- BavsNov2Point %>%
  mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(BavsNov2Point$Name, "\\d+"), sep = "")),
         Region = word(Name, 1, sep = "_"))  %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
BavsNov2Point <- BavsNov2Point %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
BavsNov2Point <- BavsNov2Point %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)
# Input surveying botanist name 
BavsNov2Point <- BavsNov2Point %>% mutate(Botanist = 'Ross Turner')
BavsNov2Point <- BavsNov2Point %>% dplyr::select(BioScapePlotID, Region, Botanist, Name:geometry)

# Plot polygons
# Clean up plot ID for uniform BioSCape plots
BavsNov2Poly <- BavsNov2Poly %>%
  mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(BavsNov2Poly$Name, "\\d+"), sep = "")),
         Region = word(Name, 1, sep = "_")) %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
BavsNov2Poly <- BavsNov2Poly %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
BavsNov2Poly <- BavsNov2Poly %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)
# Input surveying botanist name 
BavsNov2Poly <- BavsNov2Poly %>% mutate(Botanist = 'Ross Turner')
BavsNov2Poly <- BavsNov2Poly %>% dplyr::select(BioScapePlotID, Region, Botanist, Name:geometry)


#Parking
# Clean up plot ID for uniform BioSCape plots
BavsNov2Parking <- BavsNov2Parking %>% mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(BavsNov2Parking$Name, "\\d+"), sep = "")),
                                                          Region = word(Name, 1, sep = "_")) %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
BavsNov2Parking <- BavsNov2Parking %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
BavsNov2Parking <- BavsNov2Parking %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)


######  Anysberg  ######

# Plot centers
# Clean up plot ID for uniform BioSCape plots and region labels
AnysNov8Point <- AnysNov8Point %>%
  mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(AnysNov8Point$Name, "\\d+"), sep = "")),
         Region = word(Name, 1, sep = "_"))  %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
AnysNov8Point <- AnysNov8Point%>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
AnysNov8Point <- AnysNov8Point %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)
MoltAnys <- c('T016', 'T007', 'T107', 'T105', 'T109', 'T103')
EmmsAnys <- c('T104', 'T106', 'T004', 'T015', 'T108')
# Input surveying botanist
AnysNov8Point <-AnysNov8Point %>% mutate(Botanist = case_when(BioScapePlotID %in% MoltAnys  ~ 'Steven Molteno',
                                                                       BioScapePlotID %in% EmmsAnys ~ 'Paul Emms'))
AnysNov8Point <-AnysNov8Point %>% dplyr::select(BioScapePlotID, Region, Botanist, Name:geometry)

# Plot polygons
# Clean up plot ID for uniform BioSCape plots
AnysNov8Poly <- AnysNov8Poly %>%
  mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(AnysNov8Poly$Name, "\\d+"), sep = "")),
         Region = word(Name, 1, sep = "_")) %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
AnysNov8Poly <- AnysNov8Poly %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
AnysNov8Poly <- AnysNov8Poly %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)
# Input surveying botanist
AnysNov8Poly <- AnysNov8Poly %>% mutate(Botanist = case_when(BioScapePlotID %in% MoltAnys  ~ 'Steven Molteno',
                                                              BioScapePlotID %in% EmmsAnys ~ 'Paul Emms'))
AnysNov8Poly <-AnysNov8Poly %>% dplyr::select(BioScapePlotID, Region, Botanist, Name:geometry)

#Parking
# Clean up plot ID for uniform BioSCape plots
AnysNov8Parking <- AnysNov8Parking %>% mutate(BioScapePlotID = paste0('T', as.numeric(str_extract(AnysNov8Parking$Name, "\\d+"), sep = "")),
                                              Region = word(Name, 1, sep = "_")) %>% 
  mutate(BioScapePlotID  = ifelse(nchar(sub("^T", "", BioScapePlotID)) < 3, paste0("T", str_pad(sub("^T", "", BioScapePlotID), width = 3, pad = "0")), BioScapePlotID))
# Rearrange columns
AnysNov8Parking <- AnysNov8Parking %>% dplyr::select(BioScapePlotID, Region, Name:geometry)
# Rename funky column names
AnysNov8Parking <- AnysNov8Parking %>% dplyr::rename('Description' = Descriptio, 'DateTime' =  Date...Tim)


#### Join Data ####
CFRGroundPoints <- rbind(GroundCapePenPoint, GroundNCederPoint, GroundSwart2BolandPoint, PenHotKogPoint,
                         WestAlPoint,RooPoint,West2GardOct17Point, SouthCedNov2Point,
                         ExtraAgulOct25Point, BavsNov2Point, AnysNov8Point)
CFRPolys <-  rbind(GroundNCederPoly,GroundCapePenPoly, GroundSwart2BolandPoly, PenHotKogPoly, 
                   WestAlPoly,RooPoly, West2GardOct17Poly, SouthCedNov2Poly,
                   ExtraAgulOct25Poly, BavsNov2Poly, AnysNov8Poly)
CFRParking <-  rbind(ParkingNCeder,ParkingCapePen, GroundSwart2BolandParking, PenHotKogParking,
                     WestAlParking, RooParking, West2GardOct17Parking,
                     ExtraAgulOct25Parking, BavsNov2Parking, AnysNov8Parking)

#### Remove plots we decided were not necessary in OCt 2024####

# Ross Cape Point calibration plot (T096)
dim(CFRGroundPoints)
CFRGroundPoints <- CFRGroundPoints %>% dplyr::filter(!(BioScapePlotID == 'T096' & Botanist == 'Ross Turner') )
dim(CFRGroundPoints)

dim(CFRPolys)
CFRPolys <- CFRPolys %>% dplyr::filter(!(BioScapePlotID == 'T096' & Botanist == 'Ross Turner') )
dim(CFRPolys)


# Ross did the earlier sample
dim(CFRParking)
CFRParking <- CFRParking %>% dplyr::filter(!(BioScapePlotID == 'T096' & DateTime == '01 Jun 2023 at 11:46:00') )
dim(CFRParking)

# Adam plots (T194, T275, T051, T053, T139)
dim(CFRGroundPoints)
CFRGroundPoints <- CFRGroundPoints %>% dplyr::filter(!((BioScapePlotID == 'T194') |
                                      (BioScapePlotID == 'T275') |
                                    (BioScapePlotID == 'T051') |
                                      (BioScapePlotID == 'T053') |
                                      (BioScapePlotID == 'T139')) )
dim(CFRGroundPoints)

dim(CFRPolys)
CFRPolys <- CFRPolys %>% dplyr::filter(!((BioScapePlotID == 'T194') |
                                                         (BioScapePlotID == 'T275') |
                                                         (BioScapePlotID == 'T051')|
                                           (BioScapePlotID == 'T053') |
                                           (BioScapePlotID == 'T139') ) )
dim(CFRPolys)


dim(CFRParking)
CFRParking <- CFRParking %>% dplyr::filter(!((BioScapePlotID == 'T194') |
                                           (BioScapePlotID == 'T275') |
                                           (BioScapePlotID == 'T051')|
                                             (BioScapePlotID == 'T053') |
                                             (BioScapePlotID == 'T139') ) )
dim(CFRParking)



#### Check and flag plot centers against Townsend GPS ####

# Check current CRS [they should be WGS 84]
#st_crs(TownsendPoints)
#st_crs(CFRGroundPoints)

#Need to project to get distance, using Hartebeesthoek94 / Lo31 EPSG#2054 
target_crs <- 2054

# Transform both layers to the target CRS
CFRGroundPoints_transformed <- st_transform(CFRGroundPoints, crs = target_crs)
TownsendPoints_transformed <- st_transform(TownsendPoints, crs = target_crs)

# Create a 10-meter buffer around points in the BioSCape data
CFRGroundPoints_buffer <- st_buffer(CFRGroundPoints_transformed, dist = 10)

# Join the layers by Plot ID
joined <- CFRGroundPoints_transformed %>%
  left_join(as.data.frame(TownsendPoints_transformed), by = c("BioScapePlotID" = "PlotCode"), suffix = c("_CFRGroundPoints", "_TownsendPoints"))
#joined <- st_join(TownsendPoints_transformed, CFRGroundPoints_transformed, join = st_equals)

# Check if points from Townsend data fall within the 10-meter buffer of the corresponding points in BioSCape
joined$within_buffer <- st_within(joined$geometry_TownsendPoints, CFRGroundPoints_buffer$geometry)

# Filter points that fall outside the buffer
outside_buffer <- joined %>%
  filter(lengths(within_buffer) == 0) %>%
  filter(is.na(PlotType) == FALSE) %>%
  filter(BioScapePlotID != 'T096' ) #don't include the calibration plots
  

#### Join in supplementary information for plot center and patches ####

CFRGroundPointsFlag <- CFRGroundPoints %>% left_join(QualityAssess, by = c('BioScapePlotID' = 'Plot'))
LocationFlagPlots <- outside_buffer$BioScapePlotID
CFRGroundPointsFlag <- CFRGroundPointsFlag %>% mutate('LocationFlag' = case_when(BioScapePlotID %in% LocationFlagPlots  ~ 'See Townsend alternative location'))

CFRGroundPointsFlag <- CFRGroundPointsFlag %>% dplyr::select(BioScapePlotID:Botanist, DateTime, Name:Description, QualityFlag, LocationFlag, TownsendNotes, geometry ) %>%
  dplyr::rename('BotanistVisitDateTime' = DateTime, 'BotanistSpatialNotes' =  Description)

#Center of T282 was not visited by Townsend; change location flag
CFRGroundPointsFlag <- CFRGroundPointsFlag %>%
  mutate(LocationFlag = if_else(BioScapePlotID == "T282", NA, LocationFlag))



#### Add in associated Townsend plots ####

# Join the data frames
df_joined <- CFRGroundPointsFlag %>%
  left_join(PlotAssoc, by = c("BioScapePlotID" = "AssociatedBioScapePlot"))


# Concatenate PTPlot values for each AssociatedPlot
df_concatenated <- df_joined %>%
  group_by(BioScapePlotID) %>%
  summarise(PTPlot = paste(PlotCode, collapse = ";"), .groups = 'drop')

# Separate concatenated PTPlot values into different columns
df_final <- df_concatenated %>%
  separate(PTPlot, into = paste0("PTPlot", c('A','B', 'C', 'D')), sep = ";", fill = "right", extra = "drop")
df_final <- as.data.frame(df_final)
df_final <- df_final %>% dplyr::select(BioScapePlotID:PTPlotD)
CFRGroundPointsFlag_Assoc <- left_join(CFRGroundPointsFlag, df_final, by = "BioScapePlotID")

# Convert "NA" and "" to true NA values
CFRGroundPointsFlag_Assoc <- CFRGroundPointsFlag_Assoc %>%
  mutate(across(c(QualityFlag, TownsendNotes, PTPlotA,PTPlotB,PTPlotC, PTPlotD), ~na_if(., "NA"))) %>%
  mutate(across(c(QualityFlag, TownsendNotes, PTPlotA,PTPlotB,PTPlotC, PTPlotD), ~na_if(., "")))

#### Convert time of GPS to Coordinated Universal Time (UTC) ####

# Following DAAC recommendations
# Parse the time of measurement column and convert to UTC
CFRGroundPointsFlag_Assoc <- CFRGroundPointsFlag_Assoc %>%
  mutate(BotanistVisitDateTime = dmy_hms(BotanistVisitDateTime, tz = "UTC"))

#### Add in alternative Townsend plot GPS for location flags ####

# Filter rows where QualityFlag is "Yes"
quality_flags <- CFRGroundPointsFlag_Assoc %>%
  filter(LocationFlag == "See Townsend alternative location")

quality_flags <- as.data.frame(quality_flags)

# Perform the spatial join with the separate layer
joined_quality_rows <- quality_flags %>%
  left_join(TownsendPoints, by = c("BioScapePlotID" = "PlotCode")) %>% 
  dplyr::select(!elevation) %>%
  dplyr::select(!PlotType)
  
joined_quality_rows_rename <- joined_quality_rows %>% dplyr::rename(
  TownsendLong = longitude,
  TownsendLat = latitude,
  TownsendAlt_geometry = geometry.y 
)

# Merge the joined data back to the original dataframe
CFRGroundPointsFlag_Assoc_Qual <- CFRGroundPointsFlag_Assoc %>%
  left_join(joined_quality_rows_rename , by = colnames(joined_quality_rows_rename)[1:13] )

# Get rid of extra columns
CFRGroundPointsFlag_Assoc_Qual <- CFRGroundPointsFlag_Assoc_Qual %>% dplyr::select(-geometry.x) %>%
  dplyr::select(-TownsendAlt_geometry)


#### Clean up regional codes ####




#### Write data ####
current_datetime <- format(Sys.time(), "%Y_%m%_%d")
path <-  "../IntermediateSpatialData/"
publicpath <- '../BioSCape-terrestrial/VegSpatialProducts/'

#Write shape and kmls
#need to patch the kmls, they put things in wrong order.
st_write(CFRGroundPointsFlag_Assoc_Qual, paste0(path,"BioSCapeVegCenters",current_datetime, ".shp"), 
         append= FALSE)

#write.csv
st_write(CFRGroundPointsFlag_Assoc_Qual, paste0(path,"BioSCapeVegCenters",current_datetime, ".kml"), driver = "KML",
         append = FALSE)
#st_write(CFRGroundPoints, paste0(path,"BioSCapeVegCenters",current_datetime, ".gpx"), driver = "GPX",
#         append = FALSE) #fix later



st_write(CFRPolys, paste0(path,"BioSCapeVegPolys",current_datetime, ".shp"),
         append= FALSE)
st_write(CFRPolys, paste0(path,"BioSCapeVegPolys",current_datetime, ".kml"), driver = "KML",
         append = FALSE)

st_write(CFRParking, paste0(path,"BioSCapeVegParking",current_datetime, ".shp"),
         append= FALSE)
st_write(CFRParking, paste0(path,"BioSCapeVegParking",current_datetime, ".kml"), driver = "KML",
         append = FALSE)


#write as geopackage file
# Write the second sf object to the GeoPackage file, with `update = TRUE` to append to the same file
st_write(CFRGroundPointsFlag_Assoc_Qual, paste0(path,"BioSCapeVegData",current_datetime, ".gpkg"), layer = "PlotCenters", append = FALSE)
st_write(CFRPolys, paste0(path,"BioSCapeVegData",current_datetime, ".gpkg"), layer = "PlotPolygons", append = TRUE)
st_write(CFRParking, paste0(path,"BioSCapeVegData",current_datetime, ".gpkg"), layer = "PlotParking", append = TRUE)
         

# final checks against veg data to make sure plots are comparable
veg <-read.csv('/Users/henryfrye/Dropbox/Intellectual_Endeavours/Wisconsin/BioSCapeTownsend/BioSCapeTeamSpatialData/ReleasedVegData/bioscape_veg_plot_species_v20241104.csv')

setdiff(veg$Plot, CFRGroundPointsFlag_Assoc_Qual$BioScapePlotID)

