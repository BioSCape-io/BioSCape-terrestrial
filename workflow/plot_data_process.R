# Script to semi-automate veg plot cleaning


library(googlesheets4)
library(tidyverse)
library(vegan)
require(Rarefy)
library(sf)
library(piggyback)
library(units)
library(stringr)

# Define the URL or key of the Google Sheet
sheet_url <- "https://docs.google.com/spreadsheets/d/19RdI9SdqOx8zn6XBxywgUNgmAHJ6JatOjtTEK-VAcQE/edit#gid=266439266"



### Data sheets
# Authenticate and access the Google Sheet
gs4_auth(email = "adamw@buffalo.edu")
sheet <- gs4_get(sheet_url)

# Filter only plot tabs
sheets = sheet_names(sheet)
  
# Filter using grep to identify only plot and drop template sheets
plot_sheets <- sheets[grepl("plot", sheets) & !grepl("Template_Plot", sheets)]

# site data
sites=read_sheet(sheet,"SiteData")

######### Plot kml
# download plot location data kml
#used https://sites.google.com/site/gdocs2direct/ to reformat link
download.file("https://drive.google.com/uc?export=download&id=1LeoD3lxjJ_v9fl4CckYBxSzvXGnyhGyy",
              destfile="data/points.kml")


# Download data from the specified tabs as data frames
data_downloaded <- lapply(plot_sheets, function(tab) {
read_sheet(sheet,tab) %>% 
    select(-SeasonallyApparent) #drop field causing problems with weird entry in Rondevlei_147
})



# merge tables

data <- data_downloaded %>% 
  bind_rows() %>% 
  separate(SiteCode_Plot_Quadrant,into=c("SiteCode","Plot","Quadrant"),sep="_",remove = F) %>% 
  mutate("SiteCode_Plot"=paste(SiteCode,Plot,sep="_"),
         Plot=as.numeric(Plot)) %>% 
  left_join(sites,by=c("SiteCode_Plot_Quadrant","SiteCode","SiteCode_Plot","Plot","Quadrant") ) 



## Map of Plot Locations and Status


# Data upload

# upload summary data to release

tag="v0.1"
repo="BioSCape-io/BioSCape-terrestrial"
readr::write_csv(data,"data/bioscape_vegetation_data.csv")

kmlfile=paste0("bioscape_plots_",tag,".kml")
st_write(plots,file.path("data",kmlfile),append=F)

gpkgfile=paste0("bioscape_plots_",tag,".gpkg")
st_write(plots,file.path("data/",gpkgfile),append=F)


# if release doesn't exist for this tag - create it
if(!any(tag%in%pb_releases(repo)$tag_name))  pb_new_release(repo = repo,tag=tag)

pb_upload(file = file.path("data",kmlfile),
          repo="BioSCape-io/BioSCape-terrestrial",
          tag=tag)

pb_upload(file = file.path("data",gpkgfile),
          repo="BioSCape-io/BioSCape-terrestrial",
          tag=tag)


pb_upload(file = "data/bioscape_vegetation_data.csv",
          repo="BioSCape-io/BioSCape-terrestrial",
          tag=tag)


