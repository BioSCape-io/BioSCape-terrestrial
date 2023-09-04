# Script to semi-automate veg plot updating
# download the google sheets and assemble into a single table per data type
# update plot data with the plots that have been sampled.

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



# Download data from the specified tabs as data frames
data_downloaded <- lapply(plot_sheets, function(tab) {
read_sheet(sheet,tab) %>% 
    select(-SeasonallyApparent) #drop field causing problems with weird entry in Rondevlei_147
})



# merge site data and plot-level data

data <- data_downloaded %>% 
  bind_rows() %>% 
  separate(SiteCode_Plot_Quadrant,into=c("SiteCode","Plot","Quadrant"),sep="_",remove = F) %>% 
  mutate("SiteCode_Plot"=paste(SiteCode,Plot,sep="_"),
         Plot=as.numeric(Plot)) %>% 
  left_join(sites,by=c("SiteCode_Plot_Quadrant","SiteCode","SiteCode_Plot","Plot","Quadrant") ) 

######### Plot kml
# download plot location data kml

tag="v20230904" #specify the most recent version of the plot kml

repo="BioSCape-io/BioSCape-terrestrial"
gpkgfile=paste0("bioscape_plots_",tag,".gpkg")


pb_download(file = gpkgfile,repo = repo,
              dest=file.path("data"))

######### Plot updates
# download plot polygons

plot_filename=paste0("BioSCape_plot_locations_",tag,".gpkg")
pb_download(file = plot_filename,
          repo="BioSCape-io/BioSCape-terrestrial",
          tag=tag,dest = "data")

homogeneous_areas=st_read(file.path("data",plot_filename),layer = "homogeneous_areas" )

allplots=st_read(file.path("data",gpkgfile)) %>%
  mutate(sampled_site=old_plot%in%sites$Plot, #identify which have site data
         sampled_cover=old_plot%in%data$Plot,
         sampled_homogeneous=old_plot%in%homogeneous_areas$plot) #identify which have cover data

#table(allplots$sampled_cover,allplots$sampled_site)

# Data upload

# upload summary data to release

data_filename=paste0("bioscape_vegetation_data_",tag,".csv")

tag=paste0("v",gsub("-","",lubridate::today()))
repo="BioSCape-io/BioSCape-terrestrial"
readr::write_csv(data,file.path("data",data_filename))

# if release doesn't exist for this tag - create it
if(!any(tag%in%pb_releases(repo)$tag_name))  pb_new_release(repo = repo,tag=tag)

pb_upload(file = file.path("data",kmlfile),
          repo="BioSCape-io/BioSCape-terrestrial",
          tag=tag)

pb_upload(file = file.path("data",gpkgfile),
          repo="BioSCape-io/BioSCape-terrestrial",
          tag=tag)


pb_upload(file = file.path("data",data_filename),
          repo="BioSCape-io/BioSCape-terrestrial",
          tag=tag)


