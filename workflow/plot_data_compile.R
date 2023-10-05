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
library(doParallel)
library(purrr)


registerDoParallel()

######### Plot kml
# download plot location data kml

tag="v20230905" #specify the most recent version of the plot kml

repo="BioSCape-io/BioSCape-terrestrial"
gpkgfile=paste0("bioscape_vegplots_",tag,".gpkg")

pb_download(file = gpkgfile,repo = repo,dest=file.path("data"))
points=st_read(file.path("data",gpkgfile)) %>% 
  mutate(plotnum=as.numeric(gsub("T","",plot)))



# Define the URL or key of the Google Sheet
sheet_urls <- data.frame(rbind(
  c(sheet="Ross01",url="https://docs.google.com/spreadsheets/d/19RdI9SdqOx8zn6XBxywgUNgmAHJ6JatOjtTEK-VAcQE/edit#gid=266439266"), 
  c("Ross01b","https://docs.google.com/spreadsheets/d/12IYf_NobQfFlHBQTTZ3ZmuWdzklg-xlkJ5_-TZRcDug/edit#gid=266439266"), 
  c("Bio02","https://docs.google.com/spreadsheets/d/1OEE2u7NmZ37a4y8iogjh8r0R22L-eVxV04ns8yqy91A/edit#gid=266439266"),
  c("Bio03","https://docs.google.com/spreadsheets/d/11S2nd_3RnbKTMDEo6fj67mnwBPnSIgiOpZBVTx287Tc/edit#gid=266439266"), 
  c("Bio04","https://docs.google.com/spreadsheets/d/113GkBpKVlcGeA8aoWyaN0lWiW4vbtgDokzD0xcjRquQ/edit#gid=266439266")))


### Data sheets
# Authenticate and access the Google Sheet
gs4_auth(email = "adamw@buffalo.edu")

## loop through sheets and assemble data
alldata <- foreach(i=1:nrow(sheet_urls)) %do% {

    sheet_name=sheet_urls$sheet[i]
    sheet_url=sheet_urls$url[i]
    
    sheet <- gs4_get(sheet_url)

# Filter only plot tabs
sheets = sheet_names(sheet)

#if(F) find_dups=read_sheet(sheet,"SiteData") %>% distinct(SiteCode_Plot) %>% arrange() %>% View()

# site data
sitesheet=read_sheet(sheet,"SiteData") %>% 
  mutate(sheet_url=sheet_url,
         sheet_name=sheet_name,
         Plot=as.numeric(sub("T","",Plot)))


### Quadrat data
# Filter using grep to identify only plot and drop template sheets
plot_sheets <- sheets[grepl("plot", sheets) & !grepl("Template_Plot|Template_plot|Example_plot", sheets) & 
                        !grepl("Swartberg_20_plot",sheets) & #swartberg 20 is empty
                        !grepl("Gardenroute_T275_plot",sheets)
                      ] 

# Download data from the specified tabs as data frames
data_downloaded <- lapply(plot_sheets, function(tab) {
read_sheet(sheet,tab) %>% 
    select(-SeasonallyApparent,
           -NewSpecies,
           -MeanCanopyDiameter_cm) %>%  #drop field causing problems with weird entry in Rondevlei_147
    filter(!is.na(SiteCode_Plot_Quadrant))
    })

# merge site data and plot-level data
data <- data_downloaded %>% 
  bind_rows() %>% 
  separate(SiteCode_Plot_Quadrant,into=c("SiteCode","Plot","Quadrant"),sep="_",remove = F) %>% 
  mutate("SiteCode_Plot"=paste(SiteCode,Plot,sep="_"),
         Plot=as.numeric(sub("T","",Plot))) %>% 
  left_join(sitesheet,by=c("SiteCode_Plot_Quadrant","SiteCode","SiteCode_Plot","Plot","Quadrant") ) 

### Transect data

list(sites=sitesheet,quaddata=data)

}


# Use map to extract and combine specific elements from the inner lists
allsites <- map(alldata, function(x) x["sites"][[1]] %>%  
                  select(-PostFireAge_years)) %>%   # drop problem column due to varing inputs
  bind_rows()


quaddata=map(alldata, function(x) x["quaddata"][[1]] %>%  
               select(-PostFireAge_years)) %>%   # drop problem column due to varing inputs
  bind_rows()

#OLD STUFF BELOW
##################################



#########  Update plot locations with current status
# download plot polygons that were manually uploaded to github releases

plot_filename=paste0("bioscape_plotpolygons_",tag,".gpkg")
pb_download(file = plot_filename,
          repo="BioSCape-io/BioSCape-terrestrial",
          tag=tag,dest = "data")

homogeneous_areas=st_read(file.path("data",plot_filename),layer = "homogeneous_areas" )

allplots=st_read(file.path("data",gpkgfile)) %>%
  mutate(sampled_site=old_plot%in%sites$Plot, #identify which have site data
         sampled_cover=old_plot%in%data$Plot,
         sampled_homogeneous=old_plot%in%homogeneous_areas$plot) #identify which have cover data

## summarize which plots have updated plot numbers:
allplots %>% 
  mutate(plot2=as.numeric(as.character(gsub("T","",plot)))) %>% 
  filter(old_plot!=plot2)

#table(allplots$sampled_cover,allplots$sampled_site)

### Clean up new/old plot numbers

# get plots to update
numchange = points %>%
  mutate(old_plotnum=as.numeric(unlist(regmatches(old_plot, gregexpr("[0-9]+\\.?[0-9]*", old_plot))))) %>% 
  filter(plotnum!=old_plotnum) 

sites= sitesheet %>% 
  mutate(old_plot=Plot) %>% 
  select(-Plot) %>% 
  ##########
# The next section renames plots that had to be adjusted due to "J" prefix and duplicate numbers
mutate(plot=case_when(
  old_plot==22&SiteCode=="swartberg" ~ 12,
  old_plot==23&SiteCode=="swartberg" ~ 13,
  old_plot==24&SiteCode=="swartberg" ~ 14,
  TRUE ~ old_plot
)) %>% 
  left_join(points,by=c("plot"="plotnum","old_plot")) 





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


