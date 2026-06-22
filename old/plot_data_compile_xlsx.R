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
library(googledrive)
library(readxl)

registerDoParallel()

######### User settings

if (Sys.getenv("USER") == "jasper") {gmail = "jasper.slingsby@uct.ac.za"}
if (Sys.getenv("USER") == "adam") {gmail = "adamw@buffalo.edu"}

######### Plot kml
# download plot location data kml

tag="v20230828_botanists" #specify the most recent version of the plot kml

repo="BioSCape-io/BioSCape-terrestrial"
gpkgfile=paste0("bioscape_vegplots_",tag,".gpkg")

pb_download(file = gpkgfile,repo = repo,dest=file.path("spatialdata"))
points=st_read(file.path("spatialdata",gpkgfile)) %>% 
  mutate(plotnum=as.numeric(gsub("T","",plot)))


### Data sheets

# Define the URL or key of the Google Sheets
sheet_urls <- data.frame(rbind(
  c(sheet="Ross01",url="https://docs.google.com/spreadsheets/d/19RdI9SdqOx8zn6XBxywgUNgmAHJ6JatOjtTEK-VAcQE/edit#gid=266439266"), 
  c("Ross01b","https://docs.google.com/spreadsheets/d/12IYf_NobQfFlHBQTTZ3ZmuWdzklg-xlkJ5_-TZRcDug/edit#gid=266439266"), 
  c("Bio02","https://docs.google.com/spreadsheets/d/1OEE2u7NmZ37a4y8iogjh8r0R22L-eVxV04ns8yqy91A/edit#gid=266439266"),
  c("Bio03","https://docs.google.com/spreadsheets/d/11S2nd_3RnbKTMDEo6fj67mnwBPnSIgiOpZBVTx287Tc/edit#gid=266439266"), 
  c("Bio04","https://docs.google.com/spreadsheets/d/113GkBpKVlcGeA8aoWyaN0lWiW4vbtgDokzD0xcjRquQ/edit#gid=266439266")))

# Authenticate and access the Google Sheet
drive_auth(email = gmail)
gs4_auth(token = drive_token())

# Download local .xlsx versions of the Google Sheets
foreach(i=1:nrow(sheet_urls)) %do% {
drive_download(sheet_urls[i,2], path = paste0("data/",sheet_urls[i,1], ".xlsx"), overwrite = TRUE)
}

# Loop through sheets and assemble data
alldata <- foreach(i=1:nrow(sheet_urls)) %do% {

    # sheet_name=sheet_urls$sheet[i]
    # sheet_url=sheet_urls$url[i]
    
    sheet <- paste0("data/",sheet_urls[i,1], ".xlsx")

# Filter only plot tabs
sheets = excel_sheets(sheet)

#if(F) find_dups=read_sheet(sheet,"SiteData") %>% distinct(SiteCode_Plot) %>% arrange() %>% View()

# site data
sitesheet=read_xlsx(sheet,"SiteData") %>% 
  mutate(sheet_name=sheet,
         Plot=as.numeric(sub("T","",Plot)))


### Quadrat data
# Filter using grep to identify only plot and drop template sheets
plot_sheets <- sheets[grepl("plot", sheets,ignore.case = T) & !grepl("Template_Plot|Template_plot|Example_plot", sheets) & 
                        !grepl("Swartberg_20_plot",sheets) & #swartberg 20 is empty
                        !grepl("Gardenroute_T275_plot",sheets)
                      ] 

# Download data from the specified tabs as data frames
data_downloaded <- lapply(plot_sheets, function(tab) {
read_xlsx(sheet,tab) %>% 
    select(-SeasonallyApparent,  #drop field causing problems with weird entry in Rondevlei_147
          # -NewSpecies,
           -MeanCanopyDiameter_cm) %>% 
    filter(!is.na(SiteCode_Plot_Quadrant)) %>%
    mutate(NameCheck = as.character(NameCheck)) %>%
    mutate(NewSpecies = as.character(NewSpecies))
    })

# merge site data and plot-level data
data <- data_downloaded %>% 
  bind_rows() %>% 
  separate(SiteCode_Plot_Quadrant,into=c("SiteCode","Plot","Quadrant"),sep="_",remove = F) %>% 
  mutate("SiteCode_Plot"=paste(SiteCode,Plot,sep="_"),
         Plot=as.numeric(sub("T","",Plot)))  

# add transect data?

list(sites=sitesheet,quaddata=data)

}


# Use map to extract and combine specific elements from the inner lists
sites <- map(alldata, function(x) x["sites"][[1]] %>% 
              mutate(PostFireAge_years = as.character(PostFireAge_years),
                VegHeight_cm = as.character(VegHeight_cm),
                Date = as.character(Date))
             ) %>%   # convert problem column to character due to varying inputs     #NEED FIX
  bind_rows() %>%
  filter(!is.na(Plot))


quads=map(alldata, function(x) x["quaddata"][[1]]) %>%  
#               select(-PostFireAge_years)) %>%   # drop problem column due to varing inputs         #NEED FIX
  bind_rows()%>%
  filter(!is.na(Plot))



if(F){  # some EDA
quads %>%
  group_by(Plot,NameCheck,Genus_Species_combo) %>%
  summarize(percent_cover=mean(PercentCoverAlive)) %>%
  arrange(Plot,desc(percent_cover)) %>%
  filter(is.na(NameCheck)) %>% View()

  length(unique(quads$Plot))
  length(unique(quads$Genus_Species_combo))

  
  
  }


### Update to new numbers
## the original points had some duplicated numbers that were replaced
# this section updates those numbers to the new system

# get plots to update
numchange = points %>%
  mutate(old_plotnum=as.numeric(unlist(regmatches(old_plot, gregexpr("[0-9]+\\.?[0-9]*", old_plot))))) %>% 
  filter(plotnum!=old_plotnum) 


##########
if(F){ # EDA on plot renumbering
sites %>% 
  select(Plot,SiteCode) %>% 
  distinct() %>% 
  arrange(Plot) %>% 
  View()

s1=unique(sites$Plot)[unique(sites$Plot)%in%numchange$old_plotnum] %>% sort()
s1

sites %>% filter(Plot%in%s1) %>% select(SiteCode, Plot) %>%  distinct() %>%  arrange(Plot)

}


## update specific plot numbers to the new scheme
sites2 <- sites %>% 
  mutate(old_plot=Plot) %>% 
  select(-Plot) %>% 
  mutate(Plot=case_when(  #update plot numbers
    old_plot==20&grepl("swartberg",SiteCode,ignore.case=T) ~ 110,
    old_plot==22&grepl("swartberg",SiteCode,ignore.case=T) ~ 12,
    old_plot==23&grepl("swartberg",SiteCode,ignore.case=T) ~ 13,
    old_plot==24&grepl("swartberg",SiteCode,ignore.case=T) ~ 14,
  TRUE ~ old_plot
  ),
  Plot=paste0("T",sprintf("%03d", Plot)),
  SiteCode_Plot = paste(SiteCode,Plot,sep="_"),
  SiteCode_Plot_Quadrant = paste(SiteCode,Plot,Quadrant,sep="_"),
  ) %>% 
  select(Plot,SiteCode,SiteCode_Plot,SiteCode_Plot_Quadrant,Quadrant,everything()) %>% 
  arrange(Plot,Quadrant)# %>% 
#  select(old_plot,Plot,Plot2,SiteCode_Plot_Quadrant)
  #select(SiteCode,Plot,SiteCode_Plot,Quadrant,SiteCode_Plot_Quadrant,geom) %>% 
#  st_as_sf()


## update quad plot numbers to the new scheme
quads2 <- quads %>% 
  mutate(old_plot=Plot) %>% 
  select(-Plot) %>% 
  mutate(Plot=case_when(  #update plot numbers
    old_plot==20&grepl("swartberg",SiteCode,ignore.case=T) ~ 110,
    old_plot==22&grepl("swartberg",SiteCode,ignore.case=T) ~ 12,
    old_plot==23&grepl("swartberg",SiteCode,ignore.case=T) ~ 13,
    old_plot==24&grepl("swartberg",SiteCode,ignore.case=T) ~ 14,
    TRUE ~ old_plot
  ),
  Plot=paste0("T",sprintf("%03d", Plot)),
  SiteCode_Plot = paste(SiteCode,Plot,sep="_"),
  SiteCode_Plot_Quadrant = paste(SiteCode,Plot,Quadrant,sep="_"),
  ) %>% 
  select(Plot,SiteCode,SiteCode_Plot,SiteCode_Plot_Quadrant,Quadrant,everything()) %>% 
  arrange(Plot,Quadrant)


if(F){ # EDA
  # confirm all plots match in site and quad data
  complete_sites=unique(sites2$Plot) %>% sort()
  complete_quadsites=unique(quads2$Plot) %>% sort()
  complete_sites[!complete_sites%in%complete_quadsites]
  complete_quadsites[!complete_quadsites%in%complete_sites]

  checkq="T009"  
  filter(points,plot==checkq)
  filter(quads2,old_plot==9)#Plot==checkq)
  
  table(sites2$Plot%in%quads2$Plot)
  table(quads2$Plot%in%sites2$Plot)
}
# quads2 %>% mutate(plotnum=as.numeric(sub("T","",Plot))) %>% filter(plotnum!=old_plot) %>% select(SiteCode_Plot_Quadrant,Plot,plotnum, old_plot)

# date tag for filenames and the release
tag=paste0("v",gsub("-","",lubridate::today()))


# write site data at quad level
f_quadrat_summary=file.path("output",paste0("bioscape_veg_quadrat_summary_",tag,".csv"))

sites2 %>% 
  write_csv(f_quadrat_summary)

# site level
f_plot_summary=file.path("output",paste0("bioscape_veg_plot_summary_",tag,".csv"))

sites2 %>% 
  group_by(Plot,SiteCode,SiteCode_Plot,GPS_PlotCentre,Observer) %>% 
  summarize(PercentBareSoil=mean(PercentBareSoil,na.rm=T),
            PercentBareRock=mean(PercentBareRock,na.rm=T),
            PercentDeadVegetation=mean(PercentDeadVegetation,na.rm=T),
            PercentLiveVegetation=mean(PercentLiveVegetation,na.rm=T),
            SoilDepth_cm=mean(SoilDepth_cm),
            Groundwater=first(Groundwater),
            Access_Notes=first(Access_Notes),
            sampled=1) %>%
  arrange(Plot) %>% 
  left_join(select(points,Plot=plot,nearest_reserve,geom)) %>% 
  write_csv(f_plot_summary)


## Species level
f_quadrat_species=file.path("output",paste0("bioscape_veg_quadrat_species_",tag,".csv"))

quads2 %>% 
  arrange(Plot) %>% 
  write_csv(f_quadrat_species)

## Plot Dominant Summary -  mean cover, etc. for each plot
f_plot_species=file.path("output",paste0("bioscape_veg_plot_species_",tag,".csv"))

quads2 %>% 
  group_by(Plot,SiteCode,SiteCode_Plot,AcceptedGenus,AcceptedSpecies,NameCheck,Genus_Species_combo) %>%
  summarize(PercentCoverAlive=mean(PercentCoverAlive),
            PercentCoverDead=mean(PercentCoverDead),
            AbundanceAlive_count=sum(AbundanceAlive_count),
            AbundanceDead_count=sum(AbundanceDead_count)
            ) %>%
  arrange(Plot,desc(PercentCoverAlive)) %>% 
  write_csv(f_plot_species)

# geopackage of plot data

# kml of plot data



# Data upload
# upload summary data to release

repo="BioSCape-io/BioSCape-terrestrial"

# if release doesn't exist for this tag - create it
if(!any(tag%in%pb_releases(repo)$tag_name))  pb_new_release(repo = repo,tag=tag)

pb_upload(file = f_quadrat_summary,
          repo="BioSCape-io/BioSCape-terrestrial",
          tag=tag)

pb_upload(file = f_plot_summary,
          repo="BioSCape-io/BioSCape-terrestrial",
          tag=tag)

pb_upload(file = f_quadrat_species,
          repo="BioSCape-io/BioSCape-terrestrial",
          tag=tag)

pb_upload(file = f_plot_species,
          repo="BioSCape-io/BioSCape-terrestrial",
          tag=tag)

# 
#  pb_upload(file = file.path("data",gpkgfile),
#            repo="BioSCape-io/BioSCape-terrestrial",
#            tag=tag)
# 
#  pb_upload(file = file.path("data",gpkgfile),
#            repo="BioSCape-io/BioSCape-terrestrial",
#            tag=tag)
#  
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


