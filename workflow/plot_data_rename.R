# Script to semi-automate veg plot renaming
# this was to update plot numbers following irregular editing of plot names (adding "J" and other prefixes)
# this shouldn't need to be rerun.

library(googlesheets4)
library(tidyverse)
library(vegan)
require(Rarefy)
library(sf)
library(piggyback)
library(units)
library(stringr)

######### Plot kml
# download plot location data kml
#used https://sites.google.com/site/gdocs2direct/ to reformat link
download.file("https://drive.google.com/uc?export=download&id=1LeoD3lxjJ_v9fl4CckYBxSzvXGnyhGyy",
              destfile="data/points.kml")

allplots=st_read("data/points.kml")%>%
  mutate(plotnum=as.numeric(unlist(regmatches(Name, gregexpr("[0-9]+\\.?[0-9]*", Name)))),
         plotchar=sprintf("%03d", plotnum)) %>% 
  rename(oldnum=Name)


# check for duplicate names
dups=table(as.character(allplots$plotchar))
table(dups)

allplots$dup=allplots$plotchar%in%names(dups[dups>1])
allplots$rename=allplots$dup&grepl("[A-Za-z]",allplots$oldnum)

# find numbers not used in plot names for use in renaming 
plotns=1:max(allplots$plotnum)
unused_numbers=sprintf("%03d",plotns[!plotns%in%as.numeric(allplots$plotchar[!allplots$rename])])


allplots$newnum=allplots$plotchar           
allplots$newnum[allplots$rename]=unused_numbers[1:sum(allplots$rename)] 

# get park name
parks=st_read("https://drive.google.com/uc?export=download&id=1_wrvY-5AQdNDtyBmkdlIuuRjlT1j_988") %>% 
  st_transform(st_crs(allplots)) %>% 
  st_make_valid() %>% 
  select(nearest_reserve=RESERVENAM)


#filename=paste0("BioSCape_vegplots_v20230904",".kml")


plots <- 
  allplots %>% 
  mutate(plot=paste0("T",newnum)) %>% 
  arrange(plot) %>% 
  select(plot,description=Description, old_plot=oldnum,-plotnum,-plotchar,-dup,-newnum,-rename)%>%
  st_join(parks,st_nearest_feature)



# check for duplicates in new names
dups=table(as.character(plots$plot))
table(dups)


# Data upload

# upload summary data to release

tag=paste0("v",gsub("-","",lubridate::today()))
repo="BioSCape-io/BioSCape-terrestrial"


kmlfile=paste0("bioscape_vegplots_",tag,".kml")
st_write(plots,file.path("data",kmlfile),append=F)


gpkgfile=paste0("bioscape_vegplots_",tag,".gpkg")
st_write(plots,file.path("data/",gpkgfile),append=F)


# if release doesn't exist for this tag - create it
if(!any(tag%in%pb_releases(repo)$tag_name))  pb_new_release(repo = repo,tag=tag)

pb_upload(file = file.path("data",kmlfile),
          repo="BioSCape-io/BioSCape-terrestrial",
          tag=tag)

pb_upload(file = file.path("data",gpkgfile),
          repo="BioSCape-io/BioSCape-terrestrial",
          tag=tag)

# Last tag run was "v20230905"
