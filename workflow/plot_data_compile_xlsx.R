# Script to semi-automate veg plot updating
# download the google sheets and assemble into a single table per data type
# update plot data with the plots that have been sampled.

library(googlesheets4)
library(tidyverse)
library(vegan)
#require(Rarefy)
library(sf)
library(piggyback)
library(units)
library(stringr)
library(doParallel)
library(purrr)
library(googledrive)
library(readxl)

registerDoParallel()


################################
### User settings
################################

if (Sys.getenv("USER") == "jasper") {gmail = "jasper.slingsby@uct.ac.za"}
if (Sys.getenv("USER") == "adam") {gmail = "adamw@buffalo.edu"}


################################
### Download plot location data
################################

### IS THIS STILL NEEDED???

tag="v20230905" #specify the most recent version of the plot kml

repo="BioSCape-io/BioSCape-terrestrial" #"BioSCape-io/BioSCape-terrestrial"
gpkgfile=paste0("bioscape_vegplots_",tag,".gpkg")

#pb_download(file = gpkgfile,repo = repo, dest=file.path("spatialdata"))
points=st_read(file.path("spatialdata",gpkgfile)) %>%
  mutate(plotnum=as.numeric(gsub("T","",plot)))

# tag = "v20240912"
# 
# points = st_read("spatialdata/BioSCapeSpatialProducts2024_09_12/BioSCapeVegCenters2024_09_12.shp") %>% 
#   mutate(plotnum=as.numeric(gsub("T","",BScpPID)))

################################
### Download data sheets
################################

# Define the URL or key of the Google Sheets
sheet_urls <- data.frame(rbind(
  c(sheet="Ross01",url="https://docs.google.com/spreadsheets/d/19RdI9SdqOx8zn6XBxywgUNgmAHJ6JatOjtTEK-VAcQE/edit#gid=266439266"), 
  c("Ross01b","https://docs.google.com/spreadsheets/d/12IYf_NobQfFlHBQTTZ3ZmuWdzklg-xlkJ5_-TZRcDug/edit#gid=266439266"), 
  c("Ross01c","https://docs.google.com/spreadsheets/d/1BrY4c1AWGjPYdtpLTFXLNjFk1z1elMtGlCsg0ylD44g/edit#gid=266439266"), 
  c("Bio02","https://docs.google.com/spreadsheets/d/1OEE2u7NmZ37a4y8iogjh8r0R22L-eVxV04ns8yqy91A/edit#gid=266439266"),
  c("Bio03","https://docs.google.com/spreadsheets/d/11S2nd_3RnbKTMDEo6fj67mnwBPnSIgiOpZBVTx287Tc/edit#gid=266439266"), 
  c("Bio04","https://docs.google.com/spreadsheets/d/113GkBpKVlcGeA8aoWyaN0lWiW4vbtgDokzD0xcjRquQ/edit#gid=266439266")))

# Authenticate and access the Google Sheet
drive_auth(email = gmail)
gs4_auth(token = drive_token())

# Download local .xlsx versions of the Google Sheets
foreach(i=1:nrow(sheet_urls)) %do% {
drive_download(sheet_urls[i,2], path = paste0("data/", Sys.Date(), sheet_urls[i,1], ".xlsx"), overwrite = TRUE)
}

################################
### Read in data sheets
################################

# Loop through sheets and assemble data
alldata <- foreach(i=1:nrow(sheet_urls)) %do% {

    # sheet_name=sheet_urls$sheet[i]
    # sheet_url=sheet_urls$url[i]
    
# For each excel workbook file as downloaded above
sheet <- paste0("data/", Sys.Date(), sheet_urls[i,1], ".xlsx")
    
# List names of all sheet tabs
sheets = excel_sheets(sheet)

  #Check
  #if(F) find_dups=read_sheet(sheet,"SiteData") %>% distinct(SiteCode_Plot) %>% arrange() %>% View()

# Read in site data (one sheet/tab for each workbook)
sitesheet=read_xlsx(sheet,"SiteData") %>% 
  mutate(sheet_name=sheet,
         Plot=as.numeric(sub("T","",Plot))) %>%
  filter(! Plot %in% c(275, 194, 139, 51, 53))


# Read in quadrat data (multiple sheets/tabs for each workbook)
# Filter sheet names using grep to identify only plot and drop template sheets
plot_sheets <- sheets[grepl("plot", sheets,ignore.case = T) & !grepl("Template_Plot|Template_plot|Example_plot", sheets) & 
                      #  !grepl("Swartberg_20_plot",sheets) & #swartberg 20 is empty - deleted from GoogleSheets
                        !grepl("Gardenroute_T275_plot",sheets) & #Adam Labuschagne's "dodgy" plots
                        !grepl("Gardenroute_T139_plot",sheets) & #Adam Labuschagne's "dodgy" plots
                        !grepl("Gardenroute_T051_plot",sheets) & #Adam Labuschagne's "dodgy" plots
                        !grepl("Gardenroute_T053_plot",sheets) & #Adam Labuschagne's "dodgy" plots
                        !grepl("Hawequas_194 Plot",sheets) & #Adam Labuschagne's "dodgy" plots
                        !grepl("test",sheets)
                      ] 

# Download data from the specified tabs as data frames
  # Apply filters, data format changes, etc as necessary
data_downloaded <- lapply(plot_sheets, 
    function(tab) {
    read_xlsx(sheet,tab) %>% 
    #select(-SeasonallyApparent,  #drop field causing problems - now fixed
    #       -NewSpecies,
    #       -MeanCanopyDiameter_cm) %>% 
    mutate(SiteCode_Plot_Quadrant = as.character(SiteCode_Plot_Quadrant)) %>%
    filter(!is.na(SiteCode_Plot_Quadrant)) %>%
    mutate(NameCheck = as.character(NameCheck)) %>%
    mutate(NewSpecies = as.character(NewSpecies)) %>%
    mutate(SeasonallyApparent = as.character(SeasonallyApparent))
    })

# merge quadrat data from list of multiple sheets into one dataframe (for this workbook)
data <- data_downloaded %>% 
  bind_rows() %>% 
  separate(SiteCode_Plot_Quadrant,into=c("SiteCode","Plot","Quadrant"),sep="_",remove = F) %>% 
  mutate("SiteCode_Plot"=paste(SiteCode,Plot,sep="_"),
         Plot=as.numeric(sub("T","",Plot)))  

# Get line intercept data (multiple sheets/tabs for each workbook)
# Filter sheet names using grep to identify only plot and drop template sheets
lineint_sheets <- sheets[grepl("lineintercept|LineIntercept|lineIntercept|Lineintercept", sheets,ignore.case = T) & 
                        !grepl("Template_LineIntercept|Template_lineintercept|Example", sheets) & 
                        #  !grepl("Swartberg_20_plot",sheets) & #swartberg 20 is empty - deleted from GoogleSheets
                        !grepl("Gardenroute_T275_plot",sheets) & #Adam Labuschagne's "dodgy" plots
                        !grepl("Gardenroute_T139_plot",sheets) & #Adam Labuschagne's "dodgy" plots
                        !grepl("Gardenroute_T051_plot",sheets) & #Adam Labuschagne's "dodgy" plots
                        !grepl("Gardenroute_T053_plot",sheets) & #Adam Labuschagne's "dodgy" plots
                        !grepl("Hawequas_194 Plot",sheets) & #Adam Labuschagne's "dodgy" plots
                        !grepl("test",sheets)
] 

# Download data from the specified tabs as data frames
# Apply filters, data format changes, etc as necessary
li_data_downloaded <- lapply(lineint_sheets, 
                          function(tab) {
                            read_xlsx(sheet,tab) %>% #, range = cell_rows(c(1, 3:46))) %>% # Skip second row and read up to 46 rows to avoid empty extras
                              #select(-SeasonallyApparent,  #drop field causing problems - now fixed
                              #       -NewSpecies,
                              #       -MeanCanopyDiameter_cm) %>% 
                              mutate(SiteCode_Plot_LineTransect = as.character(SiteCode_Plot_LineTransect)) %>%
                              rename(any_of(c(SiteCode_Plot = "6.0"))) %>%
                              filter(!is.na(SiteCode_Plot)) %>%
                              mutate(NameCheck = as.character(NameCheck))  %>%
                              mutate(Comments = as.character(Comments)) 
                           #  mutate(NewSpecies = as.character(NewSpecies)) %>%
                           #  mutate(SeasonallyApparent = as.character(SeasonallyApparent))
                          })

# merge line intercept data from list of multiple sheets into one dataframe (for this workbook)
li_data <- li_data_downloaded %>% 
  bind_rows() %>% 
  separate(SiteCode_Plot_LineTransect,into=c("SiteCode","Plot","LineTransect"),sep="_",remove = F) %>% 
  mutate(Plot=as.numeric(sub("T","",Plot)))  

###HERE###

# #Checks by sheet
# li_data |> filter(NameCheck == "#N/A" & !is.na(Genus_Species_Combo)) |> View() # Check taxonomy - IDs to genus only, a few Phylica that were not in the accepted name list for some reason, a moss, and a few new or indet species
# li_data |> filter(is.na(OtherCoverType) & is.na(Genus_Species_Combo)) |> View() # Check that all OtherCoverType = NA can be set to "LivePlant"
# sum(!summary(as.factor(li_data$SiteCode_Plot_LineTransect)) == 22) # Check that there are 22 obs per line transect
# which(!summary(as.factor(li_data$SiteCode_Plot_LineTransect)) == 22) #Had to separate into two sections for Doug's sheet (Bio4)
# sum(!summary(as.factor(li_data$SiteCode_Plot)) == 44) # Check that there are 44 obs per plot (i.e. both line transects) - probably redundant
# which(!summary(as.factor(li_data$SiteCode_Plot)) == 44)
# sum(!summary(as.factor(li_data$MetresAlongLine)) == 2*length(unique(li_data$Plot))) # Check that no intercept points were missed
# which(!summary(as.factor(li_data$MetresAlongLine)) == 2*length(unique(li_data$Plot)))

# Need to deal with "CapePeninsula_96" - test plot. Check if we should delete/ignore?
# Did Doug do all his line intercepts in one sheet??

list(sites=sitesheet,quaddata=data,linedata=li_data)

}


################################
### Combine site and quads lists into dataframes
################################

# Use map to extract and combine specific elements from the inner lists
sites <- map(alldata, function(x) x["sites"][[1]] %>% 
              mutate(PostFireAge_years = as.character(PostFireAge_years))#,
                #VegHeight_cm = as.character(VegHeight_cm),
                #Date = as.character(Date))
            ) %>%   # convert problem column to character due to varying inputs     #NEED FIX
  bind_rows() %>%
  mutate(across(starts_with("Site"), tolower)) %>%
  filter(!is.na(Plot))


quads <- map(alldata, function(x) x["quaddata"][[1]]) %>%  
#               select(-PostFireAge_years)) %>%   # drop problem column due to varying inputs         #NEED FIX
  bind_rows() %>%
  mutate(across(starts_with("Site"), tolower)) %>%
  filter(!is.na(Plot)) %>%
  mutate(SeasonallyApparent = as.numeric(case_match(SeasonallyApparent, c("1","yes","Y","Yes") ~ 1,
                                         c("no", "No", "N") ~ 0)),
         Clonal = as.numeric(case_match(Clonal_YesNo, c("yes","Yes") ~ 1,
                                                    c("no", "No") ~ 0)))


linetransects <- map(alldata, function(x) x["linedata"][[1]]) %>%  
  bind_rows() %>%
  mutate(across(starts_with("Site"), tolower))

#Checks within linetransect
linetransects |> filter(NameCheck == "#N/A" & !is.na(Genus_Species_Combo)) |> View() # Check taxonomy - IDs to genus only, a few Phylica that were not in the accepted name list for some reason, a moss, and a few new or indet species
linetransects |> filter(is.na(OtherCoverType) & is.na(Genus_Species_Combo)) |> View() # Check that all OtherCoverType = NA can be set to "LivePlant"
sum(!summary(as.factor(linetransects$SiteCode_Plot_LineTransect)) == 22) # Check that there are 22 obs per line transect
which(!summary(as.factor(linetransects$SiteCode_Plot_LineTransect)) == 22) #Had to separate into two sections for Doug's sheet (Bio4)
sum(!summary(as.factor(linetransects$SiteCode_Plot)) == 44) # Check that there are 44 obs per plot (i.e. both line transects) - probably redundant
which(!summary(as.factor(linetransects$SiteCode_Plot)) == 44)
sum(!summary(as.factor(linetransects$MetresAlongLine)) == 2*length(unique(linetransects$Plot))) # Check that no intercept points were missed
which(!summary(as.factor(linetransects$MetresAlongLine)) == 2*length(unique(linetransects$Plot)))


#Checks between dataframes
sort(unique(quads$SiteCode_Plot)[-which(unique(quads$SiteCode_Plot) %in% unique(linetransects$SiteCode_Plot))])
sort(unique(linetransects$SiteCode_Plot)[-which(unique(linetransects$SiteCode_Plot) %in% unique(quads$SiteCode_Plot))])

################################
### Species names check
################################

# pull out incorrect genus or species or combo for botanists to check...
accnames=read_xlsx(sheet,"AcceptedSpecies")
quads <- quads %>% mutate(Taxon = paste(AcceptedGenus, AcceptedSpecies, sep = " "))

match_gen <- sprintf("\\b(%s)\\b", paste(accnames$Genus, collapse = "|"))
match_spp <- sprintf("\\b(%s)\\b", paste(accnames$Species, collapse = "|"))
match_nms <- sprintf("\\b(%s)\\b", paste(accnames$Taxon, collapse = "|"))

hmm <- quads %>% 
  mutate(GenFlag = ifelse(str_detect(AcceptedGenus, match_gen), "Yes", "No")) %>%
  mutate(SppFlag = ifelse(str_detect(AcceptedSpecies, match_spp), "Yes", "No")) %>%
  mutate(TaxFlag = ifelse(str_detect(Taxon, match_nms), "Yes", "No")) %>%
  filter(GenFlag != "Yes" | SppFlag != "Yes" | TaxFlag != "Yes") %>%
  left_join(sites[,c("SiteCode_Plot", "Observer")], relationship = "many-to-many") %>%
  filter(!Taxon %in% c("Phylica ericoides", 
  "Phylica imberbis", 
  "Phylica lanata", 
  "Phylica plumosa", 
  "Phylica lasiocarpa", 
  "Phylica rigidifolia")) %>%
  unique() #

hmm %>% View() #There will be a bunch...
  #write_sheet(ss = "https://docs.google.com/spreadsheets/d/1xfCWp8bhqz_HRFBjAlUFV6I_kFpGpUB4UleeOQz5bhw/edit#gid=0", sheet = as.character(Sys.Date()))


# some EDA - unmatched taxonomy, number of plots, number of species
if(F){  
quads %>%
  group_by(Plot,NameCheck,Genus_Species_combo) %>%
  summarize(percent_cover=sum(PercentCoverAlive)/4) %>% # This was just "mean(PercentCoverAlive) in previous version"
  arrange(Plot,desc(percent_cover)) %>%
  filter(NameCheck == "#N/A") %>% View()

  length(unique(quads$Plot))
  length(unique(quads$Genus_Species_combo))
  }

  
################################
### Update plots to new numbers
################################
# the original points had some duplicated numbers that were replaced
# this section updates those numbers to the new system

# get plots to update
numchange = points %>%
  mutate(old_plotnum=as.numeric(unlist(regmatches(old_plot, gregexpr("[0-9]+\\.?[0-9]*", old_plot))))) %>% 
  filter(plotnum!=old_plotnum) 


# EDA on plot renumbering
if(F){ 
sites %>% 
  select(Plot,SiteCode) %>% 
  distinct() %>% 
  arrange(Plot) %>% 
  View()

s1=unique(sites$Plot)[unique(sites$Plot)%in%numchange$old_plotnum] %>% sort()
s1

sites %>% filter(Plot%in%s1) %>% select(SiteCode, Plot) %>%  distinct() %>%  arrange(Plot)
}


# update specific plot numbers to the new scheme
sites2 <- sites %>% 
  mutate(old_plot=Plot) %>% 
  select(-Plot) %>% 
  mutate(Plot=case_when(  #update plot numbers
    #old_plot==20&grepl("swartberg",SiteCode,ignore.case=T) ~ 110, #Empty plot - deleted from GoogleSheets
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
  #select(old_plot,Plot,Plot2,SiteCode_Plot_Quadrant)
  #select(SiteCode,Plot,SiteCode_Plot,Quadrant,SiteCode_Plot_Quadrant,geom) %>% 
  #st_as_sf()


# update quad plot numbers to the new scheme
quads2 <- quads %>% 
  mutate(old_plot=Plot) %>% 
  select(-Plot) %>% 
  mutate(Plot=case_when(  #update plot numbers
    #old_plot==20&grepl("swartberg",SiteCode,ignore.case=T) ~ 110, #Empty plot - deleted from GoogleSheets
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


################################
### CheckSums
################################

if(F){ # EDA
  # confirm all plots match in site and quad data
  complete_sites=unique(sites2$Plot) %>% sort()
  complete_quadsites=unique(quads2$Plot) %>% sort()
  complete_sites[!complete_sites%in%complete_quadsites]
  complete_quadsites[!complete_quadsites%in%complete_sites]

  checkq="T009"  
  filter(points,plot==checkq)
  filter(quads2,old_plot==9)#Plot==checkq)
  
  table(sites2$Plot%in%quads2$Plot) # Are all site names in the quadrat data? Should be TRUE
  table(quads2$Plot%in%sites2$Plot) # Are all quadrat names in the site data? Should be TRUE
  
  sum(duplicated(sites2$SiteCode_Plot_Quadrant)) # Find plots that were sampled by multiple botanists...
  sites2[which(duplicated(sites2$SiteCode_Plot_Quadrant)),] %>% View()
}
# quads2 %>% mutate(plotnum=as.numeric(sub("T","",Plot))) %>% filter(plotnum!=old_plot) %>% select(SiteCode_Plot_Quadrant,Plot,plotnum, old_plot)

################################
### Other data checks
################################

## Does the sum of the dominant species covers (from quads) correlate with the total estimated veg cover (from sites)? Label plots by Plot number.

quads2 %>% group_by(Plot, Quadrant) %>% summarise(cover=sum(PercentCoverAlive)) %>% 
  left_join(sites2 %>% select(Plot,Quadrant,PercentLiveVegetation)) %>% 
  group_by(Plot) %>% summarise(cover=mean(cover), veg=mean(PercentLiveVegetation)) %>%
  ggplot(aes(x=cover,y=veg, label = Plot)) + 
  geom_point() + 
  geom_abline() + 
  geom_text()

## Does the square of the mean diameter multiplied by the abundance correlate with the cover? Label plots by Plot number.

quads2 %>% mutate(area = AbundanceAlive_count*pi*(MeanCanopyDiameter_cm/2)^2) %>%
  ggplot(aes(x=PercentCoverAlive,y=area, label = Plot)) + 
  geom_point() + 
  geom_abline() + 
  geom_text()
  
quads2 %>% mutate(perc_area = (AbundanceAlive_count*pi*(MeanCanopyDiameter_cm/2)^2)/7854) %>%
  group_by(Plot, Quadrant) %>% summarise(perc_area=sum(perc_area)) %>% 
  left_join(sites2 %>% select(Plot,Quadrant,PercentLiveVegetation)) %>% 
  group_by(Plot) %>% summarise(veg=mean(PercentLiveVegetation), perc_area=sum(perc_area)) %>%
  filter(!Plot == "T149") %>%
  ggplot(aes(x=veg,y=perc_area, label = Plot)) + 
  geom_point() + 
  geom_abline() + 
  geom_text()
  
  
  
# quads2 %>% group_by(Plot, Taxon) %>% 
#   summarise(diam=mean(MeanCanopyDiameter_cm),abund=sum(AbundanceAlive_count)) %>% 
#   mutate(area = abund*pi()*(diam/2)^2) %>%
#   left_join(sites2 %>% select(Plot,Quadrant,PercentLiveVegetation)) %>% 
#   group_by(Plot) %>% summarise(diam=mean(diam),abund=mean(abund), veg=mean(PercentLiveVegetation)) %>%
#   ggplot(aes(x=diam*abund,y=veg, label = Plot)) + 
#   geom_point() + 
#   geom_abline() + 
#   geom_text()


################################
### Write files
################################

# date tag for filenames and the release
tag=paste0("v",gsub("-","",lubridate::today()))


# write site data at quad level
f_quadrat_summary=file.path("output",paste0("bioscape_veg_quadrat_summary_",tag,".csv"))

sites2 %>% 
  write_csv(f_quadrat_summary)


# write site data at site level (currently with old spatial data)
f_plot_summary=file.path("output",paste0("bioscape_veg_plot_summary_",tag,".csv"))

sites2 %>% 
  group_by(Plot,SiteCode,SiteCode_Plot,GPS_PlotCentre,Observer) %>% 
  summarize(PercentBareSoil=mean(PercentBareSoil,na.rm=T),
            PercentBareRock=mean(PercentBareRock,na.rm=T),
            PercentDeadVegetation=mean(PercentDeadVegetation,na.rm=T),
            PercentLiveVegetation=mean(PercentLiveVegetation,na.rm=T),
            VegHeightMean_cm=mean(VegHeight_cm,na.rm=T),
            SoilDepth_cm=mean(SoilDepth_cm),
            Groundwater=first(Groundwater),
            Access_Notes=first(Access_Notes),
            sampled=1) %>%
  arrange(Plot) %>% 
#  left_join(select(points,Plot=plot,nearest_reserve,geom)) 
  write_csv(f_plot_summary)


# write species data at quadrat level
f_quadrat_species=file.path("output",paste0("bioscape_veg_quadrat_species_",tag,".csv"))

quads2 %>% 
  arrange(Plot) %>% 
  write_csv(f_quadrat_species)


# write species data summarised to site level - "Dominant Summary" -  mean cover, etc. for each plot
f_plot_species=file.path("output",paste0("bioscape_veg_plot_species_",tag,".csv"))

quads2 %>% 
  group_by(Plot,SiteCode,SiteCode_Plot,AcceptedGenus,AcceptedSpecies,NameCheck,Genus_Species_combo) %>%
  summarize(PercentCoverAlive=sum(PercentCoverAlive)/4,
            PercentCoverDead=sum(PercentCoverDead)/4,
            AbundanceAlive_count=sum(AbundanceAlive_count),
            AbundanceDead_count=sum(AbundanceDead_count),
            MeanCanopyDiameter_cm=mean(MeanCanopyDiameter_cm, na.rm = T),
            Clonal=first(Clonal_YesNo),
            SeasonallyApparent=max(SeasonallyApparent, na.rom = T)
            ) %>%
  arrange(Plot,desc(PercentCoverAlive)) %>% 
  write_csv(f_plot_species)


################################
### Write site data in other spatial formats?
################################
# geopackage of plot data
# kml of plot data


################################
### Data upload to GitHub release
################################

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

# plot_filename=paste0("bioscape_plotpolygons_",tag,".gpkg")
# pb_download(file = plot_filename,
#           repo="BioSCape-io/BioSCape-terrestrial",
#           tag=tag,dest = "data")
# 
# homogeneous_areas=st_read(file.path("data",plot_filename),layer = "homogeneous_areas" )
# 
# allplots=st_read(file.path("data",gpkgfile)) %>%
#   mutate(sampled_site=old_plot%in%sites$Plot, #identify which have site data
#          sampled_cover=old_plot%in%data$Plot,
#          sampled_homogeneous=old_plot%in%homogeneous_areas$plot) #identify which have cover data
# 

