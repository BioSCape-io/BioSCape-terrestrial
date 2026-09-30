

library(sf)
library(tidyverse)
# read in and process gpkgs



path <- "/Users/adamw/Library/CloudStorage/GoogleDrive-adammichaelwilson@gmail.com/Shared drives/BioSCape_Admin/GIS files/"

roi=st_read(file.path(path,"20230825_Team ROIs.gpkg")) %>% st_make_valid()
boxes=st_read(file.path(path,"20230905_G3_AVIRIS_PRISM_boxes.gpkg"))

# lines
#linefile=file.path(path,"20230718_FlightLines/20230718_FlightLines_hyplan_v0.kml")
#linelayers = grepl("AVIRIS",st_layers(linefile)$name,fixed = T)
#lines=st_read(linefile,layer = linelayers[1])



## Summarize PI area requested
roi_pi <- roi %>% 
  mutate(area = st_area(.)) %>% 
  group_by(team_PI) %>% 
  summarize(pi_total_area = set_units(sum(area),"km^2"))



# Clip ROIs to flight boxes, dissolve, and calculate areas
roi_pi_areas <- 
  st_intersection(roi, boxes) %>% 
  mutate(area = st_area(.)) %>% 
  group_by(box_nr, team_PI,priority) %>% 
  summarize(area = set_units(sum(area),"km^2")) %>% 
  left_join(st_set_geometry(roi_pi,NULL),by="team_PI") %>% 
  mutate(priority_score=case_when(
    priority=="high" ~ 3,
    priority=="medium" ~ 2,
    priority=="low" ~ 1
    ))
  


if(F){ # random plot just to check
  roi_pi_areas %>% 
    filter(box_nr%in%1:5) %>% 
    select(team_PI, priority, box_nr) %>% 
    plot()
}

## calculate priority metrics


