# Script to semi-automate veg plot renaming
# this was to update plot numbers following irregular editing of plot names (adding "J" and other prefixes)
# this shouldn't need to be rerun.
library(tidyverse)
library(sf)


st_layers("data/bioscape_vegplots_v20230828_botanists.gpkg")
plots=st_read("data/bioscape_vegplots_v20230828_botanists.gpkg")
plots=st_read("/Users/adamw/Library/CloudStorage/GoogleDrive-adammichaelwilson@gmail.com/Shared drives/BioSCape_Admin/VegPlots/GIS/PlotLocations/bioscape_vegplots_v20230828_botanists.gpkg")


# check botanist totals
plots %>% 
  group_by(proposed_observer) %>% 
  summarize(total_plots=n())%>% #, remaining=total_plots-sampled_plots) %>%  #,sampled_plots=sum(sampled)
  st_set_geometry(NULL) %>% 
  knitr::kable()



