## Rasterize LVIS

library(terra)
library(tidyverse)
library(sf)
library(stars)
library(foreach)
doParallel::registerDoParallel()

cnames=c("LFID","SHOTNUMBER","TIME","GLON","GLAT","ZG","ZG_ALT1","ZG_ALT2","HLON","HLAT","ZH","TLON","TLAT","ZT","RH10","RH15","RH20","RH25","RH30","RH35","RH40","RH45","RH50","RH55","RH60","RH65","RH70","RH75","RH80","RH85","RH90","RH95","RH96","RH97","RH98","RH99","RH100","AZIMUTH","INCIDENTANGLE","RANGE","COMPLEXITY","SENSITIVITY","CHANNEL_ZT","CHANNEL_ZG","CHANNEL_RH")

files=list.files("~/Downloads/lvis/",pattern = "*.TXT$",full.names = T)




lv <- foreach(f=files,.combine=bind_rows) %dopar%{
      read_table(f,skip = 9,
             n_max = Inf,col_names = cnames) %>% 
    filter(!is.na(GLON)&!is.na(GLAT)) %>%
        mutate(LFID=as.numeric(LFID),
               SHOTNUMBER=as.numeric(SHOTNUMBER)) %>% 
    st_as_sf(coords = c("GLON","GLAT"))
}

# filter(lv,TLAT-33.1&TLON>18.15)|> #crop the cross flight
# select(LFID)|> st_set_geometry(NULL) |> distinct()
# 
 lv2 <- lv #|>
#   filter(TIME>46758)|>
#   filter(!LFID%in%c(1960243623, 1960243624, 1960243886, 1960243887, 1960243888))
# 
# plot(lv2$ZG)

template = rast(resolution=0.00015,ymin=-33.2630, ymax=-32.961, xmin=17.89588,xmax=18.19258, crs="EPSG:4326")

  r1<-  rasterize(lv2,template,field="RH95",fun=mean,filename="data/lvis_RH95.tif",overwrite=T) 
  r2<-  rasterize(lv,template,field="ZG",fun=mean,filename="data/lvis_ZG.tif",overwrite=T) 
  r3<-  rasterize(lv,template,field="ZH",fun=mean,filename="data/lvis_ZH.tif",overwrite=T) 
  r4<-  rasterize(lv,template,field="COMPLEXITY",fun=mean,filename="data/lvis_COMPLEXITY.tif",overwrite=T)  

  
  land=r2>32; plot(land)
  r1b <-  mask(r1,land,maskvalues=0,filename="data/lvis_RH95_land.tif",overwrite=T); plot(r1b)
  
  
  plot(r1b)


