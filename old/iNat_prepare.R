
# Template
# id,taxon_name,date_obs,time_zone,description,tag_list,latitude,longitude,location,pos_acc,geoprivacy,field_id1,field_value1,field_id2,field_value2,field_id3,field_value3,field_id4,field_value4,field_id5,field_value5,field_id6,field_value6,media_name_1,media_name_2,media_name_3,media_name_4,media_name_5,media_name_6,media_name_7,media_name_8,media_name_9,media_name_10,media_name_11,media_name_12,media_name_13,media_name_14,media_name_15,media_name_16,media_name_17,media_name_18,media_name_19,media_name_20,media_name_21,media_name_22,media_name_23,media_name_24,media_name_25
# 1,Bitis arietans,12/05/2015,Johannesburg,test 1,none,-29.12345,20.12345,DS,4,open,,,,,,,,,,,,,x1.jpg,,,,,,,,,,,,,,,,,,,,,,,,
# 2,Nucras tesselata,01/12/2019,Johannesburg,test 2,none,-28.12345,19.12345,gbaf,7,open,,,,,,,,,,,,,none,,,,,,,,,,,,,,,,,,,,,,,,

library(tidyverse)

# first run Rarefaction code to generate 'data' product

data2 <- data %>%
  mutate(
    taxon_name=paste(genus,species)
  ) %>% 
  filter(photo_type=="species") %>% 
  group_by(plot_number,taxon_name) %>% 
  reframe(
    n_photos=n(),
    date_obs=date,
    time_zone="Johannesburg",
    tag_list="BioSCape",
    description=paste("BioSCape Vegetation Plot=",plot_number,
                      "; Description=",description,collapse=";"),
    latitude=mean(as.numeric(gps_latitude),na.rm=T),
    longitude=mean(as.numeric(gps_longitude),na.rm=T),
    altitude=mean(as.numeric(gps_altitude),na.rm=T),
    pos_acc=max(as.numeric(gps_position_error),na.rm=T),
    geoprivacy="open",
    media=paste(filename,collapse=";")
  ) |>
  mutate(
    id=1:n()
  ) |>
  distinct() |>
  separate(media,into=paste0("media_name_",1:10),sep=";") |>
  select(id,taxon_name,date_obs,time_zone,description,tag_list,latitude,longitude,pos_acc,geoprivacy,media_name_1,media_name_2,media_name_3,media_name_4,media_name_5)


View(data2)

#%>%
#  unpack(cols = descr)