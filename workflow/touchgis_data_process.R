# Script to semi-automate veg plot gis data cleaning


library(tidyverse)
library(sf)
library(piggyback)
library(units)
library(stringr)
library(lubridate)

sf_use_s2(FALSE) #avoids problems with some polygons

touchgis_file_tag="v2023.08.24" #which version of touchgis files to use? https://github.com/BioSCape-io/BioSCape-terrestrial/releases


######### Plot kml
# download plot location data kml

tag="v20230905" #specify the most recent version of the plot kml

repo="BioSCape-io/BioSCape-terrestrial"
gpkgfile=paste0("bioscape_vegplots_",tag,".gpkg")

pb_download(file = gpkgfile,repo = repo,dest=file.path("data"))
points=st_read(file.path("data",gpkgfile))


############
# download and process touchGIS files

touchgisprocess <- function(
              file = "BioSCape.Western.Cape.zip",
              dest = "data",
              repo="BioSCape-io/BioSCape-terrestrial",
              ha_polygon="Blue Polygon_Polygons.shp",
              parking_points="Green Point_Points.shp",
              plot_points="Blue Point_Points.shp",
              tag=touchgis_file_tag){
  
      
      wd=file.path(dest,"touchgis",sub(".zip","",file)) #extract path

      pb_download(file=file,dest=dest,repo=repo,tag=tag)
      unzip(file.path(dest,file),exdir = wd)
      
      homogeneous_areas=st_read(file.path(wd,ha_polygon))%>%
        #deal with issues like irregular names
        mutate(Name=ifelse(file=="BioSCape.Western.Cape.zip" & Name=="Blue Polygon 1","Swartberg_27_Polygon",Name))%>% 
        mutate(Name=ifelse(file=="Peninsula.Sites.2023.zip" & Name=="Yellow Polygon 1","CapePoint_85_Polygon",Name))%>% 
        mutate(Name=ifelse(file=="Peninsula.Sites.2023.zip" & Name=="Yellow Polygon 2","CapePoint_266_Polygon",Name))%>% 
        mutate(Name=ifelse(file=="Peninsula.Sites.2023.zip" & Name=="Yellow Polygon 3","CapePoint_79_Polygon",Name))%>% 
        mutate(Name=ifelse(file=="Peninsula.Sites.2023.zip" & Name=="CapePoint_plot_68_polygon","CapePoint_68_Polygon",Name))%>% 
        mutate(Name=ifelse(file=="Peninsula.Sites.2023.zip" & Name=="CapePoint_76_rooikrans_polygon ","CapePoint_76_Polygon",Name))%>% 
        filter(Name!="Yellow Polygon")%>% 
        mutate(Name=ifelse(file=="Cederberg_Boland_sites_28April2023.zip" & 
                             Name=="Cederberg 171 polygon","Cederberg_171_polygon",Name))%>% 
        mutate(Name=ifelse(file=="Cederberg_Boland_sites_28April2023.zip" & 
                             Name=="Cederberg 289 polygon","Cederberg_289_polygon",Name))%>% 
        mutate(Name=ifelse(file=="Cederberg_Boland_sites_28April2023.zip" & 
                             Name=="Cederberg_169_new_polygon","Cederberg_169_polygon",Name))%>% 
                # extract plot number
        mutate(plot=as.numeric(unlist(str_extract_all(Name, "(?<=_)[0-9]+(?=_)")))) %>% 
        #extract dates
        mutate(datetime=as.Date(Date...Tim,format="%d %b %Y at %H:%M:%S"))%>%
        select(-Descriptio, -Date...Tim)%>%
        st_buffer(dist = 0)%>%
        st_make_valid()%>%
        mutate(homogeneous_area_ha=set_units(st_area(.),"ha"))
        
      parking=st_read(file.path(wd,parking_points))%>%
        # fix any issues with naming
        mutate(Name=ifelse(file=="BioSCape.Western.Cape.zip" & Name=="Turnoff_to_Rondevlei_147","Rondevlei_147_turnoff",Name))%>% 
        mutate(Name=ifelse(file=="Cederberg_Boland_sites_28April2023.zip" & Name=="p169_park","Cederberg_169_parking",Name))%>% 
        # extract plot numbers
        mutate(plot=as.numeric(unlist(str_extract_all(Name, "(?<=_)[0-9]+(?=_)"))))%>%
        #extract dates
        mutate(datetime=as.Date(Date...Tim,format="%d %b %Y at %H:%M:%S"))%>%
        select(-Date...Tim)%>%
        # extract plot notes
        mutate(notes=unlist(str_extract_all(Name, "(?<=_)[^_]+$"))) #%>%
#        st_join(hz,join=st_nearest_feature)

      plots=st_read(file.path(wd,plot_points))%>%
        # fix any issues with naming
        mutate(Name=ifelse(file=="BioSCape.Western.Cape.zip" & Name=="Plot_161","Rondevlei_161_plot",Name))%>% 
        mutate(Name=ifelse(file=="BioSCape.Western.Cape.zip" & Name=="Plot_149","Rondevlei_149_plot",Name))%>% 
        mutate(Name=ifelse(file=="Peninsula.Sites.2023.zip" & Name=="CapePoint_74_new","CapePoint_74",Name))%>% 
        rowwise()%>%
        mutate(Name=ifelse(file=="Peninsula.Sites.2023.zip",paste(Name,"_plot",sep=""),Name))%>% 
        #Cederberg
        mutate(Name=ifelse(file=="Cederberg_Boland_sites_28April2023.zip" & Name=="CB 171 new","Cederberg_171_plot",Name))%>% 
        mutate(Name=ifelse(file=="Cederberg_Boland_sites_28April2023.zip" & Name=="Cederberg_242","Cederberg_242_plot",Name))%>% 
        mutate(Name=ifelse(file=="Cederberg_Boland_sites_28April2023.zip" & Name=="Cederberg plot 289","Cederberg_289_plot",Name))%>% 
        mutate(Name=ifelse(file=="Cederberg_Boland_sites_28April2023.zip" & Name=="Cederberg_243","Cederberg_243_plot",Name))%>% 
        mutate(Name=ifelse(file=="Cederberg_Boland_sites_28April2023.zip" & Name=="Cederberg_290","Cederberg_290_plot",Name))%>% 
        mutate(Name=ifelse(file=="Cederberg_Boland_sites_28April2023.zip" & Name=="Cederberg_240","Cederberg_240_plot",Name))%>% 
        # extract plot numbers
        mutate(plot=as.numeric(unlist(str_extract_all(Name, "(?<=_)[0-9]+(?=_)"))))%>%
        #extract dates
        mutate(datetime=as.Date(Date...Tim,format="%d %b %Y at %H:%M:%S"))%>%
        select(-Date...Tim) %>% 
        mutate(sampled=T,
               homogeneous_zone_collected=plot%in%homogeneous_areas$plot,
               parking_collected=plot%in%parking$plot)
      
      

      return(list(ha=homogeneous_areas,pa=parking,pl=plots))
}


wc=touchgisprocess(
    file = "BioSCape.Western.Cape.zip",
    dest = "data",
    repo="BioSCape-io/BioSCape-terrestrial",
    ha_polygon="Blue Polygon_Polygons.shp",
    parking_points="Green Point_Points.shp",
    plot_points="Blue Point_Points.shp",
    tag=touchgis_file_tag)
  
cp=touchgisprocess(
  file = "Peninsula.Sites.2023.zip",
  dest = "data",
  repo="BioSCape-io/BioSCape-terrestrial",
  ha_polygon="Yellow Polygon_Polygons.shp",
  parking_points="Green Point_Points.shp",
  plot_points="Red Point_Points.shp",
  tag=touchgis_file_tag)

cederberg=touchgisprocess(
  file = "Cederberg_Boland_sites_28April2023.zip",
  dest = "data",
  repo="BioSCape-io/BioSCape-terrestrial",
  ha_polygon="Blue Polygon_Polygons.shp",
  parking_points="Green Point_Points.shp",
  plot_points="Blue Point_Points.shp",
  tag=touchgis_file_tag)

# rbind the parts
ha=bind_rows(wc$ha,cp$ha,cederberg$ha)
parking=bind_rows(wc$pa,cp$pa,cederberg$pa)
plots=bind_rows(wc$pl,cp$pl,cederberg$pl)

# write it all out in a single, multi-layer geopackage

tag=paste0("v",format(lubridate::today(),"%Y%m%d"))
  
plot_filename=paste0("bioscape_plotpolygons_",tag,".gpkg")
dest="data"
print(paste("Saving ",plot_filename))

file.remove(file.path(dest,plot_filename))

st_write(ha,dsn = file.path(dest,plot_filename),layer="homogeneous_areas")
st_write(parking,dsn = file.path(dest,plot_filename),layer="parking")
st_write(plots,dsn = file.path(dest,plot_filename),layer="plots")


# if release doesn't exist for this tag - create it
if(!any(tag%in%pb_releases(repo)$tag_name))  pb_new_release(repo = repo,tag=tag)

pb_upload(file = file.path(dest,plot_filename),
          repo="BioSCape-io/BioSCape-terrestrial",
          tag=tag)

