## Ross photo cleanup
# This was used to merge and clean the photos from Ross' original spreadsheet and the new batch of photos that were processed from the google drive folders.
# The new batch of photos was processed using the same code as the other botanists in the Botanist_photo_processing.qmd script, but with some adjustments to account for the fact that Ross's photos were in a different format and had some irregularities in the descriptions. 
# The final output of this script is a cleaned and merged spreadsheet of Ross' photos that was added to the existing Rarefaction_FA spreadsheet in a new tab.
# this should not be needed going forward because the data has already been updated.  But it's kept here as a record of past processing.

library(tidyverse)
library(jsonlite)
library(exifr)
library(lubridate)
library(sf)


# folders downloaded to local disk from google drive folder
# 
path1="/Users/adamw/Library/CloudStorage/GoogleDrive-adammichaelwilson@gmail.com/Shared drives/BioSCape_Admin/VegPlots/Photos/BioSCape1_Ross/"
path2="/Users/adamw/Library/CloudStorage/GoogleDrive-adammichaelwilson@gmail.com/Shared drives/BioSCape_Admin/VegPlots/Photos/BioSCape1_Ross(1)/"
path3="/Users/adamw/Library/CloudStorage/GoogleDrive-adammichaelwilson@gmail.com/Shared drives/BioSCape_Admin/VegPlots/Photos/BioSCape1_Ross_ApplePhotos//"


rfiles <- bind_rows(
  bind_cols(folder = "ross", path = list.files(path1, recursive = TRUE, full.names = TRUE)),
  bind_cols(folder = "ross_1", path = list.files(path2, recursive = TRUE, full.names = TRUE)),
  bind_cols(folder = "ross_3", path = list.files(path3, recursive = TRUE, full.names = TRUE))) |> 
  mutate(
    filename = basename(path),         # Extract the file name from the full path
    folder = dirname(path),            # Extract the folder path
    type = str_extract(path, "[a-zA-Z0-9]+$") |> str_remove("[.]"), # Extract file extension
    photonum= str_remove(filename, "\\.[a-zA-Z0-9]+$") |> # remove file extension
      str_remove("\\.[a-zA-Z0-9]+$") |> # remove second file extension (e.g. ".HEIC.json")
      str_remove("\\([0-9]\\)")|>       #remove "(1)" etc from some file names
      str_remove("\\.[a-zA-Z0-9]+$")) |> # remove third file extension (e.g. ".HEIC.json")
  filter(!type%in%c("MP4","MOV","html","PNG")) #remove videos and other files

if(F) glimpse(rfiles)

# ## JSON File Processing

# Filter the JSON files from the list of files.
jfiles <- rfiles |>
  filter(type == "json") |>
  select(photonum, jsonpath = path)

# Function to read and compare JSON files
json_compare <- function(files) {
  jdata <- lapply(files, fromJSON)
  if (length(jdata) == 1) {
    return(TRUE)  # Return TRUE if only one file is present
  }
  all_identical <- Reduce(function(x, y) identical(x, y), jdata)  # Check if all JSON objects are identical
  return(all_identical)
}

jdupes <- jfiles |>
  group_by(photonum) |>
  reframe(
    json_path = first(jsonpath),      # Select the first JSON path
    json_n = n(),                     # Count the number of JSON files
    json_identical = json_compare(jsonpath), # Check for identical JSON content
    json_description = paste(unique(sapply(jsonpath, function(x) fromJSON(x)$description)), collapse = ";") # Combine descriptions
  )

if(F){
  table(jdupes$json_identical) #check for identical json files
  jdupes[jdupes$json_n>1&grepl(" ",jdupes$json_description),]|> View() #check for multiple descriptions
}

#### Process image files
ifiles <- rfiles |>
  filter(type %in% c("JPG", "HEIC","jpg")) |>
  select(photonum, folder, imagepath = path, filename)


# confirm no missing photonums
if(sum(is.na(ifiles$photonum))>0) stop("some photos do not have a photonum which suggests some parsing error")

# Function to extract EXIF data and confirm location/time consistency
cols <- c(ImageDescription = NA_character_)

process_images <- function(files) {
  exif_data <- lapply(files, 
                      function(x) 
                        select(read_exif(x), # use read_exif to import all the metadata
                               -ShutterSpeedValue)) %>%  # drop shutter speed because some are numeric and some character
    bind_rows() %>% 
    add_column(!!!cols[!names(cols) %in% names(.)]) %>% 
    mutate(
      filename = as.character(FileName),
      exif_caption = ImageDescription,
      gps_latitude = as.numeric(GPSLatitude),
      gps_longitude = as.numeric(GPSLongitude),
      gps_altitude = as.numeric(GPSAltitude),
      gps_position_error = as.numeric(GPSHPositioningError),
      gps_datestamp = as.character(DateTimeOriginal),
      modify_date = as_datetime(DateTimeOriginal),
      date = as_date(modify_date),
      time = hms(format(modify_date, "%H:%M:%S")),
      offset_time = as.character(OffsetTime),
      image_width = as.numeric(ImageWidth),
      image_height = as.numeric(ImageHeight),
      exposure_time = as.numeric(ExposureTime),
      media_group_uuis = as.character(MediaGroupUUID),
      file_type = as.character(FileType),
      .keep = "none")
  
  #confirm all the details match
      compare_lat = ifelse(sum(diff(exif_data$gps_latitude)) == 0, TRUE, FALSE)
      compare_lon = ifelse(sum(diff(exif_data$gps_longitude)) == 0, TRUE, FALSE)
      compare_time = ifelse(sum(diff(exif_data$date)) == 0, TRUE, FALSE)
  
      if(!all(compare_lat, compare_lon, compare_time)) stop(paste("photo differences found for photo",files))
  # keep image that has caption information (if any do)
      
      keep <- ifelse(
        sum(!is.na(exif_data$exif_caption))>0, # check if any of these have a non-NA caption
        which(!is.na(exif_data$exif_caption))[1], # if so, keep the first one 
      1) #otherwise just keep the first image
      
      return(exif_data[keep,])
}


if(F){ # just some EDA stuff 
  ifiles |> group_by(photonum)|>summarize(n=n(),apple=sum(grepl("Apple",folder)))
  files=ifiles |>filter(photonum=="IMG_0024")|>select(imagepath) |> unlist() #get vector for testing function above
}

# Some caption information is held in the exif information (not the json images) due to different processing by apple/google photos.
# This step pulls the apple photo captions out of the exif information when it's present.
# Process all the exif information
 
idupes <- ifiles |>
  group_by(photonum) |> 
  reframe(
    folder = first(folder), #select first photo folder
    photo_path = first(imagepath), #select first image path
    photo_n = n(), # count number of duplicates of this image
    process_images(imagepath) #extract EXIF information and check time and location
  ) 


# Merging Image and JSON Data
rfiles2 <- left_join(idupes, jdupes, by = "photonum") #|>
#  select(photonum, photo_n, json_n, json_identical, json_description)


if(F){
## Get google drive url of original files
library(googledrive)

dirs = c("https://drive.google.com/drive/folders/1VldWJhmmjV_bCwugv6zKi5FXo_uwiNuT", #bioscape1_ross
         "https://drive.google.com/drive/folders/1wtby7i2WO14sAQ6GMGNVzIb_sP0zmyzZ", #bioscape1_ross(1),
         "https://drive.google.com/drive/folders/11WBRLpBMq3x5DYmypcQP13MGoNenhlYS") #bioscape1_ross_applephoto


# Get all files in the folders using google interface
# ross_drive_files = lapply(dirs, function(x) drive_ls(pattern=paste(rfiles$filename,collapse="|"),path = x,recursive=T, 
#                                                      type=drive_mime_type(c("image/jpg","image/jpeg","image/png")))) |>
#   bind_rows() |> #combine all the files into a single dataframe
#   mutate(url=drive_link(id)) |> #add google url to file
#   group_by(name) |> #group by filename/image
#   slice_head(n=1) #select the first file from the duplicates (should be the same file duplicated across folders)


# Process each chunk using dplyr
ross_drive_files <- lapply(dirs, function(x) {
  rfiles2 %>%
    group_by(folder) %>%  # Group by the chunk ID
    group_split() %>%       # Split into chunks for processing
    lapply(function(chunk) {
      drive_ls(
        pattern = paste(paste0(chunk$filename,"$"), collapse = "|"),  # Use filenames in the current chunk including '$' to drop .json files with same name
        path = x,
        recursive = TRUE
        #type = drive_mime_type(c("image/jpg", "image/jpeg", "image/png","image/HEIC")) # no mime type for HEIC files!?!
      )
    }) %>%
    bind_rows()  # Combine results from all chunks
}) %>%
  bind_rows() %>%  # Combine results from all directories
  mutate(url = drive_link(id)) %>%  # Add Google URL to each file
  group_by(name) %>%  # Group by filename/image
  slice_head(n = 1)  # Select the first file from duplicates


# join with local data
rfiles2a <- rfiles2 |>
  left_join(ross_drive_files, by = c("filename" = "name"))

# check for missing files
rfiles2a[is.na(rfiles2a$id),]|> View()

# the code above results in some, but not all google file IDs for some reason.  Since we're going to reorganize them anyways, I'm not including this in the output.
}


# View unique descriptions
if(F) {
  unique(rfiles2$json_description)
  
  filter(rfiles2,filename=="IMG_0020.HEIC")
}

# read in data object from Botanist_photo_processing.qmd script
# this section is a bit circular in the sense that it reads from the Rarefaction_FA spreadsheet to figure out 
# which photos are already there and then creates data to be added to the spreadsheet (in a different tab).

if(F){ # wrapping in if(F) to avoid running this section because it's not needed for the current task

tag="vegphoto_v20241102" #paste0("vegphoto_v",format(today(),"%Y%m%d"))
photo_all_file=paste0("data/photo_all_",tag,".csv")
pb_download(file=basename(photo_all_file),tag=tag,dest = "data")

photo_all <- read_csv(photo_all_file) |>
  mutate( # generate photonum and filter to ross' photos
    photonum= str_remove(filename, "\\.[a-zA-Z0-9]+$") |> # remove file extension
      str_remove("\\.[a-zA-Z0-9]+$") |> # remove second file extension (e.g. ".HEIC.json")
      #      str_remove("\\.[a-zA-Z0-9]+$") |> # remove second file extension (e.g. ".HEIC.json")
      str_remove("\\([0-9]\\)")|>       #remove "(1)" etc from some file names
      str_remove("\\.[a-zA-Z0-9]+$")) |> # remove third file extension (e.g. ".HEIC.json")
  filter(botanist=="Ross") #remove videos and other files

# confirm all photos in this new processing are already in the existing spreadsheet
# this is to confirm we can just delete/ignore the old version of Ross' files
afiles <- anti_join(photo_all,rfiles2,by="photonum") #find photos in old spreadsheet that are not in new batch
afiles$filename
# so these are just png screenshots and not photos we need to worry about.

#  ----- 
# We can safely ignore the 'old' Ross photo spreadsheet 
#  ------ 
#     \   ^__^ 
#      \  (oo)\ ________ 
#         (__)\         )\ /\ 
#              ||------w|
#              ||      ||

# so we don't need this section
}




if(F){
  View(rfiles2)
  table(idupes$photo_n) #how many copies of photos? 
  table(rfiles3$photo_n) #how many copies of photos? 
  nrow(idupes) #how many unique photos?
  unique(idupes$exif_caption)
}



# build the 'final' ross photo spreadsheet like the ones from the other botanists.
# this list came from looking through a temporary version of the table below and identifying photos with descriptions
# that did not include the location or plot.
noloc= paste0("IMG_",sprintf("%04d",c(35:62,79:81,83:91,93:98,110:125,127:135,1608:1611,2299:2300)))
jloc = paste0("IMG_",sprintf("%04d",c(2180:2213)))

rfiles3 <- rfiles2 |>
  rowwise() |>
  mutate(description=ifelse(
    is.na(exif_caption),json_description,exif_caption)) |> #use exif_caption if it exists, otherwise json_description
  mutate(twovar = photonum %in% noloc,
         location = NA,
         plot_number = NA, 
         genus = NA, 
         species = NA)


rfiles3a <- filter(rfiles3,photonum %in% noloc)|>
  # Separate for nonstandard genus.species notation
  separate(description, into = c("genus","species"), sep = "\\.|_",extra="merge",remove = F, fill = "right")

rfiles3b <- filter(rfiles3,!photonum %in% noloc & !photonum%in%jloc)|>
  # Separate for typical location.plot.genus.species notation
  mutate(desc1 = if_else(!photonum %in% noloc, description, NA_character_)) |>
  separate(desc1, into = c("location", "plot_number", "genus","species"), sep = "\\.|_",extra="merge",remove = F, fill = "right")

rfiles3c <- filter(rfiles3,photonum %in% jloc)|>
  # Separate for nonstandard jplot notation like Rooiberg.37.[J5].S.1
  mutate(desc1 = if_else(!photonum %in% noloc, description, NA_character_)) |>
  separate(desc1, into = c("location", "plot_number","jplot", "genus","species"), sep = "\\.|_",extra="merge",remove = F, fill = "right")|>
  mutate(plot_number=paste(plot_number,jplot,sep=" "))|>
  select(-jplot)

  

rfiles4 <- bind_rows(rfiles3a,rfiles3b,rfiles3c) |>
  mutate(
    # next line does some manual processing to account irregular descriptions
    plot_photo=ifelse(genus%in%c("South","North","East","West","N","S","E","W","plot","View","view","Center","Centre","centre","center","setting","cairn","Adjacent","webbing","magnet","plot"),1,NA),
    rarefaction_start = if_else(grepl("arefaction", description)&is.na(plot_photo)&!grepl("end", description)&!grepl("add.to.",description), 1, 0),
    # Propagate the indicator to rows within 10 minutes of a "rarefaction" entry
    last_rarefaction_time = if_else(rarefaction_start == 1, modify_date, as.POSIXct(NA)),
    last_rarefaction_time = zoo::na.locf(last_rarefaction_time, na.rm = FALSE), # Fill down the last rarefaction time
    # Add an indicator variable for rows within 10 minutes of the last rarefaction
    rarefaction_min = if_else(
      !is.na(last_rarefaction_time) & abs(difftime(last_rarefaction_time, modify_date, units = "mins"))<=10,
      as.numeric(abs(difftime(last_rarefaction_time, modify_date, units = "mins"))), NA),
    rarefaction_photo=ifelse(rarefaction_min<10,1,NA)) |>
  #  separate(description, into = c("location", "plot_number", "genus","species"), sep = "\\.|_",extra="merge",remove = F) %>% 
  transmute(folder=str_replace(folder,"/Users/adamw/Library/CloudStorage/GoogleDrive-adammichaelwilson@gmail.com/Shared drives/",""),
            filename=filename,
            description=description,
            location=location,
            plot_number=plot_number,
            genus=genus,
            species=species,
            plot_photo=as.numeric(plot_photo),
            inat_photo=NA,
            rarefaction_photo=as.numeric(rarefaction_photo),
            rarefaction_replicate=ifelse(rarefaction_photo==1,1,NA),
            gps_latitude,
            gps_longitude,
            gps_altitude,
            gps_position_error,
            modify_date,
            date,
            time,
            offset_time=as.character(offset_time),
            image_width,
            image_height,
            gps_datestamp,
            file_type,
            media_group_uuid=NA,
            exposure_time,
            file_base=photonum,
            botanist="Ross",
            id=NA,
            file_url=NA) |>
  st_as_sf(coords=c("gps_longitude","gps_latitude"),remove = F) |>
  st_set_crs(4326) |>
  arrange(gps_datestamp)



# Write Ross Files to Disk
write_csv(rfiles4,"data/ross_photos2.csv",na="")



