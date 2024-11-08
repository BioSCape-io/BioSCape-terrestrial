## Ross photo cleanup
library(jsonlite)
library(exifr)
library(lubridate)




# folders downloaded to local disk from google drive folder
# 
path1="/Users/adamw/Library/CloudStorage/GoogleDrive-adammichaelwilson@gmail.com/Shared drives/BioSCape_Admin/VegPlots/Photos/BioSCape1_Ross/"
path2="/Users/adamw/Library/CloudStorage/GoogleDrive-adammichaelwilson@gmail.com/Shared drives/BioSCape_Admin/VegPlots/Photos/BioSCape1_Ross(1)/"


rfiles <- bind_rows(
  bind_cols(folder = "ross", path = list.files(path1, recursive = TRUE, full.names = TRUE)),
  bind_cols(folder = "ross_1", path = list.files(path2, recursive = TRUE, full.names = TRUE))) |> 
  mutate(
    filename = basename(path),         # Extract the file name from the full path
    folder = dirname(path),            # Extract the folder path
    type = str_extract(path, "[a-zA-Z0-9]+$") |> str_remove("[.]"), # Extract file extension
    photonum= str_remove(filename, "\\.[a-zA-Z0-9]+$") |> # remove file extension
      str_remove("\\.[a-zA-Z0-9]+$") |> # remove second file extension (e.g. ".HEIC.json")
      str_remove("\\([0-9]\\)")|>       #remove "(1)" etc from some file names
      str_remove("\\.[a-zA-Z0-9]+$")) |> # remove third file extension (e.g. ".HEIC.json")
  filter(!type%in%c("MP4","MOV","html","PNG")) #remove videos and other files

glimpse(rfiles)

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

table(jdupes$json_identical) #check for identical json files
# jdupes[jdupes$json_n>1&grepl(" ",jdupes$json_description),]|> View() #check for multiple descriptions

#### Process image files
ifiles <- rfiles |>
  filter(type %in% c("JPG", "HEIC")) |>
  select(photonum, folder, imagepath = path, filename)


# confirm no missing photonums
if(sum(is.na(ifiles$photonum))>0) stop("some photos do not have a photonum which suggests some parsing error")

# Function to extract EXIF data and confirm location/time consistency
process_images <- function(files) {
  exif_data <- lapply(files, read_exif) |>
    bind_rows() |>
    mutate(
      filename = as.character(FileName),
      gps_latitude = as.numeric(GPSLatitude),
      gps_longitude = as.numeric(GPSLongitude),
      gps_altitude = as.numeric(GPSAltitude),
      gps_position_error = as.numeric(GPSHPositioningError),
      modify_date = as_datetime(DateTimeOriginal),
      date = as_date(modify_date),
      time = hms(format(modify_date, "%H:%M:%S")),
      offset_time = as.character(OffsetTime),
      image_width = as.numeric(ImageWidth),
      image_height = as.numeric(ImageHeight),
      file_type = as.character(FileType),
      .keep = "none")
  
  #confirm all the details match
      compare_lat = ifelse(sum(diff(exif_data$gps_latitude)) == 0, TRUE, FALSE)
      compare_lon = ifelse(sum(diff(exif_data$gps_longitude)) == 0, TRUE, FALSE)
      compare_time = ifelse(sum(diff(exif_data$date)) == 0, TRUE, FALSE)
  
      if(!all(compare_lat, compare_lon, compare_time)) stop(paste("photo differences found for photo",files))
      return(exif_data[1,])
}


# start cluster for parallel dplyr
#library(multidplyr)
#cluster <- new_cluster(3)

idupes <- ifiles |>
  group_by(photonum) |> 
  #partition(cluster) |> 
  reframe(
    folder = first(folder), #select first photo folder
    photo_path = first(imagepath), #select first image path
    photo_n = n(), # count number of duplicates of this image
    process_images(imagepath) #extract EXIF information and check time and location
  ) #|>
  #collect()

table(idupes$photo_n) #how many copies of photos?
nrow(idupes) #how many unique photos?


# Merging Image and JSON Data
rfiles2 <- left_join(idupes, jdupes, by = "photonum") #|>
#  select(photonum, photo_n, json_n, json_identical, json_description)

# View unique descriptions
unique(rfiles2$json_description)

# read in data object from Botanist_photo_processing.qmd script
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


# find which photos are not in the primary spreadsheet
rfiles3 <-  anti_join(rfiles2,photo_all,by="photonum") #find photos in ross_files2 that are not in photo_all

#clean up description field  
rfiles4 <- rfiles3 |>
  separate(json_description, into = c("location", "plot_number", "genus","species"), sep = "\\.",extra="merge")

# Add/update fields found in other photo spreadsheet including parsing the json description field

#       lat <- ifelse(sum(diff(exif_data$gps_latitude)) == 0, TRUE, FALSE)
# lon <- ifelse(sum(diff(exif_data$gps_longitude)) == 0, TRUE, FALSE)
# time <- ifelse(sum(diff(exif_data$date)) == 0, TRUE, FALSE)
#  [1] "folder"                "filename"              "description"           "location"             
# [5] "plot_number"           "genus"                 "species"               "plot_photo"           
# [9] "inat_photo"            "rarefaction_photo"     "rarefaction_replicate" "gps_latitude"         
# [13] "gps_longitude"         "gps_altitude"          "gps_position_error"    "modify_date"          
# [17] "date"                  "time"                  "offset_time"           "image_width"          
# [21] "image_height"          "gps_datestamp"         "file_type"             "media_group_uuid"     
# [25] "file_base"             "botanist"              "id"                    "file_url"             
# [29] "datetime"              "photo_type" 

# folder = basename(FileName),
# description = NA,
# location = NA,
# plot_number = NA,
# genus = NA,
# species = NA,
# plot_photo = ,
# inat_photo = ,
# rarefaction_photo = ,
# rarefaction_replicate = ,
#      gps_datestamp = ) %>% #,
#      file_type = ,
#      media_group_uuid = ,
#      file_base = ,
#      botanist = "Ross",
#      id = ,
#      file_url = ,
#      datetime = ,
#      photo_type = )
