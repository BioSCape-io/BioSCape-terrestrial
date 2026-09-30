# Script to link photos to veg plots

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
library(readr)

################################
### User settings
################################

if (Sys.getenv("USER") == "jasper") {gmail = "jasper.slingsby@uct.ac.za"}
if (Sys.getenv("USER") == "adam") {gmail = "adamw@buffalo.edu"}

################################
### Download Photos metadata spreadsheet
################################

# Authenticate and access the Google Sheet
drive_auth(email = gmail)
gs4_auth(token = drive_token())

sheet_url = "https://docs.google.com/spreadsheets/d/13kJ7O4PbH6bM-Y89wGf_tbxs-R1THXlb/edit?gid=1074650735#gid=1074650735"

# Download local .xlsx versions of the Google Sheets
drive_download(sheet_url, path = paste0("data/", Sys.Date(), "PhotoMetadata", ".xlsx"), overwrite = TRUE)

################################
### Read in data sheets
################################

## Read in photos metadata

sheet = paste0("data/", Sys.Date(), "PhotoMetadata", ".xlsx")

# List names of all sheet tabs
tabs = excel_sheets(sheet)

# Read in data from all tabs
# Apply filters, data format changes, etc as necessary
data_downloaded <- lapply(tabs, 
                          function(tabs) {read_xlsx(path = sheet, tabs, na = "NA")})


# merge quadrat data from list of multiple sheets into one dataframe (for this workbook)
data <- data_downloaded %>% 
  bind_rows() 
# %>% 
#   separate(SiteCode_Plot_Quadrant,into=c("SiteCode","Plot","Quadrant"),sep="_",remove = F) %>% 
#   mutate("SiteCode_Plot"=paste(SiteCode,Plot,sep="_"),
#          Plot=as.numeric(sub("T","",Plot)))  


## Read in one sheet for now...
tmp = read_xlsx(sheet, tabs[2]) %>% 
  filter(genus %in% c("centre", "center", "S", "S2", "W", "W2", "N", "N2", "E", "E2"))

## Read in BioSCape site data
bios = read_csv("output/bioscape_veg_plot_summary_v20241021.csv")

################################
### Select photos by metadata fields and match to plot
################################

tmp$plot_number


