# Script to merge and flag veg plot data issues as observed by Phil Townsend and Doug Euston-Brown

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
### Download Townsend plot data
################################

# Authenticate and access the Google Sheet
drive_auth(email = gmail)
gs4_auth(token = drive_token())

sheet_url = "https://docs.google.com/spreadsheets/d/19rGY8R2SddIScCIJm9iX_kPN0QqMG7OQyBtjuE06crk/edit?gid=0#gid=0"

# Download local .xlsx versions of the Google Sheets
drive_download(sheet_url, path = paste0("data/", Sys.Date(), "Townsend2023VegDataVersion2", ".xlsx"), overwrite = TRUE)

################################
### Read in data sheets
################################

# List names of all sheet tabs
#sheets = excel_sheets(sheet)

# Read in townsend data
twns = read_xlsx("data/2024-10-30Townsend2023VegDataVersion2.xlsx","VegData") 

# Read in BioSCape species by plot data
bios = read_csv("output/bioscape_veg_plot_species_v20241021.csv")
bios_old = read_csv("output/bioscape_veg_plot_species_v20231024.csv")
bios$b_UID = 1:nrow(bios)

################################
### Check what has changed between BioSCape data versions
################################

bios_check = full_join(bios, bios_old, by = c("Plot", "Genus_Species_combo")) %>%
  mutate(CoverFlag = abs(PercentCoverAlive.x - PercentCoverAlive.y) > 0.001)

bios_check %>% ggplot() + geom_point(aes(x = PercentCoverAlive.y, y = PercentCoverAlive.x, col = CoverFlag, alpha = 0.9))

View(bios_check[duplicated(bios_check$b_UID),])

################################
### Fix plot names and filter for BioSCape plots
################################

twnsT = twns %>% filter(grepl("T", Plot)) %>%
  #filter(!PctCov %in% c("low", "<5", ">10", "45355.0", "45422.0")) %>%
  mutate(PctCovNum = as.numeric(PctCov))
twnsT$T_UID = 1:nrow(twnsT)

length(unique(twnsT$Plot))

sum(is.na(twnsT$PctCovNum))

################################
### Join datasets
################################

# Join Townsend data to both bioscape versions
hmm = full_join(bios_check, twnsT, by = c("Plot", "Genus_Species_combo" = "GenusSpecies")) %>%
  mutate(OldCoverFlag = abs(PctCovNum - PercentCoverAlive.y) == 0) %>%
  mutate(CoverFlag = abs(PctCovNum - PercentCoverAlive.x)/PercentCoverAlive.x > 0.5) %>%
  mutate(CoverDiff = abs(PctCovNum - PercentCoverAlive.x))

hmm %>% filter(OldCoverFlag == FALSE) %>%
  ggplot() + geom_point(aes(x = PctCovNum, y = PercentCoverAlive.x, col = CoverFlag, alpha = 0.9))

hmm %>% filter(OldCoverFlag == FALSE) %>%
  summarise(sum(CoverFlag, na.rm = T))

hmm %>% filter(OldCoverFlag == FALSE) %>%
  ggplot() + geom_histogram(aes(x = CoverDiff), bins = 100)

hmm %>% filter(OldCoverFlag == FALSE) %>%
  summarise(sum(CoverDiff > 5, na.rm = T))

hmm2 <- hmm %>% filter(OldCoverFlag == FALSE) %>%
  filter(CoverDiff > 5)

hmm2[abs(hmm2$PctCovNum - hmm2$PercentCoverAlive.y)>0.00001,c(1:3,7,8,19,27,46:48)] %>% View()

###
dat = full_join(bios, twnsT, by = c("Plot", "Genus_Species_combo" = "GenusSpecies")) %>%
  filter(!PctCov %in% c("low", "<5", ">10", "45355.0", "45422.0")) %>%
  mutate(CoverFlag = abs(PctCovNum - PercentCoverAlive)/PercentCoverAlive > 0.5)

dat %>% ggplot() + geom_point(aes(x = PctCovNum, y = PercentCoverAlive, col = CoverFlag, alpha = 0.9))


################################
### Extract bioscape rows unmatched in townsend plots
################################

bdat = dat %>% filter(is.na(Check)) # Rows unique to BioSCape data

################################
### Extract townsend rows flagged for bioscape plots
################################

fdat = dat %>% filter(Check == TRUE) # Rows flagged for attention
fdat %>% ggplot() + geom_point(aes(x = PctCovNum, y = PercentCoverAlive, col = CoverFlag, alpha = 0.9))

################################
### Extract townsend rows not in bioscape plots
################################

tdat = dat %>% filter(is.na(SiteCode_Plot)) %>% # Rows unique to Townsend data
  filter(!CapeTraitsVisitDate %in% c("NoVisit", "No Visit")) %>%
  filter(!Check == TRUE)

# drop CapeTraitsVisitDate == "No Visit"
# drop Check = TRUE # should be dealt with in fdat
# resolve Check = FALSE that have comments
# comb through Check = FALSE that have NO comments
