########################################################
# plot_spatial_compile.R
#
# Purpose: compile the botanists' plot centers, plot polygons (homogeneous
# vegetation patches) and parking points into three uniform layers, and add
# the Townsend team's plot notes, quality flags, alternative plot-center
# locations and associated opportunistic plots. The output is the spatial
# input to the veg data release (ornl_daac/vegplot_readme.qmd reads
# data/spatial/bioscape_veg_spatial_<tag>.gpkg).
#
# History: Henry Frye wrote this as BioSCapeRawSpatialAgg.R (repo root),
# Oct 2023 to Nov 2024, adding one block per botanist upload. Rewritten
# Oct 2026 as one table-driven pass with the same rules. The two commits
# before the rewrite hold the Nov 2024 script as saved and the patch that
# makes it reproduce the v20241114 output exactly. Changes in output from
# v20241114 are listed in workflow/plot_spatial_compile.md.
#
# Author(s): Henry Frye, Claude
########################################################

library(sf)
library(tidyverse)

################################
### Settings
################################

# Input bundle (not in git). Contents:
#   RawDataFromBotanist/   botanist TouchGIS layers, one folder per upload,
#                          curated by Henry Frye in QGIS (see
#                          BioSCapeTerrestrialProcessingNotes.docx)
#   Townsend_BioSCape_Plot_Centers.geojson
#                          plot centers marked by the Townsend team, one point
#                          per plot (made by Workflow3 Step1 in
#                          EnSpec/bioscape-ground-data)
#   Merged_Plot_Note_Issues.csv   Townsend team plot notes and quality flags
#   PlotAssociation.csv    links between Townsend opportunistic plots and
#                          BioSCape plots
input_dir <- Sys.getenv(
  "BIOSCAPE_SPATIAL_INPUTS",
  "data/spatial/townsend_inputs"
)
botanist_dir <- file.path(input_dir, "RawDataFromBotanist")
townsend_centers_file <- file.path(
  input_dir, "Townsend_BioSCape_Plot_Centers.geojson"
)
plot_notes_file <- file.path(input_dir, "Merged_Plot_Note_Issues.csv")
plot_association_file <- file.path(input_dir, "PlotAssociation.csv")

release_tag <- format(Sys.Date(), "v%Y%m%d")
output_file <- file.path(
  "data/spatial",
  paste0("bioscape_veg_spatial_", release_tag, ".gpkg")
)

# A plot gets a location flag when the Townsend team's center is more than
# this far (geodesic distance, metres) from the botanist's center.
location_tolerance_m <- 10

################################
### Botanist uploads
################################

# One row per upload folder. Layer files are NA where the upload had no such
# layer. A fixed region is used where the feature names carry no region;
# otherwise region is the part of the feature name before the first "_".
# A fixed botanist is used where one botanist surveyed the whole upload;
# otherwise botanists come from `plot_botanists` below. Parking points have
# no botanist.
uploads <- tribble(
  ~folder, ~centers_file, ~polygons_file, ~parking_file, ~region, ~botanist,
  "CapePointRoss", "Red Point_Points.shp",
  "CapePointRossPolyFixed.shp", "Green Point_Points.shp",
  NA, "Ross Turner",
  "NorthCederbergRoss", "Blue Point_Points.shp",
  "Blue Polygon_Polygons.shp", "Green Point_Points.shp",
  "Cederberg", "Ross Turner",
  "GrootBolandDeHoopSwartGardenRoss", "Blue Point_Points.shp",
  "Swart2BoldandRossPolysFixed.shp", "Green Point_Points.shp",
  NA, "Ross Turner",
  # Doug's centers and parking points share one layer; split below
  "PeninKogelHotsDoug", "Blue Point_Points.shp",
  "PeninKogPolysDougFixed.shp", NA,
  NA, "Doug Euston-Brown",
  "WestVroiBonAlUnsure", "Blue Point_Points.shp",
  "WestAlPolysFixed.shp", "Green Point_Points.shp",
  NA, NA,
  "Rooiberg", "RooibergPlotCenters.shp",
  "RooibergPolysFixed.shp", "RooibergParking.shp",
  NA, "Ross Turner",
  "WestAlGardenOct17", "WesttoGardenOct17VegCenter.shp",
  "WesttoGardenOct17VegPoly.shp", "WesttoGardenOct17VegParking.shp",
  NA, NA,
  "SouthCederbergDougNov2", "SouthCederberg.shp",
  "SouthCederbergPolysFixed.shp", NA,
  NA, "Doug Euston-Brown",
  "ExtraAgulhasCapensisOct25", "AdditionalAgulhasCenters.shp",
  "ExtraAgulhasPolysFixed.shp", "ExtraAgulhasParking.shp",
  NA, "Steven Molteno",
  "BaviaanskloofRossNov2", "BaviaanskloofCenter.shp",
  "BaviaanskloofPolysFixed.shp", "BaviaanskloofParking.shp",
  NA, "Ross Turner",
  "AnysbergEmmsNov8", "AnysbergPlotCenters.shp",
  "AnysbergPatchesFixed.shp", "AnysbergParking.shp",
  NA, NA
)

# Botanists for the three uploads that mix botanists (West Coast /
# Vrolijkheid / Bontebok / Agulhas; the Oct 17 West Coast to Garden Route
# update; Anysberg). Applies to centers and polygons.
plot_botanists <- bind_rows(
  tibble(
    BioScapePlotID = c(
      "T188", "T187", "T095", "T184", "T282", "T172", "T087", "T253",
      "T002", "T138", "T264", "T078", "T183", "T063", "T001", "T019",
      "T023", "T016", "T007", "T107", "T105", "T109", "T103"
    ),
    plot_botanist = "Steven Molteno"
  ),
  tibble(
    BioScapePlotID = c(
      "T083", "T180", "T270", "T181", "T178", "T175", "T176", "T182",
      "T277", "T164", "T281", "T066", "T067", "T009", "T018", "T017",
      "T022"
    ),
    plot_botanist = "Stuart Hall"
  ),
  tibble(
    BioScapePlotID = c(
      "T084", "T235", "T088", "T091", "T173", "T177", "T104", "T106",
      "T004", "T015", "T108"
    ),
    plot_botanist = "Paul Emms"
  ),
  tibble(
    BioScapePlotID = c("T194", "T053", "T139", "T275", "T051"),
    plot_botanist = "Adam Labuschagne"
  )
)
stopifnot(!anyDuplicated(plot_botanists$BioScapePlotID))

################################
### Read and standardise layers
################################

# Every upload's layers have the same TouchGIS attributes: Name,
# Descriptio and Date...Tim (clock time as text, e.g. "09 Oct 2023 at
# 15:59:24", local South African time; no timezone is recorded, so the
# text is kept as is). Plot IDs are "T" plus the first number in the
# feature name, zero-padded to 3 digits (e.g. "Cederberg_242" -> T242).
read_upload_layer <- function(folder, file, region) {
  st_read(file.path(botanist_dir, folder, file), quiet = TRUE) |>
    rename(Description = Descriptio, DateTime = Date...Tim) |>
    mutate(
      BioScapePlotID = paste0(
        "T",
        str_pad(str_extract(Name, "\\d+") |> as.numeric(), 3, pad = "0")
      ),
      Region = coalesce(region, word(Name, 1, sep = "_")),
      # "Peninsula_97" is Ross's one Cape Peninsula plot outside Cape Point
      Region = if_else(Region == "Peninsula", "CapePeninsula", Region),
      folder = folder
    )
}

centers_raw <- uploads |>
  filter(!is.na(centers_file)) |>
  pmap(\(folder, centers_file, region, ...) {
    read_upload_layer(folder, centers_file, region)
  }) |>
  bind_rows()

polygons_raw <- uploads |>
  filter(!is.na(polygons_file)) |>
  pmap(\(folder, polygons_file, region, ...) {
    read_upload_layer(folder, polygons_file, region)
  }) |>
  bind_rows()

parking_raw <- uploads |>
  filter(!is.na(parking_file)) |>
  pmap(\(folder, parking_file, region, ...) {
    read_upload_layer(folder, parking_file, region)
  }) |>
  bind_rows()

# Doug's upload puts parking points in the centers layer, named "..parking"
doug_parking <- centers_raw |>
  filter(folder == "PeninKogelHotsDoug", str_detect(Name, "parking"))
centers_raw <- centers_raw |>
  filter(!(folder == "PeninKogelHotsDoug" & str_detect(Name, "parking")))
parking_raw <- bind_rows(parking_raw, doug_parking)

# Botanist: fixed per upload, or per plot for mixed uploads
upload_botanists <- uploads |> select(folder, upload_botanist = botanist)

add_botanist <- function(layer) {
  layer |>
    left_join(upload_botanists, by = "folder") |>
    left_join(plot_botanists, by = "BioScapePlotID") |>
    mutate(Botanist = coalesce(upload_botanist, plot_botanist))
}
centers_raw <- add_botanist(centers_raw)
polygons_raw <- add_botanist(polygons_raw)
stopifnot(!anyNA(centers_raw$Botanist), !anyNA(polygons_raw$Botanist))

################################
### Remove records not in the release
################################

# T089 (Ross, Cape Point): Ross's upload has a T089 center and polygon but
# there is no veg survey data for them.
# T096 (Cape Point calibration plot): surveyed by both Ross and Doug. Doug's
# was the final plot version for the team data, so Ross's center, polygon
# and parking point (01 Jun 2023 11:46) are dropped.
# T051, T053, T139, T194, T275 (Adam Labuschagne): plots decided in Oct 2024
# not to be needed.
labuschagne_plots <- c("T194", "T275", "T051", "T053", "T139")

centers <- centers_raw |>
  filter(!(folder == "CapePointRoss" & BioScapePlotID == "T089")) |>
  filter(!(BioScapePlotID == "T096" & Botanist == "Ross Turner")) |>
  filter(!BioScapePlotID %in% labuschagne_plots)

polygons <- polygons_raw |>
  filter(!(folder == "CapePointRoss" & BioScapePlotID == "T089")) |>
  filter(!(BioScapePlotID == "T096" & Botanist == "Ross Turner")) |>
  filter(!BioScapePlotID %in% labuschagne_plots)

parking <- parking_raw |>
  filter(
    !(BioScapePlotID == "T096" & DateTime == "01 Jun 2023 at 11:46:00")
  ) |>
  filter(!BioScapePlotID %in% labuschagne_plots)

stopifnot(!anyDuplicated(centers$BioScapePlotID))

################################
### Townsend plot notes and quality flags
################################

# One row per plot: PlotNote (free text), QualityFlag and QualityFlag2
# (e.g. "Boundary", "Low vegetation"), from Townsend team plot visits.
plot_notes <- read_csv(plot_notes_file, show_col_types = FALSE) |>
  mutate(across(everything(), \(x) na_if(as.character(x), "NA")))
stopifnot(!anyDuplicated(plot_notes$Plot))

################################
### Location flags: botanist vs. Townsend plot centers
################################

# Some botanist GPS points were not taken at the plot center. The Townsend
# team marked its own center on most plot visits. Each Townsend point is
# compared with the botanist center of the same plot. (Before Oct 2026 the
# script tested whether the Townsend point fell within 10 m of ANY plot
# center, which hid T264's mislabelled point, which was T164's center.)
townsend_centers <- st_read(townsend_centers_file, quiet = TRUE) |>
  select(
    BioScapePlotID = PlotID,
    TownsendLong = longitude,
    TownsendLat = latitude
  )
stopifnot(!anyDuplicated(townsend_centers$BioScapePlotID))

paired_centers <- centers |>
  select(BioScapePlotID) |>
  inner_join(st_drop_geometry(townsend_centers), by = "BioScapePlotID")
townsend_points <- st_as_sf(
  st_drop_geometry(paired_centers),
  coords = c("TownsendLong", "TownsendLat"),
  crs = 4326
)
paired_centers$offset_m <- st_distance(
  paired_centers,
  townsend_points,
  by_element = TRUE
) |>
  as.numeric()

# Plots exempt from the location flag:
# T096 - calibration plot surveyed twice (see above); not flagged.
# T282 - the Townsend team did not visit the plot center.
location_flag_exempt <- c("T096", "T282")

flagged_plots <- paired_centers |>
  st_drop_geometry() |>
  filter(
    offset_m > location_tolerance_m,
    !BioScapePlotID %in% location_flag_exempt
  ) |>
  mutate(LocationFlag = "See Townsend alternative location") |>
  select(BioScapePlotID, LocationFlag, TownsendLong, TownsendLat)

################################
### Associated Townsend opportunistic plots
################################

# Where a BioSCape plot sat on a community boundary, had low cover, or had a
# nearby single-species patch, the Townsend team surveyed extra
# opportunistic plots (pt...) nearby. Up to four per BioSCape plot, in the
# order listed in PlotAssociation.csv.
associated_plots <- read_csv(plot_association_file, show_col_types = FALSE) |>
  filter(!is.na(AssociatedBioScapePlot), AssociatedBioScapePlot != "") |>
  select(BioScapePlotID = AssociatedBioScapePlot, PlotCode) |>
  group_by(BioScapePlotID) |>
  mutate(slot = paste0("PTPlot", LETTERS[row_number()])) |>
  ungroup()
stopifnot(all(associated_plots$slot %in% paste0("PTPlot", LETTERS[1:4])))

associated_plots_wide <- associated_plots |>
  pivot_wider(names_from = slot, values_from = PlotCode)

################################
### Assemble and write layers
################################

centers_out <- centers |>
  left_join(plot_notes, by = c("BioScapePlotID" = "Plot")) |>
  left_join(flagged_plots, by = "BioScapePlotID") |>
  left_join(associated_plots_wide, by = "BioScapePlotID") |>
  select(
    BioScapePlotID, Region, Botanist, Name, Description, DateTime,
    PlotNote, QualityFlag, QualityFlag2, LocationFlag,
    PTPlotA, PTPlotB, PTPlotC, PTPlotD, TownsendLong, TownsendLat
  )

polygons_out <- polygons |>
  select(BioScapePlotID, Region, Botanist, Name, Description, DateTime)

parking_out <- parking |>
  select(BioScapePlotID, Region, Name, Description, DateTime)

dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
if (file.exists(output_file)) {
  file.remove(output_file)
}
st_write(centers_out, output_file, layer = "PlotCenters", quiet = TRUE)
st_write(polygons_out, output_file, layer = "PlotPolygons", quiet = TRUE)
st_write(parking_out, output_file, layer = "PlotParking", quiet = TRUE)

message(
  "Wrote ", output_file, ": ", nrow(centers_out), " centers (",
  nrow(flagged_plots), " location-flagged), ", nrow(polygons_out),
  " polygons, ", nrow(parking_out), " parking points"
)
