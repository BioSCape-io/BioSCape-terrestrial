# plot_spatial_compile.R: plot centers, polygons and parking

`plot_spatial_compile.R` builds `data/spatial/bioscape_veg_spatial_<tag>.gpkg`
with three layers, `PlotCenters`, `PlotPolygons` and `PlotParking`. That file is
the spatial input to the veg data release (`ornl_daac/vegplot_readme.qmd`).

## Where the inputs come from

The script reads one input folder (`BIOSCAPE_SPATIAL_INPUTS`, default
`data/spatial/townsend_inputs`, not in git):

| Input | What it is | Origin |
|---|---|---|
| `RawDataFromBotanist/` | Plot centers, polygons and parking points, one folder per botanist upload (May to Nov 2023). | The botanists' TouchGIS exports on the team Google Drive. Henry Frye merged each upload in QGIS (selecting features not already in the merged set, and fixing polygon geometry), so these are curated copies, not untouched exports. See `BioSCapeTerrestrialProcessingNotes.docx` and `BotanistSpatialDataNotes.docx`. |
| `Townsend_BioSCape_Plot_Centers.geojson` | One plot-center point per BioSCape plot, marked by the Townsend team on its 2023 plot visits (Garmin GPSMAP 66SR). | `Workflow3_Plot_Data_Cleaning/Code/Step1_CapeTraits_2023_Plot_Data_Create.R` in `EnSpec/bioscape-ground-data`, from the raw GPS export `CapeTraits_20231206_allexcepttracks.kml`. That script documents each ID fix. |
| `Merged_Plot_Note_Issues.csv` | Townsend team plot notes and quality flags (`PlotNote`, `QualityFlag`, `QualityFlag2`). | Townsend team field notes, compiled Nov 2024. |
| `PlotAssociation.csv` | Links from Townsend opportunistic plots (`pt...`) to the BioSCape plot they were surveyed next to, with descriptions and categories. | Townsend team; the maintained copy is `Workflow3_Plot_Data_Cleaning/Auxilliary_Data/PlotAssociation.csv` in `EnSpec/bioscape-ground-data` (also read by its Workflow3 Step4). |

## Rules

- Plot ID: "T" plus the first number in the feature name, zero-padded to three
  digits.
- Region: fixed per upload where feature names carry no region (North
  Cederberg), otherwise the part of the feature name before the first `_`.
  Region values are not yet harmonised (mixed case, some typos).
- Botanist: fixed per upload, or by plot for the three mixed uploads.
- Dropped: Ross's T089 center and polygon (no veg survey data); Ross's T096
  calibration survey (Doug's is the final one); Adam Labuschagne's T051, T053,
  T139, T194 and T275.
- `LocationFlag` = "See Townsend alternative location" when the Townsend center
  is more than 10 m (geodesic) from the botanist center of the same plot, with
  the Townsend point in `TownsendLong`/`TownsendLat`. T096 (calibration plot)
  and T282 (center not visited by the Townsend team) are exempt.
- `PTPlotA`-`PTPlotD`: up to four associated Townsend opportunistic plots.
- `DateTime`: the botanist's GPS clock time as recorded, local South African
  time with no timezone in the source.

## Changes from the v20241114 output

The script was rewritten in Oct 2026. The commit before the rewrite reproduces
`BioSCapeVegData2024_11_14.gpkg` exactly. Compared with that file, the rewrite
changes:

1. **T114 is one row, not two.** The old Townsend input
   (`TownsendBioSCapePlotsGPSCleanV1.geojson`) had two T114 points, the first
   GPS mark (14.1 m from the botanist center) and the corrected re-mark (2.1 m).
   Both were joined in. With the corrected point only, T114 is under 10 m, so
   it has no location flag.
2. **The location check compares each plot with its own center.** The old code
   tested whether the Townsend point was within 10 m of *any* botanist center.
   That hid T264, whose old Townsend point was really T164's center (a GPS
   labelling mistake; the real T264 point was recorded separately and never
   joined). With the corrected input, T264's Townsend point is 6 m from the
   botanist center, so it is not flagged. Three plots now have a Townsend point
   for the first time (T003, T161, T164); all are within 10 m. No other flag
   changes: 25 plots are flagged.
3. **T074 region is `CapePoint`, not `CapePointnew`.** Cape Point regions were
   built by stripping non-letters from the feature name, so "CapePoint_74_new"
   became "CapePointnew".
4. **`PTPlotD` is one column.** The old output had `PTPlotD.x` and `PTPlotD.y`
   from a join artifact.
5. **Missing `PTPlotA` values are real NA.** The old output stored the text
   "NA" in `PTPlotA` (140 plots) but real missing values in `PTPlotB`-`PTPlotD`.
6. **Trailing spaces trimmed** in two `PlotNote` values (T013, T063).
7. **T183 gains `PTPlotA` = pt183.** The v20241114 run read an older copy of
   `PlotAssociation.csv`. The maintained copy links pt183, the Townsend team's
   own survey at T183, to T183. No other plot's associations change.

Geometry is identical for all 189 centers, 188 polygons and 122 parking points.

## Running

```r
# From the repo root, with the input folder in place:
Sys.setenv(BIOSCAPE_SPATIAL_INPUTS = "path/to/townsend_inputs")
source("workflow/plot_spatial_compile.R")
```
