# BioSCape Vegetation Plots, Cape Floristic Region, South Africa, 2023
Jasper Slingsby, Ross Turner, Douglas Euston-Brown, Greg Nicolson,
Steven Molteno, Stuart Hall, Paul Emms, Henry. A. Frye, Phillip. A.
Townsend, Anabelle Cardoso, Erin Hestir, Brian Maitner, Adam M. Wilson
2026-10-01

    [1] TRUE

## Introduction

**Dataset title:** BioSCape Vegetation Plots, Cape Floristic Region,
South Africa, 2023

**Summary:** Vegetation composition and cover at 189 field plots in the
Greater Cape Floristic Region of South Africa, collected for the
Biodiversity Survey of the Cape (BioSCape). Each plot was surveyed once
between 2023-05-03 and 2023-11-01 and divided into four quadrants (NE,
NW, SE, SW). The product reports quadrant-level and plot-level
fractional cover of bare soil, bare rock, dead vegetation, and live
vegetation, together with species-level percent cover, stem counts, and
canopy diameter for 709 species. All species with at least 5% cover were
included in the dataset, plus additional species where needed to make up
80% of the total cover. The data were collected in the field by
botanists. Plot-center coordinates are in the companion spatial file.

## Dataset overview

Project: Biodiversity Survey of the Cape (BioSCape), an integrated field
and airborne campaign in the Greater Cape Floristic Region (Cardoso et
al. 2025).

**Related publication:**

Cardoso, A.W., E.L. Hestir, J.A. Slingsby, C.J. Forbes, G.R. Moncrieff,
W. Turner, A.L. Skowno, J. Nesslage, P.G. Brodrick, K.D. Gaddis, and
A.M. Wilson. 2025. The biodiversity survey of the Cape (BioSCape),
integrating remote sensing with biodiversity science. *npj Biodiversity*
4:2. <https://doi.org/10.1038/s44185-024-00071-5>

**Related datasets:**

- Fitzpatrick, M.C., D. Spalink, A.J. Elmore, J. Measey, and S.
  Kritzinger-Klopper. 2026. BioSCape: Vegetation Composition at Plots
  with Woody Invasives, CFR, South Africa. ORNL DAAC.
  <https://doi.org/10.3334/ORNLDAAC/2511>. Those plots follow this
  survey protocol and were placed near these plots, in vegetation that
  also contained woody invasive cover.
- Slimp, M., J. Malindi, R. Meyer, A. Cunningham, J. Rourke, M.
  Hayden, A. Hargey, J. Nesslage, S. Johnson, M. Rossi, and N.
  Stavros. 2025. BioSCape Vegetation Surveys Berg and Eerste River
  Catchments, South Africa, 2022–2023. ORNL DAAC.
  <https://doi.org/10.3334/ORNLDAAC/2425>. Same campaign, different
  sites and some different measurements.

**Acknowledgments:** Cardoso et al. (2025) report NASA award
80NSSC21K0086 to A.M. Wilson and E.L. Hestir for the U.S. components of
BioSCape, and National Research Foundation grant 142438 plus the South
African Environmental Observation Network (SAEON) for J.A. Slingsby and
G.R. Moncrieff.

## Dataset characteristics

**Spatial coverage:** Greater Cape Floristic Region, Western Cape and
Eastern Cape, South Africa. Site codes in the plot summary: agulhas,
anysberg, bainskloof, bavianskloof, bontebok, capepoint, cederberg,
dehoop, franschoekpass, gardenroute, grootbos, grootwinterhoek,
Hawequas, houwhoekpass, kogelberg, langeberg, outeniqua, rooiberg,
swartberg, viljoenspass, vrolijkheid, westcoast.

**Spatial resolution:** One circular vegetation plot with a diameter of
10m at each location, subdivided into four quadrants.

**Temporal coverage:** 2023-05-03 00:00:00 to 2023-11-01 23:59:59 UTC.
`date` in the quadrat summary is the survey calendar date. Each plot has
one date.

**Temporal resolution:** One sample per plot.

**Study area:** Bounding box of the `plot_centers` layer in
bioscape_veg_spatial_v20261001.gpkg, WGS 84 decimal degrees.

| Site | Westernmost longitude | Easternmost longitude | Northernmost latitude | Southernmost latitude |
|:---|:---|:---|:---|:---|
| Greater Cape Floristic Region, South Africa | 18.0121 | 23.4480 | -32.1368 | -34.8096 |

**GCMD keywords:** BIOSPHERE \> VEGETATION \> VEGETATION COVER;
BIOSPHERE \> VEGETATION \> VEGETATION SPECIES; AFRICA \> SOUTHERN AFRICA
\> SOUTH AFRICA; platform FIELD SURVEYS; instrument GPS.

### File naming convention

`bioscape_veg_{level}_{table}_v{YYYYMMDD}.csv` and
`bioscape_veg_spatial_v{YYYYMMDD}.gpkg`

- `level` is `plot` or `quadrat`.
- `table` is `summary` (one row per location) or `species` (one row per
  taxon at that location).
- `YYYYMMDD` is the date these version 1 files were written. This
  submission is v20261001. The vegetation sources are v20241104 and
  previously released to the team on GitHub. The spatial source is
  v20241114. Publication at ORNL is version 1. There is no earlier DOI.

Names use lowercase letters, numbers, and underscores.

### File descriptions

The CSV files have one header row. Numeric missing values are `-9999`.
Text missing values are `NA`. `record_id` is unique within each CSV.

| file | rows | bytes | sha256 |
|:---|---:|---:|:---|
| bioscape_veg_plot_summary_v20261001.csv | 189 | 22006 | 5616bb12963f6ceb35978c0eadc4971296fc40d571ca714e5f52f792d095606e |
| bioscape_veg_plot_species_v20261001.csv | 1974 | 207912 | 63d79b3112a0ff0a4e88f46dd8d5b4487d94ba9b60e463fd06b015155e7abba2 |
| bioscape_veg_quadrat_summary_v20261001.csv | 756 | 162343 | 2afa3f25a04ce9d0e2697f7f8c5bd8676bb87c04fa60c91e65f92fc6eb72d09d |
| bioscape_veg_quadrat_species_v20261001.csv | 4795 | 768295 | 81fc2914fb2bbb0372805af93cbb50755911d55b97bb678161ea597a38db1357 |
| bioscape_veg_spatial_v20261001.gpkg | 500 | 2191360 | 3bb199991a2c4d39cfabcfe19c1f96c10f8f8bed68be0045f9da8f34136198fb |

- **Plot summary** (189 rows). One row per surveyed plot. Fractional
  cover, vegetation height, and soil depth are means of the four
  quadrants. `site_code_plot` is unique.
- **Plot species** (1974 rows). One row per accepted taxon per plot.
  Live and dead cover are the sum of the quadrant percent covers divided
  by 4. Counts are sums across quadrants. `site_code_plot` plus
  `genus_species_combo` is unique except where noted in Quality
  assessment.
- **Quadrat summary** (756 rows). One row per quadrant.
  `site_code_plot_quadrant` is unique. All 189 plots have NE, NW, SE,
  and SW.
- **Quadrat species** (4795 rows). One row per taxon recorded in a
  quadrant, after identical rows were dropped. The plot species file is
  the aggregate of the source table, not of this file after duplicate
  removal.

**Spatial file.** bioscape_veg_spatial_v20261001.gpkg is a GeoPackage in
WGS 84 (EPSG:4326). Layers are `plot_centers`, `plot_parking`, and
`plot_polygons`.

- `plot_centers`, 190 points and 189 plot ids. T114 is stored 2 times.
  Center ids absent from the plot summary: none. `bioscape_plot_id`
  matches `plot`.
- `plot_parking`, 122 points where a vehicle was parked for the survey.
- `plot_polygons`, 188 Patches of vegetation the botanists deemed to be
  representative of the plot (e.g. similar species, post-fire age, soil)
  that are at least 15 meters in radius and ideally over 50 meters.

`plot_centers` attributes:

| Variable | Description |
|----|----|
| bioscape_plot_id | Name of the BioSCape plot. Matches `plot`. |
| region | General region where the plot is located. |
| botanist | Name of the botanist who conducted the vegetation survey. |
| date_time | Clock time of the GPS measurement, `YYYY-MM-DDThh:mm:ss`. The source text did not include a timezone. |
| name | Name the botanist gave the plot, kept for provenance. |
| plot_note | Notes made by the botanist, kept for provenance. |
| description | Short free-text note. |
| quality_flag | Flag assigned by the Townsend team while visiting the plot during BioSCape. Values are listed below. |
| quality_flag_2 | A second Townsend visit flag. One plot is `Boundary`. |
| location_flag | The original GPS may not have been taken at the plot center. This flag is set when the BioSCape center and the Townsend center are 10 m apart. The stored value is `See Townsend alternative location`. See `townsend_long` and `townsend_lat`. |
| pt_plot_a | Townsend opportunistic plot associated with this BioSCape plot. Additional plots were collected where the original plot had a quality flag, where nearby vegetation was similar but had a different dominant species, or where a nearby patch was a single species. |
| pt_plot_b | A second Townsend plot label. See `pt_plot_a`. |
| pt_plot_c | A third Townsend plot label. See `pt_plot_a`. |
| pt_plot_d | A fourth Townsend plot label. See `pt_plot_a`. |
| townsend_long | Alternative longitude, decimal degrees, of the plot center marked by the Townsend team. |
| townsend_lat | Alternative latitude, decimal degrees, of the plot center marked by the Townsend team. |
| geom | Longitude and latitude of the BioSCape plot center. |

`quality_flag` values in `plot_centers`:

| quality_flag     | Plots |
|:-----------------|------:|
| Low vegetation   |    24 |
| Boundary         |    10 |
| Other            |     7 |
| Trampled         |     3 |
| Accessibility    |     2 |
| Removed invasive |     2 |
| Grazing          |     1 |

`Boundary` means the plot was placed on the boundary of two vegetation
types. The Townsend team then set up additional plots nearby to record
the distinct communities (`pt_plot_a` through `pt_plot_d`).
`Low vegetation` means cover was very low, and an alternative plot was
often marked nearby where cover was higher. `Trampled` means previous
visitors had trampled the plot. `Removed invasive` means a Townsend-team
botanist removed an invasive individual that could have been present
during the flights. `Grazing` means there was evidence of grazing. An
invaded flag means invasive or alien plants were present, and a
recent-burn flag means there was evidence of recent fire. Neither of
those two labels is used in this file. `Other` and `Accessibility` are
stored on some plots.

`plot_parking` columns are `bioscape_plot_id`, `region`, `name`,
`description`, and `date_time`. `plot_polygons` columns are
`bioscape_plot_id`, `region`, `botanist`, `name`, `description`, and
`date_time`.

**Code.** `workflow/plot_data_compile_xlsx.R` in
<https://github.com/BioSCape-io/BioSCape-terrestrial>. Released tables:
<https://github.com/BioSCape-io/BioSCape-terrestrial/releases/tag/v20241104>.

### Data file properties

Tabular CSV, plus one GeoPackage. The four tables have no map
projection. bioscape_veg_spatial_v20261001.gpkg uses geographic WGS 84
(EPSG:4326). Processing level: in situ field observations, with accepted
names applied at compilation. These are not a satellite Level 2–4
product.

### Data details

Shared codes:

- `quadrant`: NE, NW, SE, SW.
- `clonal` and `clonal_yes_no`: `yes`, `no`, or `NA`.
- `seasonally_apparent`: `0`, `1`, or `-9999`.
- `groundwater`: Well-drained, Impeded drainage, Seepage, Swamp, Stream
  bank, Suurvlakte, or `NA`.
- `sampled` on the plot summary is `1` for every row.

**bioscape_veg_plot_summary_v20261001.csv**

| Variable | Units | Description |
|----|----|----|
| record_id |  | Unique row number in this file. |
| plot |  | Plot code, `T` plus three digits. Unique in this file. |
| site_code |  | Survey area name. |
| site_code_plot |  | Site and plot joined by `_`. Unique row key. |
| gps_plot_centre |  | Text note on how the center was recorded. Not a coordinate. |
| observer |  | Botanist named on the field sheet. `deb` is stored as Douglas Euston-Brown. |
| percent_bare_soil | percent | Mean of the four quadrant estimates. Missing: `-9999`. |
| percent_bare_rock | percent | Mean of the four quadrant estimates. Missing: `-9999`. |
| percent_dead_vegetation | percent | Mean of the four quadrant estimates. Missing: `-9999`. |
| percent_live_vegetation | percent | Mean of the four quadrant estimates. Missing: `-9999`. |
| veg_height_mean_cm | cm | Mean vegetation height across quadrants. Missing: `-9999`. |
| soil_depth_cm | cm | Mean soil depth across quadrants. Missing: `-9999`. |
| groundwater |  | Drainage class taken from the quadrants. |
| access_notes |  | Free-text access note. |
| sampled |  | Completed-survey flag. Always 1. |

This file has no survey date. Take `date` from the quadrat summary,
joining on `plot`. Field surveys were recorded by Ross Turner, Douglas
Euston-Brown, Steven Molteno, Stuart Hall, and Paul Emms.

**bioscape_veg_plot_species_v20261001.csv**

| Variable | Units | Description |
|----|----|----|
| record_id |  | Unique row number in this file. |
| plot |  | Plot code. |
| site_code |  | Survey area name. |
| site_code_plot |  | Site and plot joined by `_`. |
| accepted_genus |  | Accepted genus. |
| accepted_species |  | Accepted species epithet. |
| name_check |  | Name-check code copied from the field sheet. The list that issued the numbers is not identified in the source workbooks. Missing: `NA`. |
| genus_species_combo |  | Accepted genus and species as one label. |
| percent_cover_alive | percent | Sum of quadrant live cover, divided by 4. Missing: `-9999`. |
| percent_cover_dead | percent | Sum of quadrant dead cover, divided by 4. Missing: `-9999`. |
| abundance_alive_count | count | Sum of live counts across quadrants. Missing: `-9999`. |
| abundance_dead_count | count | Sum of dead counts across quadrants. Missing: `-9999`. |
| mean_canopy_diameter_cm | cm | Mean of recorded quadrant canopy diameters. Missing: `-9999`. |
| clonal |  | `yes`, `no`, or `NA`. |
| seasonally_apparent |  | Maximum of the quadrant values. Missing: `-9999`. |

**bioscape_veg_quadrat_summary_v20261001.csv**

| Variable | Units | Description |
|----|----|----|
| record_id |  | Unique row number in this file. |
| plot |  | Plot code. |
| site_code |  | Survey area name. |
| site_code_plot |  | Site and plot joined by `_`. |
| site_code_plot_quadrant |  | Site, plot, and quadrant joined by `_`. Unique row key. |
| quadrant |  | NE, NW, SE, or SW. |
| gps_plot_centre |  | Text note. Not a coordinate. |
| observer |  | Botanist named on the field sheet. |
| date |  | Survey date, `YYYY-MM-DD`. |
| percent_bare_soil | percent | Quadrant estimate. Missing: `-9999`. |
| percent_bare_rock | percent | Quadrant estimate. Missing: `-9999`. |
| percent_dead_vegetation | percent | Quadrant estimate. Missing: `-9999`. |
| percent_live_vegetation | percent | Quadrant estimate. Missing: `-9999`. |
| total_cover_check | percent | Sum of the four cover fractions. 14 quadrants are not 100 and are not missing. Missing: `-9999`. |
| veg_height_cm | cm | Vegetation height. Missing: `-9999`. |
| post_fire_age_years | text | Time since fire as written on the sheet. The column is text because some entries are notes or Excel date serials rather than ages. |
| soil_depth_cm | cm | Soil depth. Missing: `-9999`. |
| groundwater |  | Drainage class. |
| parking_gps |  | Text note that a parking location was recorded. Not a coordinate. |
| access_notes |  | Free-text access note. |
| comments |  | Free-text site description. |
| sheet_name |  | Source workbook: Ross01, Ross01b, Ross01c, Bio02, Bio03, or Bio04. |
| old_plot |  | Plot number before the Swartberg renumbering in Methods. Missing: `-9999`. |

**bioscape_veg_quadrat_species_v20261001.csv**

| Variable | Units | Description |
|----|----|----|
| record_id |  | Unique row number in this file. |
| plot |  | Plot code. |
| site_code |  | Survey area name. |
| site_code_plot |  | Site and plot joined by `_`. |
| site_code_plot_quadrant |  | Site, plot, and quadrant joined by `_`. |
| quadrant |  | NE, NW, SE, or SW. |
| accepted_genus |  | Accepted genus. |
| accepted_species |  | Accepted species epithet. `NA` when the record is genus only. |
| subspecies_or_variant |  | Subspecies or variant when one was recorded. |
| genus_species_combo |  | Genus and species label. |
| name_check |  | Name-check code. `NA` on 167 of 4795 rows. |
| new_species |  | Field note for a new or uncertain taxon. URLs were removed. Remaining values can be notes or times. |
| clonal_yes_no |  | `yes`, `no`, or `NA`. |
| mean_canopy_diameter_cm | cm | Canopy diameter of the taxon in the quadrant. Missing: `-9999`. |
| abundance_alive_count | count | Live individuals. Missing: `-9999`. |
| percent_cover_alive | percent | Live cover in the quadrant. Missing: `-9999`. |
| percent_cover_dead | percent | Dead cover in the quadrant. Missing: `-9999`. |
| abundance_dead_count | count | Dead individuals. Missing: `-9999`. |
| seasonally_apparent |  | `0`, `1`, or `-9999`. |
| comment |  | Note on the taxon in that quadrant. |
| clonal |  | `yes`, `no`, or `NA`. |
| taxon |  | Same label as `genus_species_combo`. |
| old_plot |  | Plot number before Swartberg renumbering. Missing: `-9999`. |

## Application and derivation

These plots are the environmentally stratified vegetation surveys for
BioSCape. They are the ground observations for comparing field
composition and structure with the campaign airborne imaging
spectroscopy and lidar (AVIRIS-NG, PRISM, LVIS, and HyTES) over the
Greater Cape Floristic Region (Cardoso et al. 2025). Use the plot
species table for one cover and count per taxon per plot. Use the
quadrat tables when the within-plot measurements matter. The
woody-invasive plots (ORNL DAAC <https://doi.org/10.3334/ORNLDAAC/2511>)
were sited near this network and are comparable only after that
placement difference is taken into account.

## Quality assessment

Cover fractions are visual estimates. This version does not add an
uncertainty column. `total_cover_check` is the field sum of bare soil,
bare rock, dead vegetation, and live vegetation. Quadrants that do not
sum to 100:

| site_code_plot_quadrant | total_cover_check |
|:------------------------|------------------:|
| capepoint_T070_NE       |               104 |
| capepoint_T070_NW       |               105 |
| capepoint_T070_SE       |               102 |
| capepoint_T070_SW       |               105 |
| capepoint_T265_NE       |               120 |
| capepoint_T265_NW       |               130 |
| capepoint_T265_SE       |               150 |
| capepoint_T265_SW       |               140 |
| gardenroute_T148_SW     |               101 |
| langeberg_T117_NE       |               107 |
| langeberg_T117_NW       |               105 |
| outeniqua_T261_NW       |                90 |
| rooiberg_T003_SW        |               103 |
| swartberg_T026_NW       |               115 |

Version 1 changes from the source tables:

- Site codes on 12 plots in the species tables were replaced with the
  plot-summary spelling so `site_code_plot` joins.
- 1 identical quadrat-species rows were removed.
- 1 web address in `new_species` was removed.
- The observer value `deb` and the spatial botanist name
  `Doug Euston-Brown` are stored as Douglas Euston-Brown.

13 quadrat-by-taxon keys still occur more than once because the cover or
count differed. `record_id` keeps those rows distinct. The plot species
file was summed from the source table, so those repeated measurements
are included in its cover and counts.

| site_code_plot_quadrant | genus_species_combo          |   n |
|:------------------------|:-----------------------------|----:|
| anysberg_T106_SE        | Elytropappus rhinocerotis    |   2 |
| anysberg_T108_NW        | Ruschia pungens              |   2 |
| cederberg_T243_SW       | Anthospermum aethiopicum     |   2 |
| dehoop_T132_NE          | Pterocelastrus tricuspidatus |   2 |
| gardenroute_T147_NE     | Helichrysum patulum          |   2 |
| gardenroute_T147_NW     | Helichrysum patulum          |   2 |
| gardenroute_T147_SE     | Helichrysum patulum          |   2 |
| gardenroute_T147_SW     | Helichrysum patulum          |   2 |
| langeberg_T186_SE       | Erica anguliger              |   2 |
| langeberg_T186_SE       | Penaea mucronata             |   2 |
| outeniqua_T185_SE       | Protea neriifolia            |   2 |
| rooiberg_T003_SW        | Drosanthemum                 |   2 |
| rooiberg_T245_SW        | Agathosma mundtii            |   2 |

`post_fire_age_years` remains text. `name_check` is the code written on
the field sheet; the authority list is not in these tables.

## Data acquisition, materials, and methods

Plots were placed across reserves and passes in the Greater Cape
Floristic Region for the 2023 BioSCape field campaign (Cardoso et al.
2025). Each surveyed plot has four quadrants: NE, NW, SE, and SW. In
each quadrant the botanist recorded fractional cover of bare soil, bare
rock, dead vegetation, and live vegetation; vegetation height; soil
depth; drainage; and, for each vascular plant taxon, live and dead
percent cover, live and dead counts, and canopy diameter. Centers were
located with a Trimble GPS. The coordinate values are in the
`plot_centers` layer of bioscape_veg_spatial_v20261001.gpkg.

The version 1 files were written by this document. Column names are
snake case. Numeric gaps are `-9999` and text gaps are `NA`. Survey
dates are `YYYY-MM-DD`. GPS times are `YYYY-MM-DDThh:mm:ss` with the
clock time taken from the source string. Each CSV has `record_id`.
Clonal codes are `yes` or `no`. `old_plot` is the plot number before
renumbering.

Field records were kept in six Google Sheets workbooks (Ross01, Ross01b,
Ross01c, Bio02, Bio03, Bio04) and downloaded on 2024-11-04.
`workflow/plot_data_compile_xlsx.R` stacked the SiteData tabs and the
plot tabs, and rebuilt the plot and quadrant identifiers. Swartberg plot
numbers that collided with numbers used at other sites were renumbered.
The previous number is `old_plot`. The source script maps Swartberg 20
to T110, 22 to T012, 23 to T013, and 24 to T014.

Plot summary cover, height, and soil depth are the mean of the four
quadrants. Plot species live and dead cover are the sum of that taxon’s
quadrant covers divided by 4, and counts are sums. Unrecorded quadrant
occurrences add nothing to the sum. With four quadrants in every
released plot, dividing by 4 is that taxon’s mean contribution to the
plot.

## References

Cardoso, A.W., E.L. Hestir, J.A. Slingsby, C.J. Forbes, G.R. Moncrieff,
W. Turner, A.L. Skowno, J. Nesslage, P.G. Brodrick, K.D. Gaddis, and
A.M. Wilson. 2025. The biodiversity survey of the Cape (BioSCape),
integrating remote sensing with biodiversity science. *npj Biodiversity*
4:2. <https://doi.org/10.1038/s44185-024-00071-5>

Fitzpatrick, M.C., D. Spalink, A.J. Elmore, J. Measey, and S.
Kritzinger-Klopper. 2026. BioSCape: Vegetation Composition at Plots with
Woody Invasives, CFR, South Africa. ORNL DAAC, Oak Ridge, Tennessee,
USA. <https://doi.org/10.3334/ORNLDAAC/2511>

Slimp, M., J. Malindi, R. Meyer, A. Cunningham, J. Rourke, M. Hayden, A.
Hargey, J. Nesslage, S. Johnson, M. Rossi, and N. Stavros. 2025.
BioSCape Vegetation Surveys Berg and Eerste River Catchments, South
Africa, 2022–2023. ORNL DAAC, Oak Ridge, Tennessee, USA.
<https://doi.org/10.3334/ORNLDAAC/2425>

## Dataset revisions

This release is compiled from v20241104 vegetation tables and the
v20241114 spatial GeoPackage from the team GitHub repository. It was
updated to conform to ORNL data standards as v20261001. This is the
first submission of these tables to ORNL DAAC. There is no prior DOI.
