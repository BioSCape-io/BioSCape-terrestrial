# BioSCape Vegetation Plot Photo Rarefaction
Adam M. Wilson, Jasper Slingsby, and the BioSCape Team
2024-12-04

- [Summary](#summary)
- [Summarize vegetation cover data](#summarize-vegetation-cover-data)
- [Plot and Photo locations](#plot-and-photo-locations)
- [Species Accumulation Curves](#species-accumulation-curves)
  - [Data description](#data-description)
    - [vegphoto_rarefied_photo - Column
      descriptions](#vegphoto_rarefied_photo---column-descriptions)
    - [vegphoto_rarefied_plot - Column
      descriptions](#vegphoto_rarefied_plot---column-descriptions)
- [Build final table and post to
  Github](#build-final-table-and-post-to-github)
- [Visualization: Rarefaction Curves](#visualization-rarefaction-curves)
- [Visualization: Rarefaction Curves by
  Plot](#visualization-rarefaction-curves-by-plot)
  - [Example Curves](#example-curves)
  - [All plots](#all-plots)
  - [Rarefaction estimates compared to Dominant Species
    Richness](#rarefaction-estimates-compared-to-dominant-species-richness)
  - [Rarefaction estimates compared to total vegetation
    cover](#rarefaction-estimates-compared-to-total-vegetation-cover)
- [Known Issues](#known-issues)

## Summary

This analysis processes vegetation rarefaction photography data to
derive insights about species discovery over time and across different
plots. The data includes vegetation photos taken at different plots,
with each photo representing a unique species observation. The analysis
includes the following steps:

1.  **Data Preparation**: Load and preprocess vegetation photo data,
    including filtering out replicates and calculating time intervals
    between photos.
2.  **Rarefaction Curves**: Visualize rarefaction curves for each plot,
    showing the number of unique species observed over time.
3.  **Species Accumulation Curves**: Fit a Michaelis-Menten model to
    each site’s species accumulation curve to estimate total species
    richness and sampling effort.
4.  **Comparison with Dominant Species Richness**: Compare rarefaction
    estimates with dominant species richness from cover data.
5.  **Comparison with Total Vegetation Cover**: Compare rarefaction
    estimates with total vegetation cover.
6.  **Upload to Github Release**: Publish preliminary version for review

<details class="code-fold">
<summary>Show the code</summary>

``` r
# Load necessary libraries
library(tidyverse)
library(vegan)
library(sf)
library(piggyback)
library(broom)
library(leaflet)
library(cowplot)
library(DT)
# Downloading Data
photo_all_file <- paste0("data/", "vegphoto_v20241126", ".csv")
pb_download(file = "vegphoto_v20241126.csv", dest = "data", tag = "vegphoto_v20241126")
data <- read_csv(photo_all_file) %>% 
  st_as_sf(crs = 4326, coords = c("gps_longitude", "gps_latitude"))


# Download spatial data
pb_download(file="BioScapeSpatialProducts2024_11_12.zip",dest = "data")
unzip("data/BioScapeSpatialProducts2024_11_12.zip",exdir = "data/spatial")


plots=read_sf("data/spatial/BioSCapeVegCenters2024_11_12.shp")
homogeneous=read_sf("data/spatial/BioSCapeVegPolys2024_11_12.shp")

# download vegplot cover data
pdatafile="bioscape_veg_plot_summary_v20241104.csv"
pb_download(file=pdatafile,tag="v20241104",dest = "data")
pdata=read_csv(file.path("data",pdatafile))

vdatafile="bioscape_veg_plot_species_v20241104.csv"
pb_download(file=vdatafile,tag="v20241104",dest = "data")
vdata=read_csv(file.path("data",vdatafile))
```

</details>

# Summarize vegetation cover data

<details class="code-fold">
<summary>Show the code</summary>

``` r
vdatas=vdata %>% 
  group_by(Plot) %>% 
  summarize(n_species=n_distinct(Genus_Species_combo),
            n_genus=n_distinct(AcceptedGenus),
            total_cover=sum(PercentCoverAlive))# %>% 
#  left_join(pdata,by="BScpPID")
```

</details>

<details class="code-fold">
<summary>Show the code</summary>

``` r
# Transforming Rarefaction Data
rdata <- data %>%
  group_by(BScpPID, rarefaction_replicate) %>%
  mutate(species_replicate = grepl("[0-9]",species)) %>%  #flag species replicates with numbers in the name
  filter(rarefaction_photo == 1 & !species_replicate) %>% # keep only rarefaction photos that are not replicates
  mutate(
    rarefaction_time = datetime - min(datetime),
    rarefaction_min = as.numeric(rarefaction_time) / 60,
    count = row_number() - 1,
    plot_number_replicate = paste(BScpPID, rarefaction_replicate, sep = "."),
  ) %>%
  select(BScpPID, plot_number_replicate, description, genus, species, species_replicate, datetime, time, 
         rarefaction_time, rarefaction_min, rarefaction_replicate, count, botanist)

# create plot-level summaries
rdatas <- rdata %>% 
  group_by(BScpPID) %>%
  summarize(datetime_start=min(datetime),
            rarefaction_n_5min=max(if_else(rarefaction_min <= 5, count, NA_real_), na.rm = TRUE),
            rarefaction_n_10min = max(if_else(rarefaction_min <= 10, count, NA_real_), na.rm = TRUE),
            rarefaction_n_max=max(count),
            rarefaction_replicates=max(rarefaction_replicate)) %>% 
  left_join(vdatas, by = c("BScpPID" = "Plot"))
```

</details>

# Plot and Photo locations

<details class="code-fold">
<summary>Show the code</summary>

``` r
# Create a leaflet map
leaflet() %>%
  #addProviderTiles("Esri.WorldImagery",tileOptions(maxZoom = 50)) %>%
  addWMSTiles(
    baseUrl = "http://aerial.openstreetmap.org.za/ngi-aerial/{z}/{x}/{y}.jpg",  # Replace with your WMS URL
    layers = "ngi-aerial",  # Replace with the desired WMS layer name
    options = WMSTileOptions(format = "image/jpg", transparent = TRUE,maxZoom=22),
    attribution = "© SA CDNGI Geospatial 25cm"
  ) %>% 
  addPolygons(data=homogeneous,fillColor="blue",fillOpacity=0.2,
              popup = ~paste("Region: ",homogeneous$Region,
                             "<br>Plot: ",homogeneous$BScpPID)) %>%
  addCircleMarkers(data=data,
                   popup=~paste("Plot:",data$BScpPID,"<br>Photo Description:",data$description,"<br>Genus:",data$genus,"<br>Species:",data$species),
#                                "<br> <img src='data/photo_out/",photo_all3$thumbfile , "'   #style='width:150px;height:auto;'>"),
                   color="red",radius=1) %>%
  addCircleMarkers(data=plots,lng=st_coordinates(plots)[,"X"], lat=st_coordinates(plots)[,"Y"],
                   popup=~BScpPID,
                   color="blue",radius=3)
```

</details>

![](Vegplot_Rarefaction_files/figure-commonmark/unnamed-chunk-4-1.png)

# Species Accumulation Curves

In addition the raw data, we also fit a Michaelis-Menten model to each
site’s species accumulation curve using nls.

$$
S_{\text{obs}} = \frac{S_{\text{max}} \cdot \text{effort}}{B + \text{effort}}
$$

Where:

$S_{\text{obs}}$ ~ Number of species observed after n units of sampling
$S_max$ ~ Total number of species in the pool $B$ ~ Sampling effort
needed to detect 50% of those species $effort$ ~ Sampling effort (time
in minutes)

<details class="code-fold">
<summary>Show the code</summary>

``` r
d1 <- st_set_geometry(rdata,NULL) %>% 
  group_by(BScpPID)

# Define a function to fit the model
fit_nls <- function(data) {
  tryCatch(
    {
      nls(
        count ~ (Smax * rarefaction_min) / (B + rarefaction_min),
        data = data,
        start = list(Smax = max(data$count), B = 2)
      )
    },
    error = function(e) NULL
  )
}

# Fit models for each site
models <- d1 %>%
  group_by(BScpPID) %>%
  summarize(
    model = list(fit_nls(cur_data())),
    .groups = "drop"
  )

# Extract model parameters
results <- models %>%
  mutate(
    coefficients = map(model, ~ if (!is.null(.)) coef(.) else NA),
    Smax = map_dbl(coefficients, ~ if (!is.null(.)) .["Smax"] else NA),
    B = map_dbl(coefficients, ~ if (!is.null(.)) .["B"] else NA)
  )

# Add predictions to the dataset
predicted_data <- d1 %>%
  left_join(models, by = "BScpPID") %>%
  mutate(
    predicted_richness = map2_dbl(
      rarefaction_min, model,
      ~ if (!is.null(.y)) predict(.y, newdata = tibble(rarefaction_min = .x)) else NA
    )
  )
```

</details>

## Data description

The rarefaction data are shared in two formats: photo-level data and
plot-level data. The photo-level data include information about each
photo taken, including the plot, species observed, and time of
observation. The plot-level data summarize the rarefaction data for each
plot, including the estimated species richness and sampling effort.

<details class="code-fold">
<summary>Show the code</summary>

``` r
# Assemble and organize photo-level data
vegphoto_rarefied_photo <- rdata %>%
  select(BScpPID, description, genus, species, species_replicate, datetime, time, 
         rarefaction_time, rarefaction_min, rarefaction_replicate, count, botanist)

# Assemble and simplify plot data 
vegphoto_rarefied_plot <- rdatas %>%
  left_join(select(results,BScpPID,Smax,B), by = "BScpPID") %>%
  select(BScpPID, datetime_start, 
         rarefaction_n_5min, rarefaction_n_10min, rarefaction_n_max, rarefaction_replicates,
         Smax, B)
```

</details>

### vegphoto_rarefied_photo - Column descriptions

- BScpPID - plot id aligned with spatial vegplot data
- description - original caption entered by the botanists in the field
- genus - genus of the observed species (when available)
- species - species of the observed species (when available)
- species_replicate - flag indicating if the species is a replicate
  photo and should be filtered
- datetime - date and time of the photo
- time - time of day of the photo
- rarefaction_time - time since the first photo in the rarefaction
  sequence
- rarefaction_min - time since the first photo in the rarefaction
  sequence in minutes
- rarefaction_replicate - replicate number of the rarefaction sequence
  (some botanists recorded 4 separate rarefaction sequences,
  unfortunately)
- count - cumulative count of unique species observed in the rarefaction
  sequence
- botanist - name of the botanist who took the photo

<details class="code-fold">
<summary>Show the code</summary>

``` r
vegphoto_rarefied_photo |>
  ungroup() |>
  slice(1:20) |>
  datatable()
```

</details>

![](Vegplot_Rarefaction_files/figure-commonmark/unnamed-chunk-8-1.png)

### vegphoto_rarefied_plot - Column descriptions

- BScpPID - plot id aligned with spatial vegplot data
- datetime_start - date and time of the first photo in the rarefaction
  sequence
- rarefaction_n_5min - estimated species richness after 5 minutes of
  sampling
- rarefaction_n_10min - estimated species richness after 10 minutes of
  sampling
- rarefaction_n_max - maximum estimated species richness in the
  rarefaction sequence
- rarefaction_replicates - number of replicates in the rarefaction
  sequence (some botanists recorded 4 separate rarefaction sequences -
  filter to values equal to 1 to avoid these plots)
- Smax - estimated total species richness in the pool from the
  rarefaction. Note that some plots have very high (unrealisitic) values
  for this field.
- B - sampling effort needed to detect 50% of those species

<details class="code-fold">
<summary>Show the code</summary>

``` r
vegphoto_rarefied_plot |>
  slice(1:20) |>
  datatable()
```

</details>

![](Vegplot_Rarefaction_files/figure-commonmark/unnamed-chunk-9-1.png)

# Build final table and post to Github

<details class="code-fold">
<summary>Show the code</summary>

``` r
tagdate=paste0("v",format(today(),"%Y%m%d"))
tag=paste0("vegphoto_",tagdate)


vegphoto_rarefied_plot_file=paste0("data/vegphoto_rarefied_plot_",tagdate,".csv")
write_csv(vegphoto_rarefied_plot,vegphoto_rarefied_plot_file)

vegphoto_rarefied_photo_file=paste0("data/vegphoto_rarefied_photo_",tagdate,".csv")

vegphoto_rarefied_photo |>
  st_set_geometry(NULL)|>
write_csv(vegphoto_rarefied_photo_file)


# tag = "vegphoto_v20241126"
if(F){
  print(tag)
# upload to github release is manual - run the pb_upload command if desired
  pb_new_release("BioSCape-io/BioSCape-terrestrial",tag=tag)
  pb_upload(vegplot_rarefiedphoto_plot_file,tag=tag)
  pb_upload(vegphoto_rarefied_photo_file,tag=tag)
}
```

</details>

# Visualization: Rarefaction Curves

Summary plot showing rarefaction curves and estimated species richness
for each plot.

<details class="code-fold">
<summary>Show the code</summary>

``` r
p1 <- rdata %>%
  left_join(select(st_set_geometry(rdatas,NULL), BScpPID, rarefaction_n_10min)) %>% 
  filter(!is.na(BScpPID) & rarefaction_min <= 10) %>%
  ggplot(aes(x = rarefaction_min, y = count, group = plot_number_replicate, color = rarefaction_n_10min)) +
  geom_line(alpha=0.7) +
  labs(y = "Rarefaction Photos (Observed Unique Species)", x = "Search Minutes")+
  theme(legend.position = "none")+
  scale_color_viridis_c(option="rocket")

p2 <- rdatas %>%
  ggplot(aes(x="All Plots",y = rarefaction_n_10min)) +
      geom_boxplot()+
  geom_jitter(width=0.1,alpha=.2)+
  labs(y="")

# Combine plots with aligned axes
plot_grid(p1, p2, align = "v", rel_widths = c(.8,.2),axis = "tblr")  # Align horizontal and vertical axes
```

</details>

![](Vegplot_Rarefaction_files/figure-commonmark/unnamed-chunk-11-1.png)

# Visualization: Rarefaction Curves by Plot

Rarefaction curves and estimated species richness for each plot
separately. Dashed lines indicate the 5- and 10-minute rarefaction
estimates

### Example Curves

<details class="code-fold">
<summary>Show the code</summary>

``` r
choose_plots = c("T002","T218","T196")
predicted_data %>%
  filter(!is.na(BScpPID) & rarefaction_min < 10, BScpPID%in%choose_plots) %>%
  ggplot(aes(x = rarefaction_min, y = count, group = plot_number_replicate)) +
  geom_hline(aes(yintercept = Smax), data = filter(results,BScpPID%in%choose_plots), linetype = "dashed",col="red") +
  geom_vline(aes(xintercept = B), data = filter(results,BScpPID%in%choose_plots), linetype = "dotted",col="red") +
  geom_point(col="black") +
  geom_line(aes(y=predicted_richness),col="red") +
  geom_hline(aes(yintercept = rarefaction_n_10min), data = filter(rdatas,BScpPID%in%choose_plots), linetype = "dashed") +
  geom_hline(aes(yintercept = rarefaction_n_5min), data = filter(rdatas,BScpPID%in%choose_plots), linetype = "dashed") +
  facet_wrap(~BScpPID,nrow = 1) +
  labs(y = "Rarefaction Photos", x = "Minutes") +
  scale_x_continuous(lim = c(0, 10), breaks = c(0, 5, 10))+
  scale_y_continuous(lim = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100))+
  labs(title="Rarefaction curves by plot" ,subtitle="Dashed lines indicate 5 and 10 minute rarefaction estimates")
```

</details>

![](Vegplot_Rarefaction_files/figure-commonmark/unnamed-chunk-12-1.png)

### All plots

<details class="code-fold">
<summary>Show the code</summary>

``` r
predicted_data %>%
  filter(!is.na(BScpPID) & rarefaction_min < 10) %>% #,BScpPID%in%c("T002","T083","T184")) %>%
  ggplot(aes(x = rarefaction_min, y = count, group = plot_number_replicate)) +
  geom_hline(aes(yintercept = Smax), data = results, linetype = "dashed",col="red") +
  geom_vline(aes(xintercept = B), data = results, linetype = "dotted",col="red") +
  geom_point(col="black") +
  geom_line(aes(y=predicted_richness),col="red") +
  geom_hline(aes(yintercept = rarefaction_n_10min), data = rdatas, linetype = "dashed") +
  geom_hline(aes(yintercept = rarefaction_n_5min), data = rdatas, linetype = "dashed") +
  facet_wrap(~BScpPID,nrow = 5) +
  labs(y = "Rarefaction Photos", x = "Minutes") +
  scale_x_continuous(lim = c(0, 10), breaks = c(0, 5, 10))+
  scale_y_continuous(lim = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100))+
  labs(title="Rarefaction curves by plot" ,subtitle="Dashed lines indicate 5 and 10 minute rarefaction estimates")
```

</details>

![](Vegplot_Rarefaction_files/figure-commonmark/unnamed-chunk-13-1.png)

## Rarefaction estimates compared to Dominant Species Richness

<details class="code-fold">
<summary>Show the code</summary>

``` r
rdatas |>
ggplot(aes(x = n_species, y = rarefaction_n_10min)) +
  geom_point() +
  geom_smooth(method = "lm") +
#  geom_text(aes(label = BScpPID), nudge_x = 0.5, nudge_y = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "# Species in Cover Data", y = "# Photos in Rarefaction Data")
```

</details>

![](Vegplot_Rarefaction_files/figure-commonmark/unnamed-chunk-14-1.png)

## Rarefaction estimates compared to total vegetation cover

<details class="code-fold">
<summary>Show the code</summary>

``` r
rdatas |>
ggplot(aes(x = total_cover, y = rarefaction_n_10min)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(aes(label = BScpPID), nudge_x = 0.5, nudge_y = 0.5) +
  labs(x = "Total Vegetation Cover (%)", y = "# Photos in Rarefaction Data")
```

</details>

![](Vegplot_Rarefaction_files/figure-commonmark/unnamed-chunk-15-1.png)

# Known Issues

- Some plots have very high (unrealistic) estimates of total species
  richness (Smax) due to the continuous new observations until the end
  of the 10-minute period. These estimates should be interpreted with
  caution. The `rarefaction_n_10min` and `rarefaction_n_5min` estimates
  are more reliable though underestimate total richness.
- Some plots have multiple rarefaction replicates (the botanist recorded
  multiple short rarefaction sequencies rather than one 10-minute
  sequence. These plots can be removed by filtering to
  `rarefaction_replicates==1`.
- Some plots may have duplicate photos of the same species - we will go
  through to ensure there is no double counting of species in the final
  analysis.
