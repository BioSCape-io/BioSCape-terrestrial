# Rarefaction Plan

## Goal

Fix bugs and improve robustness in `Vegplot_Rarefaction.qmd`, which processes the
cleaned photo dataset to derive per-plot species accumulation curves and rarefaction
estimates.

---

## Bug Fixes (Critical)

### 1. EML `attributeList` assignment syntax error (line 379)
**Problem:** `attributeList <- EML::set_attributes(...)` inside the `dataTable1` list
uses `<-` (assignment to global env) instead of `=` (list element). The attribute list
is silently lost from `dataTable1`.  
**Fix:** Change `attributeList <-` to `attributeList =`.

### 2. `n_species` missing from `vegphoto_rarefied_plot` (line 628)
**Problem:** The comparison plot at line 628 uses `n_species` from `vegphoto_rarefied_plot`,
but the `select()` on line 280–282 does not include `n_species`. This will produce an
error or silently produce NAs.  
**Fix:** Add `n_species` (and optionally `n_genus`) to the `select()` call that builds
`vegphoto_rarefied_plot`.

### 3. `arrange(datetime)` missing before `row_number()` (line 88)
**Problem:** `count = row_number() - 1` assumes photos are time-ordered within each
`BScpPID` group. No `arrange(datetime)` is called before this, so counts may be
assigned in the wrong order if the data arrives unsorted.  
**Fix:** Add `arrange(datetime)` (within the group) before `mutate(count = row_number() - 1)`.

### 4. Dangling debug filter (line 577)
**Problem:** `predicted_data %>% filter(botanist == "")` is called and the result is
discarded. This is leftover debugging code that adds visual noise and a stray pipe.  
**Fix:** Remove lines 577–578.

---

## Robustness Improvements

### 5. `max(rarefaction_replicate)` with no `na.rm` (line 102)
**Problem:** If `rarefaction_replicate` is all-NA for a plot, `max()` returns `-Inf`
with a warning.  
**Fix:** Add `na.rm = TRUE`; wrap in `coalesce(..., 1L)` to default to 1 when missing.

### 6. Hardcoded data version (line 39)
**Problem:** `vegphoto_all_v20250228` is hardcoded. Every time the upstream data is
regenerated, this must be manually updated, which is easy to forget.  
**Fix:** Move the version string to a parameter at the top of the script:
```r
photo_version <- "vegphoto_v20250228"
photo_all_file <- paste0("data/vegphoto_all_", photo_version, ".csv")
```

### 7. Species replicate detection documentation (line 83)
**Problem:** `grepl("[0-9]", species)` flags any species name containing a digit as a
"replicate". This is intentional (replicates are named e.g. "sp1", "sp2") but is not
explained, and could accidentally flag legitimate names.  
**Fix:** Add a comment explaining the convention, e.g.:
```r
# Replicate photos are flagged with a number in the species field (e.g. "sp1", "sp2")
# by the botanists to indicate a repeat observation of the same species.
```

---

## Code Quality

### 8. Standardise on native pipe `|>`
Replace all `%>%` with `|>` throughout the script.

### 9. `rdatas` `st_set_geometry(NULL)` in `left_join` (line 547)
`rdatas` is an sf object (inherits geometry from `rdata`). When used in `left_join`
and ggplot calls, the geometry column may cause unexpected behaviour. Add
`st_set_geometry(NULL)` or `st_drop_geometry()` when building `rdatas`.

---

## Output Completeness

### 10. Add iNat tag back to veg dataset
The task description notes that after iNat upload, the iNat observation IDs should be
joined back to `bioscape_veg_plot_species_v20241104.csv`. A section should be added to
`Vegplot_Rarefaction.qmd` (or a separate script) to:
1. Download the iNat observation export for the BioSCape project.
2. Join on `taxon_name` + `BScpPID` (via `location` field).
3. Write an updated version of the veg species file with the `inat_id` column populated.
