# iNat Upload & Back-Link Plan

## Goal

Upload BioSCape botanist vegetation-plot photos to iNaturalist and create a
durable, taxonomy-independent link between each iNat observation and the
corresponding row in `bioscape_veg_plot_species_v20241104.csv`.

---

## Key Design Decisions

### Consistent naming across datasets

All files and scripts must use the naming conventions from the veg plot data:

| Concept | Column name |
|---|---|
| Site code (location name) | `SiteCode` |
| Plot number within site | `Plot` |
| Combined site + plot identifier | `SiteCode_Plot` |
| Unique species-at-plot key | `SiteCode_Plot_SPID` |

The photo pipeline currently uses `BScpPID` for the combined site+plot
identifier. This should be treated as equivalent to `SiteCode_Plot` and renamed
wherever it appears in `iNat_prepare.qmd` and downstream scripts.

### Taxonomy-independent join key: `SiteCode_Plot_SPID`

Joining on species name alone is fragile — iNat may update a taxon name after
upload, breaking the match. Instead, each row in the veg plot table is assigned
a stable numeric species ID (`SPID`) and a compound key
`SiteCode_Plot_SPID` (e.g. `vrolijkheid_T001_SP0042`). This key is embedded
in every iNat observation as a custom observation field and is the primary
key for joining iNat observation IDs back to the veg plot table.

---

## Step 1 — Add stable species IDs to the veg plot data

**File:** `bioscape_veg_plot_species_v20241104.csv`

Add two new columns:

```r
veg_plot <- read_csv("data/bioscape_veg_plot_species_v20241104.csv") |>
  mutate(
    # SPID: zero-padded sequential integer, stable within this version of the file
    SPID              = sprintf("SP%04d", row_number()),
    # Primary join key: site + plot + species ID
    SiteCode_Plot_SPID = paste(SiteCode_Plot, SPID, sep = "_")
  )

write_csv(veg_plot, "data/bioscape_veg_plot_species_v20241104.csv")
```

> **Note:** `SPID` is assigned by row order in this versioned file. Do not
> re-order or insert rows between versions without regenerating SPIDs and
> re-uploading.

---

## Step 2 — Update `botanist_photo_processing.qmd`

Two bug fixes must be applied before running `iNat_prepare.qmd`:

1. **`photo_type` regex** — the current pattern
   `"plot|S|N|W|E|start|rarefaction|end|stop"` matches the single letters
   S/N/W/E anywhere inside a genus name (e.g. "Erica" → matches "E").
   Fix: use `\\b(plot|start|rarefaction|end|stop)\\b|^[SNWE]$` so cardinal
   directions only match when the entire `genus` field is that letter.

2. **`st_drop_geometry()` before `write_csv`** — the sf geometry column is
   currently serialised as WKT into the CSV. Drop it before writing.

The output `vegphoto_all_<tagdate>.csv` should include columns:
`SiteCode_Plot` (= current `BScpPID`), `SiteCode`, `Plot`, `genus`,
`species`, `photo_type`, `date`, `gps_latitude`, `gps_longitude`,
`gps_position_error`, `description`, `botanist`, `newfile`.

---

## Step 3 — Update `iNat_prepare.qmd`

### 3a. Rename `BScpPID` → `SiteCode_Plot`

After loading `photo_all`, rename and derive the consistent columns:

```r
photo_all <- photo_all |>
  rename(SiteCode_Plot = BScpPID) |>
  # Derive SiteCode and Plot from SiteCode_Plot if not already present
  mutate(
    SiteCode = str_extract(SiteCode_Plot, "^[^_]+"),
    Plot     = str_extract(SiteCode_Plot, "[^_]+$")
  )
```

### 3b. Join to veg plot data to obtain `SPID` and `SiteCode_Plot_SPID`

```r
veg_plot <- read_csv("data/bioscape_veg_plot_species_v20241104.csv",
                     show_col_types = FALSE) |>
  select(SiteCode_Plot, Genus_Species_combo, SPID, SiteCode_Plot_SPID)

species_photos <- species_photos |>
  mutate(Genus_Species_combo = paste(genus, species)) |>
  left_join(veg_plot,
            by = c("SiteCode_Plot", "Genus_Species_combo"))
```

Flag photos that do not match any veg plot row — they will have
`SiteCode_Plot_SPID = NA` and should be reviewed before upload.

### 3c. Populate iNat observation fields (multiple fields)

In `build-template`, replace the stub field columns with:

```r
field_id1    = "BioSCape SiteCode",
field_value1 = SiteCode,
field_id2    = "BioSCape Plot",
field_value2 = Plot,
field_id3    = "BioSCape SiteCode_Plot_SPID",
field_value3 = SiteCode_Plot_SPID,
```

> **Prerequisite:** The three observation fields must be created on
> iNaturalist first (via iNat project settings or the iNat API). The
> `field_id` value must match the exact field name as it appears on iNat.
> Confirm the exact field IDs and update the strings above accordingly.

Also update `location` to use `SiteCode_Plot` and embed it in `description`:

```r
location    = SiteCode_Plot,
description = paste0(
  "BioSCape Vegetation Plot=", SiteCode_Plot,
  "; SiteCode=", SiteCode,
  "; Plot=", Plot,
  "; SPID=", SiteCode_Plot_SPID,
  "; Botanist=", botanist,
  "; Notes=", description
),
```

### 3d. Add pre-upload validation

```r
# Obs with no veg-plot match (no SPID assigned)
unmatched <- inat_table |> filter(is.na(SiteCode_Plot_SPID))
if (nrow(unmatched) > 0)
  warning(nrow(unmatched), " observations have no veg-plot match — review before upload.")

# Veg-plot rows with no iNat observation
no_obs <- veg_plot |>
  anti_join(inat_table, by = "SiteCode_Plot_SPID")
message(nrow(no_obs), " veg-plot species rows have no photos / iNat observation.")
```

### 3e. Updated column set for iNat template

Add `field_id3` / `field_value3` to the `select()` call and to
`template_cols` in the write-batches chunk.

---

## Step 4 — Post-upload join script

After uploading all batches:

1. Export your observations from iNaturalist (Project → Export, or via the
   iNat API filtering by tag "BioSCape").
2. The export CSV will include `id` (iNat observation ID) and a column for
   each observation field value.

```r
inat_export <- read_csv("data/inat_export.csv", show_col_types = FALSE)

# Column name from iNat export for the SPID field — adjust if needed
spid_col <- "field:BioSCape SiteCode_Plot_SPID"

veg_plot_with_inat <- veg_plot |>
  left_join(
    inat_export |>
      select(inat_obs_id = id,
             inat_url    = url,
             SiteCode_Plot_SPID = all_of(spid_col)),
    by = "SiteCode_Plot_SPID"
  )

write_csv(veg_plot_with_inat,
          "data/bioscape_veg_plot_species_inat_linked.csv")
```

---

## File Change Summary

| File | Change |
|---|---|
| `bioscape_veg_plot_species_v20241104.csv` | Add `SPID` and `SiteCode_Plot_SPID` columns |
| `botanist_photo_processing.qmd` | Fix `photo_type` regex; add `st_drop_geometry()` before write; ensure output uses `SiteCode_Plot` not `BScpPID` |
| `iNat_prepare.qmd` | Rename `BScpPID`→`SiteCode_Plot`; join veg plot to get SPID; populate 3 observation fields; add validation; update template cols |
| `inat_plan.md` | Update column mapping table to reflect 3 observation fields and `SiteCode_Plot_SPID` key |
| *(new)* `workflow/inat_backlink.R` | Post-upload join script (Step 4) |

---

## Open Questions / Pre-flight Checklist

- [ ] Confirm `BScpPID` format matches `SiteCode_Plot` exactly (e.g. `vrolijkheid_T001` in both datasets).
- [ ] Create the three iNat observation fields on iNaturalist and record their exact field name strings.
- [ ] Decide whether `SPID` should be re-generated fresh from a new versioned CSV or carried forward from the current file if rows are added.
- [ ] Run the photo-renaming chunk in `botanist_photo_processing.qmd` (`eval=F`) before `iNat_prepare.qmd` so `newfile` is populated.
- [ ] Test with `batch_01.csv` (≤50 obs) before uploading all batches.
