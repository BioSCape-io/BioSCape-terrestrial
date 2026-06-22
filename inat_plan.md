# iNat Upload Plan

## Goal

Convert the cleaned photo dataset output from `botanist_photo_processing.qmd` into
batch CSV files ready for the iNat bulk uploader tool (`workflow/dist/iNat_bulk_uploader.exe`).

## Inputs

- `data/vegphoto_all_<tagdate>.csv` — the versioned output of `botanist_photo_processing.qmd`,
  one row per photo, with columns including `photo_type`, `BScpPID`, `genus`, `species`,
  `date`, `gps_latitude`, `gps_longitude`, `gps_position_error`, `description`, `botanist`,
  and `newfile` (the renamed/organised photo path from the photo renaming step).

## Outputs

- `data/inat_batches/batch_01.csv`, `batch_02.csv`, … — one CSV per 50 observations,
  each matching the iNat bulk uploader template exactly.
- The photo files referenced by `media_name_*` columns are the renamed files produced
  by the photo reorganisation step in `botanist_photo_processing.qmd` (`photo_all3$newfile`).

## Column Mapping

| iNat field      | Source column / value                                         |
|-----------------|---------------------------------------------------------------|
| `id`            | sequential integer within the full table (1, 2, 3, …)        |
| `taxon_name`    | `paste(genus, species)`                                       |
| `date_obs`      | `format(date, "%d/%m/%Y")`                                    |
| `time_zone`     | `"Africa/Johannesburg"` (fixed)                               |
| `description`   | collapsed `description` values for all photos of that species at that plot |
| `tag_list`      | `"BioSCape"` (fixed)                                          |
| `latitude`      | mean GPS latitude of all photos for that species × plot       |
| `longitude`     | mean GPS longitude                                            |
| `location`      | `BScpPID`                                                     |
| `pos_acc`       | max `gps_position_error` for that species × plot              |
| `geoprivacy`    | `"open"` (fixed)                                              |
| `field_id1`     | (empty)                                                       |
| `field_value1`  | (empty)                                                       |
| `media_name_1`–`media_name_10` | `basename(newfile)` for each photo of that species at that plot (up to 10) |

## Processing Steps

1. **Load data** — read the versioned `photo_all_file` CSV from `botanist_photo_processing.qmd`.
2. **Filter** — keep only `photo_type == "species"` rows with non-NA `genus` + `species` + `BScpPID`.
3. **Group** — group by `BScpPID` + `taxon_name` (one row per species per plot).
4. **Collapse photos** — within each group, take up to 10 photos and spread their basenames
   into `media_name_1` … `media_name_10`. Record `n_photos` for reference.
5. **Assign batch** — add a `batch` column: `ceiling(row_number() / 50)`.
6. **Write output** — for each unique `batch` value, write a CSV named
   `data/inat_batches/batch_<sprintf("%02d", batch)>.csv` containing only the
   iNat template columns (no `batch` column in the output files).

## Bug Fixes Required in `botanist_photo_processing.qmd` First

- **`photo_type` regex** — the current pattern `"plot|S|N|W|E|start|rarefaction|end|stop"`
  matches single letters S/N/W/E anywhere inside a genus name (e.g. "Erica" → matches "E").
  Fix: use `\\b(plot|start|rarefaction|end|stop)\\b|^[SNWE]$` so single-letter cardinal
  directions only match when the entire `genus` field is that letter.
- **`st_drop_geometry()` before `write_csv`** — the sf geometry column is currently written
  into the CSV as WKT, which is not needed downstream. Drop it before writing.

## Known Limitations / Notes

- The renamed photo files (`newfile`) are only generated when the photo renaming chunk
  in `botanist_photo_processing.qmd` has been run (`eval=F` currently — must be run manually).
  The `iNat_prepare.qmd` will validate that `newfile` is populated and warn if not.
- iNat batch size = 50 observations per the user's request.
- Up to 10 media files per observation (the template supports up to 25, but 10 is a
  practical limit given how many photos per species exist in the dataset).
