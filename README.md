# Melbourne Level Up – Melbourne visitor planner

R Shiny app with an embedded Tableau Public dashboard. The app lives in `MelbourneLevelUp/` and is self-contained.
(The root `app.R` and `data/processed/` are the original bare-bone base, kept for reference.)

## Run

```sh
Rscript -e 'shiny::runApp("MelbourneLevelUp")'
```

Packages: `shiny`, `bslib`, `leaflet`, `sf`, `jsonlite`, `ggplot2`, `htmlwidgets`. On Homebrew R, `leaflet`/`sf` need `brew install gdal udunits` first.

## Data

`MelbourneLevelUp/data/` is committed, so the app runs without any downloads. To rebuild it from source:

1. Download the PTV GTFS schedule (https://opendata.transport.vic.gov.au/dataset/gtfs-schedule, ~270MB), take `stops.txt` from `3/google_transit.zip` inside it, and save it as `data/raw/ptv-tram-stops.txt`.
2. From the repo root: `Rscript R/prep_places.R` (downloads the rest into `data/raw/`, which is gitignored).

### `places.csv` – `name, category, sub, lat, lon, area, in_ftz, rare, id`

| category | source | notes |
|---|---|---|
| Landmark | CoM – landmarks-and-places-of-interest-… | `sub` = sub-theme |
| Food | CoM – cafes-and-restaurants-with-seating-capacity (CLUE) | latest census year, one row per business |
| Toilet | CoM – public-toilets | `sub` = operator |
| Park | data.vic – VPA open space | public open space in CoM, one point per named park |
| Tram stop | PTV GTFS – stops.txt (metro tram) | only stops inside CoM are kept |
| Ped sensor | CoM – pedestrian-counting-system-sensor-locations | active only; `sub` = location_id; used by the crowd view, not shown as a place |

- `area`: the CLUE small area the point falls in (spatial join). Points outside the City of Melbourne are dropped.
- `in_ftz`: inside the Free Tram Zone.
- `rare`: food outlets outside the three dominant CLUE industries (cafés/restaurants, takeaway, pubs/bars).

### Polygons

- `areas.geojson`: the 13 CLUE small areas (CoM – small-areas-for-census-of-land-use-and-employment-clue). These are statistical areas, not official suburbs. We use them because the CLUE food data is reported on the same boundaries.
- `ftz.geojson`: Free Tram Zone. There is no official open dataset, so this is a user-drawn KML from ArcGIS Online and the boundary is approximate.

### Pedestrian counts

The hourly counts (Oct 2025 – Oct 2026) are aggregated and visualised in the Tableau Public dashboard (`MelLevel10Insights`), not in this repo.
