# Melbourne CBD Visitor Planner – bare-bone base

Unstyled starting point. Fork it and explore styles on your own branch.

## Run

```sh
Rscript scripts/prep_places.R      # only if data/processed/places.csv is missing; downloads raw data to data/raw/
Rscript -e 'shiny::runApp()'
```

Packages: `shiny`, `leaflet`. On Homebrew R, `leaflet` needs `brew install gdal udunits` first.

## Data

`data/processed/places.csv` – `name, category, sub, lat, lon`, from City of Melbourne Open Data:

| category | source dataset | notes |
|---|---|---|
| Landmark | landmarks-and-places-of-interest-… | `sub` = theme |
| Food | cafes-and-restaurants-with-seating-capacity | latest census year, one row per business |
| Toilet | public-toilets | `sub` = operator |
| Ped sensor | pedestrian-counting-system-sensor-locations | active only, `sub` = location_id (joins to hourly counts) |

Not yet included: hourly pedestrian counts, open space, Free Tram Zone, Tableau embed.
