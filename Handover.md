# Project Handout – Melbourne CBD Visitor Planner

> Brief for starting implementation (e.g. with Claude Code). Assignment 3, Information Visualisation – group of 4.
> Status: topic agreed in principle; items marked **(open)** still need a group decision.

---

## 1. What we are building

An **R Shiny** web app, with **at least one embedded Tableau visualisation**, that helps **tourists planning or currently on a visit to Melbourne** decide **where to go and when to go** in the CBD.

- **Client:** City of Melbourne (Assignment Option 1 – Tourism)
- **Users:** tourists, before or during their trip
- **Device:** **desktop only**. No responsive or mobile layout.
  - Why: simplest to build and test, suits trip planning on a big screen, and leaves room for the map and the Tableau view side by side.

## 2. Hard requirements (from the brief)

- [ ] R Shiny app
- [ ] Embeds **≥1 Tableau visualisation** (Lab 8.5 technique: a Tableau Public viz embedded in Shiny)
- [ ] **No `install.packages()`** anywhere in the code; load with `library()` only
- [ ] Intuitive, tailored to the user group and device
- [ ] Helps users **discover patterns / make decisions**, not just list data
- [ ] Design justified with lecture principles: data graphics, cartography, HCI
- [ ] Other deliverables (not code): design summary, pattern / use-case summary, video pitch to the client

## 3. Core user tasks (every feature should map to one)

1. **Find things to do**: browse landmarks and points of interest by category on a map.
2. **Pick a good time**: see how busy an area usually is by weekday and hour; avoid the crowds or find the buzz.
3. **Find food and amenities nearby**: cafés/restaurants, toilets, and open space around a chosen spot.

## 4. Data (City of Melbourne Open Data – data.melbourne.vic.gov.au)

All datasets can be downloaded as CSV or GeoJSON. Store raw downloads in `data/raw/` and keep them out of git if they are large.

| Dataset | Role in app | Priority |
|---|---|---|
| Pedestrian Counting System – counts per hour (`pedestrian-counting-system-monthly-counts-per-hour`) | Crowd level by weekday × hour; the main pattern story | **Core** |
| Pedestrian Counting System – Sensor Locations | Sensor coordinates and status; join on `sensor_id` | **Core** |
| Landmarks and places of interest | Attractions layer | **Core** |
| Café, restaurant, bistro seats (CLUE) | Food layer | **Core** |
| Public toilets | Amenity layer | Planned |
| Open space / parks | "Take a break" layer | Planned |
| Free Tram Zone boundary | Getting-around context layer (one polygon, cheap) | Planned |
| Microclimate sensors | "Good day to be outside?" | Nice to have |
| Drinking fountains | Amenity layer | **(open)**: leaning towards dropping it; group to confirm |

**Data caveats**
- The hourly pedestrian data is huge (2009–present). Use **only the last 1–2 years**.
- Some sensors were removed or relocated, so filter using the status and notes in the sensor locations file.
- CLUE data is at block level and covers years up to 2023; use the latest year only.

## 5. Interface

Use a tabbed layout (`navbarPage` or `bslib::page_navbar`):

| Tab | Content | Tech |
|---|---|---|
| **Explore** | Map with attractions, food and amenity layers; filter by category; click a place to see what is nearby and its usual crowd level | `leaflet` |
| **When to go** | Heatmap of average foot traffic by weekday × hour; pick a sensor or area to compare | **Embedded Tableau** |
| **Discover** | Short guided insights: weekday vs weekend, busiest night-time areas, quiet hours near major landmarks | Shiny (`ggplot2` / `plotly`) + text |

**Interaction notes**
- Amenity layers can be toggled and are **off by default** to avoid clutter.
- Selecting an area on the map should highlight or filter the matching sensor in the "When to go" view (via a URL parameter or filter on the Tableau embed, if feasible).
- Use consistent colour coding per category across tabs, and a sequential palette for crowd levels.

## 6. Suggested repo structure

```
app.R                # or ui.R + server.R
R/
  prep_pedestrian.R  # offline: raw -> aggregated
  prep_places.R      # offline: clean landmarks, CLUE, amenities; spatial joins
  mod_map.R          # Shiny module: Explore tab
  mod_when.R         # Shiny module: Tableau embed
  mod_discover.R     # Shiny module: insights
data/
  raw/               # downloads (gitignored if large)
  processed/         # small CSV/RDS files the app actually loads
www/                 # css, images
README.md
```

## 7. Build plan (in order)

1. **Data prep (offline scripts, never run inside the app)**
   - Pedestrian data: filter to recent years → join sensor locations → aggregate to `sensor_id × weekday × hour → mean_count` → `data/processed/ped_agg.rds`
   - Export the same aggregate as CSV for Tableau.
   - Places: clean landmarks, CLUE cafés, and amenities into a common schema (`name, category, lat, lon`).
   - Spatial join: nearest sensor for each landmark (`sf::st_nearest_feature`) → `places.rds`
2. **App skeleton**: tabs, data loading, shared category palette.
3. **Explore tab**: leaflet map with layers, filters and popups.
4. **Tableau viz**: build the heatmap in Tableau, publish it to Tableau Public, and embed it in Shiny.
5. **Discover tab**: 3–4 insights backed by charts.
6. **Polish and test**: usability pass, check load time, and confirm there is no `install.packages()`.

**Likely packages:** `shiny`, `bslib`, `leaflet`, `sf`, `dplyr`, `tidyr`, `lubridate`, `readr`, `ggplot2`, `plotly`

## 8. Team split (draft)

| Member | Responsibility |
|---|---|
| A | Data cleaning and aggregation (pedestrian + CLUE) |
| B | Shiny app structure and Explore map |
| C | Tableau visualisation and embedding |
| D | Discover tab, design and use-case summaries, video pitch |

## 9. Still open

- **(open)** Final list of amenity layers, especially drinking fountains (see section 4).
- **(open)** How tightly the map and the Tableau view can be linked, depending on embed technique.
