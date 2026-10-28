# Offline: download open data -> MelbourneLevelUp/data/{places.csv, areas.geojson, ftz.geojson}
# Run from repo root: Rscript R/prep_places.R
# Needs data/raw/ptv-tram-stops.txt first (manual download, see the tram stops section).
library(sf)

base <- "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/"
out <- "MelbourneLevelUp/data"
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create(out, recursive = TRUE, showWarnings = FALSE)

fetch <- function(f, url) {
  f <- file.path("data/raw", f)
  if (!file.exists(f)) download.file(url, f, mode = "wb")
  f
}
get <- function(id) read.csv(fetch(paste0(id, ".csv"), paste0(base, id, "/exports/csv?delimiter=,")),
                             fileEncoding = "UTF-8-BOM")

# --- points ---
lm <- get("landmarks-and-places-of-interest-including-schools-theatres-health-services-spor")
xy <- do.call(rbind, strsplit(lm$co_ordinates, ", "))
landmarks <- data.frame(name = lm$feature_name, category = "Landmark",
                        sub = lm$sub_theme, lat = as.numeric(xy[, 1]), lon = as.numeric(xy[, 2]))
# drop sub-themes a visitor would not go to; "Office" also holds two public heritage sites, kept by name
not_for_visitors <- c("Office", "Primary Schools", "Secondary Schools", "School - Primary and Secondary Education",
                      "Further Education", "Police Station", "Fire Station", "Public Hospital", "Private Hospital",
                      "Medical Services", "Current Construction Site", "Current Construction Site - Commercial",
                      "Vacant Land - Undeveloped Site", "Store Yard", "Industrial (Manufacturing)", "Film & RV Studio")
keep_anyway <- c("Parliament House", "Old Treasury Building")
landmarks <- landmarks[!landmarks$sub %in% not_for_visitors | landmarks$name %in% keep_anyway, ]

cf <- get("cafes-and-restaurants-with-seating-capacity")
cf <- cf[cf$census_year == max(cf$census_year), ]
cf <- cf[!duplicated(cf[c("trading_name", "business_address")]), ]  # one row per seating type
food <- data.frame(name = cf$trading_name, category = "Food",
                   sub = cf$industry_anzsic4_description, lat = cf$latitude, lon = cf$longitude)

tl <- get("public-toilets")
toilets <- data.frame(name = tl$name, category = "Toilet", sub = tl$operator, lat = tl$lat, lon = tl$lon)

ps <- get("pedestrian-counting-system-sensor-locations")
ps <- ps[ps$status == "A", ]  # active sensors only
sensors <- data.frame(name = ps$sensor_description, category = "Ped sensor",
                      sub = ps$location_id, lat = ps$latitude, lon = ps$longitude)

# VPA open space (data.vic), public parks in City of Melbourne, one point per named park
os <- st_read(fetch("open-space.geojson", paste0(
  "https://services5.arcgis.com/DmRfik4clMVydXO3/arcgis/rest/services/VPA_Draft_Open_Space_Data/",
  "FeatureServer/0/query?where=LGA%3D%27MELBOURNE%27+AND+OS_TYPE%3D%27Public+open+space%27",
  "+AND+PARK_NAME+IS+NOT+NULL&outFields=PARK_NAME,OS_CATEGOR&outSR=4326&f=geojson")), quiet = TRUE)
os <- os[trimws(os$PARK_NAME) != "", ]
os <- aggregate(os["OS_CATEGOR"], list(name = os$PARK_NAME), \(x) x[1])
pt <- st_coordinates(st_point_on_surface(st_make_valid(os)))
parks <- data.frame(name = os$name, category = "Park", sub = os$OS_CATEGOR, lat = pt[, 2], lon = pt[, 1])

# PTV tram stops: stops.txt from 3/google_transit.zip inside
# https://opendata.transport.vic.gov.au/dataset/gtfs-schedule (manual download, 270MB)
ts <- read.csv("data/raw/ptv-tram-stops.txt", fileEncoding = "UTF-8-BOM")
trams <- data.frame(name = ts$stop_name, category = "Tram stop", sub = ts$stop_id,
                    lat = ts$stop_lat, lon = ts$stop_lon)

places <- rbind(landmarks, food, toilets, sensors, parks, trams)
places <- places[complete.cases(places[c("lat", "lon")]), ]
pts <- st_as_sf(places, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# --- polygons ---
areas <- st_read(fetch("clue-small-areas.geojson", paste0(base,
  "small-areas-for-census-of-land-use-and-employment-clue/exports/geojson")), quiet = TRUE)
areas <- st_make_valid(areas["featurenam"])
names(areas)[1] <- "area"

# Free Tram Zone: user-drawn KML on ArcGIS Online (approximate, no official open data)
ftz <- st_zm(st_read(fetch("ftz_arcgis.kml",
  "https://www.arcgis.com/sharing/rest/content/items/bc66dbb0b3e947ad8fcc90ad3c118ac0/data"), quiet = TRUE))
st_write(ftz["Name"], file.path(out, "ftz.geojson"), delete_dsn = TRUE, quiet = TRUE)

# --- attributes ---
pts$area <- st_join(pts, areas)$area
pts <- pts[!is.na(pts$area), ]  # City of Melbourne only (drops most tram stops)
pts$in_ftz <- lengths(st_intersects(pts, ftz)) > 0
# "rare" food = anything outside the three dominant CLUE industries
common <- c("Cafes and Restaurants", "Takeaway Food Services", "Pubs, Taverns and Bars")
pts$rare <- pts$category == "Food" & !pts$sub %in% common
pts$id <- seq_len(nrow(pts))
write.csv(st_drop_geometry(pts), file.path(out, "places.csv"), row.names = FALSE)

st_write(areas, file.path(out, "areas.geojson"), delete_dsn = TRUE, quiet = TRUE)

print(table(pts$category))
print(table(pts$category[pts$category == "Tram stop"], pts$in_ftz[pts$category == "Tram stop"]))
cat("rare food:", sum(pts$rare), "\n")
