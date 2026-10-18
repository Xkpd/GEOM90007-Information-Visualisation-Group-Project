# Offline: download CoM open data -> data/processed/places.csv (name, category, lat, lon)
# Run from repo root: Rscript R/prep_places.R

base <- "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/"
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

get <- function(id) {
  f <- file.path("data/raw", paste0(id, ".csv"))
  if (!file.exists(f)) download.file(paste0(base, id, "/exports/csv?delimiter=,"), f, mode = "wb")
  read.csv(f, fileEncoding = "UTF-8-BOM")
}

lm <- get("landmarks-and-places-of-interest-including-schools-theatres-health-services-spor")
xy <- do.call(rbind, strsplit(lm$co_ordinates, ", "))
landmarks <- data.frame(name = lm$feature_name, category = "Landmark",
                        sub = lm$theme, lat = as.numeric(xy[, 1]), lon = as.numeric(xy[, 2]))

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

places <- rbind(landmarks, food, toilets, sensors)
places <- places[complete.cases(places[c("lat", "lon")]), ]
write.csv(places, "data/processed/places.csv", row.names = FALSE)
print(table(places$category))
