# Melbourne Level Up – Person B: app structure, area levels, check-ins and saved progress
#
# Run: open this app.R in RStudio and press Run App (it opens in your web browser),
# or from R: shiny::runApp("MelbourneLevelUp"). The folder is self-contained.
# Packages: shiny, bslib, leaflet, sf, jsonlite, ggplot2, htmlwidgets
# (install them separately; this script never installs anything).
#
# Idea (after JapanEx, zhung.com.tw/japanex): colour each City of Melbourne area by how
# well you know it (Lv 0-5), check in at places, and watch your MEL Level grow.
# Beyond the game, the app turns open data into next steps for a visitor:
#   - each area's "character" (categories over-represented compared with the city),
#   - frontier suggestions (unexplored areas that border ones you know),
#   - practical amenities (food, toilet, tram, park) nearest to a selected place.
# Achievements and the pixel-art style were dropped after team review: they cost
# readability and made the map harder to scan.
#
# Data (data/ next to this file): the team's enriched places.csv (name, category, sub, lat,
# lon, area, in_ftz, rare), areas.geojson and ftz.geojson, copied from branch
# style/noirj0e. When A updates the prepared data, copy the new files into data/.
# Areas are the 13 CLUE statistical areas, not official suburbs.
#
# Team hooks inside server(): sel_area(), sel_place(), rv$lv, rv$visits.
# C: the published Tableau Public view is in TABLEAU_URL below; TABLEAU_TWO_WAY links it
# both ways (set it to FALSE for the plain one-way iframe).

library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(jsonlite)
library(ggplot2)

options(shiny.maxRequestSize = 1024^2)  # backups are a few KB; refuse anything over 1 MB

# ---- Data -----------------------------------------------------------------------
DATA_DIR <- "data"
if (!all(file.exists(file.path(DATA_DIR, c("places.csv", "areas.geojson")))))
  stop("Missing data/places.csv or data/areas.geojson next to app.R.", call. = FALSE)

areas <- st_transform(st_read(file.path(DATA_DIR, "areas.geojson"), quiet = TRUE), 4326)
areas <- areas[order(areas$area), ]
AREAS <- areas$area
ftz_file <- file.path(DATA_DIR, "ftz.geojson")
ftz <- if (file.exists(ftz_file)) st_read(ftz_file, quiet = TRUE) else NULL
label_pts <- suppressWarnings(st_coordinates(st_point_on_surface(st_geometry(areas))))
rownames(label_pts) <- AREAS
# the Remainder polygon wraps around South Yarra; centre its label on the Botanic Gardens instead
if ("Melbourne (Remainder)" %in% AREAS) label_pts["Melbourne (Remainder)", ] <- c(144.9800, -37.8295)
SHORT <- setNames(AREAS, AREAS)
SHORT[c("Melbourne (CBD)", "Melbourne (Remainder)", "West Melbourne (Industrial)", "West Melbourne (Residential)")] <-
  c("CBD", "Melbourne\n(Remainder)", "West Melbourne\n(Industrial)", "West Melbourne\n(Residential)")
SHORT <- SHORT[AREAS]
# tighter names for the narrow heatmap; full names stay in tooltips
TINY <- setNames(gsub("\n", " ", SHORT), AREAS)
TINY[c("Melbourne (Remainder)", "West Melbourne (Industrial)", "West Melbourne (Residential)")] <-
  c("Melb. Remainder", "West Melb. Industrial", "West Melb. Residential")
NEIGHBOURS <- setNames(lapply(st_touches(areas), \(i) AREAS[i]), AREAS)

places <- read.csv(file.path(DATA_DIR, "places.csv"), stringsAsFactors = FALSE)
if (!all(c("name", "category", "sub", "lat", "lon", "area") %in% names(places)))
  stop("data/places.csv needs the columns name, category, sub, lat, lon and area.", call. = FALSE)
# pedestrian sensors feed the crowd view; they are not places to visit
places <- places[places$category != "Ped sensor" & places$area %in% AREAS &
                 is.finite(places$lat) & is.finite(places$lon), ]
# a key built from content survives the team regenerating the CSV in a different order
places$key <- sprintf("%s|%s|%.5f,%.5f", places$category, places$name, places$lat, places$lon)
places <- places[!duplicated(places$key), ]
rownames(places) <- places$key
places$label <- places$name
places$sub_label <- ifelse(places$category == "Tram stop", "", places$sub)
toilet <- places$category == "Toilet"
places$label[toilet] <- trimws(sub("^Public Toilet\\s*-\\s*(Toilet\\s*\\d+\\s*-\\s*)?", "", places$name[toilet]))
places$label[toilet] <- paste("Toilet ·", places$label[toilet])

# Ordinal levels -> single-hue sequential ramp (white = never been)
LEVELS <- data.frame(
  lv = 0:5,
  label = c("Never been", "Passed through", "Stopped by", "Explored", "Stayed the night", "Lived there"),
  hint = c("Not been there yet", "Only travelled through", "Stopped at a place or two",
           "Spent time walking around", "Slept there at least one night", "Live or used to live there"),
  col = c("#ffffff", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c"),
  ink = c("#1d2b53", "#1d2b53", "#1d2b53", "#1d2b53", "#ffffff", "#ffffff"),
  stringsAsFactors = FALSE)
MAX_SCORE <- 5L * length(AREAS)
NEXT_COL <- "#e08e0b"  # amber tag for suggested areas: outside both the blue ramp and category hues

# Qualitative category colours, kept away from the blue level ramp
CATS <- c(Landmark = "#d55e00", Food = "#e7298a", Park = "#1b9e77", `Tram stop` = "#7570b3", Toilet = "#4d4d4d")
CATS <- CATS[names(CATS) %in% places$category]
PLURAL <- c(Landmark = "landmarks", Food = "places to eat", Park = "parks", `Tram stop` = "tram stops", Toilet = "toilets")
CAT_TOTALS <- table(factor(places$category, names(CATS)))
AREA_CATS <- table(factor(places$area, AREAS), factor(places$category, names(CATS)))

# An area's character: categories it has at least 1.5x the city-wide share of
# (location quotient), with at least 5 places so tiny counts do not dominate.
LQ <- sweep(prop.table(AREA_CATS, 1), 2, prop.table(colSums(AREA_CATS)), "/")
KNOWN_FOR <- setNames(lapply(AREAS, \(a) {
  # toilets are an amenity, not a reason to visit, so they never define an area
  ok <- setdiff(names(CATS)[LQ[a, ] >= 1.5 & AREA_CATS[a, ] >= 5], "Toilet")
  ok[order(-LQ[a, ok])][seq_len(min(2, length(ok)))]
}), AREAS)

empty_lv <- function() setNames(integer(length(AREAS)), AREAS)
empty_visits <- function() data.frame(key = character(), date = character(), stringsAsFactors = FALSE)
melb_today <- function() format(Sys.time(), "%Y-%m-%d", tz = "Australia/Melbourne")
nice_date <- function(d) trimws(format(as.Date(d), "%e %b %Y"))
lv_label <- function(l) LEVELS$label[match(l, LEVELS$lv)]
plural <- function(n, one, many = paste0(one, "s")) paste(format(n, big.mark = ","), if (n == 1) one else many)

# straight-line distance in metres (not a walking route)
dist_m <- function(lat, lon, lats, lons) {
  r <- pi / 180
  a <- sin((lats - lat) * r / 2)^2 + cos(lat * r) * cos(lats * r) * sin((lons - lon) * r / 2)^2
  6371000 * 2 * asin(pmin(1, sqrt(a)))
}
and_list <- function(x) if (length(x) < 2) x else paste(paste(x[-length(x)], collapse = ", "), "or", x[length(x)])
fmt_dist <- function(m) if (m < 1000) sprintf("%d m", as.integer(round(m, -1))) else sprintf("%.1f km", m / 1000)

# ---- Save format ----------------------------------------------------------------
# {"app":"mel-level-up","version":1,"saved_at":..,"levels":{area:lv},"checkins":[{place,date}]}
# Unknown areas/places and bad values are skipped (and counted), never guessed.
save_json <- function(lv, visits) {
  as.character(toJSON(list(app = "mel-level-up", version = 1L,
    saved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), levels = as.list(lv),
    checkins = unname(lapply(seq_len(nrow(visits)), \(i) list(place = visits$key[i], date = visits$date[i])))),
    auto_unbox = TRUE))
}

parse_save <- function(txt) {
  s <- tryCatch(fromJSON(txt, simplifyVector = FALSE), error = \(e) NULL)
  if (!is.list(s) || is.null(names(s)) || !identical(s$app, "mel-level-up"))
    stop("This is not a Melbourne Level Up save file.", call. = FALSE)
  if (!identical(s$version, 1L)) stop("This save comes from an unsupported version of the app.", call. = FALSE)
  lv <- empty_lv()
  skipped <- 0L
  levels <- if (is.list(s$levels)) s$levels else list()
  if (length(levels) && is.null(names(levels))) { skipped <- length(levels); levels <- list() }
  for (a in names(levels)) {
    v <- levels[[a]]
    if (a %in% AREAS && is.numeric(v) && length(v) == 1 && v %in% 0:5) lv[[a]] <- as.integer(v)
    else skipped <- skipped + 1L
  }
  rows <- if (is.list(s$checkins)) s$checkins else list()
  ok <- vapply(rows, \(r) is.list(r) && is.character(r$place) && length(r$place) == 1 &&
    r$place %in% places$key && is.character(r$date) && length(r$date) == 1 &&
    grepl("^\\d{4}-\\d{2}-\\d{2}$", r$date) && !is.na(as.Date(r$date, optional = TRUE)) &&
    r$date <= melb_today(), logical(1))
  visits <- data.frame(key = vapply(rows[ok], \(r) r$place, ""), date = vapply(rows[ok], \(r) r$date, ""),
                       stringsAsFactors = FALSE)
  visits <- visits[order(visits$date), ]
  dup <- duplicated(visits$key)  # one check-in per place; keep the first visit
  skipped <- skipped + sum(!ok) + sum(dup)
  visits <- visits[!dup, ]
  rownames(visits) <- NULL
  list(lv = lv, visits = visits, skipped = skipped)
}

skipped_note <- function(n, will = FALSE) {
  it <- if (n == 1) "1 saved item does not match the current map data, so it" else sprintf("%d saved items do not match the current map data, so they", n)
  paste(it, if (will) "will be skipped." else if (n == 1) "was skipped." else "were skipped.")
}

# ---- Suggestions ----------------------------------------------------------------
# Frontier: areas you have barely seen that border an area you know (Lv 2+).
# With nothing rated yet, start with the three areas richest in landmarks.
frontier <- function(lv) {
  known <- AREAS[lv >= 2]
  cand <- setdiff(unique(unlist(NEIGHBOURS[known])), AREAS[lv >= 2])
  if (!length(cand)) {
    cand <- AREAS[lv < 2]
    by <- NULL
  } else by <- vapply(cand, \(a) { k <- intersect(NEIGHBOURS[[a]], known); k[which.max(lv[k])] }, "")
  if (!length(cand)) return(NULL)
  d <- data.frame(area = cand, by = if (is.null(by)) NA else by, landmarks = as.integer(AREA_CATS[cand, "Landmark"]))
  d <- d[order(lv[d$area], -d$landmarks), ]
  head(d, 3)
}

# Places to try in an area: unvisited, nearest first from a starting point.
places_to_try <- function(area, visited, cats, from, n = 4, exclude = NULL) {
  d <- places[places$area == area & !places$key %in% c(visited, exclude) & places$category %in% cats, ]
  if (!nrow(d)) return(d)
  d$dist <- dist_m(from[2], from[1], d$lat, d$lon)
  head(d[order(d$category != "Landmark", d$dist), ], n)
}

# ---- Map helpers ----------------------------------------------------------------
area_tooltip <- function(a, lv, visits) {
  mine <- sum(places$area[match(visits$key, places$key)] == a, na.rm = TRUE)
  HTML(sprintf("<b>%s</b><br>Lv %d · %s · %s checked in", htmltools::htmlEscape(a), lv[[a]],
               lv_label(lv[[a]]), plural(mine, "place")))
}

draw_areas <- function(map, lv, visits, selected, tier = 1L) {
  # fill fades with zoom: strong colour for the city overview, a light tint on the street
  addPolygons(map, data = areas, layerId = ~area, group = "areas",
    fillColor = LEVELS$col[lv[areas$area] + 1],
    fillOpacity = unname(ifelse(lv[areas$area] == 0, 0.06, c(0.6, 0.42, 0.28, 0.18)[tier])),
    color = "#1d2b53", opacity = 0.9, weight = ifelse(areas$area %in% selected, 4, 1.5),
    label = lapply(areas$area, area_tooltip, lv = lv, visits = visits),
    labelOptions = labelOptions(sticky = TRUE, direction = "top"),
    highlightOptions = highlightOptions(weight = 3, bringToFront = FALSE),
    options = pathOptions(pane = "areas"))
}

# One cluster layer for every place type, so bubbles never stack on each other. Each
# bubble is a small doughnut: arc length = share of each category inside it, centre =
# count, "✓n" badge = places already checked in.
CLUSTER_ICON <- JS(sprintf("function(cl) {
  var order = %s, ms = cl.getAllChildMarkers(), n = ms.length, by = {}, v = 0, acc = 0, stops = [];
  ms.forEach(function(m) { by[m.options.color] = (by[m.options.color] || 0) + 1; if (m.options.fillOpacity > 0.5) v++; });
  order.forEach(function(c) { if (!by[c]) return; stops.push(c + ' ' + (acc / n * 360) + 'deg ' + ((acc + by[c]) / n * 360) + 'deg'); acc += by[c]; });
  var s = n < 10 ? 24 : n < 50 ? 28 : n < 200 ? 32 : 36;
  return L.divIcon({ className: 'pt-cluster', iconSize: L.point(s, s),
    html: '<div style=\"background:conic-gradient(' + stops.join(',') + ')\"><span>' + n + '</span>' + (v ? '<i>✓' + v + '</i>' : '') + '</div>' });
}", toJSON(unname(CATS))))

# hollow = not visited, filled = checked in
draw_places <- function(map, cats, visited) {
  map <- clearGroup(map, "places")
  d <- places[places$category %in% cats, ]
  if (!nrow(d)) return(map)
  v <- d$key %in% visited
  col <- unname(CATS[d$category])
  addCircleMarkers(map, data = d, lng = ~lon, lat = ~lat, layerId = ~key, group = "places",
    radius = ifelse(v, 7, 5), weight = 2, opacity = 1, color = col, fillColor = col, fillOpacity = ifelse(v, 1, 0.15),
    label = paste0(d$label, ifelse(nzchar(d$sub_label), paste0(" · ", d$sub_label), ""), ifelse(v, " · ✓ checked in", "")),
    options = pathOptions(pane = "pts"), clusterId = "places",
    clusterOptions = markerClusterOptions(disableClusteringAtZoom = 16,
      # wider merging at city scale keeps bubbles apart; tighter once on the street
      maxClusterRadius = JS("function(z) { return z <= 13 ? 65 : z <= 14 ? 55 : 45; }"),
      showCoverageOnHover = FALSE, spiderfyOnMaxZoom = FALSE, iconCreateFunction = CLUSTER_ICON))
}

# JapanEx-style quick picker shown where the user clicked an area
level_popup <- function(a, current) {
  btns <- vapply(5:0, \(l) sprintf(
    "<button type='button' class='pop-lv%s' style='background:%s;color:%s' onclick='Shiny.setInputValue(\"set_lv\", {area: %s, lv: %d}, {priority: \"event\"})'>Lv %d <span>%s</span></button>",
    if (l == current) " current" else "", LEVELS$col[l + 1], LEVELS$ink[l + 1],
    htmltools::htmlEscape(toJSON(a, auto_unbox = TRUE), attribute = TRUE), l, l, LEVELS$label[l + 1]), "")
  sprintf("<div class='pop'><div class='pop-title'>%s</div><div class='pop-q'>How well do you know it?</div>%s</div>",
          htmltools::htmlEscape(a), paste(btns, collapse = ""))
}

# Shareable postcard of the player's map (the JapanEx "Save IMG" idea)
postcard <- function(lv, n_visits = 0L) {
  d <- areas
  d$lv <- factor(lv[d$area], levels = 0:5)
  lab <- data.frame(x = label_pts[, 1], y = label_pts[, 2], area = SHORT, lv = lv[AREAS])
  ggplot(d) +
    geom_sf(aes(fill = lv), colour = "#1d2b53", linewidth = 0.35, show.legend = TRUE) +
    { if (!is.null(ftz)) geom_sf(data = ftz, fill = NA, colour = "#1d2b53", linetype = "22", linewidth = 0.5) } +
    geom_label(data = lab[lab$lv == 0, ], aes(x, y, label = area), size = 2.3, colour = "#6b7486", lineheight = 0.85,
               fill = "#ffffffd9", linewidth = 0, label.padding = unit(1.5, "pt")) +
    { if (any(lab$lv > 0)) geom_label(data = lab[lab$lv > 0, ], aes(x, y, label = sprintf("%s\nLv %d", area, lv)), size = 2.7,
                                      lineheight = 0.9, linewidth = 0, fill = "#ffffffcc", colour = "#1d2b53", fontface = "bold") } +
    scale_fill_manual(values = setNames(LEVELS$col, 0:5), breaks = as.character(5:0),
                      labels = paste0("Lv ", 5:0, " ", LEVELS$label[6:1]), drop = FALSE, name = NULL) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE, reverse = TRUE)) +
    labs(title = sprintf("My Melbourne · MEL Level %d / %d", sum(lv), MAX_SCORE),
         subtitle = sprintf("%d of %d City of Melbourne areas rated · %s", sum(lv > 0), length(AREAS), plural(n_visits, "check-in")),
         caption = paste("Melbourne Level Up ·", nice_date(melb_today()))) +
    # keep every level in the key, even ones nobody has reached yet
    coord_sf(datum = NA, expand = TRUE, clip = "off") +
    theme_void(base_size = 12) +
    theme(plot.title = element_text(face = "bold", colour = "#1d2b53", size = 16),
          plot.subtitle = element_text(colour = "#5b6577", margin = margin(2, 0, 8, 0)),
          plot.caption = element_text(colour = "#5b6577", size = 9),
          legend.position = "bottom", legend.text = element_text(colour = "#1d2b53", size = 9.5),
          legend.key = element_rect(colour = "#9aa4b5"), legend.key.size = unit(14, "pt"),
          plot.background = element_rect(fill = "#ffffff", colour = NA),
          plot.margin = margin(14, 14, 10, 14))
}

# ---- Welcome card ---------------------------------------------------------------------
# Three pictures, few words: what to click and what the colours mean.
intro_modal <- function() {
  step <- function(n, pic, head, body) div(class = "intro-step", div(class = "intro-n", n), div(class = "intro-pic", pic),
                                            div(class = "intro-h", head), div(class = "intro-b", body))
  modalDialog(easyClose = TRUE, size = "l", title = NULL,
    div(class = "intro",
      div(class = "intro-title", "Melbourne Level Up"),
      div(class = "intro-sub", "Map how well you know the City of Melbourne, then find where to go next."),
      div(class = "intro-grid",
        step(1, div(class = "ramp intro-ramp", lapply(1:6, \(i) span(style = paste0("background:", LEVELS$col[i])))),
             "Rate an area", "Click it on the map: Lv 0 never been → Lv 5 lived there."),
        step(2, div(style = "font-size:1.4rem;letter-spacing:6px", dot("Landmark", hollow = TRUE), "→", dot("Landmark")),
             "Check in", "Click a place you visited. Hollow → filled."),
        step(3, div(class = "intro-hm", lapply(c(0.15, 0.85, 0.35, 0.6), \(a) span(style = sprintf("background:%s%02X", CATS[["Park"]], as.integer(255 * a))))),
             "Spot the character", "Darker cell = more of that place type than the city average. Amber dot = try next."))),
    footer = actionButton("intro_done", "Start exploring", class = "btn-primary"))
}

# ---- Tableau (Person C) -----------------------------------------------------------
TABLEAU_URL <- "https://public.tableau.com/views/MelLevel10Insights/Whentogo"
if (!grepl("^https://public\\.tableau\\.com/views/[^?#\"'<> ]+$", TABLEAU_URL)) TABLEAU_URL <- ""
TABLEAU_FIELD <- "Area"
# Two-way link through the Tableau Embedding API v3:
#   Shiny -> Tableau: selecting an area filters every sheet in the dashboard (no reload).
#   Tableau -> Shiny: clicking a counter on the Tableau map selects its area in the app.
# Set to FALSE to fall back to the plain one-way iframe.
TABLEAU_TWO_WAY <- TRUE
# Areas with pedestrian counts in the Tableau data (Oct 2025 - Oct 2026). The other four
# areas have no counters, so the dashboard shows the whole city for them instead of a blank view.
CROWD_AREAS <- c("Carlton", "Docklands", "Kensington", "Melbourne (CBD)", "Melbourne (Remainder)",
                 "North Melbourne", "Parkville", "Southbank", "West Melbourne (Residential)")

TABLEAU_JS <- "
(function() {
  var pending = {field: 'Area', value: null}, ready = false, filtered = false;
  function viz() { return document.getElementById('tableau-viz'); }
  // every worksheet in the embedded view (a dashboard holds several)
  function sheets() {
    var v = viz();
    if (!v || !v.workbook) return [];
    var s = v.workbook.activeSheet;
    return s.sheetType === 'dashboard' ? s.worksheets : [s];
  }
  async function apply() {
    var f = pending, list = sheets();
    // clearing a filter that was never set makes Tableau report an error, so skip it
    if (f.value === null && !filtered) return;
    filtered = f.value !== null;
    for (var i = 0; i < list.length; i++) {
      try {
        if (f.value === null) await list[i].clearFilterAsync(f.field);
        else await list[i].applyFilterAsync(f.field, [f.value], 'replace');
      } catch (e) { console.warn('Tableau filter skipped on ' + list[i].name + ': ' + e.message); }
    }
  }
  // Shiny -> Tableau; kept until the dashboard is ready if it is still loading
  Shiny.addCustomMessageHandler('tableau_filter', function(f) {
    pending = {field: f.field, value: typeof f.value === 'string' ? f.value : null};
    if (ready) apply();
  });
  // Tableau's events do not bubble, so listen in the capture phase on the whole page
  document.addEventListener('firstinteractive', function(e) {
    if (e.target && e.target.id === 'tableau-viz') { ready = true; apply(); }
  }, true);
  // Tableau -> Shiny: the first selected mark that carries an Area field
  document.addEventListener('markselectionchanged', async function(e) {
    if (!e.target || e.target.id !== 'tableau-viz') return;
    try {
      var marks = await e.detail.getMarksAsync();
      for (var t = 0; t < marks.data.length; t++) {
        var table = marks.data[t];
        var col = table.columns.find(function(c) { return /^Area\\b/.test(c.fieldName); });
        if (col && table.data.length) {
          Shiny.setInputValue('tableau_area', table.data[0][col.index].value, {priority: 'event'});
          return;
        }
      }
    } catch (err) { console.warn('Tableau selection not read: ' + err.message); }
  }, true);
})();
"

# ---- UI -------------------------------------------------------------------------
CSS <- "
:root{--ink:#1d2b53;--muted:#5b6577;--line:#e2e6ee;--bg:#f4f6f9;--accent:#3182bd;--radius:10px}
body{background:var(--bg);color:var(--ink)}
.navbar{border-bottom:1px solid var(--line);padding-top:4px;padding-bottom:4px}
.navbar-brand{font-weight:800;letter-spacing:.1px;font-size:1.05rem} .brand{display:inline-flex;align-items:center;gap:10px}
.brand-mark{display:grid;grid-template-columns:repeat(3,7px);gap:2px}
.brand-mark i{width:7px;height:7px;border-radius:2px;display:block}
.hud{display:flex;align-items:center;gap:14px}
.score{font-weight:700;white-space:nowrap} .score{font-size:.95rem} .score b{font-size:1.3rem;color:var(--accent);font-weight:800} .score small{color:var(--muted);font-weight:500}
.help-btn{display:inline-flex;align-items:center;justify-content:center;width:24px;height:24px;border-radius:50%;border:1px solid var(--line);
  color:var(--muted);font-weight:700;text-decoration:none;font-size:.85rem} .help-btn:hover{border-color:#8fb6da;color:var(--ink)}
.intro{padding:6px 6px 0} .modal-footer:has(#intro_done){border-top:0;padding-top:0} .intro-title{font-size:1.3rem;font-weight:800} .intro-sub{color:var(--muted);margin:2px 0 18px}
.intro-grid{display:grid;grid-template-columns:repeat(3,1fr);gap:14px}
.intro-step{border:1px solid var(--line);border-radius:10px;padding:14px;position:relative}
.intro-n{position:absolute;top:10px;left:12px;font-weight:800;color:var(--accent);font-size:.8rem}
.intro-pic{height:44px;display:flex;align-items:center;justify-content:center;margin:6px 0 10px}
.intro .ramp.intro-ramp{gap:2px} .intro .ramp.intro-ramp span{width:26px;height:18px;border-radius:3px}
.intro-hm{display:grid;grid-template-columns:repeat(4,22px);gap:3px} .intro-hm span{height:22px;border-radius:3px}
.intro-h{font-weight:700} .intro-b{color:var(--muted);font-size:.84rem;margin-top:2px}
.hm-colh{display:flex;flex-direction:column;align-items:center;gap:1px;font-size:.6rem;font-weight:600;color:var(--muted)}
.save-status{font-size:.78rem;color:var(--muted);white-space:nowrap} .save-status.warn{color:#b45309;font-weight:600}
.explore{display:grid;grid-template-columns:minmax(0,1fr) 360px;gap:14px;height:100%;min-height:540px}
.mapwrap{position:relative;border:1px solid var(--line);border-radius:var(--radius);overflow:hidden;min-height:0;background:#e9edf2}
.overlay{position:absolute;z-index:1000;background:rgba(255,255,255,.96);border:1px solid var(--line);border-radius:var(--radius);
  box-shadow:0 2px 10px rgba(29,43,83,.10)}
.mapcol{display:flex;flex-direction:column;gap:10px;min-height:0;min-width:0}
.mapcol .mapwrap{flex:1}
.toolbar{display:flex;gap:10px;align-items:center;flex-wrap:wrap}
.tb-search{flex:0 1 300px;min-width:220px} .tb-search .form-group{margin:0}
.tb-search .selectize-input{border:1px solid var(--line);border-radius:8px;padding:8px 10px;font-size:.92rem;box-shadow:none}
.tb-layers{display:flex;align-items:center;gap:8px;flex:1 1 auto;min-width:0}
.tb-label{font-size:.72rem;text-transform:uppercase;letter-spacing:.7px;color:var(--muted);font-weight:700}
.tb-layers .form-group{margin:0} .tb-layers .shiny-options-group{display:flex;flex-wrap:wrap;gap:4px}
.tb-layers label.checkbox-inline{margin:0;padding:3px 10px 3px 8px;border:1px solid var(--line);border-radius:14px;font-size:.8rem;color:var(--muted);
  cursor:pointer;display:flex;align-items:center;gap:6px;background:#fff;user-select:none}
.tb-layers label.checkbox-inline input{position:absolute;opacity:0;pointer-events:none}
.tb-layers label.checkbox-inline:has(input:checked){background:#eaf2fa;color:var(--ink);border-color:#8fb6da;font-weight:600}
.tb-layers label.checkbox-inline:not(:has(input:checked)) .dot{background:#fff!important}
.tb-layers label.checkbox-inline:has(input:focus-visible){outline:3px solid #fdb515}
.tb-layers .n{opacity:.7;font-size:.76rem}
.ov-legend{left:10px;bottom:10px;padding:6px 10px;font-size:.72rem;color:var(--muted);box-shadow:none}
.leg-row{display:flex;align-items:center;gap:5px;font-weight:600}
.ramp{display:flex;gap:1px} .ramp span{width:16px;height:9px;border:1px solid #9aa4b5;border-radius:1px}
.ov-reset{top:10px;right:10px;padding:4px 10px;font-size:.78rem;color:var(--ink);box-shadow:none;line-height:1.4}
/* Bootstrap turns button text white on hover/focus/active; keep it readable on the white chip */
.ov-reset:hover,.ov-reset:focus,.ov-reset:active,.ov-reset.active{background:#fff!important;color:var(--ink)!important;border-color:#8fb6da!important}
.key{display:flex;flex-wrap:wrap;gap:2px 10px;margin-top:4px;color:var(--muted)} .key>span{white-space:nowrap}
.dot{display:inline-block;width:10px;height:10px;border-radius:50%;border:2px solid;vertical-align:-1px;margin-right:4px;flex:none}
.dot.hollow{background:#fff!important}
.dash{display:inline-block;width:20px;border-top:3px dashed var(--ink);margin-right:5px;vertical-align:3px}
.panel{display:flex;flex-direction:column;gap:12px;overflow-y:auto;min-height:0;padding-right:2px}
.pane{background:#fff;border:1px solid var(--line);border-radius:var(--radius);padding:16px 18px}
.eyebrow{font-size:.72rem;text-transform:uppercase;letter-spacing:.7px;color:var(--muted);font-weight:700;margin:0 0 6px}
.title{font-size:1.05rem;font-weight:700;margin:0;line-height:1.25}
.sub{color:var(--muted);font-size:.86rem;margin:2px 0 0}
.head{display:flex;justify-content:space-between;align-items:flex-start;gap:8px}
.x{background:none;border:0;color:var(--muted);font-size:1.1rem;line-height:1;padding:2px 4px;cursor:pointer}
.chips{display:flex;flex-wrap:wrap;gap:6px;margin-top:10px}
.chip{font-size:.78rem;padding:3px 9px;border-radius:12px;background:#eef2f7;font-weight:600}
.seg{display:grid;grid-template-columns:repeat(6,1fr);gap:4px;margin-top:12px}
.seg button{height:30px;border:1px solid #9aa4b5;border-radius:5px;font-weight:700;font-size:.82rem;cursor:pointer}
.seg button.current{outline:2px solid var(--ink);outline-offset:1px}
.seg button:focus-visible{outline:3px solid #fdb515;outline-offset:1px}
.seg-label{margin-top:8px;font-size:.86rem} .seg-label b{font-weight:700}
.hint{color:var(--muted);font-size:.78rem;margin-top:4px}
kbd{font-size:.72rem;padding:1px 5px;border:1px solid var(--line);border-bottom-width:2px;border-radius:4px;background:#fff;color:var(--ink)}
.bars{display:grid;grid-template-columns:92px 1fr 54px;gap:6px 10px;align-items:center;font-size:.84rem}
.bar{height:8px;background:#eef1f5;border-radius:4px;overflow:hidden} .bar>div{height:100%;border-radius:4px}
.num{color:var(--muted);text-align:right;font-variant-numeric:tabular-nums;font-size:.8rem}
.list{list-style:none;margin:0;padding:0}
.list li{border-top:1px solid #eef1f5}
.list li:first-child{border-top:0}
.list button{all:unset;box-sizing:border-box;display:flex;align-items:center;gap:8px;width:100%;padding:8px 2px;cursor:pointer;font-size:.88rem}
.list button:hover .nm{text-decoration:underline} .list button:focus-visible{outline:3px solid #fdb515}
.list .nm{flex:1;min-width:0;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}
.list .meta{color:var(--muted);font-size:.78rem;white-space:nowrap}
.checked-block{padding:9px 12px;border-radius:6px;background:#e3f2e8;color:#14532d;font-weight:600;text-align:center;font-size:.92rem}
.status{display:inline-block;padding:2px 8px;border-radius:10px;font-size:.76rem;font-weight:600;background:#eef2f7;margin-top:8px}
.linkbtn{background:none;border:0;padding:0;color:var(--accent);text-decoration:underline;cursor:pointer;font-size:.84rem}
.empty{color:var(--muted);font-size:.88rem;line-height:1.5;margin:0}
.eyebrow-n{float:right;letter-spacing:0;font-variant-numeric:tabular-nums}
.howto{display:flex;flex-wrap:wrap;gap:6px 14px;font-size:.78rem;color:var(--muted);margin-top:6px}
.howto>span{display:inline-flex;align-items:center;gap:5px}
.sw-mini{width:12px;height:12px;border-radius:2px;background:#6baed6;border:1px solid #9aa4b5;display:inline-block}
.hm{display:flex;flex-direction:column;gap:2px}
.hm-row{all:unset;box-sizing:border-box;display:grid;grid-template-columns:minmax(0,1fr) 22px repeat(4,24px);gap:2px;align-items:center;
  padding:2px 4px;border-radius:5px;cursor:pointer;font-size:.8rem;border:1.5px solid transparent}
.hm-row:hover{background:#f1f4f8} .hm-row:focus-visible{outline:3px solid #fdb515}
.hm-row.next{background:#fdf6ea}
.hm-head{cursor:default;color:var(--muted);font-size:.7rem;font-weight:700;text-align:center} .hm-head:hover{background:none}
.hm-head .dot{margin:0}
.hm-name{white-space:nowrap;overflow:hidden;text-overflow:ellipsis;font-size:.78rem}
.hm-lv{height:16px;border:1px solid #9aa4b5;border-radius:3px;font-size:.72rem;font-weight:700;display:flex;align-items:center;justify-content:center}
.hm-cell{height:16px;border-radius:2px}
.nx{display:inline-block;width:7px;height:7px;border-radius:50%;background:#e08e0b;margin-right:5px;vertical-align:1px}
.hm-key{display:flex;align-items:center;justify-content:flex-end;gap:6px;font-size:.7rem;color:var(--muted);margin-top:8px}
.hm-ramp{width:70px;height:8px;border-radius:2px;background:linear-gradient(90deg,#d55e0010,#d55e00)}
.key-bar{display:inline-block;width:18px;height:8px;border-radius:4px;background:#5b6577;margin-right:5px;vertical-align:0}
.key-tick{display:inline-block;width:2px;height:12px;background:#1d2b53;margin-right:5px;vertical-align:-2px}
.mix{display:flex;height:8px;border-radius:4px;overflow:hidden;border:1px solid #9aa4b5;margin:8px 0 4px}
.pt-cluster div{width:100%;height:100%;border-radius:50%;padding:4px;display:flex;align-items:center;box-sizing:border-box;
  justify-content:center;font:700 12px/1 system-ui,sans-serif;color:var(--ink);box-shadow:0 1px 3px rgba(0,0,0,.25);position:relative}
.pt-cluster span{flex:1;align-self:stretch;border-radius:50%;background:#fff;display:flex;align-items:center;justify-content:center}
.pt-cluster i{position:absolute;top:-8px;right:-10px;color:#fff;background:var(--ink);font-style:normal;font-size:10px;padding:2px 4px;border-radius:8px}
.area-label{background:rgba(255,255,255,.8);border:0;border-radius:4px;padding:0 4px;box-shadow:none;font-weight:700;font-size:10px;color:var(--ink);
  text-shadow:0 0 3px #fff,0 0 3px #fff,0 0 3px #fff}
.area-label:before{display:none}
.area-label.next{color:#9a5b00}
.street-zoom .leaflet-labels-pane{display:none}
.leaflet-container:not(.street-zoom) .leaflet-streets-pane{display:none}
.lv-popup .leaflet-popup-content{margin:12px 12px 10px;width:210px!important}
.pop-title{font-weight:750;font-size:.98rem;color:var(--ink)} .pop-q{color:var(--muted);font-size:.8rem;margin-bottom:8px}
.pop-lv{display:flex;justify-content:space-between;align-items:center;width:100%;border:1px solid #9aa4b5;border-radius:5px;
  padding:5px 9px;margin-bottom:4px;font-weight:700;font-size:.82rem;cursor:pointer}
.pop-lv span{font-weight:500} .pop-lv.current{outline:2px solid var(--ink);outline-offset:1px}
#shiny-notification-panel{right:auto;left:50%;transform:translateX(-50%);bottom:14px;width:300px}
.shiny-notification{border-radius:8px;font-size:.82rem;padding:8px 12px;border:1px solid var(--line);box-shadow:0 4px 16px rgba(29,43,83,.18);opacity:1}
/* My Melbourne page */
.mm-page{max-width:1240px;margin:0 auto;width:100%;display:flex;flex-direction:column;gap:14px}
.mm{display:grid;grid-template-columns:minmax(0,1fr) minmax(0,1fr);gap:16px;align-items:start}
.ins-row{display:grid;grid-template-columns:96px minmax(0,1fr);gap:10px;align-items:center;padding:7px 0;border-top:1px solid #eef1f5}
.ins-row:first-child{border-top:0;padding-top:0}
.ins-l{font-size:.72rem;text-transform:uppercase;letter-spacing:.6px;color:var(--muted);font-weight:700}
.ins-v{display:flex;flex-wrap:wrap;gap:6px;min-width:0}
.next-btn{all:unset;box-sizing:border-box;display:flex;align-items:center;gap:6px;width:100%;padding:6px 10px;border-radius:8px;
  background:#fdf6ea;cursor:pointer;font-size:.88rem} .next-btn:hover{background:#fbedd6} .next-btn:focus-visible{outline:3px solid #fdb515}
.next-btn b{white-space:nowrap} .next-btn .meta{flex:1 1 auto;min-width:0;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}
.next-btn .go{flex:none;margin-left:auto;color:var(--accent);font-weight:600;font-size:.8rem;white-space:nowrap}
.util-bar{display:flex;flex-wrap:wrap;align-items:center;gap:8px;padding:10px 14px;border:1px dashed var(--line);border-radius:var(--radius);background:#fafbfc}
.util-label{font-size:.78rem;color:var(--muted);margin-right:auto}
.util-file .form-group{margin:0} .util-file .shiny-file-input-progress{display:none!important}
.util-file .input-group{flex-wrap:nowrap} .util-file .btn,.util-file .form-control{font-size:.8rem;padding:3px 8px;height:auto}
.mm .pane h2{font-size:.95rem;font-weight:700;margin:0 0 10px}

.kpis{display:grid;grid-template-columns:repeat(4,1fr);gap:0;border:1px solid var(--line);border-radius:var(--radius);background:#fff}
.kpi{padding:14px 16px;border-left:1px solid var(--line)} .kpi:first-child{border-left:0}
.kpi .l{white-space:nowrap} .kpi .v{font-size:1.45rem;font-weight:800;line-height:1.1} .kpi .l{color:var(--muted);font-size:.78rem;margin-top:2px}
.stack{display:flex;flex-direction:column;gap:16px}
.postcard img{width:100%;height:auto;border-radius:6px}
.hist{width:100%;font-size:.86rem;border-collapse:collapse}
.hist th{color:var(--muted);font-weight:600;text-align:left;border-bottom:1px solid var(--line);padding:6px 6px 6px 0}
.hist td{padding:7px 6px 7px 0;border-bottom:1px solid #f0f2f6}
.hist-wrap{overflow-x:auto}
.btn-row{display:flex;flex-wrap:wrap;gap:8px;align-items:center}
.save-tools .form-group{margin:12px 0 0} .save-tools .shiny-file-input-progress{margin:0;height:0} .save-tools .shiny-input-container{width:100%}
.profile-note{font-size:.86rem;margin:10px 0 0;color:var(--ink)}
/* wide screens: taller heatmap rows fill the side column instead of leaving a gap */
@media (min-width:1241px) and (min-height:760px){
  .hm{gap:4px} .hm-cell,.hm-lv{height:24px} .hm-row{grid-template-columns:minmax(0,1fr) 28px repeat(4,34px);gap:3px;font-size:.86rem}
  .hm-name{font-size:.86rem}
}
@media (max-width:1240px){
  .explore{grid-template-columns:minmax(0,1fr) 310px;gap:12px}
  .tb-label,.tb-layers .n{display:none} .tb-search{flex-basis:220px;min-width:180px}
  .tb-layers label.checkbox-inline{padding:4px 9px 4px 7px;font-size:.8rem}
  .pane{padding:14px 14px} .navbar-brand{font-size:1.05rem}
  .kpi{padding:12px} .kpi .v{font-size:1.25rem}
}
.tableau-frame{width:100%;height:100%;min-height:620px;border:0;background:#fff;border-radius:var(--radius)}
/* keep the dashboard its full height inside the scroll area instead of being squashed */
.when-scroll>*{flex:none} tableau-viz{display:block;flex:none;min-height:720px;width:100%;background:#fff;border-radius:var(--radius);overflow:hidden}
"

JS <- "
var KEY = 'mel-level-up';
$(document).on('shiny:connected', function() {
  var r = {ok: true, value: '', intro: false};
  try { r.value = localStorage.getItem(KEY) || ''; r.intro = localStorage.getItem(KEY + '-intro') === '1'; } catch (e) { r.ok = false; }
  Shiny.setInputValue('store', r, {priority: 'event'});
});
Shiny.addCustomMessageHandler('intro_seen', function(x) { try { localStorage.setItem(KEY + '-intro', '1'); } catch (e) {} });
Shiny.addCustomMessageHandler('save', function(s) {
  var ok = true;
  try { localStorage.setItem(KEY, s); } catch (e) { ok = false; }
  Shiny.setInputValue('save_ok', ok, {priority: 'event'});
});
// keep an unreadable save instead of silently overwriting it
Shiny.addCustomMessageHandler('quarantine', function(s) { try { localStorage.setItem(KEY + '-unreadable', s); } catch (e) {} });
// another tab saved: load that version so the two tabs never overwrite each other
window.addEventListener('storage', function(e) {
  if (e.key === KEY) Shiny.setInputValue('store_external', e.newValue || '', {priority: 'event'});
});
// keys 0-5 rate the selected area (ignored while typing in a field)
document.addEventListener('keydown', function(e) {
  if (e.ctrlKey || e.metaKey || e.altKey || /input|textarea|select/i.test(e.target.tagName) || e.target.isContentEditable) return;
  if (/^[0-5]$/.test(e.key)) Shiny.setInputValue('key_lv', +e.key, {priority: 'event'});
  if (e.key === 'Escape') Shiny.setInputValue('esc', Date.now(), {priority: 'event'});
});
$(document).on('shown.bs.tab', function() {
  window.scrollTo(0, 0);
  var w = window.HTMLWidgets && HTMLWidgets.find('#map');
  if (w && w.getMap()) setTimeout(function() { w.getMap().invalidateSize(); }, 60);
});
function pick(kind, value) { Shiny.setInputValue('pick', {kind: kind, value: value}, {priority: 'event'}); }
"

pick_js <- function(kind, value) sprintf("pick('%s', %s)", kind, toJSON(value, auto_unbox = TRUE))
dot <- function(cat, hollow = FALSE) span(class = paste("dot", if (hollow) "hollow"), style = sprintf("background:%s;border-color:%s", CATS[[cat]], CATS[[cat]]))

ui <- page_navbar(
  title = tags$span(class = "brand", span(class = "brand-mark", `aria-hidden` = "true",
    lapply(c(2, 3, 4, 3, 5, 6, 4, 5, 6), \(i) tags$i(style = paste0("background:", LEVELS$col[i])))), "Melbourne Level Up"),
  window_title = "Melbourne Level Up", id = "tab", fillable = c("Explore", "When to go"),
  theme = bs_theme(version = 5, primary = "#3182bd", "navbar-bg" = "#ffffff", "font-size-base" = "0.9rem",
                   base_font = font_collection("Inter", "system-ui", "-apple-system", "Segoe UI", "sans-serif")),
  header = tags$head(tags$style(HTML(CSS)), tags$script(HTML(JS)),
    if (TABLEAU_TWO_WAY && nzchar(TABLEAU_URL)) tagList(
      tags$script(type = "module", src = "https://public.tableau.com/javascripts/api/tableau.embedding.3.latest.min.js"),
      tags$script(HTML(TABLEAU_JS)))),
  nav_panel("Explore",
    div(class = "explore",
      div(class = "mapcol",
        div(class = "toolbar",
          div(class = "tb-search", selectizeInput("search", NULL, choices = NULL, width = "100%",
            options = list(placeholder = "Search an area or place", maxOptions = 40))),
          div(class = "tb-layers", span(class = "tb-label", "Show"),
            checkboxGroupInput("cats", NULL, inline = TRUE, selected = intersect("Landmark", names(CATS)), choiceValues = names(CATS),
              choiceNames = lapply(names(CATS), \(c) span(title = paste(format(CAT_TOTALS[[c]], big.mark = ","), "mapped"), dot(c), c))))),
      div(class = "mapwrap",
        leafletOutput("map", height = "100%"),
        actionButton("reset_view", "↺ Reset view", class = "overlay ov-reset", title = "Show all 13 areas"),
        div(class = "overlay ov-legend",
          div(class = "leg-row", span("Your level"), span("0"),
              div(class = "ramp", lapply(1:6, \(i) span(style = paste0("background:", LEVELS$col[i]), title = paste0("Lv ", i - 1, " ", LEVELS$label[i])))),
              span("5")),
          div(class = "key", span(span(class = "dot hollow", style = "border-color:#5b6577"), "to visit"),
              span(span(class = "dot", style = "background:#5b6577;border-color:#5b6577"), "checked in"),
              span(tags$i(class = "nx"), "try next"),
              if (!is.null(ftz)) span(span(class = "dash"), "Free Tram Zone"))))),
      div(class = "panel", uiOutput("detail")))),
  # the page is pinned to the window, so the dashboard scrolls inside its own area
  nav_panel("When to go", div(class = "when-scroll", style = "display:flex;flex-direction:column;gap:8px;height:100%;overflow-y:auto;min-height:0;padding-bottom:12px",
    uiOutput("tableau_note"), uiOutput("tableau"))),
  nav_panel("My Melbourne",
    div(class = "mm-page",
      div(class = "mm",
        div(class = "pane postcard", div(class = "head", tags$h2("Your postcard"),
            downloadButton("postcard_png", "Save image", class = "btn-sm btn-outline-primary")),
          plotOutput("postcard", height = "480px")),
        div(class = "stack",
          div(class = "pane", tags$h2("What your map says"), uiOutput("insights")),
          div(class = "pane", tags$h2("Your mix vs the city"), uiOutput("profile")),
          div(class = "pane", div(class = "head", tags$h2("Recent check-ins"), uiOutput("history_toggle", inline = TRUE)), uiOutput("history")))),
      # housekeeping, kept out of the way
      div(class = "util-bar",
        span(class = "util-label", "Saved in this browser"),
        downloadButton("export", "Download backup", class = "btn-sm btn-outline-secondary"),
        div(class = "util-file", fileInput("import", NULL, accept = c(".json", "application/json"), width = "230px",
                                            buttonLabel = "Restore…", placeholder = "backup .json")),
        actionButton("undo", "Undo", class = "btn-sm btn-outline-secondary"),
        actionButton("reset", "Reset all", class = "btn-sm btn-outline-danger")))),
  nav_spacer(),
  nav_item(div(class = "hud",
    actionLink("help", "?", class = "help-btn", title = "How it works", `aria-label` = "How it works"),
    div(class = "score", `aria-live` = "polite", title = sprintf("Sum of your levels across the %d areas (max %d)", length(AREAS), MAX_SCORE), "MEL Level ", tags$b(textOutput("score", inline = TRUE)), tags$small(paste0(" / ", MAX_SCORE))),
    uiOutput("save_status")))
)

# ---- Server ---------------------------------------------------------------------
server <- function(input, output, session) {
  rv <- reactiveValues(lv = empty_lv(), visits = empty_visits(), loaded = FALSE,
                       storage_ok = NA, saved_at = NULL)
  sel_area <- reactiveVal(NULL)
  sel_place <- reactiveVal(NULL)
  undo_stack <- reactiveVal(list())
  pending_import <- reactiveVal(NULL)

  updateSelectizeInput(session, "search", server = TRUE, selected = "",
    choices = c(setNames(paste0("area:", AREAS), paste0(AREAS, " — area")),
                setNames(paste0("place:", places$key), sprintf("%s — %s, %s", places$label, tolower(places$category), places$area))))

  persist <- function() session$sendCustomMessage("save", save_json(rv$lv, rv$visits))
  # every edit goes through here so it can be undone and is saved straight away
  change <- function(lv = rv$lv, visits = rv$visits) {
    undo_stack(c(list(list(lv = rv$lv, visits = rv$visits)), head(undo_stack(), 29)))
    rv$lv <- lv
    rv$visits <- visits
    persist()
  }

  # ---- load / save ----
  apply_saved <- function(txt, source) {
    if (!nzchar(txt)) { rv$lv <- empty_lv(); rv$visits <- empty_visits(); return(invisible()) }
    s <- tryCatch(parse_save(txt), error = \(e) e)
    if (inherits(s, "error")) {
      session$sendCustomMessage("quarantine", txt)
      showNotification("Your saved progress could not be read, so the map starts empty. The old save was kept in this browser.",
                       type = "warning", duration = 10)
      return(invisible())
    }
    rv$lv <- s$lv
    rv$visits <- s$visits
    if (s$skipped > 0) showNotification(skipped_note(s$skipped), type = "warning", duration = 8)
    if (source == "tab") showNotification("Updated with changes from your other tab.", duration = 4)
  }
  observeEvent(input$store, {
    rv$storage_ok <- isTRUE(input$store$ok)
    apply_saved(input$store$value %||% "", "start")
    rv$loaded <- TRUE
    if (!isTRUE(input$store$intro)) showModal(intro_modal())
  })
  observeEvent(input$help, showModal(intro_modal()))
  observeEvent(input$intro_done, { removeModal(); session$sendCustomMessage("intro_seen", TRUE) })
  observeEvent(input$store_external, {
    apply_saved(input$store_external, "tab")
    undo_stack(list())
  })
  observeEvent(input$save_ok, {
    rv$storage_ok <- isTRUE(input$save_ok)
    if (rv$storage_ok) rv$saved_at <- format(Sys.time(), "%H:%M", tz = "Australia/Melbourne")
  })
  output$save_status <- renderUI({
    if (isFALSE(rv$storage_ok))
      div(class = "save-status warn", title = "This browser blocks local storage. Use Download backup on My Melbourne.", "⚠ Not saved")
    else div(class = "save-status", title = "Progress is kept in this browser",
             if (is.null(rv$saved_at)) "Auto-saves" else paste("✓ Saved", rv$saved_at))
  })

  # ---- map ----
  bb <- unname(st_bbox(areas))
  output$map <- renderLeaflet({
    # quarter-step zoom lets the 13 areas fill the frame instead of jumping a whole level out
    m <- leaflet(options = leafletOptions(minZoom = 11, maxZoom = 19, zoomSnap = 0.25, zoomDelta = 0.5)) |>
      addMapPane("streets", 405) |> addMapPane("areas", 410) |> addMapPane("ftz", 420) |> addMapPane("pts", 430) |> addMapPane("labels", 620) |>
      addProviderTiles(providers$Esri.WorldGrayCanvas, options = providerTileOptions(maxNativeZoom = 16, maxZoom = 19)) |>
      # street names only when zoomed in; our own area labels at city scale (no double labels)
      addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}",
               options = tileOptions(maxNativeZoom = 16, maxZoom = 19, pane = "streets")) |>
      fitBounds(bb[1], bb[2], bb[3], bb[4], options = list(padding = c(12, 12))) |>
      setMaxBounds(bb[1] - 0.05, bb[2] - 0.05, bb[3] + 0.05, bb[4] + 0.05) |>
      # street names below zoom 15 would clash with our area names, so the map swaps them
      htmlwidgets::onRender("function(el, x) {
        var map = this; map.zoomControl.setPosition('bottomright');
        function tier() { el.classList.toggle('street-zoom', map.getZoom() >= 15); }
        map.on('zoomend', tier); tier();
      }")
    if (!is.null(ftz)) m <- addPolygons(m, data = ftz, fill = FALSE, color = "#1d2b53", weight = 3, dashArray = "8 6",
                                        options = pathOptions(pane = "ftz", interactive = FALSE))
    m
  })
  # proxy updates wait until both the save and the map have loaded
  ready <- reactive(rv$loaded && !is.null(input$map_zoom))
  # 1 = city, 4 = street; only changes when a threshold is crossed, so areas are not redrawn on every zoom
  zoom_tier <- reactiveVal(1L)
  observeEvent(input$map_zoom, {
    t <- findInterval(input$map_zoom, c(-Inf, 14, 15, 16))
    if (!identical(zoom_tier(), t)) zoom_tier(t)
  })
  observeEvent(input$reset_view, {
    leafletProxy("map") |> clearPopups() |> flyToBounds(bb[1], bb[2], bb[3], bb[4], options = list(padding = c(12, 12)))
  })

  observe({
    req(ready())
    leafletProxy("map") |> draw_areas(rv$lv, rv$visits, sel_area(), zoom_tier())
  })
  # area names at city scale; suggested next areas get an amber tag (shared borders stay clean)
  observe({
    req(ready())
    nxt <- AREAS %in% frontier_areas()
    m <- leafletProxy("map") |> clearGroup("area_labels")
    for (i in seq_along(AREAS)) m <- addLabelOnlyMarkers(m, lng = label_pts[i, 1], lat = label_pts[i, 2], group = "area_labels",
      label = if (nxt[i]) HTML(paste0("<i class='nx'></i>", htmltools::htmlEscape(sub("\n", " ", SHORT[[i]])))) else sub("\n", " ", SHORT[[i]]),
      # own pane between areas and places, so names never cover a place bubble
      labelOptions = labelOptions(noHide = TRUE, direction = "top", offset = c(0, -14), interactive = FALSE, pane = "labels",
        className = if (nxt[i]) "area-label next" else "area-label"))
    m
  })
  observe({
    req(ready())
    leafletProxy("map") |> draw_places(input$cats %||% character(), rv$visits$key)
  })
  observe({
    req(ready())
    m <- leafletProxy("map") |> clearGroup("sel")
    k <- sel_place()
    if (!is.null(k)) addCircleMarkers(m, lng = places[k, "lon"], lat = places[k, "lat"], group = "sel", radius = 13,
                                      color = "#1d2b53", weight = 3, fill = FALSE, options = pathOptions(pane = "pts", interactive = FALSE))
  })

  select_place <- function(k, zoom = FALSE) {
    sel_place(k)
    sel_area(places[k, "area"])
    leafletProxy("map") |> clearPopups()
    if (zoom) {
      if (!places[k, "category"] %in% input$cats) updateCheckboxGroupInput(session, "cats", selected = c(input$cats, places[k, "category"]))
      leafletProxy("map") |> setView(places[k, "lon"], places[k, "lat"], 17)
    }
  }
  select_area <- function(a, zoom = FALSE, popup_at = NULL) {
    force(zoom)  # callers may compare with sel_area(), so read it before it changes
    sel_area(a)
    sel_place(NULL)
    m <- leafletProxy("map") |> clearPopups()
    # open the picker first: a popup opened mid-flight cancels the zoom animation
    if (!is.null(popup_at)) addPopups(m, popup_at$lng, popup_at$lat, level_popup(a, rv$lv[[a]]), layerId = "lvpop",
                                      options = popupOptions(className = "lv-popup", autoPan = !zoom, autoPanPadding = c(40, 40)))
    if (zoom) {
      b <- unname(st_bbox(areas[areas$area == a, ]))
      flyToBounds(m, b[1], b[2], b[3], b[4], options = list(padding = c(30, 30), duration = 0.6))
    }
  }
  observeEvent(input$map_shape_click, {
    e <- input$map_shape_click
    # a new area zooms in; clicking the area you are already in only reopens the picker
    if (isTRUE(e$id %in% AREAS)) select_area(e$id, zoom = !identical(sel_area(), e$id), popup_at = e)
  })
  observeEvent(input$map_marker_click, { k <- input$map_marker_click$id; if (isTRUE(k %in% places$key)) select_place(k) })
  observeEvent(input$search, {
    s <- input$search
    req(nzchar(s))
    v <- sub("^(area|place):", "", s)
    if (startsWith(s, "area:") && v %in% AREAS) select_area(v, zoom = TRUE)
    if (startsWith(s, "place:") && v %in% places$key) select_place(v, zoom = TRUE)
    updateSelectizeInput(session, "search", selected = "")  # the box is for finding, not a lasting filter
  }, ignoreInit = TRUE)
  observeEvent(input$pick, {
    v <- input$pick$value
    if (identical(input$pick$kind, "area") && isTRUE(v %in% AREAS)) { nav_select("tab", "Explore"); select_area(v, zoom = TRUE) }
    if (identical(input$pick$kind, "place") && isTRUE(v %in% places$key)) { nav_select("tab", "Explore"); select_place(v, zoom = TRUE) }
  })
  observeEvent(input$esc, { sel_place(NULL); sel_area(NULL); leafletProxy("map") |> clearPopups() })
  observeEvent(input$close_place, sel_place(NULL))
  observeEvent(input$close_area, { sel_place(NULL); sel_area(NULL); leafletProxy("map") |> clearPopups() })

  # ---- levels and check-ins ----
  set_level <- function(a, l) {
    req(isTRUE(a %in% AREAS), isTRUE(l %in% 0:5), rv$loaded)
    leafletProxy("map") |> clearPopups()
    sel_area(a)
    if (rv$lv[[a]] == l) return()
    lv <- rv$lv
    lv[[a]] <- as.integer(l)
    change(lv = lv)
  }
  observeEvent(input$set_lv, set_level(input$set_lv$area, input$set_lv$lv))
  observeEvent(input$key_lv, {
    if (is.null(sel_area())) showNotification("Select an area first, then press 0–5 to rate it.", duration = 3)
    else set_level(sel_area(), input$key_lv)
  })

  observeEvent(input$checkin, {
    k <- sel_place()
    req(k, rv$loaded, !k %in% rv$visits$key)  # a double click must not check in twice
    a <- places[k, "area"]
    lv <- rv$lv
    raised <- lv[[a]] < 2L
    if (raised) lv[[a]] <- 2L  # being at a place means at least "Stopped by"
    change(lv = lv, visits = rbind(rv$visits, data.frame(key = k, date = melb_today())))
    showNotification(paste0("Checked in at ", places[k, "label"], ".",
                            if (raised) paste0(" ", a, " is now Lv 2 · Stopped by.") else ""), duration = 4)
  })
  remove_checkin <- function(k) {
    req(isTRUE(k %in% rv$visits$key))
    change(visits = rv$visits[rv$visits$key != k, ])  # levels are self-rated, so they stay
  }
  observeEvent(input$uncheck, remove_checkin(sel_place()))
  observeEvent(input$remove_visit, remove_checkin(input$remove_visit))

  observeEvent(input$undo, {
    s <- undo_stack()
    if (!length(s)) { showNotification("Nothing to undo.", duration = 3); return() }
    rv$lv <- s[[1]]$lv
    rv$visits <- s[[1]]$visits
    undo_stack(s[-1])
    persist()
    showNotification("Last change undone.", duration = 2)
  })

  observeEvent(input$reset, {
    showModal(modalDialog(title = "Reset all progress?", easyClose = TRUE,
      "This clears every area level and check-in in this browser. You can still undo straight after.",
      footer = tagList(modalButton("Cancel"), actionButton("reset_ok", "Reset everything", class = "btn-danger"))))
  })
  observeEvent(input$reset_ok, { removeModal(); change(lv = empty_lv(), visits = empty_visits()) })

  output$export <- downloadHandler(
    filename = \() paste0("mel-level-up-", melb_today(), ".json"),
    content = \(file) writeLines(save_json(rv$lv, rv$visits), file, useBytes = TRUE),
    contentType = "application/json")

  observeEvent(input$import, {
    s <- tryCatch(parse_save(paste(readLines(input$import$datapath, warn = FALSE, encoding = "UTF-8"), collapse = "\n")),
                  error = \(e) e)
    if (inherits(s, "error")) { showNotification(conditionMessage(s), type = "error", duration = 8); return() }
    pending_import(s)
    showModal(modalDialog(title = "Restore this backup?", easyClose = TRUE,
      p(sprintf("It has %s and %s (MEL Level %d).", plural(sum(s$lv > 0), "rated area"), plural(nrow(s$visits), "check-in"), sum(s$lv))),
      if (s$skipped > 0) p(class = "text-muted", skipped_note(s$skipped, will = TRUE)),
      p("This replaces your current progress. You can undo straight after."),
      footer = tagList(modalButton("Cancel"), actionButton("import_ok", "Restore", class = "btn-primary"))))
  })
  observeEvent(input$import_ok, {
    s <- req(pending_import())
    removeModal()
    pending_import(NULL)
    change(lv = s$lv, visits = s$visits)
    showNotification("Backup restored.", duration = 3)
  })

  # ---- header ----
  output$score <- renderText(sum(rv$lv))

  # ---- right panel ----
  place_pane <- function(k) {
    p_row <- places[k, ]
    done <- k %in% rv$visits$key
    # nearest practical amenity of each other kind: the questions a visitor asks next
    near <- lapply(setdiff(c("Food", "Toilet", "Tram stop", "Park"), p_row$category), \(c) {
      d <- places[places$category == c, ]
      if (!nrow(d)) return(NULL)
      m <- dist_m(p_row$lat, p_row$lon, d$lat, d$lon)
      i <- which.min(m)
      if (m[i] > 1500) return(NULL)
      tags$li(tags$button(type = "button", onclick = pick_js("place", d$key[i]),
        dot(c), span(class = "nm", d$label[i]), span(class = "meta", fmt_dist(m[i]))))
    })
    div(class = "pane",
      div(class = "head", div(div(class = "eyebrow", "Place"), div(class = "title", p_row$label)),
          actionButton("close_place", "✕", class = "x", title = "Close", `aria-label` = "Close place details")),
      div(class = "sub", dot(p_row$category), p_row$category, if (nzchar(p_row$sub_label)) paste(" ·", p_row$sub_label),
          if (isTRUE(as.logical(p_row$in_ftz))) " · in the Free Tram Zone"),
      # the confirmation sits where the button was, so a double click cannot hit "Remove"
      div(style = "margin-top:12px",
        if (done) tagList(div(class = "checked-block", paste("✓ Checked in on", nice_date(rv$visits$date[rv$visits$key == k]))),
                          div(style = "text-align:right;margin-top:6px", actionButton("uncheck", "Remove check-in", class = "linkbtn")))
        else actionButton("checkin", "✓ Check in here", class = "btn-primary btn-sm w-100")),
      if (length(Filter(Negate(is.null), near))) tagList(
        div(class = "eyebrow", style = "margin-top:16px", title = "Straight-line distance, not a walking route", "Nearest to here"),
        tags$ul(class = "list", near)))
  }

  area_pane <- function(a) {
    l <- rv$lv[[a]]
    in_area <- places$area == a
    mine <- places$key[in_area] %in% rv$visits$key
    kf <- KNOWN_FOR[[a]]
    from <- if (!is.null(sel_place())) c(places[sel_place(), "lon"], places[sel_place(), "lat"]) else label_pts[a, ]
    try_d <- places_to_try(a, rv$visits$key, intersect(c("Landmark", input$cats), names(CATS)), from, exclude = sel_place())
    div(class = "pane",
      div(class = "head", div(div(class = "eyebrow", "Area"), div(class = "title", a)),
          actionButton("close_area", "✕", class = "x", title = "Close", `aria-label` = "Close area details")),
      if (length(kf)) div(class = "chips", lapply(kf, \(c) span(class = "chip", style = sprintf("border-left:4px solid %s", CATS[[c]]),
        title = sprintf("%.1f times the city-wide share of %s", LQ[a, c], PLURAL[[c]]), sprintf("▲ %.1f× %s", LQ[a, c], PLURAL[[c]])))),
      div(class = "seg", role = "radiogroup", `aria-label` = "Your level for this area", title = "Shortcut: keys 0–5 rate, Esc closes",
        lapply(0:5, \(i) tags$button(type = "button", role = "radio", `aria-checked` = tolower(i == l),
          class = if (i == l) "current", title = sprintf("%s: %s", LEVELS$label[i + 1], LEVELS$hint[i + 1]),
          style = sprintf("background:%s;color:%s", LEVELS$col[i + 1], LEVELS$ink[i + 1]),
          onclick = sprintf("Shiny.setInputValue('set_lv', %s, {priority: 'event'})", toJSON(list(area = a, lv = i), auto_unbox = TRUE)),
          i))),
      div(class = "seg-label", tags$b(sprintf("Lv %d · %s", l, LEVELS$label[l + 1]))),
      div(class = "eyebrow", style = "margin-top:16px", title = "Checked in / mapped places", "What's here",
          span(class = "eyebrow-n", sprintf("%d / %d", sum(mine), sum(in_area)))),
      # with a place open the panel stays one screen tall: the per-type bars fold into one bar
      if (!is.null(sel_place())) div(class = "bar", style = "margin-top:6px",
        div(style = sprintf("width:%.1f%%;background:var(--accent)", 100 * sum(mine) / max(1, sum(in_area))))),
      if (is.null(sel_place())) div(class = "bars", lapply(names(CATS), \(c) {
        n <- sum(places$category[in_area] == c)
        v <- sum(places$category[in_area][mine] == c)
        tagList(span(dot(c), c), div(class = "bar", div(style = sprintf("width:%.0f%%;background:%s", if (n) 100 * v / n else 0, CATS[[c]]))),
                span(class = "num", sprintf("%d / %d", v, n)))
      })),
      if (nrow(try_d) && is.null(sel_place())) tagList(
        div(class = "eyebrow", style = "margin-top:16px", "Still to visit"),
        tags$ul(class = "list", lapply(seq_len(min(3, nrow(try_d))), \(i) tags$li(tags$button(type = "button", onclick = pick_js("place", try_d$key[i]),
          dot(try_d$category[i], hollow = TRUE), span(class = "nm", try_d$label[i]), span(class = "meta", fmt_dist(try_d$dist[i]))))))))
  }

  # toilets are an amenity, not part of an area's character
  HM_CATS <- setdiff(names(CATS), "Toilet")
  SHORT_CAT <- c(Landmark = "Sight", Food = "Food", Park = "Park", `Tram stop` = "Tram")
  overview_pane <- function() {
    lv <- rv$lv
    nxt <- frontier_areas()
    # one card answers "how far am I, what is each area like, and where next?"
    div(class = "pane",
      div(class = "head", div(class = "eyebrow", "Your Melbourne"), span(class = "eyebrow-n", sprintf("%d / %d rated", sum(lv > 0), length(AREAS)))),
      div(class = "mix", lapply(5:0, \(l) if (sum(lv == l)) span(style = sprintf("flex:%d;background:%s", sum(lv == l), LEVELS$col[l + 1]),
                                                                 title = sprintf("%s: %d", LEVELS$label[l + 1], sum(lv == l))))),
      div(class = "hm", role = "table", title = "Cell shade: how over-represented each place type is, compared with the whole city",
        div(class = "hm-row hm-head", role = "row", span(style = "text-align:left", "Known for"), span(title = "Your level", "Lv"),
            lapply(HM_CATS, \(c) span(class = "hm-colh", title = c, dot(c), span(SHORT_CAT[[c]])))),
        lapply(AREAS, \(a) tags$button(type = "button", class = paste("hm-row", if (a %in% nxt) "next"), role = "row",
          onclick = pick_js("area", a), title = paste0(a, if (a %in% nxt) " · suggested next"),
          span(class = "hm-name", if (a %in% nxt) tags$i(class = "nx"), TINY[[a]]),
          span(class = "hm-lv", style = sprintf("background:%s;color:%s", LEVELS$col[lv[[a]] + 1], LEVELS$ink[lv[[a]] + 1]), lv[[a]]),
          lapply(HM_CATS, \(c) span(class = "hm-cell",
            style = sprintf("background:%s%s", CATS[[c]], sprintf("%02X", as.integer(255 * min(0.85, max(0.06, LQ[a, c] / 3))))),
            title = sprintf("%s · %s: %d places, %.1f× city share", a, c, AREA_CATS[a, c], LQ[a, c])))))),
      div(class = "hm-key", span(tags$i(class = "nx"), "try next"), span(style = "flex:1"), span("city avg"), span(class = "hm-ramp"), span("3× avg")),
      div(class = "howto", span(span(class = "sw-mini"), "click area → rate"), span(dot("Landmark", hollow = TRUE), "click place → check in")))
  }

  frontier_areas <- reactive({ f <- frontier(rv$lv); if (is.null(f)) character() else f$area })

  output$detail <- renderUI({
    a <- sel_area()
    k <- sel_place()
    if (is.null(a)) return(overview_pane())
    tagList(if (!is.null(k)) place_pane(k), area_pane(a))
  })

  # ---- When to go (Tableau by Person C) ----
  # only areas with pedestrian counters filter the dashboard; the rest show the whole city
  crowd_area <- reactive({ a <- sel_area(); if (!is.null(a) && a %in% CROWD_AREAS) a else NULL })

  output$tableau_note <- renderUI({
    if (!nzchar(TABLEAU_URL)) return(NULL)
    a <- sel_area()
    msg <- if (is.null(a)) {
      paste0("Showing all areas. Select an area on the Explore map",
             if (TABLEAU_TWO_WAY) " or click a counter on the map below", " to focus this view.")
    } else if (!a %in% CROWD_AREAS) {
      paste0("There are no pedestrian counters in ", a, ", so this shows all areas.")
    } else paste0("Filtered to ", a, ", your selected area.")
    p(class = "sub", style = "margin:0", msg, if (!is.null(a)) tagList(" ", actionLink("tableau_all", "Show all areas")))
  })

  output$tableau <- renderUI({
    if (!nzchar(TABLEAU_URL)) return(div(class = "pane", style = "max-width:720px;margin:24px auto",
      div(class = "eyebrow", "Crowd patterns"),
      p(class = "empty", "The Tableau view is not connected yet. Paste the published Tableau Public link into TABLEAU_URL in app.R and restart the app.")))
    # two-way: draw the dashboard once; area changes are sent as filters below, so it never reloads
    if (TABLEAU_TWO_WAY) return(tag("tableau-viz", list(id = "tableau-viz", src = TABLEAU_URL, toolbar = "bottom",
      `hide-tabs` = NA, width = "100%", height = "720px")))
    # one-way fallback: reload the iframe with the area in the URL
    a <- crowd_area()
    src <- paste0(TABLEAU_URL, "?:showVizHome=no&:embed=true&:toolbar=bottom",
                  if (!is.null(a)) paste0("&", URLencode(TABLEAU_FIELD, reserved = TRUE), "=", URLencode(a, reserved = TRUE)))
    tags$iframe(class = "tableau-frame", src = src, title = "Tableau view: pedestrian activity by day and hour")
  })

  # Shiny -> Tableau
  observe({
    req(TABLEAU_TWO_WAY, nzchar(TABLEAU_URL))
    a <- crowd_area()
    session$sendCustomMessage("tableau_filter", list(field = TABLEAU_FIELD, value = if (is.null(a)) NA else a))
  })
  # Tableau -> Shiny: a counter clicked in the dashboard selects its area in the app
  observeEvent(input$tableau_area, {
    a <- input$tableau_area
    req(isTRUE(a %in% AREAS))
    select_area(a)
    showNotification(paste0(a, " selected. It is also highlighted on the Explore map."), duration = 3)
  })
  observeEvent(input$tableau_all, { sel_place(NULL); sel_area(NULL); leafletProxy("map") |> clearPopups() })

  # ---- My Melbourne ----
  # what the map says: strongest pattern, home turf, blind spots and where to go next
  SIGHTS <- c("Landmark", "Food", "Park")
  output$insights <- renderUI({
    lv <- rv$lv
    if (!any(lv > 0) && !nrow(rv$visits)) return(div(class = "empty",
      p(style = "margin:0 0 10px", "Rate an area or check in on the map and your patterns appear here."),
      tags$button(type = "button", class = "btn btn-sm btn-primary", onclick = pick_js("area", frontier(lv)$area[1]),
                  paste("Start with", frontier(lv)$area[1], "→"))))
    v <- places[rv$visits$key, ]
    mine <- prop.table(table(factor(v$category, names(CATS))))
    city <- prop.table(CAT_TOTALS)
    n_sights <- sum(v$category %in% SIGHTS)
    top <- if (n_sights >= 3) SIGHTS[which.max(mine[SIGHTS] / city[SIGHTS])] else NULL
    gap <- if (any(lv > 0)) SIGHTS[mine[SIGHTS] == 0 & colSums(AREA_CATS[AREAS[lv > 0], SIGHTS, drop = FALSE]) > 0] else character()
    home <- AREAS[lv == max(lv) & lv > 0]
    nxt <- frontier(lv)
    row <- function(label, ...) div(class = "ins-row", span(class = "ins-l", label), div(class = "ins-v", ...))
    chip <- function(c, txt, faded = FALSE) span(class = "chip", style = sprintf("border-left:4px solid %s%s", CATS[[c]], if (faded) ";opacity:.75" else ""), txt)
    tagList(
      row("You lean to", if (!is.null(top)) chip(top, sprintf("▲ %.1f× %s", mine[[top]] / city[[top]], PLURAL[[top]]))
                         else span(class = "chip", sprintf("%d more landmark, food or park check-in%s", 3 - n_sights, if (3 - n_sights == 1) "" else "s"))),
      if (length(home)) row("Home turf", lapply(head(home, 2), \(a) span(class = "chip", style = sprintf("border-left:4px solid %s", LEVELS$col[lv[[a]] + 1]),
                                                                       sprintf("%s · Lv %d", a, lv[[a]])))),
      if (length(gap)) row("Not tried yet", lapply(gap, \(c) chip(c, paste("○", PLURAL[[c]]), faded = TRUE))),
      if (!is.null(nxt)) row("Go next", tags$button(type = "button", class = "next-btn", onclick = pick_js("area", nxt$area[1]),
        tags$i(class = "nx"), tags$b(nxt$area[1]),
        span(class = "meta", if (is.na(nxt$by[1])) plural(nxt$landmarks[1], "landmark") else paste("next to", nxt$by[1])),
        span(class = "go", "Show on map →"))))
  })

  output$postcard <- renderPlot(postcard(rv$lv, nrow(rv$visits)), res = 96)
  output$postcard_png <- downloadHandler(
    filename = \() paste0("my-melbourne-", melb_today(), ".png"),
    content = \(file) ggsave(file, postcard(rv$lv, nrow(rv$visits)), width = 6.4, height = 6.8, dpi = 200, bg = "white"),
    contentType = "image/png")

  # explorer profile: your share of each place type vs the city's mix
  output$profile <- renderUI({
    v <- places[rv$visits$key, ]
    if (!nrow(v)) return(p(class = "empty", "Check in at landmarks, food or parks to compare your mix with the city."))
    # shares among the places people choose to visit (amenities such as toilets are left out)
    v <- v[v$category %in% SIGHTS, ]
    if (!nrow(v)) return(p(class = "empty", "Check in at landmarks, food or parks to compare your mix with the city."))
    mine <- prop.table(table(factor(v$category, SIGHTS)))
    city <- prop.table(CAT_TOTALS[SIGHTS])
    tagList(
      div(class = "bars", style = "grid-template-columns:92px 1fr 54px", lapply(SIGHTS, \(c) tagList(
        span(dot(c), c),
        div(style = "position:relative", div(class = "bar", div(style = sprintf("width:%.0f%%;background:%s", 100 * mine[[c]], CATS[[c]]))),
            span(title = sprintf("City share %.0f%%", 100 * city[[c]]),
                 style = sprintf("position:absolute;top:-3px;left:calc(%.1f%% - 1px);width:2px;height:14px;background:#1d2b53", 100 * city[[c]]))),
        span(class = "num", title = sprintf("You %.0f%% · city %.0f%%", 100 * mine[[c]], 100 * city[[c]]), sprintf("%.0f%%", 100 * mine[[c]]))))),
      div(class = "hm-key", style = "justify-content:flex-start;gap:14px;margin-top:10px",
          span(span(class = "key-bar"), "your check-ins"), span(span(class = "key-tick"), "city mix")))
  })

  show_all_hist <- reactiveVal(FALSE)
  observeEvent(input$hist_toggle, show_all_hist(!show_all_hist()))
  output$history_toggle <- renderUI({
    n <- nrow(rv$visits)
    if (n > 5) actionLink("hist_toggle", if (show_all_hist()) "Show fewer" else sprintf("Show all %d", n), class = "small")
  })
  output$history <- renderUI({
    v <- rv$visits
    if (!nrow(v)) return(p(class = "empty", "No check-ins yet. Select a place on the Explore map and press Check in."))
    v <- v[order(v$date, decreasing = TRUE), ]
    if (!show_all_hist()) v <- head(v, 5)
    div(class = "hist-wrap", tags$table(class = "hist",
      tags$thead(tags$tr(tags$th("Date"), tags$th("Place"), tags$th("Area"), tags$th(span(class = "visually-hidden", "Remove")))),
      tags$tbody(lapply(seq_len(nrow(v)), \(i) {
        p_row <- places[v$key[i], ]
        tags$tr(tags$td(style = "white-space:nowrap", nice_date(v$date[i])),
          tags$td(tags$button(type = "button", class = "linkbtn", style = "color:var(--ink);text-align:left", onclick = pick_js("place", v$key[i]),
                              dot(p_row$category), p_row$label)),
          tags$td(p_row$area),
          tags$td(style = "text-align:right", tags$button(type = "button", class = "linkbtn", `aria-label` = paste("Remove check-in at", p_row$label),
            onclick = sprintf("Shiny.setInputValue('remove_visit', %s, {priority:'event'})", toJSON(v$key[i], auto_unbox = TRUE)), "Remove")))
      }))))
  })
}

# open in the system browser even when started from RStudio's Run App button
shinyApp(ui, server, options = list(launch.browser = TRUE))
