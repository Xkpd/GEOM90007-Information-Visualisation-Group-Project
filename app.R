# MEL 制霸: conquest map. Click an area to set your level, click a place to check in,
# click an achievement to see where its targets are.
library(shiny)
library(leaflet)
library(sf)
library(jsonlite)

places <- read.csv("data/processed/places.csv")
places <- places[places$category != "Ped sensor", ]  # data layer, not a game target
areas <- st_read("data/processed/areas.geojson", quiet = TRUE)
names(areas)[names(areas) == "area"] <- "name"
ftz <- st_read("data/processed/ftz.geojson", quiet = TRUE)

# area level: ordinal -> single-hue sequential
LEVELS <- data.frame(lv = 5:0,
  label = c("Lived there", "Stayed the night", "Explored", "Stopped by", "Passed through", "Never been"),
  col = c("#08519c", "#3182bd", "#6baed6", "#9ecae1", "#c6dbef", "#ffffff"))
lv_col <- function(l) LEVELS$col[match(l, LEVELS$lv)]

# place category: qualitative, kept away from the blue area ramp
CATS <- c(Landmark = "#d55e00", Food = "#e7298a", Park = "#1b9e77", `Tram stop` = "#7570b3", Toilet = "#4d4d4d")
counts <- table(factor(places$area, areas$name), factor(places$category, names(CATS)))

quiet <- names(which(table(places$area[places$category == "Food"]) < 60))
# f = progress from visited places v and area levels lv; g = which places count towards it
ACH <- list(
  list(t = "FIRST STEP",     d = "Check in anywhere",                          n = 1,  f = \(v, lv) nrow(v)),
  list(t = "SIGHTSEER",      d = "Visit 10 landmarks",                         n = 10, f = \(v, lv) sum(v$category == "Landmark"),
       g = \(d) d$category == "Landmark"),
  list(t = "GREEN THUMB",    d = "Visit 5 parks",                              n = 5,  f = \(v, lv) sum(v$category == "Park"),
       g = \(d) d$category == "Park"),
  list(t = "HIDDEN GEM",     d = "Eat at 3 rare spots (bakery, club, grocer...)", n = 3, f = \(v, lv) sum(v$rare),
       g = \(d) d$rare),
  list(t = "OFF THE MAP",    d = "Eat in a quiet suburb (<60 eateries)",       n = 1,  f = \(v, lv) sum(v$category == "Food" & v$area %in% quiet),
       g = \(d) d$category == "Food" & d$area %in% quiet),
  list(t = "FREE RIDER",     d = "Use 5 tram stops inside the Free Tram Zone", n = 5,  f = \(v, lv) sum(v$category == "Tram stop" & v$in_ftz),
       g = \(d) d$category == "Tram stop" & d$in_ftz),
  list(t = "ZONE HOPPER",    d = "Ride trams inside AND outside the FTZ",      n = 2,  f = \(v, lv) { t <- v$in_ftz[v$category == "Tram stop"]; any(t) + any(!t) },
       g = \(d) d$category == "Tram stop"),
  list(t = "NATURE CALLS",   d = "Find 3 public toilets",                      n = 3,  f = \(v, lv) sum(v$category == "Toilet"),
       g = \(d) d$category == "Toilet"),
  list(t = "CITY CONQUEROR", d = "Reach Lv1+ in all 13 areas",                 n = 13, f = \(v, lv) sum(lv > 0)),
  list(t = "LOCAL LEGEND",   d = "Reach MEL LEVEL 30",                         n = 30, f = \(v, lv) sum(lv)))

area_tip <- function(a, lv, visited) {
  mine <- table(factor(places$category[places$id %in% visited & places$area == a], names(CATS)))
  rows <- paste0(sprintf("<span style='color:%s'>●</span> %s %d/%d", CATS, names(CATS), mine, counts[a, ]), collapse = "<br>")
  HTML(sprintf("<b>%s</b> · Lv%d %s<br>%s", a, lv, LEVELS$label[match(lv, LEVELS$lv)], rows))
}
draw_areas <- function(map, lv, visited, which = names(lv)) {
  d <- areas[match(which, areas$name), ]
  addPolygons(map, data = d, layerId = ~name, fillColor = lv_col(lv[d$name]),
              fillOpacity = unname(ifelse(lv[d$name] == 0, 0.05, 0.5)), color = "#1d2b53", weight = 2,
              label = lapply(d$name, \(a) area_tip(a, lv[[a]], visited)),
              highlightOptions = highlightOptions(weight = 4), options = pathOptions(pane = "areas"))
}
# cluster bubble: category colour ring, size by count, "✓k" once some are checked in
CLUSTER_ICON <- JS("function(cl) {
  var ms = cl.getAllChildMarkers(), n = ms.length, v = 0, col = ms[0].options.color;
  ms.forEach(function(m) { if (m.options.fillColor !== '#ffffff') { v++; col = m.options.fillColor; } });
  var s = n < 10 ? 30 : n < 50 ? 38 : n < 200 ? 46 : 54;
  return L.divIcon({ className: 'pt-cluster', iconSize: L.point(s, s),
    html: '<div style=\"border-color:' + col + '\">' + n + (v ? '<i style=\"background:' + col + '\">✓' + v + '</i>' : '') + '</div>' });
}")

# hollow = not visited yet, filled = checked in; one cluster group per category,
# individual points from zoom 16 (street level)
add_places <- function(map, d, visited) {
  for (c in unique(d$category)) {
    x <- d[d$category == c, ]
    v <- x$id %in% visited
    map <- addCircleMarkers(map, data = x, lng = ~lon, lat = ~lat, layerId = ~as.character(id),
      radius = ifelse(v, 7, 5), weight = 2, opacity = 1, fillOpacity = 1,
      color = ifelse(v, "#1d2b53", CATS[[c]]), fillColor = ifelse(v, CATS[[c]], "#ffffff"),
      label = paste0(x$name, " | ", x$sub, ifelse(v, "  ✓ checked in", "")),
      options = pathOptions(pane = "pts"), clusterId = c,
      clusterOptions = markerClusterOptions(disableClusteringAtZoom = 16, maxClusterRadius = 50,
        showCoverageOnHover = FALSE, iconCreateFunction = CLUSTER_ICON))
  }
  map
}

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Press+Start+2P&family=VT323&display=swap"),
    tags$link(rel = "stylesheet", href = paste0("pixel.css?v=", as.integer(file.mtime("www/pixel.css")))),  # bust cache
    tags$script(HTML("
      $(document).on('shiny:connected', () => {
        let s = ''; try { s = localStorage.getItem('mel-zhiba') || '' } catch (e) {}
        Shiny.setInputValue('saved', s);
      });
      Shiny.addCustomMessageHandler('save', s => { try { localStorage.setItem('mel-zhiba', s) } catch (e) {} });"))),
  div(class = "hud",
    div(class = "title", "MEL LEVEL ", textOutput("level", inline = TRUE)),
    div(class = "stats", textOutput("stats", inline = TRUE))),
  div(class = "grid",
    div(class = "panel",
      h3("AREA LEVEL"), uiOutput("legend"),
      h3("PLACES"),
      checkboxGroupInput("cats", NULL, selected = "Landmark", choiceValues = names(CATS),
        choiceNames = lapply(names(CATS), \(c) tagList(span(class = "dot", style = paste0("background:", CATS[[c]])), c))),
      div(class = "key", span(class = "dot hollow"), "to visit", span(class = "dot"), "checked in",
          span(class = "dash"), "Free Tram Zone"),
      p(class = "hint", "Click an area to set your level. Click a place to check in (again to undo).",
        "Click an achievement to show its targets on the map."),
      actionButton("reset", "RESET SAVE")),
    div(class = "mapwrap", leafletOutput("map", height = "100%"), uiOutput("focusbar")),
    div(class = "panel ach-panel",
      tags$button(class = "collapse-btn", title = "Collapse / expand",
        onclick = "document.querySelector('.grid').classList.toggle('ach-collapsed');
                   setTimeout(() => window.dispatchEvent(new Event('resize')), 50);"),
      h3("ACHIEVEMENTS"), div(class = "ach-mini", textOutput("ach_mini", inline = TRUE)), uiOutput("ach"))))

server <- function(input, output, session) {
  rv <- reactiveValues(visited = integer(0), lv = setNames(rep(0, nrow(areas)), areas$name), ready = FALSE)
  focus <- reactiveVal(NULL)  # index into ACH, or NULL = use layer checkboxes

  output$map <- renderLeaflet({
    leaflet() |>
      addMapPane("areas", 410) |> addMapPane("ftz", 420) |> addMapPane("pts", 430) |>
      # light grey basemap so the data layers carry the colour (OSM/CARTO now block or watermark)
      addProviderTiles(providers$Esri.WorldGrayCanvas, options = providerTileOptions(maxNativeZoom = 16, maxZoom = 19)) |>
      addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}",
               options = tileOptions(maxNativeZoom = 16, maxZoom = 19)) |>
      setView(144.955, -37.808, 14) |>
      addPolygons(data = ftz, fill = FALSE, color = "#1d2b53", weight = 3, dashArray = "8 6",
                  options = pathOptions(pane = "ftz", interactive = FALSE))
  })

  # restore save once both the browser storage and the map are ready
  observe({
    req(!rv$ready, !is.null(input$saved), !is.null(input$map_zoom))
    s <- tryCatch(fromJSON(input$saved), error = \(e) NULL)
    isolate({
      if (!is.null(s)) {
        rv$visited <- as.integer(s$visited)
        lv <- unlist(s$lv)
        ok <- intersect(names(lv), areas$name)
        rv$lv[ok] <- lv[ok]
      }
      leafletProxy("map") |> draw_areas(rv$lv, rv$visited)
      rv$ready <- TRUE
    })
  })

  visible <- reactive({
    if (is.null(focus())) places[places$category %in% input$cats, ] else places[ACH[[focus()]]$g(places), ]
  })

  observe({
    req(rv$ready)
    d <- visible()
    m <- leafletProxy("map") |> clearMarkers() |> clearMarkerClusters() |> add_places(d, isolate(rv$visited))
    if (!is.null(focus()) && nrow(d))
      fitBounds(m, min(d$lon), min(d$lat), max(d$lon), max(d$lat), options = list(padding = c(30, 30)))
  })

  observe({
    req(rv$ready)
    session$sendCustomMessage("save", as.character(toJSON(list(visited = rv$visited, lv = as.list(rv$lv)), auto_unbox = TRUE)))
  })

  observeEvent(input$map_marker_click, {
    id <- as.integer(req(input$map_marker_click$id))
    p <- places[places$id == id, ]
    if (id %in% rv$visited) {
      rv$visited <- setdiff(rv$visited, id)
    } else {
      rv$visited <- c(rv$visited, id)
      showNotification(paste("CHECK-IN!", p$name), duration = 2)
      if (rv$lv[p$area] < 2) rv$lv[p$area] <- 2  # checking in counts as at least "Stopped by"
    }
    m <- leafletProxy("map") |> draw_areas(rv$lv, rv$visited, p$area)
    if (id %in% visible()$id) add_places(m, p, rv$visited)
  })

  observeEvent(input$map_shape_click, {
    a <- input$map_shape_click$id
    showModal(modalDialog(title = a, easyClose = TRUE, footer = NULL,
      div(class = "modal-stats", area_tip(a, rv$lv[[a]], rv$visited)),
      lapply(seq_len(nrow(LEVELS)), \(i) tags$button(
        class = paste("lv-btn", if (LEVELS$lv[i] == rv$lv[[a]]) "current"),
        style = sprintf("background:%s;color:%s", LEVELS$col[i], if (LEVELS$lv[i] >= 4) "#fff" else "#000"),
        onclick = sprintf("Shiny.setInputValue('set_lv', %s, {priority: 'event'})",
                          toJSON(list(area = a, lv = LEVELS$lv[i]), auto_unbox = TRUE)),
        sprintf("Lv %d  %s", LEVELS$lv[i], LEVELS$label[i])))))
  })

  observeEvent(input$set_lv, {
    a <- input$set_lv$area
    rv$lv[a] <- input$set_lv$lv
    removeModal()
    leafletProxy("map") |> draw_areas(rv$lv, rv$visited, a)
  })

  observeEvent(input$focus, focus(if (identical(focus(), input$focus)) NULL else input$focus))
  observeEvent(input$unfocus, focus(NULL))

  observeEvent(input$reset, {
    rv$visited <- integer(0)
    rv$lv[] <- 0
    leafletProxy("map") |> draw_areas(rv$lv, integer(0)) |> clearMarkers() |> clearMarkerClusters() |> add_places(visible(), integer(0))
  })

  prog <- reactive({
    v <- places[places$id %in% rv$visited, ]
    sapply(ACH, \(a) min(a$f(v, rv$lv), a$n))
  })
  done <- reactive(prog() >= sapply(ACH, `[[`, "n"))

  unlocked <- reactiveVal(NULL)
  observe({
    req(rv$ready)
    now <- sapply(ACH, `[[`, "t")[done()]
    old <- isolate(unlocked())
    if (!is.null(old)) for (t in setdiff(now, old))
      showNotification(paste("★ ACHIEVEMENT UNLOCKED:", t), duration = 4, type = "warning")
    unlocked(now)
  })

  output$level <- renderText(sum(rv$lv))
  output$stats <- renderText(sprintf("CHECK-INS %d   ACHIEVEMENTS %d/%d",
                                     length(rv$visited), sum(done()), length(ACH)))

  output$ach_mini <- renderText(sprintf("★%d/%d", sum(done()), length(ACH)))

  output$focusbar <- renderUI({
    req(focus())
    a <- ACH[[focus()]]
    div(class = "focusbar", sprintf("%s · %d places count towards this", a$t, sum(a$g(places))),
        actionLink("unfocus", "✕ show all"))
  })

  output$legend <- renderUI(lapply(seq_len(nrow(LEVELS)), \(i) div(class = "lg",
    span(class = "sw", style = paste0("background:", LEVELS$col[i])),
    span(LEVELS$label[i]), span(class = "lgn", sprintf("×%d", sum(rv$lv == LEVELS$lv[i]))))))

  output$ach <- renderUI(lapply(seq_along(ACH), \(i) {
    a <- ACH[[i]]
    has_g <- !is.null(a$g)
    div(class = paste("ach", if (done()[i]) "done", if (has_g) "clickable", if (identical(focus(), i)) "focused"),
      onclick = if (has_g) sprintf("Shiny.setInputValue('focus', %d, {priority: 'event'})", i),
      div(class = "ach-t", if (done()[i]) "★ " else "☆ ", a$t),
      div(class = "ach-d", a$d),
      div(class = "bar", div(style = sprintf("width:%d%%", round(100 * prog()[i] / a$n)))),
      div(class = "ach-n", sprintf("%d/%d", prog()[i], a$n)))
  }))
}

shinyApp(ui, server)
