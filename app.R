# MEL 制霸: pixel-art conquest map. Click an area to set your level, click a marker to check in.
library(shiny)
library(leaflet)
library(sf)
library(jsonlite)

places <- read.csv("data/processed/places.csv")
places <- places[places$category != "Ped sensor", ]  # data layer, not a game target
areas <- st_read("data/processed/areas_pixel.geojson", quiet = TRUE)
ftz <- st_read("data/processed/ftz.geojson", quiet = TRUE)
area_lbl <- st_transform(st_point_on_surface(st_transform(areas, 3857)), 4326)

# PICO-8 palette
LEVELS <- data.frame(lv = 5:0,
  label = c("Lived there", "Stayed the night", "Explored", "Stopped by", "Passed through", "Never been"),
  col = c("#ff004d", "#ffa300", "#ffec27", "#00e436", "#29adff", "#fff1e8"))
lv_col <- function(l) LEVELS$col[match(l, LEVELS$lv)]

CATS <- c(Landmark = "#ff77a8", Food = "#ab5236", Park = "#008751", `Tram stop` = "#7e2553", Toilet = "#83769c")
SPRITES <- list(
  Landmark    = c("...##...", "..#oo#..", "###oo###", "#oooooo#", ".#oooo#.", ".#o##o#.", "#o#..#o#", "##....##"),
  Food        = c(".#.#.#..", "........", "######..", "#oooo###", "#oooo#.#", "#oooo###", ".####...", "........"),
  Park        = c("..####..", ".#oooo#.", "#oooooo#", "#oooooo#", ".#oooo#.", "..####..", "...##...", "..####.."),
  `Tram stop` = c("...##...", "########", "#o#oo#o#", "#o#oo#o#", "#oooooo#", "#o#oo#o#", "########", ".#....#."),
  Toilet      = c(".######.", ".#oooo#.", ".#oooo#.", ".#oo#o#.", ".#oooo#.", ".#oooo#.", ".#oooo#.", "########"))

sprite <- function(rows, fill) {
  px <- unlist(lapply(seq_along(rows), \(y) {
    ch <- strsplit(rows[y], "")[[1]]
    x <- which(ch != ".")
    sprintf('<rect x="%d" y="%d" width="1" height="1" fill="%s"/>', x - 1, y - 1,
            ifelse(ch[x] == "#", "#1d2b53", fill))
  }))
  svg <- paste0('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 8 8" shape-rendering="crispEdges">',
                paste(px, collapse = ""), "</svg>")
  paste0("data:image/svg+xml;base64,", base64_enc(svg))
}
# row 1 = not visited (greyed, "locked"), row 2 = visited
ICON <- sapply(names(CATS), \(c) c(sprite(SPRITES[[c]], "#c2c3c7"), sprite(SPRITES[[c]], CATS[[c]])))

quiet <- names(which(table(places$area[places$category == "Food"]) < 60))
ACH <- list(
  list(t = "FIRST STEP",     d = "Check in anywhere",                         n = 1,  f = \(v, lv) nrow(v)),
  list(t = "SIGHTSEER",      d = "Visit 10 landmarks",                        n = 10, f = \(v, lv) sum(v$category == "Landmark")),
  list(t = "GREEN THUMB",    d = "Visit 5 parks",                             n = 5,  f = \(v, lv) sum(v$category == "Park")),
  list(t = "HIDDEN GEM",     d = "Eat at 3 rare spots (bakery, club, grocer...)", n = 3, f = \(v, lv) sum(v$rare)),
  list(t = "OFF THE MAP",    d = "Eat in a quiet suburb (<60 eateries)",      n = 1,  f = \(v, lv) sum(v$category == "Food" & v$area %in% quiet)),
  list(t = "FREE RIDER",     d = "Use 5 tram stops inside the Free Tram Zone", n = 5, f = \(v, lv) sum(v$category == "Tram stop" & v$in_ftz)),
  list(t = "ZONE HOPPER",    d = "Ride trams inside AND outside the FTZ",     n = 2,  f = \(v, lv) { t <- v$in_ftz[v$category == "Tram stop"]; any(t) + any(!t) }),
  list(t = "NATURE CALLS",   d = "Find 3 public toilets",                     n = 3,  f = \(v, lv) sum(v$category == "Toilet")),
  list(t = "CITY CONQUEROR", d = "Reach Lv1+ in all 13 areas",                n = 13, f = \(v, lv) sum(lv > 0)),
  list(t = "LOCAL LEGEND",   d = "Reach MEL LEVEL 30",                        n = 30, f = \(v, lv) sum(lv)))

draw_areas <- function(map, lv, which = names(lv)) {
  d <- areas[match(which, areas$name), ]
  addPolygons(map, data = d, layerId = ~name, fillColor = lv_col(lv[d$name]), fillOpacity = 0.55,
              color = "#1d2b53", weight = 3, label = ~name, options = pathOptions(pane = "areas"))
}
add_places <- function(map, d, visited) {
  url <- ICON[cbind(d$id %in% visited + 1, match(d$category, colnames(ICON)))]
  addMarkers(map, data = d, lng = ~lon, lat = ~lat, layerId = ~as.character(id), group = ~category,
             icon = icons(iconUrl = url, iconWidth = 16, iconHeight = 16),
             label = ~paste0(name, " | ", sub))
}

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Press+Start+2P&family=VT323&display=swap"),
    tags$link(rel = "stylesheet", href = "pixel.css"),
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
      h3("LAYERS"), checkboxGroupInput("cats", NULL, names(CATS), selected = "Landmark"),
      p(class = "hint", "Click an area to set your level. Click a marker to check in (again to undo)."),
      actionButton("reset", "RESET SAVE")),
    leafletOutput("map", height = "100%"),
    div(class = "panel", h3("ACHIEVEMENTS"), uiOutput("ach"))))

server <- function(input, output, session) {
  rv <- reactiveValues(visited = integer(0), lv = setNames(rep(0, nrow(areas)), areas$name), ready = FALSE)

  output$map <- renderLeaflet({
    leaflet() |>
      addMapPane("areas", 410) |> addMapPane("ftz", 420) |>
      # 4x upscaled low-zoom tiles + CSS image-rendering: pixelated = chunky pixel basemap
      addProviderTiles(providers$OpenStreetMap.Mapnik,
                       options = providerTileOptions(tileSize = 1024, zoomOffset = -2)) |>
      setView(144.955, -37.808, 14) |>
      addPolygons(data = ftz, fill = FALSE, color = "#ff004d", weight = 4, dashArray = "8 8",
                  options = pathOptions(pane = "ftz", interactive = FALSE)) |>
      addLabelOnlyMarkers(data = area_lbl, label = ~name,
                          labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, className = "area-label"))
  })

  # restore save once both the browser storage and the map are ready
  observe({
    req(!rv$ready, !is.null(input$saved), !is.null(input$map_zoom))
    s <- tryCatch(fromJSON(input$saved), error = \(e) NULL)
    isolate({
      if (!is.null(s)) {
        rv$visited <- as.integer(s$visited)
        lv <- unlist(s$lv)
        rv$lv[intersect(names(lv), areas$name)] <- lv[intersect(names(lv), areas$name)]
      }
      leafletProxy("map") |> draw_areas(rv$lv) |> add_places(places, rv$visited)
      rv$ready <- TRUE
    })
  })

  observe({
    req(rv$ready)
    m <- leafletProxy("map")
    for (c in names(CATS)) m <- if (c %in% input$cats) showGroup(m, c) else hideGroup(m, c)
  })

  observe({
    req(rv$ready)
    session$sendCustomMessage("save", as.character(toJSON(list(visited = rv$visited, lv = as.list(rv$lv)), auto_unbox = TRUE)))
  })

  observeEvent(input$map_marker_click, {
    id <- as.integer(req(input$map_marker_click$id))
    p <- places[places$id == id, ]
    m <- leafletProxy("map")
    if (id %in% rv$visited) {
      rv$visited <- setdiff(rv$visited, id)
    } else {
      rv$visited <- c(rv$visited, id)
      showNotification(paste("CHECK-IN!", p$name), duration = 2)
      if (rv$lv[p$area] < 2) {  # checking in counts as at least "Stopped by"
        rv$lv[p$area] <- 2
        m <- draw_areas(m, rv$lv, p$area)
      }
    }
    add_places(m, p, rv$visited)
  })

  observeEvent(input$map_shape_click, {
    a <- input$map_shape_click$id
    n <- sum(places$id %in% rv$visited & places$area == a)
    showModal(modalDialog(title = a, easyClose = TRUE, footer = NULL,
      p(sprintf("Lv %d  |  %d check-ins here", rv$lv[[a]], n)),
      lapply(seq_len(nrow(LEVELS)), \(i) tags$button(
        class = "lv-btn", style = paste0("background:", LEVELS$col[i]),
        onclick = sprintf("Shiny.setInputValue('set_lv', %s, {priority: 'event'})",
                          toJSON(list(area = a, lv = LEVELS$lv[i]), auto_unbox = TRUE)),
        sprintf("Lv %d  %s", LEVELS$lv[i], LEVELS$label[i])))))
  })

  observeEvent(input$set_lv, {
    a <- input$set_lv$area
    rv$lv[a] <- input$set_lv$lv
    removeModal()
    leafletProxy("map") |> draw_areas(rv$lv, a)
  })

  observeEvent(input$reset, {
    rv$visited <- integer(0)
    rv$lv[] <- 0
    leafletProxy("map") |> draw_areas(rv$lv) |> add_places(places, integer(0))
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

  output$legend <- renderUI(lapply(seq_len(nrow(LEVELS)), \(i) div(class = "lg",
    span(class = "sw", style = paste0("background:", LEVELS$col[i])),
    span(LEVELS$label[i]), span(class = "lgn", sprintf("Lv%d x%d", LEVELS$lv[i], sum(rv$lv == LEVELS$lv[i]))))))

  output$ach <- renderUI(lapply(seq_along(ACH), \(i) {
    a <- ACH[[i]]
    div(class = paste("ach", if (done()[i]) "done"),
      div(class = "ach-t", if (done()[i]) "★ " else "☆ ", a$t),
      div(class = "ach-d", a$d),
      div(class = "bar", div(style = sprintf("width:%d%%", round(100 * prog()[i] / a$n)))),
      div(class = "ach-n", sprintf("%d/%d", prog()[i], a$n)))
  }))
}

shinyApp(ui, server)
