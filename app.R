# Bare-bone base map. No styling on purpose: everyone forks this to explore styles.
library(shiny)
library(leaflet)

places <- read.csv("data/processed/places.csv")
cats <- unique(places$category)

ui <- fluidPage(
  titlePanel("Melbourne CBD Visitor Planner (bare-bone)"),
  sidebarLayout(
    sidebarPanel(width = 2,
      checkboxGroupInput("cats", "Layers", cats, selected = "Landmark")
    ),
    mainPanel(width = 10, leafletOutput("map", height = "85vh"))
  )
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() |> addTiles() |> setView(144.9631, -37.8136, zoom = 15)
  })

  observe({
    d <- places[places$category %in% input$cats, ]
    leafletProxy("map", data = d) |>
      clearMarkers() |>
      addMarkers(~lon, ~lat, popup = ~paste0("<b>", name, "</b><br>", category, " / ", sub),
                 label = ~name)
  })
}

shinyApp(ui, server)
