library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(sf)
library(ggplot2)
library(tigris)

# Read in the CSV files
lst_2000 <- read.csv("Philadelphia_Landsat7_LST_2000.csv")%>%
  mutate(GEOID = as.character(GEOID))
lst_2010 <- read.csv("Philadelphia_Landsat7_LST_2010.csv")%>%
  mutate(GEOID = as.character(GEOID))
lst_2020 <- read.csv("Philadelphia_Landsat7_LST_2020.csv")%>%
  mutate(GEOID = as.character(GEOID))

# Set the TIGER options for downloading
options(tigris_class = "sf") # Ensure data is downloaded as sf objects
options(tigris_use_cache = TRUE) # Cache downloaded files for reuse

# Download census tracts for Pennsylvania (2010)
philly_tracts <- tracts(
  state = "PA",         # State abbreviation
  county = "Philadelphia", # County name
  cb = FALSE,           # Use detailed TIGER/Line boundaries (not simplified)
  year = 2020           # Specify the year (2010 Census)
)

# Merge each year's LST data with the shapefile
lst_2000_sf <- philly_tracts %>%
  left_join(lst_2000, by = "GEOID")

lst_2010_sf <- philly_tracts %>%
  left_join(lst_2010, by = "GEOID")

lst_2020_sf <- philly_tracts %>%
  left_join(lst_2020, by = "GEOID")

# Find the global range of temperatures across all datasets
global_min <- min(lst_2000_sf$mean_LST_Celsius, lst_2010_sf$mean_LST_Celsius, lst_2020_sf$mean_LST_Celsius, na.rm = TRUE)
global_max <- max(lst_2000_sf$mean_LST_Celsius, lst_2010_sf$mean_LST_Celsius, lst_2020_sf$mean_LST_Celsius, na.rm = TRUE)


# Load water or open space shapefile
water_sf <- st_read("Hydrographic_Features_Poly/Hydrographic_Features_Poly.shp")
open_space_sf <- st_read("PPR_Properties/PPR_Properties.shp")

# Define the UI
ui <- fluidPage(
  titlePanel("Land Surface Temperature (LST) Maps (07.15-08.11) : 2000, 2010, 2020"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", 
                  choices = c("2000", "2010", "2020"), 
                  selected = "2000")
    ),
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Reactive function to return the selected dataset
  selected_data <- reactive({
    if (input$year == "2000") {
      lst_2000_sf
    } else if (input$year == "2010") {
      lst_2010_sf
    } else {
      lst_2020_sf
    }
  })
  
  # Render the Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -75.1652, lat = 39.9526, zoom = 11) # Center on Philadelphia
  })
  
  # Observe changes in the selected year and update the map
  observe({
    data <- selected_data()
    pal <- colorNumeric(palette = rev(viridis::viridis(256)), domain = c(global_min, global_max))
    
    leafletProxy("map", data = data) %>%
      clearShapes() %>%
      clearControls() %>%
      # Add polygons for LST
      addPolygons(
        fillColor = ~pal(mean_LST_Celsius),
        color = "white", weight = 0.5, fillOpacity = 0.7,
        popup = ~paste0("GEOID: ", GEOID, "<br>Mean LST: ", round(mean_LST_Celsius, 2), " °C")
      ) %>%
      # Add water features
      addPolygons(
        data = water_sf,
        color = "blue", weight = 0.5, fillOpacity = 1,
        group = "Water Features"
      ) %>%
      # Add open space
      addPolygons(
        data = open_space_sf,
        color = "green", weight = 0.5, fillOpacity = 1,
        group = "Open Space"
      ) %>%
      # Add a legend for LST
      addLegend(
        pal = pal, values = c(global_min, global_max),
        title = "Mean LST (°C)", position = "bottomright"
      ) %>%
      # Add layer control
      addLayersControl(
        overlayGroups = c("Water Features", "Open Space"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}

# Run the app
shinyApp(ui, server)