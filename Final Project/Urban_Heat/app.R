library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(sf)
library(ggplot2)
library(tigris)

# Read in the CSV files
lst_2000 <- read.csv("Philadelphia_Landsat7_LST_2000.csv")
lst_2010 <- read.csv("Philadelphia_Landsat7_LST_2010.csv")
lst_2020 <- read.csv("Philadelphia_Landsat7_LST_2020.csv")

# Compute mean LST for each dataset
mean_lst_2000 <- mean(lst_2000$mean_LST_Celsius, na.rm = TRUE)
mean_lst_2010 <- mean(lst_2010$mean_LST_Celsius, na.rm = TRUE)
mean_lst_2020 <- mean(lst_2020$mean_LST_Celsius, na.rm = TRUE)
overall_mean_temperature <- mean(c(mean_lst_2000, mean_lst_2010, mean_lst_2020), na.rm = TRUE)

lst_2000 <- lst_2000 |>
  mutate(GEOID = as.character(GEOID)) |>
  mutate(Heat_Exposure_Score = mean_LST_Celsius - overall_mean_temperature)
lst_2010 <- lst_2010 |>
  mutate(GEOID = as.character(GEOID)) |>
  mutate(Heat_Exposure_Score = mean_LST_Celsius - overall_mean_temperature)
lst_2020 <- lst_2020 |>
  mutate(GEOID = as.character(GEOID)) |>
  mutate(Heat_Exposure_Score = mean_LST_Celsius - overall_mean_temperature)

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
lst_2000_sf <- philly_tracts |>
  left_join(lst_2000, by = "GEOID")

lst_2010_sf <- philly_tracts |>
  left_join(lst_2010, by = "GEOID")

lst_2020_sf <- philly_tracts |>
  left_join(lst_2020, by = "GEOID")

# Find the global range of temperatures across all datasets
global_min <- min(lst_2000_sf$mean_LST_Celsius, lst_2010_sf$mean_LST_Celsius, lst_2020_sf$mean_LST_Celsius, na.rm = TRUE)
global_max <- max(lst_2000_sf$mean_LST_Celsius, lst_2010_sf$mean_LST_Celsius, lst_2020_sf$mean_LST_Celsius, na.rm = TRUE)


# Load water or open space shapefile
water_sf <- st_read("Hydrographic_Features_Poly/Hydrographic_Features_Poly.shp")
open_space_sf <- st_read("PPR_Properties/PPR_Properties.shp")

# Define the UI
ui <- navbarPage(
  title = "Philadelphia Urban Heat Vulnerability",
  tabPanel(
    "About",
    fluidPage(
      h3("About This Application"),
      p("This Shiny application visualizes the Land Surface Temperature (LST) data for Philadelphia over three decades: 2000, 2010, and 2020."),
      p("The application allows users to interactively explore temperature changes and examine the impact of water features and open spaces.")
    )
  ),
  tabPanel(
    "LST Map Viewer",
    fluidPage(
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
  ),
  tabPanel(
    "Data Summary",
    fluidPage(
      h3("Data Summary for Land Surface Temperature (LST)"),
      p("This section provides an overview of the LST data, including distribution and trends across years."),
      plotOutput("boxplot_all", height = "300px"),  # Boxplot for all years combined
      plotOutput("histogram_all", height = "300px"),  # Histogram for all years combined
      plotOutput("trend_plot", height = "300px"),  # Line plot of average LST trends over time
      h4("Overall Mean Temperature"),
      textOutput("overall_mean"),
      h4("Insights"),
      p("The visualizations above provide a detailed look into the LST data. The boxplot shows the spread of temperatures, while the histogram highlights the frequency distribution.")
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
    leaflet() |>
      setView(lng = -75.1652, lat = 39.9526, zoom = 11) |>
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery") |>
      addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark") |>
      addLayersControl(
        baseGroups = c("CartoDB Positron", "ESRI World Imagery", "CartoDB Dark"),
        overlayGroups = c("Water Features", "Open Space"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observe({
    data <- selected_data()
    pal <- colorNumeric(palette = rev(viridis::viridis(256)), domain = c(global_min, global_max))
    
    # Define color palette for Heat Exposure Score
    pal_heat <- colorNumeric(palette = "YlOrRd", domain = data$Heat_Exposure_Score)
    
    leafletProxy("map", data = data) %>%
      clearShapes() %>%
      clearControls() %>%
      # Add polygons for Mean LST
      addPolygons(
        fillColor = ~pal(mean_LST_Celsius),
        color = "white", weight = 0.5, fillOpacity = 0.7,
        popup = ~paste0(
          "GEOID: ", GEOID, 
          "<br>Mean LST: ", round(mean_LST_Celsius, 2), " °C",
          "<br>Heat Exposure Score: ", round(Heat_Exposure_Score, 2)
        ),
        group = "Mean LST"
      ) %>%
      # Add polygons for Heat Exposure Score
      addPolygons(
        fillColor = ~pal_heat(Heat_Exposure_Score),
        color = "white", weight = 0.5, fillOpacity = 0.7,
        popup = ~paste0(
          "GEOID: ", GEOID,
          "<br>Heat Exposure Score: ", round(Heat_Exposure_Score, 2)
        ),
        group = "Heat Exposure Score"
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
      # Add legends for each layer
      addLegend(
        pal = pal, values = c(global_min, global_max),
        title = "Mean LST (°C)", position = "bottomright", group = "Mean LST"
      ) %>%
      addLegend(
        pal = pal_heat, values = range(data$Heat_Exposure_Score, na.rm = TRUE),
        title = "Heat Exposure Score", position = "bottomright", group = "Heat Exposure Score"
      ) %>%
      # Add layer control
      addLayersControl(
        overlayGroups = c("Mean LST", "Heat Exposure Score", "Water Features", "Open Space"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  output$boxplot_all <- renderPlot({
    # Combine all datasets
    combined_data <- bind_rows(
      lst_2000 %>% mutate(Year = 2000),
      lst_2010 %>% mutate(Year = 2010),
      lst_2020 %>% mutate(Year = 2020)
    )
    
    # Create the boxplot
    ggplot(combined_data, aes(x = as.factor(Year), y = mean_LST_Celsius)) +
      geom_boxplot() +
      labs(
        title = "Boxplot of Mean LST Across Years",
        x = "Year",
        y = "Mean LST (°C)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none"
      )
  })
}

# Run the app
shinyApp(ui, server)