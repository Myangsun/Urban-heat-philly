library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(sf)
library(ggplot2)
library(tigris)

# Load df
lst_2000_sf <- st_read("lst_2000_sf.shp")
lst_2010_sf <- st_read("lst_2010_sf.shp")
lst_2020_sf <- st_read("lst_2020_sf.shp")

# Download the boundary
chi_bnd <- 
  places(state = "PA") |> 
  filter(NAME == "Philadelphia")

# Load water or open space shapefile
water_sf <- st_read("Hydrographic_Features_Poly/Hydrographic_Features_Poly.shp") |> st_transform(4326)
open_space_sf <- st_read("PPR_Properties/PPR_Properties.shp") |> st_transform(4326)
water_sf <- st_make_valid(water_sf)
open_space_sf <- st_make_valid(open_space_sf)

# Define the UI
ui <- navbarPage(
  tags$style(HTML("
    .leaflet-control-layers-list {
      text-align: left !important;
    }
  ")),
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
    "Map Viewer",
    fluidPage(
      titlePanel("Philadelphia Urban Heat Vulnerability"),
      fluidRow(
        column(4, align = "left",
               selectInput(
                 "year", "Select Year:",
                 choices = c("2000", "2010", "2020"),
                 selected = "2000",
                 width = "100%"
               )
        )),
      # Year Selection
      fluidRow(
        column(4, align = "center",
               tags$h4("Heat Exposure Map"),
               leafletOutput("heat_exposure_map", height = "600px")
        ),
        
        # socioeconomic_vulnerability_map with Dropdown
        column(4, align = "center",
               tags$h4("Socioeconomic Vulnerability Map"),
               leafletOutput("socioeconomic_vulnerability_map", height = "600px")
        ),
        
        # Heat Vulnerability Map
        column(4, align = "center",
               tags$h4("Heat Vulnerability Map"),
               leafletOutput("heat_vulnerability_map", height = "600px"))))),
  
  
tabPanel(
  "Data Summary",
  fluidPage(
    h3("Data Summary for Land Surface Temperature (LST)"),
    fluidRow(
      # Left column for text and explanations
      column(
        width = 6,
        h4("Overview"),
        p("This section provides an overview of the LST data, including distribution and trends across years."),
        h4("Overall Mean Temperature"),
        textOutput("overall_mean"),
        h4("Insights"),
        p("The visualizations to the right provide a detailed look into the LST data."),
        p("- The boxplot shows the spread of temperatures across years."),
        p("- The histogram highlights the frequency distribution of temperatures."),
        p("- The trend plot illustrates how the average LST has changed over time.")
      ),
      # Right column for charts
      column(
        width = 6,
        h4("Visualizations"),
        plotOutput("boxplot_all", height = "300px"),  # Boxplot for all years combined
        plotOutput("histogram_all", height = "300px"),  # Histogram for all years combined
        plotOutput("trend_plot", height = "300px")  # Line plot of average LST trends over time
      )
    )
  )
)
)

server <- function(input, output, session) {
  selected_data <- reactive({
    if (input$year == "2000") {
      lst_2000_sf
    } else if (input$year == "2010") {
      lst_2010_sf
    } else {
      lst_2020_sf
    }
  })
  
  # Get the centroid
  centroid <- st_centroid(chi_bnd)
  
  # Extract latitude and longitude
  centroid_coords <- st_coordinates(centroid)
  
  lat_philly <- centroid_coords[2]
  lng_philly <- centroid_coords[1]
  
  output$heat_exposure_map <- renderLeaflet({
    data <- selected_data()
    #pal <- colorNumeric(palette = rev(viridis::viridis(256)), domain = data$m_LST_C)
    pal_heat <- colorNumeric(palette = "Reds", domain = data$Ht_Ex_S)
    
    leaflet(data) |>
      setView(lng = lng_philly, lat = lat_philly, zoom = 11) |>
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery") |>
      addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark") |>
      # addPolygons(
      #   fillColor = ~pal(m_LST_C),
      #   color = "white", weight = 0.5, fillOpacity = 1,
      #   popup = ~paste0(
      #     "GEOID: ", GEOID,
      #     "<br>Mean LST: ", round(m_LST_C, 2), " °C"
      #   ),
      #   group = "Mean LST"
      # ) |>
      addPolygons(
        fillColor = ~pal_heat(Ht_Ex_S),
        color = "white", weight = 0.5, fillOpacity = 0.9,
        popup = ~paste0(
          "GEOID: ", GEOID,
          "<br>Heat Exposure Score: ", round(Ht_Ex_S, 2)
        ),
        group = "Heat Exposure Score"
      ) |>
      addPolygons(
        data = water_sf,
        color = "#40798c", weight = 0.5, fillOpacity = 1,
        group = "Water Features"
      ) |>
      addPolygons(
        data = open_space_sf,
        color = "#cfe0c3", weight = 0.5, fillOpacity = 1,
        group = "Open Space"
      ) |>
      # addLegend(
      #   pal = pal, values = data$mean_LST_Celsius,
      #   title = "Mean LST (°C)", position = "bottomright", group = "Mean LST"
      # ) |>
      addLegend(
        pal = pal_heat, values = c(-1,1),
        title = "Heat Exposure Score", position = "bottomright", group = "Heat Exposure Score"
      ) |>
      addLayersControl(
        baseGroups = c("CartoDB Positron", "ESRI World Imagery", "CartoDB Dark"),
        #overlayGroups = c("Mean LST", "Heat Exposure Score", "Water Features", "Open Space"),
        overlayGroups = c("Heat Exposure Score", "Water Features", "Open Space"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  # Render Demographic Map
  output$socioeconomic_vulnerability_map <- renderLeaflet({
    data <- selected_data()  # Reactive soc_data
    
    pal_demo <- colorNumeric(
      palette = "Blues", 
      domain = data$sccnmc_  #Vul score data, na.rm = TRUE
    )
    
    leaflet(data) |>
      setView(lng = lng_philly, lat = lat_philly, zoom = 11) |>
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery") |>
      addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark") |>
      # addPolygons(
      #   fillColor = ~pal_demo(-1),
      #   color = "white", weight = 0.5, fillOpacity = 0.5,
      #   popup = ~paste0(
      #     "GEOID: ", GEOID,
      #     "<br>Socioeconomic Vulnerability Score: ", round(sccnmc_, 2)
      #   ),
      #   group = "Socioeconomic Vulnerability Base"
      # ) |>
      addPolygons(
        fillColor = ~pal_demo(sccnmc_),
        color = "white", weight = 0.5, fillOpacity = 0.9,
        popup = ~paste0(
          "GEOID: ", GEOID,
          "<br>Socioeconomic Vulnerability Score: ", round(sccnmc_, 2)
        ),
        group = "Socioeconomic Vulnerability"
      ) |>
      addPolygons(
        data = water_sf,
        color = "#40798c", weight = 0.5, fillOpacity = 1,
        group = "Water Features"
      ) |>
      addPolygons(
        data = open_space_sf,
        color = "#cfe0c3", weight = 0.5, fillOpacity = 1,
        group = "Open Space"
      ) |>

      addLegend(
        pal = pal_demo, values = c(-1,1),
        title = "Socioeconomic Vulnerability Score:", position = "bottomright", group = "Socioeconomic Vulnerability Score"
      ) |>
      addLayersControl(
        baseGroups = c("CartoDB Positron", "ESRI World Imagery", "CartoDB Dark"),
        overlayGroups = c("Socioeconomic Vulnerability", "Water Features", "Open Space"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  # Render Heat Vulnerability Score Map
  output$heat_vulnerability_map <- renderLeaflet({
    data <- selected_data()
    pal_vuln <- colorNumeric(palette = "Purples", domain = data$scald_v)
    pal_priority <- colorFactor(palette = c("#3d348b"), domain = c(1))
    
    leaflet(data)|>
      setView(lng = lng_philly, lat = lat_philly, zoom = 11) |>
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery") |>
      addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark") |>
      addPolygons(
        fillColor = ~pal_vuln(scald_v),
        color = "white", weight = 0.5, fillOpacity = 0.9,
        popup = ~paste0(
          "GEOID: ", GEOID,
          "<br>Heat Vulnerability Score: ", round(scald_v, 2)
        ),
        group = "Heat Vulnerability Score"
      )  |>
      # Add 10%
      addPolygons(
        data = data |> filter(tp_10_p == 1),  # Filter polygons where tp_10_p == 1
        fillColor = "#3c096c",
        color = "#240046", weight = 1, fillOpacity = 1,
        popup = ~paste0("GEOID: ", GEOID),
        group = "1st Priority Neighborhood"
      ) |>
      # Add water features
      addPolygons(
        data = water_sf,
        color = "#40798c", weight = 0.5, fillOpacity = 1,
        group = "Water Features"
      ) |>
      # Add open space
      addPolygons(
        data = open_space_sf,
        color = "#cfe0c3", weight = 0.5, fillOpacity = 1,
        group = "Open Space"
      ) |>
      addLegend(
        pal = pal_vuln, values = c(-1,1),
        title = "Heat Vulnerability Score:", position = "bottomright", group = "Heat Vulnerability Score"
      ) |>
      addLegend(
        colors = "#240046",                   # Static color for the legend
        labels = "1st Priority Neighborhood", # Custom label
        title = "Legend",                     # Title for the legend
        position = "bottomright"              # Legend position
      ) |>
      addLayersControl(
        baseGroups = c("CartoDB Positron", "ESRI World Imagery", "CartoDB Dark"),
        overlayGroups = c("Water Features", "Open Space", "Heat Vulnerability Score","1st Priority Neighborhood"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  output$boxplot_all <- renderPlot({
    # Combine all datasets
    combined_data <- bind_rows(
      lst_2000 |> mutate(Year = 2000),
      lst_2010 |> mutate(Year = 2010),
      lst_2020 |> mutate(Year = 2020)
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