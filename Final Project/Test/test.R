library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(sf)
library(ggplot2)
library(tigris)

# Read in the CSV files
lst_2000 <- read.csv("Urban_Heat/Philadelphia_Landsat7_LST_2000.csv")|>
  mutate(GEOID = as.character(GEOID))
lst_2010 <- read.csv("Urban_Heat/Philadelphia_Landsat7_LST_2010.csv")|>
  mutate(GEOID = as.character(GEOID))
lst_2020 <- read.csv("Urban_Heat/Philadelphia_Landsat7_LST_2020.csv")|>
  mutate(GEOID = as.character(GEOID))

# Compute mean LST for each dataset
mean_lst_2000 <- mean(lst_2000$mean_LST_Celsius, na.rm = TRUE)
mean_lst_2010 <- mean(lst_2010$mean_LST_Celsius, na.rm = TRUE)
mean_lst_2020 <- mean(lst_2020$mean_LST_Celsius, na.rm = TRUE)
overall_mean_temperature <- mean(c(mean_lst_2000, mean_lst_2010, mean_lst_2020), na.rm = TRUE)

lst_2000 <- lst_2000 |>
  mutate(GEOID = as.character(GEOID))
lst_2010 <- lst_2010 |>
  mutate(GEOID = as.character(GEOID))
lst_2020 <- lst_2020 |>
  mutate(GEOID = as.character(GEOID))

# Set the TIGER options for downloading
options(tigris_class = "sf") # Ensure data is downloaded as sf objects
options(tigris_use_cache = TRUE) # Cache downloaded files for reuse

# Download census tracts for Pennsylvania (2020)
philly_tracts <- tracts(
  state = "PA",         # State abbreviation
  county = "Philadelphia", # County name
  cb = FALSE,           # Use detailed TIGER/Line boundaries (not simplified)
  year = 2020           # Specify the year (2010 Census)
)

# Download the boundary
chi_bnd <- 
  places(state = "PA") |> 
  filter(NAME == "Philadelphia")



# Merge each year's LST data with the shapefile
lst_2000_sf <- philly_tracts |>
  left_join(lst_2000, by = "GEOID")|> st_transform(4326)

lst_2010_sf <- philly_tracts |>
  left_join(lst_2010, by = "GEOID")|> st_transform(4326)

lst_2020_sf <- philly_tracts |>
  left_join(lst_2020, by = "GEOID")|> st_transform(4326)

chi_bnd <- chi_bnd |> st_transform(4326)

# Load water or open space shapefile
water_sf <- st_read("Urban_Heat/Hydrographic_Features_Poly/Hydrographic_Features_Poly.shp") |> st_transform(4326)
open_space_sf <- st_read("Urban_Heat/PPR_Properties/PPR_Properties.shp") |> st_transform(4326)
water_sf <- st_make_valid(water_sf)
open_space_sf <- st_make_valid(open_space_sf)

# Perform spatial intersection to keep features within the boundary
water_sf <- st_intersection(water_sf, chi_bnd) 
open_space_sf <- st_intersection(open_space_sf, chi_bnd)

# Find the global range of temperatures across all datasets
global_min <- min(lst_2000_sf$mean_LST_Celsius, lst_2010_sf$mean_LST_Celsius, lst_2020_sf$mean_LST_Celsius, na.rm = TRUE)
global_max <- max(lst_2000_sf$mean_LST_Celsius, lst_2010_sf$mean_LST_Celsius, lst_2020_sf$mean_LST_Celsius, na.rm = TRUE)

# Calculate Heat Exposure Score and Normalize to 0-1
# Function to calculate normalized heat exposure score
calculate_normalized_score <- function(data) {
  data |>
    mutate(
      Mean_LST = mean(mean_LST_Celsius, na.rm = TRUE),
      SD_LST = sd(mean_LST_Celsius, na.rm = TRUE),
      Normalized_Heat_Score = (mean_LST_Celsius - Mean_LST) / SD_LST
    )
}

# Apply the function to each dataset
lst_2000_sf <- lst_2000_sf |> calculate_normalized_score()
lst_2010_sf <- lst_2010_sf |> calculate_normalized_score()
lst_2020_sf <- lst_2020_sf |> calculate_normalized_score()

# Calculate global range for Normalized Heat Scores
global_normalized_min <- min(c(
  lst_2000_sf$Normalized_Heat_Score,
  lst_2010_sf$Normalized_Heat_Score,
  lst_2020_sf$Normalized_Heat_Score
), na.rm = TRUE)

global_normalized_max <- max(c(
  lst_2000_sf$Normalized_Heat_Score,
  lst_2010_sf$Normalized_Heat_Score,
  lst_2020_sf$Normalized_Heat_Score
), na.rm = TRUE)

# Function to scale values to the range [-1, 1]
scale_to_minus_one_to_one <- function(data) {
  data |>
    mutate(
      Heat_Exposure_Score = 2 * (Normalized_Heat_Score - global_normalized_min) / (global_normalized_max - global_normalized_min) - 1
    )
}

# Apply the scaling function to each dataset
lst_2000_sf <- scale_to_minus_one_to_one(lst_2000_sf)
lst_2010_sf <- scale_to_minus_one_to_one(lst_2010_sf)
lst_2020_sf <- scale_to_minus_one_to_one(lst_2020_sf)