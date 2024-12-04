library(dplyr)
library(sf)
library(ggplot2)
library(tigris)
library(patchwork)
library(viridis)
# Read in the CSV files
lst_2000 <- read.csv("Urban_Heat/Philadelphia_Landsat7_LST_2000.csv")|>
  mutate(GEOID = as.character(GEOID))
lst_2010 <- read.csv("Urban_Heat/Philadelphia_Landsat7_LST_2010.csv")|>
  mutate(GEOID = as.character(GEOID))
lst_2020 <- read.csv("Urban_Heat/Philadelphia_Landsat7_LST_2020.csv")|>
  mutate(GEOID = as.character(GEOID))

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
lst_2000 <- lst_2000 |> calculate_normalized_score()
lst_2010 <- lst_2010 |> calculate_normalized_score()
lst_2020 <- lst_2020 |> calculate_normalized_score()


# Set the TIGER options for downloading
options(tigris_class = "sf") # Ensure data is downloaded as sf objects
options(tigris_use_cache = TRUE) # Cache downloaded files for reuse

# Download the boundary
chi_bnd <- 
  places(state = "PA") |> 
  filter(NAME == "Philadelphia")

# Download census tracts for Pennsylvania (2020)
philly_tracts <- tracts(
  state = "PA",         # State abbreviation
  county = "Philadelphia", # County name
  cb = FALSE,           # Use detailed TIGER/Line boundaries (not simplified)
  year = 2020           # Specify the year (2010 Census)
)

st_crs(chi_bnd)
st_crs(philly_tracts)

# Merge each year's LST data with the shapefile
lst_2000_sf <- philly_tracts %>%
  left_join(lst_2000, by = "GEOID")

lst_2010_sf <- philly_tracts %>%
  left_join(lst_2010, by = "GEOID")

lst_2020_sf <- philly_tracts %>%
  left_join(lst_2020, by = "GEOID")

st_crs(lst_2000_sf)

# Find the global min and max LST
global_min <- min(lst_2000_sf$mean_LST_Celsius, lst_2010_sf$mean_LST_Celsius, lst_2020_sf$mean_LST_Celsius, na.rm = TRUE)
global_max <- max(lst_2000_sf$mean_LST_Celsius, lst_2010_sf$mean_LST_Celsius, lst_2020_sf$mean_LST_Celsius, na.rm = TRUE)

# Print the range
print(c(global_min, global_max))

# Map for 2000
map_2000 <- ggplot(data = lst_2000_sf) +
  geom_sf(aes(fill = mean_LST_Celsius), color = NA) +
  scale_fill_viridis_c(name = "LST (°C)", option = "magma", na.value = "white", limits = c(global_min, global_max),direction = -1 ) +
  labs(title = "2000") +
  theme_minimal()

# Map for 2010
map_2010 <- ggplot(data = lst_2010_sf) +
  geom_sf(aes(fill = mean_LST_Celsius), color = NA) +
  scale_fill_viridis_c(name = "LST (°C)", option = "magma", na.value = "white", limits = c(global_min, global_max),direction = -1 ) +
  labs(title = "2010") +
  theme_minimal()

# Map for 2020
map_2020 <- ggplot(data = lst_2020_sf) +
  geom_sf(aes(fill = mean_LST_Celsius), color = NA) +
  scale_fill_viridis_c(name = "LST (°C)", option = "magma", na.value = "white", limits = c(global_min, global_max),direction = -1 ) +
  labs(title = "2020") +
  theme_minimal()

# Combine the maps into a single row
combined_map <- map_2000 + map_2010 + map_2020 +
  plot_layout(ncol = 3) & # Arrange in a row with 3 columns
  theme(legend.position = "bottom") # Adjust legend position

# Display the combined map
print(combined_map)

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

print(c(global_normalized_min, global_normalized_max))

# Function to scale values to the range [-1, 1]
scale_to_minus_one_to_one <- function(data) {
  data |>
    mutate(
      Scaled_Heat_Score = 2 * (Normalized_Heat_Score - global_normalized_min) / (global_normalized_max - global_normalized_min) - 1
    )
}

# Apply the scaling function to each dataset
lst_2000_sf <- scale_to_minus_one_to_one(lst_2000_sf)
lst_2010_sf <- scale_to_minus_one_to_one(lst_2010_sf)
lst_2020_sf <- scale_to_minus_one_to_one(lst_2020_sf)

# Heat Exposure Score
# Map for 2000
map_2000 <- ggplot(data = lst_2000_sf) +
  geom_sf(aes(fill = Scaled_Heat_Score), color = NA) +
  geom_sf(data = chi_bnd, color = "blue", 
          linewidth = 0.5, fill = NA) + 
  scale_fill_viridis_c(name = "Heat Exposure Score", option = "magma", na.value = "white", limits = c(-1,1),direction = -1 ) +
  labs(title = "2000") +
  theme_minimal()

# Map for 2010
map_2010 <- ggplot(data = lst_2010_sf) +
  geom_sf(aes(fill = Scaled_Heat_Score), color = NA) +
  geom_sf(data = chi_bnd, color = "blue", 
          linewidth = 0.5, fill = NA) + 
  scale_fill_viridis_c(name = "Heat Exposure Score", option = "magma", na.value = "white", limits = c(-1,1),direction = -1 ) +
  labs(title = "2010") +
  theme_minimal()

# Map for 2020
map_2020 <- ggplot(data = lst_2020_sf) +
  geom_sf(aes(fill = Scaled_Heat_Score), color = NA) +
  geom_sf(data = chi_bnd, color = "blue", 
          linewidth = 0.5, fill = NA) + 
  scale_fill_viridis_c(name = "Heat Exposure Score", option = "magma", na.value = "white",limits = c(-1,1),direction = -1 ) +
  labs(title = "2020") +
  theme_minimal()

# Combine the maps into a single row
combined_map <- map_2000 + map_2010 + map_2020 +
  plot_layout(ncol = 3) & # Arrange in a row with 3 columns
  theme(legend.position = "bottom") # Adjust legend position

# Display the combined map
print(combined_map)


# Add a year column to each dataset
lst_2000_sf <- lst_2000_sf %>% mutate(year = 2000)
lst_2010_sf <- lst_2010_sf %>% mutate(year = 2010)
lst_2020_sf <- lst_2020_sf %>% mutate(year = 2020)

# Combine into one dataset
lst_combined <- bind_rows(lst_2000_sf, lst_2010_sf, lst_2020_sf)

# Calculate summary statistics for each year
lst_combined %>%
  group_by(year) %>%
  summarize(mean_LST = mean(mean_LST_Celsius, na.rm = TRUE),
            max_LST = max(mean_LST_Celsius, na.rm = TRUE),
            min_LST = min(mean_LST_Celsius, na.rm = TRUE))

# Boxplot of LST across years
ggplot(lst_combined, aes(x = factor(year), y = mean_LST_Celsius))+
  geom_boxplot() +
  labs(title = "Land Surface Temperature Trends (2000–2020)",
       x = "Year",
       y = "Mean LST (°C)") +
  theme_minimal()

ggplot(lst_combined, aes(x = as.factor(year), y = Normalized_Heat_Score)) +
  geom_boxplot() +
  labs(
    title = "Normalized Heat Exposure Scores by Year",
    x = "Year",
    y = "Normalized Heat Exposure Score (Z)"
  ) +
  theme_minimal()