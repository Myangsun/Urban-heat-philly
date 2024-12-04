library(dplyr)
library(sf)
library(ggplot2)
library(tigris)
library(patchwork)
library(viridis)
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
