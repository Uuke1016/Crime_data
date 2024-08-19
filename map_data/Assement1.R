# Assement 1

# Set working directory
setwd("/Users/duke/desktop/map_data")

# Load packages
library(ggspatial)
library(sf)
library(readr)
library(readxl)
library(tidyverse)

# Read the data
lancashire_asb <- read_tsv("lancashire_asb.tab") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(crs = "EPSG:27700")

lancashire_districts <- st_read("lancashire_districts.geojson") |>
  st_transform(crs = "EPSG:27700")

lancashire_wards <- st_read("lancashire_wards.gpkg") |>
  st_transform(crs = "EPSG:27700")

lancashire_ward_pop <- read_excel("lancashire_ward_pop.xlsx")

# Create a pdf to save map
pdf("Three_maps.pdf", width = 16, height = 6)

########## First map: Density map ##########

# Get KDE for anti-social behaviour
asb_density <- lancashire_asb |>
  hotspot_kde(bandwidth_adjust = 0.2, quiet = TRUE) |>
  st_transform(crs = "EPSG:27700")

# Clip the density layer to Lancashire boundaries
asb_density_clip <- st_intersection(asb_density, lancashire_districts)


# Plot the map1
ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0, progress = "none") +
  geom_sf(aes(fill = kde), data = asb_density_clip, alpha = 0.75, colour = NA) +
  geom_sf(data = lancashire_districts, colour = "seagreen3", fill = NA) +
  geom_sf_label(
    aes(label = district),
    data = lancashire_districts,
    alpha = 0.5,
    colour = "seagreen",
    lineheight = 1,
    size = 5,
    label.size = NA
  ) +
  scale_fill_distiller(direction = 1) +
  theme_void() +
  labs(
    title = "Concentration of Anti-Social Behaviour in Lancashire (2022)",
    subtitle = "KDE showing areas of high incident concentration",
    caption = "Data Source: Lancashire Anti-Social Behaviour Data"
  ) +
  theme(legend.position = "right")

########## Second map: anti-social behaviour incidents ##########

# Get the counts of anti-social behaviour incidents per ward
asb_counts <- lancashire_asb |>
  st_join(lancashire_wards) |>
  st_drop_geometry() |>
  count(ward_name, name = "count")

# Join offence counts back to ward boundaries
ward_counts <- lancashire_wards |>
  left_join(asb_counts, by = "ward_name") |>
  replace_na(list(count = 0))

# Find the ward with the highest number of incidents
highest_count <- ward_counts |>
  arrange(desc(count)) |>
  slice(1)

# Plot the map showing the incidence of anti-social behaviour by ward
ggplot(ward_counts) +
  annotation_map_tile(type = "cartolight", zoomin = 0, progress = "none") +
  geom_sf(aes(fill = count), alpha = 0.75) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs(
    title = "Anti-Social Behaviour Incidents by Ward in Lancashire (2022)",
    fill = "Number of Incidents"
  ) +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  geom_sf_label(
    aes(label = ward_name),
    data = highest_count,
    alpha = 0.5,
    colour = "red",
    lineheight = 1,
    size = 5,
    label.size = NA,
    nudge_y = 3000,
    nudge_x = 3000
  )

########## Third map: Hotspots maps ##########

# Calculate the number of incidents per ward within each district
ward_counts <- lancashire_asb %>%
  st_join(lancashire_wards) %>%
  st_join(lancashire_districts) %>%
  st_drop_geometry() %>%
  group_by(district, ward_name) %>%
  summarise(incident_count = n()) %>%
  arrange(district, desc(incident_count))

# Identify the ward with the highest number of incidents in each district
top_wards_per_district <- ward_counts %>%
  group_by(district) %>%
  slice_max(order_by = incident_count, n = 1)

# Filter the wards data to include only the top wards in each district
hotspot_wards <- lancashire_wards %>%
  inner_join(top_wards_per_district, by = "ward_name")

# Plot the wards identified as hotspots on the map
ggplot() +
  annotation_map_tile(type = "osmtransport", zoomin = 0, progress = "none") +
  geom_sf(data = hotspot_wards, fill = "red", alpha = 0.75) +
  scale_fill_distiller(palette = "Reds", direction = 1, name = "Incidents") +
  labs(
    title = "Hotspots of Anti-Social Behaviour by Ward in Lancashire (2022)",
    subtitle = "Ward with the highest number of incidents in each district",
    caption = "Data Source: Lancashire Anti-Social Behaviour Data"
  ) +
  theme_void()

# Stop saving plots
dev.off()
