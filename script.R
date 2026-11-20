#################################################
# Libraries
#################################################
library(sf)
library(ggplot2)
library(ggspatial)
library(ggrepel)
library(sf)
library(tidyverse)
library(cluster)
library(factoextra)
library(ggcorrplot)
library(leaflet)
library(skimr)
library(gridExtra)
library(grid)
library(stats)
library(htmltools)
library(patchwork)
set.seed(123)

#################################################
# Data
#################################################
data_biodigesters_size <- read_csv("dataset/biodigesters.csv")

#################################################
# Shapefile
#################################################
data_censo <- st_read("shapefile/censo/censo_2010_e_2022.shp")

# Transform Geometry
data <- st_transform(data_censo, 4674)
point <- st_point_on_surface(data)

# Extract coordinates
coord <- st_coordinates(point)

# Adding the coordinates to the dataframe
data$longitude <- coord[, "X"]
data$latitude  <- coord[, "Y"]

#################################################
# Cluster
#################################################
data_centroid <- st_centroid(data_censo)
data_centroid <- st_transform(data_centroid, 31984)
data_xy <- st_coordinates(data_centroid)[, c("X", "Y")]

#################################################
# Apply Kmeans
#################################################
# Choose the best option
limit_centroid <- 5000
total_cluster <-3

# k-means
groups_cluster <- kmeans(
  data_xy,
  centers = total_cluster,
  nstart = 100
)

p1 <- fviz_cluster(
  groups_cluster,
  data = data_xy,
  main = NULL,
  labelsize = 10,
  geom = "point",
  shape = 13,
  pointsize = 3,
  ellipse = TRUE,
  ggtheme = theme_bw()
) +
  geom_point(size = 1) +
  labs(
    title = "(a)",
    x = "Standardized X coordinate",
    y = "Standardized Y coordinate"
  ) +
  theme_bw() +
  theme(legend.position = "none")

groups_cluster$centers

# Data Clusters
# groups_cluster$centers
# groups_cluster$size
# groups_cluster$cluster

data <- data %>%
  mutate(
    cluster = groups_cluster$cluster    
  )

p2 <- ggplot(data) +
  geom_sf(
    aes(fill = factor(cluster)),
    color = "black",
    linewidth = 0.2
  ) +
  labs(
    fill = "Clusters:"
  ) +
  labs(
    title = "(b)",
    x = "Longitude",
    y = "Latitude"
  ) +
  scale_x_continuous(
    breaks = seq(
      floor(min(sf::st_bbox(data)["xmin"]) / 0.20) * 0.20,
      ceiling(max(sf::st_bbox(data)["xmax"]) / 0.20) * 0.20,
      by = 0.10
    )
  ) +
  theme_bw() +
  annotation_scale() +
  annotation_north_arrow (location = "br", style = north_arrow_nautical()) + 
  theme(legend.position = "right")


(p1 + p2) + 
  plot_layout(ncol = 2, widths = c(1, 1)) +
  plot_annotation(
    title = paste("Territorial Division with", total_cluster, "Clusters")
  )

# Total waste per person
waste_person <- 1.05

# Choice of the biodigester by cluster
data_choice_biodigester <-  data %>%
  st_drop_geometry() %>%
  group_by(cluster) %>%
  summarise(
    total = n(),
    poppulation = sum(C001, na.rm = TRUE)
  )

data_choice_biodigester <- data_choice_biodigester %>%
  mutate(
    waste_ton = (poppulation*waste_person)/1000
  )

data_choice_biodigester <- data_choice_biodigester %>%
  mutate(
    station_type = map_chr(waste_ton, function(x) {
      tipo <- data_biodigesters_size %>%
        filter(x >= value_min, x <= value_max) %>%
        pull(station_type)
      
      if (length(tipo) == 1) tipo else NA_character_
    })
  )

#################################################
# Map
#################################################
data_map <- data.frame(groups_cluster$centers)
data_map <- data_map %>%
  mutate (
    station_type = data_choice_biodigester$station_type
  )

data_map_sf <- st_as_sf(
  data_map,
  coords = c("X", "Y"),
  crs = 31984
)

# Convert to longitude and latitude
data_map_sf <- st_transform(data_map_sf, 4326)

# Extract longitude and latitude
coords <- st_coordinates(data_map_sf)
data_map$longitude <- coords[, "X"]
data_map$latitude  <- coords[, "Y"]
data_map$names <- paste0(
  "Biodigester ",
  rownames(data_map),
  " (",
  data_map$station_type,
  ") with Radius = ",
  limit_centroid,
  "m"
)

# Coverage area in meters
leaflet(data = data_map) %>%
  addTiles(group = "OpenStreetMap") %>%
  addMarkers(
    lng = ~longitude,
    lat = ~latitude,
    popup = ~names,
    group = "Biogas Base"
  ) %>%
  addLabelOnlyMarkers(
    lng = ~longitude,
    lat = ~latitude,
    label = data_map$names,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "bottom",
      textOnly = TRUE,
      style = list(
        "color" = "black",
        "background-color" = "white",
        "border" = "1px solid gray",
        "border-radius" = "5px",
        "padding" = "5px",
        "font-weight" = "bold",
        "font-size" = "10px",
        "box-shadow" = "2px 2px 4px rgba(0,0,0,0.2)"
      )
    )
  ) %>%
  addCircles(
    lng = ~longitude,
    lat = ~latitude,
    radius = ~limit_centroid,
    color = "blue",
    group = "Area of Operation (m)"
  ) %>%
  addLayersControl(
    options = layersControlOptions(collapsed = FALSE)
  )%>%
  addControl(
    html = htmltools::HTML(
      "<div style='
        background-color: white;
        padding: 10px;
        border: 1px solid gray;
        border-radius: 5px;
        font-weight: bold;
        font-size: 14px;
      '>
        Biodigester Type:<br>
        Micro: up to 20 tons<br>
        Small: 21–120 tons<br>
        Medium: 121–500 tons<br>
        Large: 501–1000 tons<br>
        Exceptional: above 1000 tons<br>"
    ),
    position = "bottomright"
  )

