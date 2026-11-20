##############################################################################
# Libraries
##############################################################################
library(sf)
library(ggplot2)
library(ggspatial)
library(tidyverse)
library(cluster)
library(spdep)
library(factoextra)
library(ggcorrplot)
library(leaflet)
library(skimr)
library(gridExtra)
library(grid)
library(stats)
library(htmltools)
set.seed(123)

##############################################################################
# Dataset
##############################################################################
census_data <- read_csv("dataset/data.csv")

##############################################################################
# Shapefile
##############################################################################
output_areas <- st_read("shapefile/censo/censo_2010_e_2022.shp")

# Coordinate system in meters
output_areas <- st_transform(output_areas, 31984)

# Calculate centroids
centroides <- st_centroid(st_geometry(output_areas))
coords <- st_coordinates(centroides)

# Coordinates by neighborhood
nb <- poly2nb(output_areas, queen = FALSE)

# Create neighbor pairs, avoiding duplicate edges.
pares <- do.call(rbind, lapply(seq_along(nb), function(i) {
  vizinhos <- nb[[i]]
  vizinhos <- vizinhos[vizinhos > i]
  
  if (length(vizinhos) > 0) {
    data.frame(origem = i, destino = vizinhos)
  }
}))

# Convert pairs into sf segments
edges <- st_sfc(
  lapply(seq_len(nrow(pares)), function(i) {
    st_linestring(
      rbind(
        coords[pares$origem[i], 1:2],
        coords[pares$destino[i], 1:2]
      )
    )
  }),
  crs = st_crs(output_areas)
)

edges <- st_sf(geometry = edges)

##############################################################################
# Join the census data to the shapefile
##############################################################################
oa_census <- output_areas %>%
  left_join(
    census_data,
    by = "NOME_BAIRR"
  )

##############################################################################
# Hot Spot Analysis - Getis-Ord Gi*
##############################################################################
# Retain only neighborhoods with available waste generation data
oa_hot <- oa_census %>%
  filter(!is.na(C001.x))

# Create the spatial neighborhood
# queen = FALSE: Considers neighbors that share borders
neighbours <- poly2nb(oa_hot, queen = FALSE)


# Include your own neighborhood in the Gi* calculation
neighbours_gistar <- include.self(neighbours)

# Create spatial weight matrix
listw_gistar <- nb2listw(
  neighbours_gistar,
  style = "B",
  zero.policy = TRUE
)

# Calculate the Getis-Ord Gi* statistic
gi_star <- as.numeric(
  localG(
    oa_hot$C001.x,
    listw_gistar,
    zero.policy = TRUE
  )
)

# Critical limits for the classification of results:
z_90 <- qnorm(0.95)
z_95 <- qnorm(0.975)
z_99 <- qnorm(0.995)

gi_star_values <- data.frame(
  z_score = sort(gi_star)
)

vx <- 0.15
vy <- 0.39
vsize <- 3

# Sort the results according to localG Z-values
oa_hot <- oa_hot %>%
  mutate(
    gi_star = gi_star,
    gi_star_class = case_when(
      gi_star >=  z_99 ~ "Hot spot 99%",
      gi_star >=  z_95 ~ "Hot spot 95%",
      gi_star >=  z_90 ~ "Hot spot 90%",
      gi_star <= -z_99 ~ "Cold spot 99%",
      gi_star <= -z_95 ~ "Cold spot 95%",
      gi_star <= -z_90 ~ "Cold spot 90%",
      TRUE             ~ "Not significant"
    )
  )

##############################################################################
# Plot the hot spot map
##############################################################################
ggplot(oa_hot) +
  geom_sf(
    aes(fill = gi_star_class),
    color = "white",
    linewidth = 0.15
  ) +
  scale_fill_manual(breaks = c(
      "Hot spot 99%",
      "Hot spot 95%",
      "Hot spot 90%",
      "Not significant",
      "Cold spot 90%",
      "Cold spot 95%",
      "Cold spot 99%"
    ),
    name = "Population Hotspots:",
    values = c(
      "Hot spot 99%"       = "#B2182B",
      "Hot spot 95%"       = "#EF8A62",
      "Hot spot 90%"       = "#FDDBC7",
      "Cold spot 99%"      = "#2166AC",
      "Cold spot 95%"      = "#67A9CF",
      "Cold spot 90%"      = "#D1E5F0",
      "Not significant"    = "#D9D9D9"
    ),
    drop = FALSE
  ) +
  labs(
    title = "Population Hotspots by Neighborhood",
    subtitle = "Getis-Ord Gi* Statistics"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  )


