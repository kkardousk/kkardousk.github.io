library(tidyverse)
library(sf)

# crop to focus area - remove alaska
map_bounds <- list(
  xmin = -125, xmax = -65,
  ymin = 41, ymax = 52
)

# filter out Alaska from US (longitude > -140 is roughly Alaska)
continental_us_canada <- 
  countries |>
  mutate(
    # create geometries without Alaska
    geometry = case_when(
      name == "United States of America" ~ st_crop(
        geometry, xmin = -130, ymin = 20, xmax = -60, ymax = 50
      ),
      TRUE ~ geometry
    )
  ) |>
  # remove any empty geometries after cropping
  filter(!st_is_empty(geometry))

# combine all tibbles into one with perc_changes
spike_prep_sf <- 
  port_names_with_perc_changes |> 
  left_join(
    port_coords, 
    join_by(geo == geo)
  ) |> 
  left_join(
    valid_coords |> 
      mutate(
        geo = str_remove(geo, ' Canada US border')
      ),
    join_by(geo == geo)
  ) |> 
  mutate(
    latitude = coalesce(latitude, lat),
    longitude = coalesce(longitude, long)
  ) |>
  filter(!is.na(latitude)) |> 
  select(geo:longitude) |> 
  st_as_sf(
    coords = c("longitude", "latitude"), 
    crs = 4326, agr = "constant", 
    remove = FALSE
  ) 

# generating regions for easier filtering of spikes
crossing_data <- 
  spike_prep_sf |> 
  filter(
    abs(perc_change) <= .50 & longitude >= -122.76 & #exclude far west/Vancouver area and start at Douglas
      !geo %in% c('Kelowna', 'Calgary Area', 'Calgary', 'Regina', '') 
  ) |> 
  mutate(
    perc_change = if_else(between(perc_change, -.39, -.23), perc_change / 10, perc_change)
  ) |> 
  # filter to match article criteria - only significant declines
  filter(
    abs(perc_change) <= .45
  ) %>%
  # add regional grouping for better selection
  mutate(
    region = case_when(
      longitude < -115 ~ "west_coast",
      longitude >= -115 & longitude < -100 ~ "prairie", 
      longitude >= -100 & longitude < -85 ~ "central",
      longitude >= -85 ~ "east",
      TRUE ~ "other"
    ),
    normalized_heights = abs(perc_change) / max(abs(perc_change))
  )

# select representative spikes by region to match article count (~40-50 total)
selected_spikes <- 
  crossing_data |> 
  filter(latitude <= 49.5 & longitude < -65) |> 
  group_by(region) |> 
  arrange(perc_change) |> # most negative first
  mutate(
    no = as.integer(case_when(
      region == "west_coast" ~ 12,
      region == "prairie" ~ 8, 
      region == "central" ~ 6,
      region == "east" ~ 15,
      TRUE ~ 3)
    )
  ) |> 
  ungroup() |> 
  slice(.by = region, 1:no) |> 
  select(-no) |> 
  bind_rows(
    # pull select positive changes 
    crossing_data |> 
      filter(perc_change > 0) |> 
      filter(longitude < -65) 
  )

# transform points to projected coordinate system; original is more elliptic in shape rather than a 'straight' plane
selected_spikes_projected <- 
  selected_spikes |>
  st_transform(crs = "+proj=aeqd +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0")

# extract projected coordinates
selected_spikes_coords <- 
  selected_spikes_projected |>
  mutate(
    proj_x = st_coordinates(geometry)[, 1],
    proj_y = st_coordinates(geometry)[, 2]
  ) |>
  st_drop_geometry()

# Create triangles using projected coordinates
create_projected_triangles <- function(data, height_scalar = 1) {
  
  height_mult <<- height_scalar
  # function to create a triangle for each point using projected coordinates
  create_triangle <- function(x, y, perc_change, normalized_heights, height_mult) {
    
    # base width and height in projected units (meters)
    base_width <- 8e4  # 80km base width
    height <- abs(normalized_heights) * height_scalar * 4e5  # scale height based on normalized perc change
    
    # triangle vertices - purely vertical orientation
    if (perc_change < 0) {
      # downward pointing triangle only for a couple of nstances
      vertices <- data.frame(
        x = c(x - base_width, x + base_width, x, x - base_width),
        y = c(y, y, y - height, y)
      )
    } else {
      # upward pointing triangle; vast majority of cases
      vertices <- data.frame(
        x = c(x - base_width, x + base_width, x, x - base_width),
        y = c(y, y, y + height, y)
      )
    }
    
    # create polygon geometry
    st_polygon(list(as.matrix(vertices)))
  }
  
  # create triangles for each point
  triangles <- 
    data |>
    rowwise() |>
    mutate(
      triangle_geom = list(create_triangle(proj_x, proj_y, perc_change, normalized_heights))
    ) |>
    ungroup()
  
  # convert to sf object with triangle geometries
  triangle_sf <- 
    triangles |>
    mutate(
      geometry = st_sfc(triangle_geom, crs = "+proj=aeqd +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0")
    ) |>
    st_sf()
  
  return(triangle_sf)
}

# create triangle geometries using projected coordinates
triangle_sf <- create_projected_triangles(selected_spikes_coords, height_scalar = 2.5)

# Transform countries first
countries_proj <- countries |>
  st_transform(crs = "+proj=aeqd +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0")

lakes_proj <- lakes |>
  st_transform(crs = "+proj=aeqd +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0")

# Get the bounding box and expand it significantly for better fade effect
bbox <- st_bbox(countries_proj)
x_range <- bbox[3] - bbox[1]
y_range <- bbox[4] - bbox[2]

# Expand bbox by 50% to create more fade area
bbox_expanded <- c(
  xmin = bbox[1] - 0.5 * x_range,
  ymin = bbox[2] - 0.5 * y_range,
  xmax = bbox[3] + 0.5 * x_range,
  ymax = bbox[4] + 0.5 * y_range
)

# Create a much finer grid for smoother gradient
grid_resolution <- 400  # Increased for even smoother gradient
x_seq <- seq(bbox_expanded[1], bbox_expanded[3], length.out = grid_resolution)
y_seq <- seq(bbox_expanded[2], bbox_expanded[4], length.out = grid_resolution)

# Create vertical gradient for within-country areas
gradient_data <- expand.grid(x = x_seq, y = y_seq) |>
  mutate(
    # Convert to sf points to check intersection with countries
    geometry = st_sfc(map2(x, y, ~st_point(c(.x, .y))), crs = st_crs(countries_proj))
  ) |>
  st_sf() |>
  # Check which points are within countries
  mutate(
    within_countries = lengths(st_intersects(geometry, countries_proj)) > 0
  ) |>
  st_drop_geometry() |>
  mutate(
    # Calculate vertical position for gradient (center = 0)
    y_center = 0,  # Center of our projection
    y_range_total = bbox_expanded[4] - bbox_expanded[2],
    
    # Normalize y position from center (-1 to 1, where 0 is center)
    y_normalized = (y - y_center) / (y_range_total * 0.5),
    
    # Create vertical gradient: darker at center, lighter toward top/bottom
    # Distance from horizontal center line
    dist_from_horizontal_center = abs(y_normalized),
    
    # Apply smooth transition function
    vertical_fade = pmin(dist_from_horizontal_center^0.7, 1),
    
    # Create color values: #e0e0e0 at center fading to white at edges
    # Convert #e0e0e0 (224,224,224) to white (255,255,255)
    red_value = 224 + (255 - 224) * vertical_fade,
    green_value = 224 + (255 - 224) * vertical_fade,
    blue_value = 224 + (255 - 224) * vertical_fade,
    
    # Create hex color
    color_hex = rgb(red_value, green_value, blue_value, maxColorValue = 255),
    
    # Only apply gradient within countries, white elsewhere
    final_color = ifelse(within_countries, color_hex, "white"),
    
    # Alpha - fully opaque within countries, transparent outside
    alpha_value = if_else(within_countries, 1.0, 0.0)
  )

# Create a mask for areas within the bounding box
bbox_polygon <- st_polygon(list(rbind(
  c(bbox_expanded[1], bbox_expanded[2]),
  c(bbox_expanded[3], bbox_expanded[2]),
  c(bbox_expanded[3], bbox_expanded[4]),
  c(bbox_expanded[1], bbox_expanded[4]),
  c(bbox_expanded[1], bbox_expanded[2])
))) |>
  st_sfc(crs = st_crs(countries_proj))

# Create the main plot with vertical gradient within countries
main_plot <- ggplot() +
  # Add white background
  geom_raster(data = gradient_data,
              aes(x = x, y = y),
              fill = "white") +
  
  # Add the vertical gradient within countries only
  geom_raster(data = filter(gradient_data, within_countries),
              aes(x = x, y = y, fill = final_color, alpha = alpha_value)) +
  
  # Use identity scales for the custom colors
  scale_fill_identity() +
  scale_alpha_identity() +
  
  # Add country boundaries
  geom_sf(data = countries_proj,
          fill = NA, color = "white", linewidth = 0.3) +
  
  # Great lakes
  geom_sf(data = lakes_proj,
          fill = "white", color = "white", linewidth = 0.1) +
  # Triangle spikes
  geom_sf(
    data = triangle_sf,
    aes(color = ifelse(perc_change < 0, "negative", "positive")),
    fill = '#d65f00', size = 0.8, alpha = 0.5
  ) +
  # Triangle color scale
  scale_color_manual(
    values = c("negative" = "#d65f00", "positive" = "#2b9d6c"),
    guide = "none"
  ) +
  # Coordinate system
  coord_sf(
    xlim = c(-4e6, 4e6),
    ylim = c(-1.75e6, 2.1e6),
    expand = FALSE,
    clip = 'off'
  ) + 
  # Clean theme
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = 'none'
  )

# Display the enhanced plot
print(main_plot)