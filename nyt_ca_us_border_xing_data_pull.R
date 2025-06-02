# 
library(ggplot2)
library(tidyverse)
library(janitor)
# install.packages('rnaturalearth')
library(rnaturalearth)
library(tidygeocoder)
library(sf)
library(rnaturalearthdata)

theme_set_custom <- function() {
  
  # loading google Fonts
  sysfonts::font_add_google("Libre Franklin", "franklin")
  sysfonts::font_add(
    family = "franklin-medium", 
    regular = "renv/library/macos/R-4.5/aarch64-apple-darwin20/sysfonts/fonts/Libre_Franklin/static/LibreFranklin-Medium.ttf"
  )
  showtext::showtext_auto()
  
  # applying ggplot2 theme
  ggplot2::theme_set(
    ggplot2::theme_minimal(base_family = "franklin") +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "#F9F9F9", color = NA),
        plot.background = ggplot2::element_rect(fill = "#F9F9F9", color = NA)
      )
  )
}

theme_set_custom()

# steps to recreate last plot/map displaying drops in border crossings between Canada and US
# tested it on 1 example/1 port with Only ports with at least 1,000 crossings in March 2025
# and the results match the visual to a percentage + or -
## go to below link for 2024
# https://www150.statcan.gc.ca/t1/tbl1/en/cv!recreate.action?pid=2410005301&selectedNodeIds=1D146,2D3,2D18,2D41,2D42,2D75,2D91&checkedLevels=2D1&refPeriods=20240301,20240301&dimensionLayouts=layout2,layout3,layout2,layout2&vectorDisplay=false
## go to below link for 2025
# https://www150.statcan.gc.ca/t1/tbl1/en/cv!recreate.action?pid=2410005301&selectedNodeIds=1D146,2D41,2D42&checkedLevels=2D1,2D2&refPeriods=20250301,20250301&dimensionLayouts=layout3,layout3,layout3,layout2&vectorDisplay=false

# filters applied to test on Niagara Falls - Queenston Bridge
" GEOGRAPHY
Select specific levels only -> keep blank
Niagara Area -> check only Niagara Falls - Queenston Bridge
 TRAVELLER CHARACTERISITCS
Select specific levels only -> keep blank
Canadian-resident visitors returning to Canada -> check only Canadian residents returning from the United States of America 
 TRAVELLER TYPE
Keep defaults (all checked; 'Travellers' is a the final rollup number/totl)
 REFERENCE PERIOD
March 2024

SAME EXACT STEPS FOR 2025

DOWNLOAD BOTH AS CSV's and select 'Download selected data (for database loading) "


(56331 - 103307) / 103307 # Niagara Falls - Rainbow Bridge
(90202 - 144738) / 144738 # Niagara Falls - Queenston Bridge
(0.3768 + 0.4547 ) / 2

# let's try to recreate those figures now that we have loaded the full datasets
map_data_ca_xings <- 
  read.csv('/Users/kardouskarrim/Desktop/2024_ca_us_ttl_xing_main_ports.csv') |> 
  # mutate(year = 2025) |> 
  clean_names() |> 
  bind_rows(
    read_csv('/Users/kardouskarrim/Desktop/2025_ca_us_ttl_xing_main_ports.csv') |> 
      # mutate(year = 2024) |> 
      clean_names()
  ) |> 
  as_tibble()
  
map_data_ca_xings |> 
  mutate(
    ref_year = word(ref_date, 1, sep = '\\-')
  ) |> 
  filter(
    traveller_type == 'Travellers' &
      geo %in% c('Niagara Falls - Queenston Bridge', 'Niagara Falls - Rainbow Bridge') &
      str_detect(traveller_characteristics, 'Canadian residents')
  ) |> 
  count(ref_year, geo, crossings = value) |> 
  summarise(
    perc_change_niagara_busiest_2ports = (
      sum(ifelse(ref_year == 2025, crossings, 0)) - sum(ifelse(ref_year == 2024, crossings, 0))
    ) / 
      sum(ifelse(ref_year == 2024, crossings, 0))
  ) # 41% decline - article says 42%; close enough; some values, especially for 2025 may have been preliminary and may have been slightly adjusted 

# let's create a function now that loops thru all major ports name and generate a port name by % decline (2 column tibble)
port_name_list <- map_data_ca_xings |> pull(geo) |> unique()
perc_change_by_port <- function(x, port_name){
  
  map_data_ca_xings |> 
    mutate(
      ref_year = word(ref_date, 1, sep = '\\-')
    ) |> 
    filter(
      traveller_type == 'Travellers' & 
        str_detect(traveller_characteristics, 'Canadian residents') &
        geo %in% port_name
    ) |> 
    count(ref_year, geo, crossings = value) |> 
    filter(crossings >= 1000) |> 
    summarise(
      perc_change = (
        sum(ifelse(ref_year == 2025, crossings, 0)) - sum(ifelse(ref_year == 2024, crossings, 0))
      ) / 
        sum(ifelse(ref_year == 2024, crossings, 0)),
      .by = geo
    ) |> 
    filter(geo != 'Canada' & !is.na(perc_change) & between(abs(perc_change), 0, 1)) |> 
    mutate_if(is.numeric, function(x) round(x, 2))
}
port_names_with_perc_changes <- map_dfr(.x = port_name_list, ~ perc_change_by_port(port_name = .x)) 
# Canada as a whole country, has seen 24% fewer passengers in March 2025 vs. March 2024
# adding lat/longs to then be able to add as a alayer on the map
port_coords <- tribble(
  ~geo, ~latitude, ~longitude,
  "Niagara Falls - Queenston Bridge",        43.1633, -79.0507,
  "Niagara Falls - Rainbow Bridge",          43.0896, -79.0703,
  "Niagara Falls - Whirlpool Bridge",        43.1051, -79.0645,
  "St. Stephen - 3rd Bridge",                45.1856, -67.2792,
  "St. Stephen - ferry and other locations", 45.2025, -67.2783,
  "St-Armand/Philipsburg",                   45.0412, -73.0486,
  "Stanstead: Route 55",                     45.0036, -72.0981,
  "Stanstead: Route 143",                    45.0066, -72.1047,
  "Lacolle: Route 221",                      45.0878, -73.3717,
  "Lacolle: Route 223",                      45.0861, -73.3597,
  "St-Bernard-de-Lacolle: Highway 15",       45.0051, -73.3723,
  "Douglas",                                 49.0033, -122.7578,
  "Pacific Highway",                         49.0038, -122.7382,
  "Abbotsford/Huntingdon",                   49.0027, -122.2555
)
port_names_with_perc_changes |> 
  left_join(
    port_coords
  ) |> 
  summarise(
    na_prop = 1 - mean(is.na(latitude))
    ) # currently only a 10% match rate from online searches, so let's explore tidygeoder package to get more granular
# and see if we can extend the matching to at least 60-70% then we can at least go from there

yet_to_match <- port_names_with_perc_changes |> 
  left_join(
    port_coords
  ) |> 
  filter(is.na(latitude)) |> 
  pull(geo) |> 
  str_c(' Canada US border')

geocode_arcgis <- map_dfr(.x = yet_to_match, ~geo(address = .x, method = 'arcgis'))

# this massively improved the matches- but let's see if those coordinates realisticall can be considered 
# border regions between CA and US 

# validating whether location pertains to the 49th horizontal or at least not further than 200km away
validate_border_crossings <- function(geocoded_df, max_km_from_49th = 200) {
  
  # Canada rough bounds
  canada_lat <- c(41.5, 84.0)
  canada_lon <- c(-141.0, -52.0)
  
  # simple distance to 49th parallel (main border)
  distance_to_49th <- function(lat) abs(lat - 49.0) * 111  # ~111 km per degree latitude
  
  geocode_arcgis |>
    filter(!is.na(lat), !is.na(long)) |>
    mutate(
      within_canada = lat >= canada_lat[1] & 
        lat <= canada_lat[2] &
        long >= canada_lon[1] & 
        long <= canada_lon[2],
      
      km_from_49th = distance_to_49th(lat),
      
      is_valid = within_canada & km_from_49th <= max_km_from_49th,
      
      status = case_when(
        !within_canada ~ "outside canada bounds",
        km_from_49th > max_km_from_49th ~ paste0("too far out (", round(km_from_49th), "km)"),
        TRUE ~ "valid"
      )
    )
}

# apply to your full dataset
validated_coords <- validate_border_crossings(geocode_arcgis, max_km_from_49th = 300)
# using above threshold to sorta account for US 'curvature' into Canada, especially on the North-East region, 
# as the border is not jsut one latitude/'horizontal' line 
valid_coords <- 
  validated_coords |> 
  filter(is_valid) |>
  select(geo = address, lat, long) # this has moved from 10 matches up to 96 out of 140 ~ 70% match rate
# looking at no. of spikes in the article, 96 spikes should hopefully cover most if not all location of spikes in
# the article, which from visual insepction, looks like around 50 are draw/plotted on the Canada/US map/border

# combine all tibbles into one with perc_changes
spike_prep <- port_names_with_perc_changes |> 
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
  select(geo:longitude) 

# add a geometry column to then turn into an sf() object
spike_prep_sf_azimuthal <- 
  spike_prep |>
  filter(
    latitude <= 49.8,
    abs(longitude) >= 65
  ) |>  
  st_as_sf(
    coords = c("longitude", "latitude"), 
    crs = 4326, agr = "constant", 
    remove = FALSE
  ) |> 
  # Transform to azimuthal equidistant projection
  st_transform(crs = "+proj=aeqd +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0") |>
  mutate(
    color_scale = if_else(perc_change <= 0, '#d65f00', '#2b9d6c')
  )

## starting the work on the map 
# get country polygons for US and Canada
countries <- ne_countries(
  scale = "medium",
  returnclass = "sf"
) |>
  filter(name %in% c("United States of America", "Canada"))

# get lakes data
lakes <- ne_download(
  scale = "medium", 
  type = "lakes", 
  category = "physical",
  returnclass = "sf"
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

# display the enhanced plot
main_plot
