# ---------------------------------------------------------
# Load packages
# ---------------------------------------------------------
if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}
librarian::shelf(readr, here, dplyr, tidyr, ggplot2, sf)

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------
# Load shapefiles
ca_poly <- st_read(here("data", "ca_boundary", "StateBoundary.shp")) %>%  # https://gis-california.opendata.arcgis.com/datasets/CDEGIS::california-state-boundary/about
  st_make_valid()
nps_poly <- st_read(here("data", "nps_boundary", "yose_kica_sequ_boundaries.shp"))  # pCloudDrive/gis/coverages/nps_boundary
ca_ecoregions_poly <- st_read(here("data", "ca_eco_l3", "ca_eco_l3.shp")) # https://www.epa.gov/eco-research/ecoregion-download-files-state-region-9

# Create layer of known hypl localities
hypl_points <- read_csv(here("data", "ArctosData_hydromantes_23oct2025.csv")) %>% 
  select(DEC_LAT, DEC_LONG) %>% 
  rename(lat = DEC_LAT,
         lon = DEC_LONG) %>% 
  drop_na(lat, lon) %>% 
  distinct(lat, lon)

hypl_points_sf <- st_as_sf(hypl_points, coords = c("lon", "lat"), crs = 4326)

# Create layer of study site locations
sites_df <- read_csv(here("data", "sites.csv"))

sites_sf <- st_as_sf(sites_df, coords = c("lon", "lat"), crs = 4326)

# ---------------------------------------------------------
# Re-project layers to California Albers
# ---------------------------------------------------------

target_crs <- st_crs(3310) 

# Transform aligned layers
ca_poly_trans <- st_transform(ca_poly, target_crs)
ca_ecoregions_poly_trans <- st_transform(ca_ecoregions_poly, target_crs)
nps_poly_trans <- st_transform(nps_poly, target_crs)
hypl_sf_trans <- st_transform(hypl_points_sf, target_crs)
sites_sf_trans <- st_transform(sites_sf, target_crs)

# Create Sierra Nevada polygon
sierra_poly_trans <- ca_ecoregions_poly_trans %>% 
  filter(US_L3CODE == 5)

# Create range polygon
hypl_poly_trans <- st_concave_hull(st_union(hypl_sf_trans), ratio = 0.5)

# ---------------------------------------------------------
# 3. Create map
# ---------------------------------------------------------

ggplot() +
  geom_sf(data = ca_poly, fill = "gray90", color = "gray50", linewidth = 0.8) +
  geom_sf(data = sierra_poly_trans, fill = "palegreen") + 
  geom_sf(data = nps_poly_trans, fill = "deepskyblue2", color = NA) +
  geom_sf(data = hypl_sf_trans, fill = "white", color = "cornsilk4", stroke = 0.5, shape = 21) +
  geom_sf(data = sites_sf_trans, color = "black", size = 4, shape = 17) + 
  geom_sf_text(data = sites_sf_trans, aes(label = site_name), 
               hjust = 0,    # anchor left side of text label to site point 
               nudge_x = 60000,   # Nudge text right (units in meters for EPSG:3310)
               color = "black", 
               fontface = "bold", 
               size = 4) +
  coord_sf(datum = NA) +
  theme_void()
 # theme(panel.grid.major = element_line(color = "gray90", linetype = "dashed"))

ggsave(here("out", "hypl_map_sites.png"), bg = "white")
