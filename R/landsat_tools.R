library(here)
# Deprecated; they don't have what we want :(
# devtools::install_github("16EAGLE/getSpatialData")
# library(getSpatialData)
library(httr)
library(lubridate)
library(raster)
library(rgdal)
library(sf)
library(sp)
library(tidyverse)

# A function to identify the path and row of the relevant Landsat tile for each location
# test <- coordinate_to_path_row(coordinates_df = coordinates_df)
# TODO: Can we make this any faster? It is so slow!
coordinate_to_path_row <- function(coordinates_df){

  # Load in shapefile of Landsat tiles
  wrs <- st_read(here("spatial_data/WRS2_descending_0/WRS2_descending.shp"))

  # Intersect coordinates_df
  paths_rows <- coordinates_df %>%
    st_as_sf(coords = c("lon","lat"), crs = 4326) %>%
    st_intersection(wrs) %>%
    rename(path = PATH, row = ROW) %>%
    dplyr::select(Study, Site_name, path, row)

  return(paths_rows)
}
