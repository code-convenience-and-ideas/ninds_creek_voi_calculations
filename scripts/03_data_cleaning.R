source(here::here("scripts", "03_data_cleaning_functions.R"))

########################################################################################
# Any initial additional columns and cleaning
########################################################################################

# Set up the shape files
area_shape_files <- disk_data[["bay_dem_area_files"]]

site_location_coordinate_system <- st_crs(area_shape_files)

# Note that the tiff file had encode extreme negative values to show a black background against the DEM
# Therefore, use  unrealistically low value to flag these and replace for filtering in other scripts
# Using value instead of a NaN as raster doesn't handle the NaNs nicely
dem_tiff_file_data <- disk_data[["tiff_data"]]

dem_tiff_file_data[dem_tiff_file_data < (-100)] <- NA_real_


raster_data_of_site <- list()
raster_data_of_site$z <- dem_tiff_file_data %>% t()
n_x_pixels <- attr(dem_tiff_file_data, "width")
n_y_pixels <- attr(dem_tiff_file_data, "length")

raster_data_of_site$x <- seq(
  from = 146.052,
  to = 146.0657,
  length.out = n_x_pixels
)

raster_data_of_site$y <- seq(
  from = -17.56606,
  to = -17.55708,
  length.out = n_y_pixels
)

raster_image <- raster(raster_data_of_site) |> flip(direction = "y")

crs(raster_image) <- site_location_coordinate_system
