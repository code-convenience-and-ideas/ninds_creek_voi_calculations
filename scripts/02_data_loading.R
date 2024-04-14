source(here::here("scripts", "02_data_loading_functions.R"))

# --- Load data from disk
load_disk_data <- function() {
  disk_data <- list()

  # Load in the shape file
  rawdata_dir <- here::here("data", "rawData")
  conservation_area_borders_directory <- file.path(rawdata_dir, "CEAs_V3")

  # Order load differences based on images
  hecras_cea_regions_path <- file.path(conservation_area_borders_directory, "Fig 2c")
  dem_cea_regions_path <- file.path(conservation_area_borders_directory, "Fig 2b")
  bay_flood_cea_regions_path <- file.path(conservation_area_borders_directory, "Fig 2a")

  # Bring in datasets and fill in gaps to have complete CEA set for each scenario
  full_hecras_data <- load_folders_shape_files(hecras_cea_regions_path)

  full_dem_data <- load_folders_shape_files(dem_cea_regions_path) |>
    merge_two_shape_datasets(full_hecras_data)

  bay_dem_data <- load_folders_shape_files(bay_flood_cea_regions_path) |>
    merge_two_shape_datasets(full_hecras_data)

  disk_data[["hecras_area_files"]] <- full_hecras_data
  disk_data[["tide_dem_area_files"]] <- full_dem_data
  disk_data[["bay_dem_area_files"]] <- bay_dem_data


  # Load in the tiff file
  dem_tiff_file_path <- file.path(
    rawdata_dir,
    "DEMRasterClippedToProjectNindCreek.tif"
  )
  dem_tiff_file_data <- tiff::readTIFF(dem_tiff_file_path, info = TRUE)

  disk_data[["tiff_data"]] <- dem_tiff_file_data


  return(disk_data)
}

disk_data <- load_disk_data()
