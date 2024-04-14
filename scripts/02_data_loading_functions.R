####################
# Additional functions
####################

#' Loads all the shapefiles inside a folder
#'
#' @param shape_file_directory - directory with shapefiles inside it
#'
#' @return
load_folders_shape_files <- function(shape_file_directory) {
  shape_files_from_directory <- Sys.glob(file.path(shape_file_directory, "*.shp"))
  areas_labels <- gsub(".shp", "", basename(shape_files_from_directory))

  area_shape_files <- dplyr::bind_rows(
    lapply(shape_files_from_directory, read_sf)
  ) |>
    dplyr::mutate(
      name_based_on_file = areas_labels
    )

  return(area_shape_files)
}

#' Patches entries from ref_land_areas and 2 together to cover all cases
#'
#' @param ref_land_areas - dataset with base land-areas expectedfe
#' @param filler_dataset - dataset to use for filling missing land-areas
#'
#' @return
merge_two_shape_datasets <- function(ref_land_areas, filler_dataset) {

  new_data_names <- ref_land_areas$Name |> unique()

  missing_land_area_rows <- filler_dataset |>
    dplyr::filter(!(Name %in% new_data_names))

  ref_land_areas_with_missing <- dplyr::bind_rows(
      ref_land_areas,
      missing_land_area_rows
  ) |>
      dplyr::arrange(Name)

  return(ref_land_areas_with_missing)
}
