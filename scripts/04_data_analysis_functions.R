# Initial parameter calculations ------------


#' calculate_mean_tidal_height function
#'
#' This function calculates the mean tidal height using the high tide level and tidal range.
#'
#' @param high_tide_level numeric value representing the high tide level
#' @param tidal_range numeric value representing the tidal range
#' @return numeric value representing the mean tidal height
#'
#' @examples
#' calculate_mean_tidal_height(high_tide_level = 10, tidal_range = 4)
#'
calculate_mean_tidal_height <- function(high_tide_level, tidal_range) {
  # Calculate the mean tidal height using the high tide level and tidal range
  return(high_tide_level - (tidal_range / 2))
}

#' calculate_standardized_tidal_position_index function
#'
#' This function calculates the standardized tidal position index using the target height, mean tidal level and high tide level.
#'
#' @param target_height numeric value representing the target height
#' @param mean_tide_level numeric value representing the mean tidal height
#' @param high_tide_level numeric value representing the high tide level
#' @return numeric value representing the standardized tidal position index
#'
#' @examples
#' calculate_standardized_tidal_position_index(target_height = 8, mean_tide_level = 6, high_tide_level = 10)
#'
calculate_standardized_tidal_position_index <- function(target_height, mean_tide_level, high_tide_level) {
  # Calculate the standardized tidal position index using the target height, mean tidal level and high tide level
  return((target_height - mean_tide_level) / (high_tide_level - mean_tide_level))
}

#' Helper function to create a list of row and column ranges based on starting and ending points
#'
#' @param range_start The starting point of the range, a numeric vector with length of 2
#' @param range_end The ending point of the range, a numeric vector with length of 2
#' @return A list containing the row range and column range
dim_range_from_corners <- function(range_start, range_end) {
  row_range <- range_start[1]:range_end[1]
  column_range <- range_start[2]:range_end[2]

  return(list("rows" = row_range, "columns" = column_range))
}

# Initial raster + shape manipulation -------
#' Overlay boundaries from a shape file onto a raster image
#'
#' @param rasterlayer_data A rasterlayer object contain image over a geographic space
#' @param sf_boundary_data A collection of shapefiles which represent boundaries in the geographic space
#'
#' @return A ggplot objects with the raster + shape layers in a specific colour scheme and theme
raster_and_shape_plot <- function(rasterlayer_data, sf_boundary_data){

    # Get coordinate system to share between images
    boundary_data_crf <- sf::st_crs(sf_boundary_data)

    # Convert raster and shapefile to data.frame suitable for ggplot
    raster_image_df <- as.data.frame(rasterlayer_data, xy = TRUE) |>
        na.omit()

    # First add the shape-files for the ggplot2 then build raster than boundary layer and tweak presentation
    # Made some tweaks per: https://tmieno2.github.io/R-as-GIS-for-Economists/geom-raster.html
    raster_under_shape_boundaries <- sf_boundary_data |>
        ggplot2::ggplot() +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = layer),
                    data = raster_image_df) +
        ggplot2::geom_sf(ggplot2::aes(geometry = geometry),
                colour = 'black',
                alpha = 0, lwd = 1.0) +
        # Wanting a colourblind friendly scale
        ggplot2::scale_fill_viridis_c() +
        # x,y lat-longs don't mean much to a viewer
        ggplot2::theme_void() +
        ggplot2::theme(
            legend.position = "bottom"
        ) +
        # Small image so equi-spaced lat-long shouldn't be too bad
        ggplot2::coord_sf(crs = boundary_data_crf)

    return(raster_under_shape_boundaries)
}

# Functions for interacting with the BlueCAM excel document ----

#' Helper function to extract the value of a cell given a vector index
#'
#' @param worksheet The worksheet object
#' @param vector_index The index of the cell to extract, a numeric vector with length of 2
#' @return The value of the cell
extract_value_from_index <- function(worksheet, vector_index) {
  current_cell <- worksheet$Cells(vector_index[1], vector_index[2])
  return(current_cell[["Value"]])
}


#' Helper function to extract values from a range of cells in a worksheet
#'
#' @param worksheet The worksheet object
#' @param range_start The starting point of the range, a numeric vector with length of 2
#' @param range_end The ending point of the range, a numeric vector with length of 2
#' @return A data frame containing the extracted values
extract_values_from_index_range <- function(worksheet, range_start, range_end) {
  range_values <- dim_range_from_corners(range_start, range_end)

  row_range <- range_values[["rows"]]
  column_range <- range_values[["columns"]]

  starting_row <- row_range[1]

  column_vectors <- list()
  for (column_index in column_range) {
    value_list <- list()
    for (row_index in row_range) {
      # Assuming header row
      if (row_index == starting_row) {
        header_name <- extract_value_from_index(worksheet, c(row_index, column_index))
      } else {
        extract_value <- extract_value_from_index(worksheet, c(row_index, column_index))
        extract_value <- ifelse(is.null(extract_value), NA, extract_value)
        value_list[[as.character(row_index)]] <- extract_value
      }
    }
    column_vectors[[header_name]] <- unlist(value_list)
  }

  extracted_value_data_frame <- dplyr::bind_cols(column_vectors)
  return(extracted_value_data_frame)
}


#' Extract data frame values from a worksheet based on the starting cell index and dimensions
#'
#' @param worksheet The worksheet object
#' @param corner_index The index of the starting cell, a numeric vector with length of 2
#' @param ncols The number of columns to extract
#' @param nrows The number of rows to extract
#' @return A data frame containing the extracted values
extract_data_frame_values <- function(worksheet, corner_index, ncols, nrows) {
  starting_cell <- corner_index
  ending_cell <- c(corner_index[1] + nrows, corner_index[2] + ncols - 1)
  return(extract_values_from_index_range(worksheet, starting_cell, ending_cell))
}

#' This function enters a value into a specific index in an Excel worksheet and returns the original value that was in that cell.
#'
#' @param worksheet An Excel worksheet object.
#' @param vector_index A vector that specifies the row and column of the index in the worksheet where the value will be entered.
#' @param value The value to be entered into the worksheet.
#'
#' @return The original value that was in the cell before it was updated.
enter_value_into_index <- function(worksheet, vector_index, value) {
  # get the current cell object
  current_cell <- worksheet$Cells(vector_index[1], vector_index[2])
  # save the original value that was in the cell
  original_value <- current_cell[["Value"]]
  # update the cell with the new value
  current_cell[["Value"]] <- value
  # return the original value
  return(original_value)
}

#' This function enters values from a data frame into an Excel worksheet.
#'
#' @param worksheet An Excel worksheet object.
#' @param df_starting_cell A vector that specifies the starting row and column in the worksheet where the data frame values will be entered.
#' @param data_to_write A data frame containing the values to be entered into the worksheet.
#'
enter_values_from_dataframe <- function(worksheet, df_starting_cell, data_to_write) {
  # get the dimensions of the data frame
  data_dimensions <- dim(data_to_write)

  # create vectors to represent the row and column ranges for the loop
  row_range <- 0:(data_dimensions[1] - 1)
  column_range <- 0:(data_dimensions[2] - 1)

  # get the starting row and column for the loop
  starting_row <- df_starting_cell[1] + 1
  starting_col <- df_starting_cell[2]

  # loop over the columns and rows of the data frame
  for (column_index in column_range) {
    for (row_index in row_range) {
      # get the index of the current cell to be updated
      current_cell_index <- c(starting_row + row_index, starting_col + column_index)
      # enter the value into the current cell
      enter_value_into_index(worksheet, current_cell_index, data_to_write[[row_index + 1, column_index + 1]])
    }
  }
}

#' This function extracts overall results from an Excel worksheet.
#'
#' @param worksheet An Excel worksheet object.
#'
#' @return A data frame containing the overall results.
extract_overall_results <- function(worksheet) {
  # combine data from two different locations in the worksheet into one data frame
  overall_results <- dplyr::bind_cols(
    extract_data_frame_values(worksheet, carryover_starting_cell, 1, 1),
    extract_data_frame_values(worksheet, results_starting_cell, 4, 1)
  )

  # return the overall results
  return(overall_results)
}

#' get_soil_results
#'
#' This function extracts soil results from an Excel worksheet.
#'
#' @param worksheet An Excel worksheet object.
#' @param nrows The number of rows of data to extract from the worksheet.
#'
#' @return A data frame containing the soil results.
get_soil_results <- function(worksheet, nrows) {
  # Extract soil results from worksheet
  n_soil_cols <- 5
  return(
    extract_data_frame_values(worksheet, soil_results_starting_cell, n_soil_cols, nrows) |>
      # Add result type and CEA number to the table
      dplyr::mutate(
        result_type = "Soil",
        `CEA number` = paste("CEA", row_number())
      )
  )
}

#' Get vegetation results
#'
#' Extracts vegetation results from a worksheet and formats them into a data frame.
#'
#' @param worksheet The worksheet to extract the results from.
#' @param nrows The number of rows of results to extract.
#' @return A data frame containing the vegetation results.
#' @export
get_vegetation_results <- function(worksheet, nrows) {
  # Extract vegetation results from worksheet
  n_vegetation_cols <- 3
  return(
    extract_data_frame_values(worksheet, vegetation_results_starting_cell, n_vegetation_cols, nrows) |>
      # Add result type and CEA number to the table
      dplyr::mutate(
        result_type = "Vegetation",
        `CEA number` = paste("CEA", row_number())
      )
  )
}

#' Get GHG results
#'
#' Extracts greenhouse gas results from a worksheet and formats them into a data frame.
#'
#' @param worksheet The worksheet to extract the results from.
#' @param nrows The number of rows of results to extract.
#' @return A data frame containing the greenhouse gas results.
#' @export
get_ghg_results <- function(worksheet, nrows) {
  # Extract greenhouse gas results from worksheet
  n_ghg_cols <- 8
  return(
    extract_data_frame_values(worksheet, ghgs_results_starting_cell, n_ghg_cols, nrows) |>
      # Add result type and CEA number to the table
      dplyr::mutate(
        result_type = "Greenhouse gases",
        `CEA number` = paste("CEA", row_number())
      )
  )
}

#' Convert wide-format CEA results table to long format
#'
#' Converts a wide-format table of CEA results to a long-format table.
#'
#' @param cea_results_table A wide-format table of CEA results.
#' @return A long-format table of CEA results.
#' @export
wide_to_long_cea_results_table <- function(cea_results_table) {
  # Convert wide format CEA results table to long format using gather()
  return(gather(cea_results_table, key = "Measure", value = "Measurement", -result_type, -`CEA number`))
}


#' Extract CEA-specific results
#'
#' Extracts all results for each CEA from a worksheet and formats them into a data frame.
#'
#' @param worksheet The worksheet to extract the results from.
#' @param n_ceas The number of CEAs to extract results for.
#' @return A data frame containing the results for each CEA.
#' @export
extract_cea_specific_results <- function(worksheet, n_ceas) {
  # Extract results specific to each CEA from worksheet
  ghg_results <- get_ghg_results(worksheet, n_ceas)
  vegetation_results <- get_vegetation_results(worksheet, n_ceas)
  soil_results <- get_soil_results(worksheet, n_ceas)

  # Combine results into a single table
  cea_combined_results <- dplyr::bind_rows(
    wide_to_long_cea_results_table(ghg_results),
    wide_to_long_cea_results_table(vegetation_results),
    wide_to_long_cea_results_table(soil_results)
  ) |>
    # Add measure type column based on whether the measurement is a component or total value
    dplyr::mutate(measure_type = if_else(stringr::str_detect(Measure, "total"), "Total", "Component"))

  return(cea_combined_results)
}

#' Extracts all results details from a worksheet
#'
#' This function extracts both the overall results and the specific CEA results
#' from a given worksheet and returns them as a list.
#'
#' @param worksheet An Excel worksheet containing the results data
#' @param n_ceas The number of CEAs in the worksheet
#'
#' @return A list containing two data frames: 'cea' and 'overall'. 'cea' contains
#' the specific results for each CEA, broken down by measurement type, while
#' 'overall' contains the overall results for all CEAs.
#'
#' @examples
#' # Extract all results from the 'Results' worksheet
#' all_results <- extract_all_results_details(Results, 3)
#'
#' # Access specific data frames
#' cea_results <- all_results$cea
#' overall_results <- all_results$overall
#'
extract_all_results_details <- function(worksheet, n_ceas) {
  # Extract both CEA-specific and overall results from worksheet
  specific_results <- extract_cea_specific_results(worksheet, n_ceas)
  overall_results <- extract_overall_results(worksheet)

  # Return a list containing the two tables
  return(
    list("cea" = specific_results, "overall" = overall_results)
  )
}

#' Extracts CEA input data from a worksheet
#'
#' This function extracts the input data for each CEA from a given worksheet
#' and returns it as a data frame.
#'
#' @param worksheet An Excel worksheet containing the CEA input data
#' @param n_ceas The number of CEAs in the worksheet
#'
#' @return A data frame containing the input data for each CEA
#'
#' @examples
#' # Extract CEA input data from the 'CEA Input Data' worksheet
#' cea_input_data <- extract_cea_input_data(CEA_Input_Data, 3)
#'
extract_cea_input_data <- function(worksheet, n_ceas) {
  number_of_cea_entry_cols <- 11

  # Extract CEA input data from worksheet
  extract_data_frame_values(worksheet, cea_parameter_starting_cell, number_of_cea_entry_cols, n_ceas)
}


#' Sets up the constants in the BlueCam initial spreadsheet
#'
#' This function sets up the constants in the BlueCam initial spreadsheet for the project timing and CEA core data.
#'
#' @param worksheet The worksheet to set up the constants in
#' @param initial_project_period A data frame containing the initial project period details
#' @param initial_cea_data A data frame containing the initial CEA data
#' @param initial_carryover The initial carryover value
#'
#' @return None
set_up_bluecam_initial_spreadsheet_constants <- function(worksheet,
                                                         initial_project_period,
                                                         initial_cea_data,
                                                         initial_carryover) {
  # Enter carry over value as a constant
  enter_value_into_index(worksheet, c(carryover_starting_cell[1] + 1, carryover_starting_cell[2]), initial_carryover)

  # The CEA core data as constants
  # Enter the project area stuff as a constant
  enter_values_from_dataframe(worksheet, cea_parameter_starting_cell, initial_cea_data)

  # Enter project timing as constant
  enter_values_from_dataframe(worksheet, time_period_parameter_starting_cell, initial_project_period)
}



#' Convert manual CEA data to BlueCam format
#'
#' This function takes as input a data frame of manual CEA data and a template data frame in BlueCam format, and returns a data frame in BlueCam format that includes the data from the manual CEA as well as any missing fields filled in with default values.
#' @param initial_cea_data A data frame containing the manual CEA data to be converted to BlueCam format
#' @param template_from_bluecam_data A data frame in BlueCam format to use as a template for the output data frame
#' @return A data frame in BlueCam format with the manual CEA data included
convert_manual_cea_data_to_bluecam_format <- function(initial_cea_data, template_from_bluecam_data) {
  # Get the column names of the initial CEA data
  initial_cea_data_colnames <- colnames(initial_cea_data)

  # Combine the initial CEA data with the BlueCam data template
  updated_initial_cea_data <- dplyr::bind_cols(
    initial_cea_data, template_from_bluecam_data
  ) |>
    # Map the relevant column names from the manual CEA data to the corresponding column names in the BlueCam template
    dplyr::mutate(
      `CEA area (ACEAi) (ha)` = `Size (Ha)`,
      `Elevation of CEA (m AHD)` = `Elevation (m AHD)`,
      `CEA baseline land type ` = `BlueCam Site Classification`,
      `Land type for CEA: last reporting period end` = `BlueCam Site Classification`,
      `Land type for CEA:  current reporting period end` = `BlueCam Site Classification`,
      `Age of blue carbon vegetation in previous reporting period (years)` = `Vegetation Age`,
      `Age of blue carbon vegetation in current reporting period (years)` = dplyr::coalesce(`Age of blue carbon vegetation in current reporting period (years)`, 0),
      `Excavation area within CEA (hectares)` = dplyr::coalesce(`Excavation area within CEA (hectares)`, 0)
    ) |>
    # Remove the initial CEA data columns to only keep the BlueCam format data
    dplyr::select(-all_of(initial_cea_data_colnames))

  return(updated_initial_cea_data)
}

#' Estimate new CEA dataframe
#'
#' This function estimates a new CEA dataframe based on the old CEA dataframe, tidal height, and CEA raster data.
#'
#' @param old_cea_dataframe a dataframe of the old CEA data
#' @param tidal_height the tidal height value to use
#' @param cea_raster_data the CEA raster data
#' @return temp_cea_data_frame the new estimated CEA dataframe
estimate_new_cea_dataframe <- function(old_cea_dataframe, current_tidal_range, cea_raster_data_df,
                                       new_mean_tidal_height,
                                       new_actual_ahd_hat,
                                       stpi_mapping,
                                       is_first_reporting_period) {
  # define the possible land types for CEA
  all_possible_cea_land_types <- c(
    "Tidally restricted fresh and brackish wetlands",
    "Mangroves",
    "Saltmarsh",
    "Supratidal forest",
    "Seagrass",
    "Sparsely vegetated saltmarsh (saltflats)",
    "Other coastal wetland ecosystem",
    "Flooded agricultural land, managed wet meadow or pasture ",
    "Other use land",
    "Drainage channels",
    "Grazing land",
    "Cropping land",
    "Sugarcane land",
    "Forest land",
    "Saline water bodies",
    "Ponds and other constructed water bodies"
  )


  # Calculate the STPI for every CEA in the region and then use that with the STPI habitat model table to update habitat types
  temp_stpi_example <- old_cea_dataframe |>
    dplyr::mutate(
      `Calculated STPI value` = calculate_standardized_tidal_position_index(`Elevation of CEA (m AHD)`,
        mean_tide_level = new_mean_tidal_height,
        high_tide_level = new_actual_ahd_hat
      )
    ) |>
    dplyr::mutate(
      `Land type for CEA:  current reporting period end` =
        calculate_new_land_type(`CEA baseline land type `, `Calculated STPI value`, stpi_mapping)
    ) |>
    # UPDATE VEGETATION AGES BASED ON WHETHER HTE HABITAT HAS TRANSITIONED TO A NEW TYPE OR NOT
    dplyr::mutate(
      `Land Type Changed` = if_else(`Land type for CEA:  current reporting period end` != `Land type for CEA: last reporting period end`, TRUE, FALSE),
      `Age of blue carbon vegetation in current reporting period (years)` = if_else(`Land Type Changed`, reporting_time_step_years, 0)
      # Habitat transition is considered instant in this scenario - not recalculate STPI annually after sea-level rise
    )

  # Calculate new CEA area and height
  new_cea_areas_and_heights <- calculate_flooding_and_land_height_for_region_raster(cea_raster_data_df, new_actual_ahd_hat) |>
    dplyr::rename(
      "CEA area (ACEAi) (ha)" = "Hectares flooded",
      "Elevation of CEA (m AHD)" = "Flooded fraction mAHD",
      "CEA number" = "cea_names"
    )

  # New updated CEA dataset
  output_cea_dataframe <- temp_stpi_example |>
    dplyr::select(-`Elevation of CEA (m AHD)`, -`CEA area (ACEAi) (ha)`) |>
    dplyr::left_join(new_cea_areas_and_heights) |>
    dplyr::mutate(`New CEA or first reporting period` = if_else(is_first_reporting_period, "Yes", "No")) |>
    dplyr::select(dplyr::all_of(colnames(old_cea_dataframe))) |>
    # Zero replacer put here as zero area CEAs and hieght should have no impact on carbon abatement condition
    dplyr::mutate(
      across(c(`CEA area (ACEAi) (ha)`, `Elevation of CEA (m AHD)`), function(x) dplyr::coalesce(x, 0))
    )

  return(output_cea_dataframe)
}

#' Prepare data for the next reporting period
#'
#' Update the age of blue carbon vegetation and land type based on the last and current reporting periods.
#'
#' @param cea_data A data frame containing the current reporting period data
#' @param timestep_size The size of the reporting period in years
#'
#' @return A modified version of cea_data for the next reporting period
#'
#' @details
#' This function prepares the data for the next reporting period by updating the age of blue carbon vegetation and land type based on the last and current reporting periods. If the land type has changed between the current and last reporting periods, the age of the vegetation is set to the current reporting period age. If the land type has not changed, the age is updated based on the size of the reporting period.
#'
#' @examples
#' # Load sample data
#' data(cea_data)
#'
#' # Prepare data for next reporting period
#' prepare_for_next_time_step(cea_data, 5)
#'
#' @import dplyr
#' @export
prepare_for_next_time_step <- function(cea_data, timestep_size) {
  cea_data |>
    # Check if land type changed and update "Land Type Changed" accordingly
    dplyr::mutate(`Land Type Changed` = if_else(`Land type for CEA:  current reporting period end` != `Land type for CEA: last reporting period end`, TRUE, FALSE)) |>
    # Update age of blue carbon vegetation based on whether there was a land type change
    dplyr::mutate(
      `Age of blue carbon vegetation in previous reporting period (years)` =
        if_else(`Land Type Changed`,
          `Age of blue carbon vegetation in current reporting period (years)`,
          `Age of blue carbon vegetation in previous reporting period (years)` + timestep_size
        )
    ) |>
    # Update land type and age of blue carbon vegetation for next reporting period
    dplyr::mutate(
      `Land type for CEA: last reporting period end` = `Land type for CEA:  current reporting period end`,
      `Age of blue carbon vegetation in current reporting period (years)` = 0
    ) |>
    # Return the updated CEA data with the same columns as the input dataframe
    dplyr::select(dplyr::all_of(colnames(cea_data)))
}



#' Update tidal height in sheet
#'
#' This function updates the tidal height value in a specific cell of a worksheet.
#'
#' @param worksheet the worksheet object to update
#' @param tidal_height the tidal height value to use for updating the worksheet
#' @return nothing is returned
update_tidal_height_in_sheet <- function(worksheet, tidal_height) {
  # define the tidal height cell index
  tidal_height_cell_index <- c(
    time_period_parameter_starting_cell[1] + 1,
    time_period_parameter_starting_cell[2] + 4
  )
  # enter the tidal height value into the specific cell of the worksheet
  enter_value_into_index(worksheet, tidal_height_cell_index, tidal_height)
}



#' Update new CEA data in sheet
#'
#' This function updates the CEA data in a specific range of cells in a worksheet.
#'
#' @param worksheet the worksheet object to update
#' @param new_cea_data the new CEA data to use for updating the worksheet
#' @return nothing is returned
update_new_cea_in_sheet <- function(worksheet, new_cea_data) {
  # flat out copy the CEA data across
  enter_values_from_dataframe(worksheet, cea_parameter_starting_cell, new_cea_data)
}




#' Update specific tidal height and CEA data in a worksheet
#'
#' This function updates the specified worksheet with the given tidal height and CEA data.
#'
#' @param worksheet The worksheet to update
#' @param tidal_height The tidal height to enter into the worksheet
#' @param new_cea_data The new CEA data to enter into the worksheet
#' @return NULL
#'
#' @examples
#' update_excel_results(worksheet, 2.5, new_cea_data)
update_excel_results <- function(worksheet, tidal_height, new_cea_data) {
  # Update specific tidal height cell
  update_tidal_height_in_sheet(worksheet, tidal_height)

  # Update all relevant CEA data
  update_new_cea_in_sheet(worksheet, new_cea_data)
}

#' Calculate new land types for CEA based on STPI
#'
#' This function takes historical land types, a current STPI value, and a landtype_mapping_table, and returns
#' the new land type for the CEA based on the STPI value.
#'
#' @param historical_land_types The historical land types
#' @param current_stpi The current STPI value
#' @param landtype_mapping_table The mapping table of land types to STPI ranges
#' @return The new land type for the CEA based on the STPI value
#'
#' @examples
#' calculate_new_land_type(historical_land_types, current_stpi, landtype_mapping_table)
calculate_new_land_type <- function(historical_land_types, current_stpi, landtype_mapping_table) {
  # current_stpi <- temp_stpi_example$`Calculated STPI value`
  # landtype_mapping_table <- relevant_stpi_mapping_table
  # historical_land_types <- temp_stpi_example$`CEA baseline land type `
  # current_land_types <- temp_stpi_example$`Land type for CEA: last reporting period end`

  # First, in the mapping table, remove ambiguity by remove entries that require prior presence on site but which have not been seen
  remaining_land_type_mappings <- landtype_mapping_table |>
    dplyr::mutate(
      eligible_habitat = dplyr::if_else(`Prior presence needed` == "No", TRUE, `Vegetation Type` %in% historical_land_types)
    ) |>
    dplyr::filter(eligible_habitat)

  cea_stpi_values <- dplyr::tibble(stpi_values = current_stpi) |> dplyr::mutate(cea_id = dplyr::row_number())

  new_area_type_df <- remaining_land_type_mappings |>
    tidyr::crossing(cea_stpi_values) |>
    dplyr::filter(
      stpi_values > `STPI range of Carbon Estimation Area (CEA) - lower`,
      stpi_values <= `STPI range of Carbon Estimation Area (CEA) - upper`
    ) |>
    dplyr::select(`Model / default value to use`, cea_id) |>
    dplyr::arrange(cea_id)

  return(new_area_type_df |> pull(`Model / default value to use`))
}

################ TESTING FUNCTIONS
################
################

#' Create a new blank Excel file with the required sheets.
#'
#' @param file_path Path to the Excel file to be created
#' @param sheet_names Vector of sheet names to be created in the Excel file
#' @return Nothing, but creates a new Excel file with the required sheets
#' @import RDCOMClient
create_blank_excel_file_with_sheets <- function(file_path, sheet_names) {
  # create an instance of Excel
  excel_app <- COMCreate("Excel.Application")

  # make Excel visible (for debugging)
  # excel_app[['Visible']] <- TRUE

  # create a new workbook
  workbook <- excel_app$Workbooks()$Add()

  # rename sheets
  for (i in seq_along(sheet_names)) {
    sheet <- workbook$Worksheets(i)
    sheet$Name <- sheet_names[i]
  }

  # save and close workbook
  workbook$SaveAs(file_path)
  workbook$Close()

  # quit excel
  excel_app$Quit()

  # release resources
  excel_app <- NULL
  workbook <- NULL
  sheet <- NULL
}

#################################
# New functions for data-analysis
#################################

#' Cleans up the names of CEA (Coastal Erosion Area) layers
#'
#' Given a character vector of possible names for CEA (Coastal Erosion Area) layers, this function cleans up the names and returns them as a character vector. The function removes any extra whitespace and ensures that all names have the format "CEA ".
#'
#' @param possible_cea_names A character vector of possible names for CEA (Coastal Erosion Area) layers.
#' @return A character vector of cleaned up names for CEA (Coastal Erosion Area) layers.
#'
#' @examples
#' # Clean up CEA names
#' possible_cea_names <- c("CEA1", " CEA2", "CEA 3")
#' clean_cea_names(possible_cea_names)
clean_cea_names <- function(possible_cea_names) {
  stringr::str_replace(possible_cea_names, ".*CEA\\s*(\\d+).*", "CEA \\1")
}



# Used for calculating inundation over the land
#' Calculate the fraction of values in a numeric vector that are over a given cutoff
#'
#' Given a numeric vector and a cutoff value, this function calculates the fraction of values in the vector that are greater than or equal to the cutoff.
#'
#' @param x A numeric vector.
#' @param cutoff A numeric value indicating the cutoff value.
#' @return A numeric value between 0 and 1 representing the fraction of values in \code{x} that are greater than or equal to \code{cutoff}.
#'
#' @examples
#' # Calculate the fraction of values greater than or equal to 3 in a vector
#' x <- c(1, 2, 3, 4, 5)
#' fraction_over_cutoff(x, 3)
fraction_over_cutoff <- function(x, cutoff) {
  mean(x >= cutoff, na.rm = T)
}


#' Calculates the average height of flooded land
#'
#' Given a numeric vector representing the heights of land, and a cutoff value, this function calculates the average height of flooded land, which is defined as the average height of all values in \code{x} that are less than the cutoff value.
#'
#' @param x A numeric vector representing the heights of land.
#' @param cutoff A numeric value indicating the cutoff value for flooded land.
#' @return A numeric value representing the average height of flooded land.
#'
#' @examples
#' # Calculate the average height of flooded land
#' x <- c(1, 2, 3, 4, 5)
#' height_for_flooded_land(x, 4)
height_for_flooded_land <- function(x, cutoff) {
  mean(x[x < cutoff], na.rm = T)
}


#' Calculates the average heights and flooded fraction
#'
#' Given a raster and a numeric value representing the new highest astronomical tide, this function calculates the new coastal extent area in mHD.
#'
#' @param largest_area_cea_raster A raster representing the largest Coastal extent area.
#' @param new_highest_astronomical_tide A numeric value representing the new highest astronomical tide.
#' @return A new raster object with the new coastal extent area in mHD.
#'
#' @examples
#' # Calculate the new coastal extent area in mHD
#' data <- raster::raster(matrix(rnorm(100), nrow = 10))
#' new_highest_astronomical_tide <- 1
#' calculate_flooding_and_land_height_for_region_raster(data, new_highest_astronomical_tide)
calculate_flooding_and_land_height_for_region_raster <- function(raster_tibble,
                                                                 new_highest_astronomical_tide) {
  return(
    raster_tibble |>
      dplyr::mutate(
        `Non flooded fraction` = map(raster_values, ~ fraction_over_cutoff(.x, new_highest_astronomical_tide)),
        `Flooded fraction mAHD` = map(raster_values, ~ height_for_flooded_land(.x, new_highest_astronomical_tide))
      ) |>
      tidyr::unnest(cols = c("Non flooded fraction", "Flooded fraction mAHD")) |>
      dplyr::mutate(`Hectares flooded` = Hectares * (1 - `Non flooded fraction`)) |>
      dplyr::select(-raster_values)
  )
}

#' Applies a function to each element of a list, unpacking a second argument, and then returns a dataframe where each column corresponds to an element of the list and each row corresponds to the value of the function applied with different values of the second argument.
#'
#' @param input_list_vals A list of input values that will be passed as first argument to the function.
#' @param alternative_num_step A numeric vector that will be passed as second argument to the function.
#' @param application_func A function that will receive each value of the list (first argument) and each value of the numeric vector (second argument).
#' @return A dataframe where each column corresponds to an element of the list and each row corresponds to the value of the function applied with different values of the second argument.
#' @examples
#'
#' # Apply the function mean to each element of a list using a vector with different values as second argument
#' input_list_vals <- list(a = c(1, 2, 3), b = c(4, 5, 6))
#' alternative_num_step <- c(2, 4, 6)
#' pairwise_map_unpack(input_list_vals, alternative_num_step, function(x, y) mean(x) * y)
pairwise_map_unpack <- function(input_list_vals, alternative_num_step, application_func) {
  map(
    input_list_vals,
    function(x) map(alternative_num_step, function(y) application_func(x, y))
  ) |>
    map(~ unlist(.x)) |>
    dplyr::bind_cols() |>
    dplyr::mutate(`Numeric values` = alternative_num_step)
}

#' Establish a tibble containing CEA raster data
#'
#' Given a list of raster values and additional area fields, this function joins them with a shape file, adds cleaned CEA names to a new tibble and returns it.
#'
#' @param list_of_raster_values A list of raster values
#' @param additional_area_fields Additional area fields for joining with the shape file
#' @return A new tibble containing CEA raster data
#' @examples
#' establish_cea_raster_data_tibble(list_of_raster_values = list(a = r1, b = r2, c = r3), additional_area_fields = c("area_1", "area_2"))
#' establish_cea_raster_data_tibble(list_of_raster_values = list(raster1, raster2), addition_area_fields = c("area"))
establish_cea_raster_data_tibble <- function(list_of_raster_values, additional_area_fields) {
  tibble::tibble(
    cea_names = names(list_of_raster_values),
    raster_values = list_of_raster_values
  ) |>
    dplyr::left_join(additional_area_fields) |>
    dplyr::mutate(cea_names = clean_cea_names(cea_names))
  # Assumes that they have a joining column in area_shape_files called cea_names
}

#' Convert pairwise data to long format with one column for measures and one for CEA names
#'
#' Takes pairwise_data table and converts it from wide to long format using pivot_longer.
#'
#' @param pairwise_data table with Tidal Height, CEAs and measures
#' @param measure_name name to give column where measures will be placed
#'
#' @return long form table with CEA names, tidal height and the measures as separate columns
#'
#' @examples
#' pairwise_data <- tibble::tribble(
#'   ~`Tidal Height`, ~`CEA 1`, ~`CEA 2`,
#'   1, 0.1, 0.2,
#'   2, 0.3, 0.4,
#'   3, 0.5, 0.6
#' )
#' pairwise_map_measure_to_longform(pairwise_data, "Measure")
#'
#' # Output
#' # A tibble: 6 x 3
#' # Tidal Height cea_names Measure
#' # <dbl> <chr> <dbl>
#' # 1 1 CEA 1 0.1
#' # 2 1 CEA 2 0.2
#' # 3 2 CEA 1 0.3
#' # 4 2 CEA 2 0.4
#' # 5 3 CEA 1 0.5
#' # 6 3 CEA 2 0.6
pairwise_map_measure_to_longform <- function(pairwise_data, measure_name) {
  pairwise_data |>
    tidyr::pivot_longer(-`Tidal Height`,
      names_to = "cea_names",
      values_to = measure_name
    )
}

#' Add CEA extract loop constants to data frame
#'
#' This function adds constants to the given data frame based on the parameters passed.
#'
#' @param df A data frame.
#' @param tide_val A numeric value specifying the tidal range.
#' @param time_period A numeric value specifying the timestep.
#' @param area_val A numeric value specifying the total hectares in CEAs.
#'
#' @return A modified data frame with new columns for the given constants.
#'
#' @examples
#' add_cea_extract_loop_constants(df, 3, 10, 500)
#' add_cea_extract_loop_constants(my_data_frame, 2, 5, 200)
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr |>
#'
add_cea_extract_loop_constants <- function(df, tide_val, time_period, area_val) {
  df |>
    dplyr::mutate(
      `Tidal range` = tide_val,
      timestep = time_period,
      `Total hectares in CEAs` = area_val
    )
}


#' Summarize the total site area from an area tibble
#'
#' Given an area tibble, summarize the total site area (in hectares) by summing the Hectares column.
#'
#' @param area_tibble The input area tibble
#'
#' @return The total site area (in hectares)
#'
#' @examples
#' area_tibble <- tibble::tibble(SiteID = c("A", "B", "C"), Hectares = c(10, 15, 20))
#' raster_tibble_area_summary(area_tibble)
#' # output: 45
raster_tibble_area_summary <- function(area_tibble) {
  area_tibble |>
    dplyr::summarise(totalSiteArea = sum(Hectares)) |>
    dplyr::pull(totalSiteArea)
}

#' Extracts a specific data frame from a list of lists of data frames
#'
#' Given a list of lists of data frames and a string indicating the position of the data frame to be extracted from each list, returns a list of those data frames.
#'
#' @param list_of_lists_of_dfs A list of lists of data frames
#' @param df_entry_to_get A string indicating the position of the data frame to be extracted from each list
#' @return A list of data frames extracted from each list
#'
#' @examples
#' my_list <- list(
#'   list(data.frame(a = 1:5, b = letters[1:5]), data.frame(c = 1:5, d = letters[1:5])),
#'   list(data.frame(e = 1:5, f = letters[1:5]), data.frame(g = 1:5, h = letters[1:5]))
#' )
#' extract_list_entry_subdf(my_list, 2)
extract_list_entry_subdf <- function(list_of_lists_of_dfs, df_entry_to_get) {
  lapply(list_of_lists_of_dfs, function(x) x[[df_entry_to_get]])
}

#' Extract and merge a specific sub-dataset from a list of datasets
#'
#' This function extracts a specific sub-dataset from a list of datasets and merges them into a single data frame using dplyr::bind_rows().
#'
#' @param list_of_lists_of_dfs A list of data frames where each data frame represents a dataset for a different scenario.
#' @param df_entry_to_get The name of the sub-dataset to extract from each data frame in the list.
#'
#' @return A merged data frame containing the specified sub-dataset for each scenario.
#'
#' @examples
#' # Define a list of data frames
#' df1 <- data.frame(A = c(1, 2), B = c(3, 4))
#' df2 <- data.frame(A = c(5, 6), B = c(7, 8))
#' df_list <- list(df1, df2)
#'
#' # Extract and merge the 'B' column from each data frame in the list
#' extract_and_merge_scenario_subdataset(df_list, "B")
#'
#' # Output:
#' # B
#' # 1 3
#' # 2 4
#' # 3 7
#' # 4 8
#'
extract_and_merge_scenario_subdataset <- function(list_of_lists_of_dfs, df_entry_to_get) {
  extract_list_entry_subdf(list_of_lists_of_dfs, df_entry_to_get) |>
    dplyr::bind_rows()
}


#' typical_pointed_ggplot_line_plot
#'
#' This function creates a ggplot line plot with points of the provided data.
#'
#' @param data_to_plot the data frame to plot.
#' @param x_var the name of the variable to be plotted on the x-axis.
#' @param y_var the name of the variable to be plotted on the y-axis.
#' @param col_var the name of the variable to be plotted as color. If NULL or NA, color will not be plotted.
#'
#' @examples
#' typical_pointed_ggplot_line_plot(mtcars, "wt", "mpg")
#' typical_pointed_ggplot_line_plot(mtcars, "wt", "mpg", "cyl")
#' typical_pointed_ggplot_line_plot(mtcars, "wt", "mpg", NULL)
#'
#' @import ggplot2
#' @import dplyr
#' @import cowplot
#' @import scales
#'
typical_pointed_ggplot_line_plot <- function(data_to_plot, x_var, y_var, col_var) {
  # Symbols for each variable
  x_sym <- sym(x_var)
  y_sym <- sym(y_var)
  col_var <- if (is.null(col_var) || is.na(col_var)) "dummy" else col_var
  col_sym <- sym(col_var)

  # Construct the ggplot lines plot
  output_lineplot <- data_to_plot |>
    dplyr::mutate(dummy = "1") |>
    ggplot2::ggplot(ggplot2::aes(
      x = !!x_sym,
      y = !!y_sym,
      color = !!col_sym
    )) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::geom_line(lwd = 1.1) +
    cowplot::theme_cowplot() +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 7), labels = scales::number_format()) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 7), labels = scales::number_format())

  return(output_lineplot)
}

#' Get minimum and maximum values of a numeric vector
#'
#' This function takes a numeric vector as input and returns the minimum and maximum values as a vector.
#'
#' @param x a numeric vector
#' @param na.rm a logical value indicating whether NA values should be removed from the vector
#' @return a numeric vector of length 2 containing the minimum and maximum values of x
#' @examples
#' min_max_vec(c(1, 5, 2, 7, 3))
#' min_max_vec(c(1, 5, 2, NA, 3))
#' min_max_vec(c(1, 5, 2, NA, 3), na.rm = TRUE)
min_max_vec <- function(x, na.rm = F) {
  c(min(x, na.rm = na.rm), max(x, na.rm = na.rm))
}

#' Calculate the annual costings of a scenario
#'
#' Given an original data frame, this function selects the `Tidal range` and `Average Tonnes of CO2 abatement per Hectare per Year` columns from the data frame, then performs a cross join with an alternative hypothesis data frame that has a column `Carbon price per tonne`. This column is then renamed as `Carbon price per tonne`, and the `Annualised Site revenue` is calculated as the product of `Carbon price per tonne` and `Average Tonnes of CO2 abatement per Hectare per Year`.
#'
#' @param original_data_df The original data frame with the `Tidal range` and `Average Tonnes of CO2 abatement per Hectare per Year` columns.
#' @param alternative_hypothesis A data frame with a column `Carbon price per tonne`.
#'
#' @return A data frame with the `Carbon price per tonne` and `Annualised Site revenue` columns.
#'
#' @examples
#' original_df <- data.frame("Tidal range" = c(1, 2, 3), "Average Tonnes of CO2 abatement per Hectare per Year" = c(4, 5, 6))
#' alternative_df <- data.frame("Carbon price per tonne" = c(7, 8, 9))
#' calculate_scenario_annual_costings(original_df, alternative_df)
calculate_scenario_annual_costings <- function(original_data_df, alternative_hypothesis) {
  original_data_df |>
    dplyr::select(`Tidal range`, `Average Tonnes of CO2 abatement per Hectare per Year`) |>
    tidyr::crossing(alternative_hypothesis) |>
    dplyr::rename("Carbon price per tonne" = "alternative_hypothesis") |>
    dplyr::mutate(`Annualised Site revenue` = `Carbon price per tonne` * `Average Tonnes of CO2 abatement per Hectare per Year`)
}


#' build_underlying_heatmap
#'
#' A function that takes in a tibble and plots a heatmap with x_var and y_var as the axes and fill_var as the color gradient.
#'
#' @param heatmap_tibble a tibble containing the data to plot
#' @param x_var the variable to plot on the x-axis
#' @param y_var the variable to plot on the y-axis
#' @param fill_var the variable to use for the color gradient
#'
#' @return a ggplot2 object containing the heatmap plot
#'
#' @examples
#' # Load data
#' library(ggplot2)
#' library(cowplot)
#' data(mtcars)
#'
#' # Build heatmap
#' heatmap_plot <- build_underlying_heatmap(
#'   heatmap_tibble = mtcars,
#'   x_var = "wt",
#'   y_var = "mpg",
#'   fill_var = "disp"
#' )
#'
#' # View the plot
#' print(heatmap_plot)
#'
#' @import ggplot2
#' @importFrom cowplot theme_cowplot
#' @importFrom scales pretty_breaks number_format
#' @importFrom dplyr select rename crossing mutate
build_underlying_heatmap <- function(heatmap_tibble, x_var, y_var, fill_var) {
  heatmap_tibble |>
    ggplot2::ggplot(aes(x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(fill_var))) +
    ggplot2::geom_tile() +
    cowplot::theme_cowplot() +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 7), labels = scales::number_format()) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 7), labels = scales::number_format())
}

#########################
# Probability functions
#########################

#' Calculate the truncated normal distribution for a given value, mean and standard deviation
#'
#' This function calculates the truncated normal distribution for a given value, mean and standard deviation.
#'
#' @param value The value to calculate the truncated normal distribution for
#' @param location The mean value for the normal distribution
#' @param scale The standard deviation for the normal distribution
#'
#' @return The truncated normal distribution for the given value, mean and standard deviation
#'
#' @examples
#' tidal_range_trunc_norm(5, 10, 2)
tidal_range_trunc_norm <- function(value, location, scale) {
  truncnorm::ptruncnorm(value,
    a = tidal_range_hypotheses_bounds[1],
    b = tidal_range_hypotheses_bounds[2],
    location,
    scale
  )
}


#' truncated_pnorm_func_generator
#'
#' A function that creates a truncated probability distribution function with bounds specified in the argument.
#'
#' @param bounds a numeric vector of length 2, containing the lower and upper bounds of the truncated distribution.
#' @return A function object that represents the truncated probability distribution function.
#' @export
#'
#' @examples
#' # create a truncated probability distribution function for values between -1 and 1 with mean=0 and sd=1
#' trunc_pnorm <- truncated_pnorm_func_generator(c(-1, 1))
#' # calculate the probability density for value 0
#' trunc_pnorm(0, 0, 1)
#'
truncated_pnorm_func_generator <- function(bounds) {
  return(function(value, location, scale) {
    truncnorm::ptruncnorm(value,
      a = bounds[1],
      b = bounds[2],
      location,
      scale
    )
  })
}

#' Calculate the discretised belief given a step size
#'
#' The function calculates the discretised belief using a belief function,
#' given a step size, value, location and scale.
#'
#' @param value The value used in the belief function.
#' @param location The location used in the belief function.
#' @param scale The scale used in the belief function.
#' @param stepsize The step size used for discretisation.
#' @param belief_func The belief function used for calculating the belief.
#'
#' @return The discretised belief.
#'
#' @examples
#' discretised_belief_with_stepsize(value = 2, location = 5, scale = 1, stepsize = 0.5, belief_func = pnorm)
discretised_belief_with_stepsize <- function(value, location, scale, stepsize, belief_func) {
  belief_func(value + stepsize, location, scale) -
    belief_func(value, location, scale)
}

#' Generates a discretized belief scenario generator function with given values, stepsize and generator function.
#'
#' This function returns a generator function that generates a discretized belief scenario. The scenario is generated by taking the values and stepsize to calculate the probability density of the scenario using the generator function.
#'
#' @param values A numeric vector of possible scenario values.
#' @param stepsize_for_hyp A numeric value specifying the step size of the discretized belief function.
#' @param generator_func A function that returns the probability density function of a given scenario.
#'
#' @return A discretized belief scenario generator function
#'
#' @examples
#' generator_function <- discretized_scenario_belief_generator(c(1, 2, 3, 4), 0.1, dnorm)
#' generator_function(2, 0.5)
#' generator_function(3, 0.5)
#' generator_function(2, 1)
discretized_scenario_belief_generator <- function(values,
                                                  stepsize_for_hyp,
                                                  generator_func) {
  return(function(location, scale) {
    discretised_belief_with_stepsize(
      values,
      location,
      scale,
      stepsize_for_hyp,
      generator_func
    )
  })
}


#' Replace NaNs in a matrix with zeros
#'
#' This function replaces any NaN values in the input matrix with zero.
#'
#' @param input_matrix_with_nans A numeric matrix with NaN values to be replaced with zeros.
#' @return A numeric matrix with NaN values replaced with zeros.
#' @examples
#' input_matrix <- matrix(c(1, 2, NaN, 4, 5, 6), nrow = 2, ncol = 3)
#' replace_matrix_nans(input_matrix)
#'
#' # Output:
#' #     [,1] [,2] [,3]
#' # [1,]    1    4    6
#' # [2,]    2    5    0
replace_matrix_nans <- function(input_matrix_with_nans) {
  output_matrix <- input_matrix_with_nans
  output_matrix[is.nan(output_matrix)] <- 0
  return(output_matrix)
}


#' Build a joint probability belief tibble from a belief matrix
#'
#' This function takes a belief matrix and constructs a tibble with columns for the x-variable and each y-variable, and a column for belief probabilities. The function is intended for use in constructing output tibbles that summarize model results.
#'
#' @param belief_matrix a matrix object containing belief probabilities
#' @param x_var a character string specifying the name of the x-variable
#' @param y_vars a character vector specifying the names of the y-variables
#'
#' @return a tibble object containing the belief probabilities
#'
#' @examples
#'
#' # Example usage
#' belief_mat <- matrix(runif(16), ncol = 4)
#' build_joint_result_tibble(belief_mat, "X1", paste0("Y", 1:4))
#'
#' @importFrom tibble as_tibble setNames
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
build_joint_result_tibble <- function(belief_matrix, x_var, y_vars) {
  belief_matrix |>
    tibble::as_tibble(.name_repair = "unique") |>
    setNames(y_vars) |>
    dplyr::mutate(`X var` = x_var) |>
    tidyr::pivot_longer(-`X var`,
      names_to = "Y var",
      values_to = "belief"
    ) |>
    dplyr::mutate(`Y var` = as.numeric(`Y var`))
}



#' Builds a heatmap plot for a joint belief tibble
#'
#' This function builds a heatmap plot for a joint belief tibble given an x and y variable.
#' The plot is built using the build_underlying_heatmap() function from the plot_helper.R script and colored with the scale_fill_viridis_c() function.
#'
#' @param belief_tibble A tibble containing the belief matrix in "long format"
#' @param x_var A string with the name of the column containing the x variable
#' @param y_var A string with the name of the column containing the y variable
#'
#' @return A heatmap plot of the joint belief tibble
#' @export
#'
#' @examples
#' data <- build_joint_result_tibble(
#'   belief_matrix = matrix(1:16, 4, 4),
#'   x_var = "X",
#'   y_vars = c("Y1", "Y2", "Y3", "Y4")
#' )
#' build_joint_result_heatmap(
#'   belief_tibble = data,
#'   x_var = "X",
#'   y_var = "Y var"
#' )
build_joint_result_heatmap <- function(belief_tibble, x_var, y_var) {
  build_underlying_heatmap(belief_tibble, x_var, y_var, "belief") +
    scale_fill_viridis_c()
}

#' Create an array of ones
#'
#' Creates an array of ones with the specified length.
#'
#' @param array_length the length of the array to be created
#' @return an array filled with ones
#'
#' @examples
#' one_array(5)
#'
#' @export
one_array <- function(array_length) {
  rep(1, array_length)
}


#' Convert tidal vector to joint array
#'
#' This function converts a tidal vector into a joint array given the dimensions and number of inundation options.
#'
#' @param tidal_vector A vector representing tidal data.
#' @param tidal_dims A vector representing the dimensions of tidal data.
#' @param n_inund_options A numeric value representing the number of inundation options.
#'
#' @return A joint array derived from the given tidal vector, dimensions and number of inundation options.
#'
#' @examples
#' tidal_vector_to_joint_array(c(1, 2, 3, 4, 5, 6), c(2, 3), 2)
#'
#' @import array
#' @importFrom rep rep
#'
#' @export
tide_vector_to_joint_array <- function(tidal_vector, tidal_dims, n_inund_options) {
  array(tidal_vector, dim = tidal_dims)[one_array(n_inund_options), ]
}


#' Create a tibble with a column for beliefs and a column for target value.
#'
#' Given a vector of beliefs and a target value, this function creates a tibble with the beliefs in one column and the target values in another column.
#'
#' @param beliefs A vector of numeric values representing beliefs.
#' @param target_value A numeric value representing the target value.
#'
#' @return A tibble with two columns: belief and target_value.
#'
#' @examples
#' simple_belief_df(c(0.3, 0.6, 0.1), 1)
#' simple_belief_df(c(0.1, 0.2, 0.7), 0)
#' simple_belief_df(c(0.5, 0.2, 0.3), 1)
#'
simple_belief_df <- function(beliefs, target_value) {
  tibble::tibble(belief = beliefs, target_value = target_value)
}


#' Plot a univariate belief bar chart
#'
#' Plots a bar chart with the values of the target variable on the x-axis and the beliefs on the y-axis.
#'
#' @param belief_df a dataframe with the following columns: target variable values and corresponding beliefs
#' @param x_var a string with the column name of the target variable values
#' @param y_var a string with the column name of the corresponding beliefs
#'
#' @return a univariate belief bar chart
#'
#' @examples
#' belief_data <- data.frame(target_value = c(1, 2, 3), beliefs = c(0.1, 0.3, 0.6))
#' simple_univariate_belief_bar(belief_data, "target_value", "beliefs")
#'
#' @import ggplot2
#' @importFrom scales pretty_breaks number_format
#' @importFrom cowplot theme_cowplot
simple_univariate_belief_bar <- function(belief_df, x_var, y_var) {
  belief_df |>
    ggplot2::ggplot(aes(x = !!sym(x_var), y = !!sym(y_var))) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 7), labels = scales::number_format()) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 7), labels = scales::number_format()) +
    cowplot::theme_cowplot()
}

#' Convert a joint belief matrix to a heatmap
#'
#' This function takes a joint belief matrix, along with the names of its X and Y dimensions, and returns a heatmap of the matrix. The X and Y dimensions of the heatmap are set by the \code{x_var} and \code{y_var} arguments.
#'
#' @param belief_matrix a matrix of joint beliefs
#' @param matrix_x_var the name of the X dimension in the joint belief matrix
#' @param matrix_y_var the name of the Y dimension in the joint belief matrix
#' @param x_var the name of the X dimension in the heatmap
#' @param y_var the name of the Y dimension in the heatmap
#' @return a heatmap of the joint belief matrix with the X and Y dimensions set by the \code{x_var} and \code{y_var} arguments
#' @examples
#' # Create a joint belief matrix
#' belief_matrix <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("True", "False"), c("True", "False")))
#' # Create a heatmap of the joint belief matrix
#' joint_straight_to_heatmap(belief_matrix, "X", "Y", "X Axis", "Y Axis")
joint_straight_to_heatmap <- function(belief_matrix, matrix_x_var, matrix_y_var, x_var, y_var) {
  build_joint_result_tibble(belief_matrix, matrix_x_var, matrix_y_var) |>
    dplyr::rename(!!sym(x_var) := "X var", !!sym(y_var) := "Y var") |>
    build_joint_result_heatmap(y_var, x_var)
}


#' Applies a function to an array and retains its dimensions
#'
#' This function applies a specified function to an array and returns an array with the same dimensions as the original array. It is a wrapper around the apply function.
#'
#' @param x The array to apply the function to.
#' @param app_fun The function to apply to the array.
#' @param ... Additional arguments to be passed to the applied function.
#'
#' @return An array with the same dimensions as the original array after applying the function to it.
#'
#' @examples
#' x <- array(1:12, dim = c(2, 2, 3))
#' apply_and_retain_sample_dims(x, mean)
#'
#' y <- array(1:6, dim = c(2, 3))
#' apply_and_retain_sample_dims(y, sum)
#'
#' z <- array(1:30, dim = c(2, 5, 3))
#' apply_and_retain_sample_dims(z, function(x) sum(x^2))
apply_and_retain_sample_dims <- function(x, app_fun, ...) {
  apply(x, MARGIN = c(1, 2), FUN = app_fun, ...)
}

#' Apply function to an array and retain actual dimensions
#'
#' This function applies a function to an array, retaining the actual dimensions of the array.
#'
#' @param x an array
#' @param app_fun a function to apply to the array
#' @param ... additional arguments passed to the function
#'
#' @return an array with the same dimensions as x
#'
#' @examples
#' x <- array(runif(24), dim = c(2, 3, 2))
#' apply_and_retain_actual_dims(x, mean)
apply_and_retain_actual_dims <- function(x, app_fun, ...) {
  apply(x, MARGIN = c(3, 4), FUN = app_fun, ...)
}


#' Convert date string to Excel number.
#'
#' This function takes a date string as input, converts it to a Date object with the format 'YYYY-MM-DD', and then calculates the Excel number for that date by subtracting numeric form of December 30, 1899 which is the original date for Excels numeric date representation.
#'
#' @param date_string A character string representing the date in 'YYYY-MM-DD' format.
#'
#' @return A numeric value representing the Excel number for the input date.
#'
#' @examples
#' date_string_to_excel_number("2023-04-23")
#' date_string_to_excel_number("2021-09-30")
date_string_to_excel_number <- function(date_string) {
  as.numeric(as.Date(date_string, format = "%Y-%m-%d") - as.Date("1899-12-30", format = "%Y-%m-%d"))
}
