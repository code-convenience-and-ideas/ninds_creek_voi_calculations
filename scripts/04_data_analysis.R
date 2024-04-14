source(here::here("scripts", "04_data_analysis_functions.R"))
source(here::here("scripts", "00_key_variables.R"))

########################################################################################################################
# IMPORTANT: We don't have true / actual landholder prior beliefs
# Assumes landholder has unbiased views of inundation + tidal height
# So, end up using some of the satellite imagery for prior
# Assumptions to simply the problem significantly but could be explored further later
########################################################################################################################
# Set stages being rerun

# Flag whether we need to redo initial plots
replot_site_relief_files <- (stage_to_start_from <= analysis_stages_vectors[1])

# Flag if we need to rerun the carbon estimation through bluecam or not
reestimate_carbon_sequestration <- (stage_to_start_from <= analysis_stages_vectors[2])

# Flag need to redo bluecam plots
replot_carbon_sequestration <- (stage_to_start_from <= analysis_stages_vectors[3])

# Flag need to re-do initial prior characterisation
restablish_priors <- (stage_to_start_from <= analysis_stages_vectors[4])

# Flag need to redo voi analysis entirely
recalculate_voi <- (stage_to_start_from <= analysis_stages_vectors[5])

# Flag redoing the final voi plots
redo_voi_outcome_plots <- (stage_to_start_from <= analysis_stages_vectors[6])

########################################################################################################################
#  Tidal height bounds are relatively arbitrary
########################################################################################################################

# Give the eight hypotheses
# -5, 0, 5, 10, 15, 20, 25, 30 for inundation
# Historical tidal height was 1.77
minimum_tidal_height_allowed <- 0.01
step_size_in_tidal_height <- 0.05
maximum_tidal_height_allowed <- 6.01 + step_size_in_tidal_height / 2

# 0.76 in standard run
actual_tidal_height_measurement <- model_params$`Mean tidal height onsite`$value

# mAHD of HAT as measure onsite which is 1.9M above the LAT

heighest_astronomical_tide_ahd <- 0.76

# 1.9 in standard run
tidal_range_onsite <- model_params$`Tidal range onsite`$value # - in YAML

mean_tidal_height <- calculate_mean_tidal_height(
  heighest_astronomical_tide_ahd, tidal_range_onsite
)

# No third layer - comfortable assuming this is a known fact
# In practice, I'd expect the land holder to have prior belief about land height
# that is reasonably unbiased

#########################################################################################################
# Set up value ranges for various hypotheses
###########################################################################################################
tidal_range_hypotheses <- seq(
    minimum_tidal_height_allowed,
    maximum_tidal_height_allowed,
    step_size_in_tidal_height)

# Just reducing edge effect # min_max_vec(tidal_range_hypotheses)
tidal_range_hypotheses_bounds <- c(0, maximum_tidal_height_allowed + step_size_in_tidal_height / 2)

total_site_area <- area_shape_files$Hectares |> sum()
total_site_area_rounded_up_to <- ceiling(total_site_area / 10) * 10

inundation_range_of_values <- c(0, total_site_area)
inundation_step_size <- 1
inundation_value_sequence <- seq(0, ceiling(total_site_area) + 3, by = inundation_step_size)

# Counts used for array indexing and so-on
n_inundation_opt <- length(inundation_value_sequence)
n_tidal_range_opt <- length(tidal_range_hypotheses)

########## Values used to add the financial element
# baseline was 976.8
cattle_revenue_per_hectare_per_year <- model_params$`Baseline cattle revenue`$value # - in YAML

# number of steps to fractions
number_of_revenue_options <- model_params$`Number of cattle revenue options`$value

cattle_revenue_fraction_step <- 1 / number_of_revenue_options

# fractions to get fuller scenarios
cattle_revenue_per_hectare_options <- seq(
  cattle_revenue_fraction_step,
  1,
  cattle_revenue_fraction_step
) * cattle_revenue_per_hectare_per_year

accu_price_step_size <- model_params$`Steps between considered carbon prices`$value  # 5 baseline
accu_price_max_considered <- model_params$`Maximum carbon price considered`$value # 150 baseline

accu_considered_matrix_prices <- seq(
  accu_price_step_size, # - in YAML
  accu_price_max_considered,
  accu_price_step_size
)

##################################################################################################
# Use the DEM model relief and shapefiles for a prior belief of inundation for each tidal height #
##################################################################################################

# Set up paths for output items in this analysis reused elsewhere
raster_tibble_storage_path <- file.path(processeddata_dir, 'rasters_in_tibble.rds')

if (replot_site_relief_files) {
    # Plot an overlaid raster + shapefile plot of the site for my reference
    site_elevation_plot_with_boundaries <- raster_and_shape_plot(raster_image, area_shape_files) +
        labs(fill = "Elevation compared to sea level (m)",
             title = "Site elevation with area boundaries overlaid")

    elevation_image_path <- file.path(figures_dir, 'site_elevatio_with_cea_boundaries.png')
    ggplot2::ggsave(elevation_image_path, site_elevation_plot_with_boundaries)

    # Alignment of both files is good
    raster_values_in_shape_files <- raster::extract(
        raster_image,
        sf::as_Spatial((sf::st_zm(sf::st_geometry(area_shape_files))))
    )

    names(raster_values_in_shape_files) <- clean_cea_names(area_shape_files$name_based_on_file)

    # Prepare CEA constants
    cea_average_mahd_vector <- unlist(raster_values_in_shape_files |> map(function(x) mean(x, na.rm = T)))

    # Calculate inundated land in hectares and average height
    inundated_land_rename_vector <- c("Tidal Height" = "Numeric values")

    fraction_not_flooded_in_scenario <- pairwise_map_unpack(
        raster_values_in_shape_files,
        tidal_range_hypotheses,
        fraction_over_cutoff
    ) |>
        dplyr::rename(!!!inundated_land_rename_vector)

    average_flooded_land_height <- pairwise_map_unpack(
        raster_values_in_shape_files,
        tidal_range_hypotheses,
        height_for_flooded_land
    ) |>
        dplyr::rename(!!!inundated_land_rename_vector)

    # Put rasters into a tibble
    # Extract the area within each region
    area_key_name_vars <- c("name_based_on_file", "Hectares")

    cea_land_area_hectares_in_shapefile <- area_shape_files |>
        sf::st_drop_geometry() |>
        dplyr::select(dplyr::all_of(area_key_name_vars)) |>
        dplyr::mutate(name_based_on_file = clean_cea_names(name_based_on_file)) |>
        dplyr::rename("cea_names" = "name_based_on_file") |>
        dplyr::mutate(meanAHD = cea_average_mahd_vector)

    raster_tibble <- establish_cea_raster_data_tibble(
        raster_values_in_shape_files,
        cea_land_area_hectares_in_shapefile
    )

    # Save the raster tibble out to disk
    saveRDS(raster_tibble, raster_tibble_storage_path)


    # Calculations within areas
    flooded_region_coverage <- pairwise_map_measure_to_longform(
        fraction_not_flooded_in_scenario, "Fraction of area not flooded"
    ) |>
        dplyr::left_join(pairwise_map_measure_to_longform(
            average_flooded_land_height, "Average flooded land height (mAHD)"
        )) |>
        dplyr::left_join(cea_land_area_hectares_in_shapefile) |>
        dplyr::mutate(
            `Hectares flooded` = Hectares * (1 - `Fraction of area not flooded`)
        )

    # Individual CEA flooding fraction across tidal height
    cea_flooding_at_heights <- typical_pointed_ggplot_line_plot(
        flooded_region_coverage,
        "Tidal Height",
        "Fraction of area not flooded",
        "cea_names"
    ) +
        ggplot2::labs(x = "mAHD for max tidal height",
                      colour = 'CEA Names') +
        scale_color_viridis_d()

    flooding_land_scenario_path <- file.path(figures_dir, "flooded_land_scenarios.png")
    ggplot2::ggsave(
        flooding_land_scenario_path,
        cea_flooding_at_heights,
        width = 8
    )
} else {
    # Read the tbble df saved to disk
    raster_tibble <- readRDS(raster_tibble_storage_path)
}

#######################################################################################################
# Use the raster to establish potential grazing land values
#######################################################################################################
full_site_area_mentioned_in_paper <- 86.8

current_grazing_land_ceas <- c("CEA 3", "CEA 4", "CEA 5", "CEA 6")

grazing_site_rea <- raster_tibble |>
  dplyr::filter(cea_names %in% current_grazing_land_ceas) |>
  raster_tibble_area_summary()

grazing_land_scenario_val <- c(total_site_area, 60, grazing_site_rea)
grazing_land_scenario_name <- c(
  "Entire site (79 ha)",
  "Arbitrary (60 ha)",
  "Current (56 ha)"
)

# Setup up paths in advance
reduced_land_section_path <- file.path(processeddata_dir, 'reducing_grazing_land_section.parquet')
enumerated_grazing_land_path <- file.path(processeddata_dir, 'reducing_frazing_land_section.parquet')

if (replot_site_relief_files) {

    # Build a simple reduced section for plotting against some carbon scenarios
    grazing_land_reduced_selection <- tibble::tibble(
        grazing_land_available = grazing_land_scenario_val,
        `Scenario for hectares of usable land` = grazing_land_scenario_name
    ) |>
        dplyr::mutate(fraction_of_land_for_grazing = grazing_land_available / total_site_area) |>
        tidyr::crossing(`Reference grazing revenue per hectare per year` = cattle_revenue_per_hectare_options) |>
        dplyr::mutate(`Annualised Site revenue` = fraction_of_land_for_grazing * `Reference grazing revenue per hectare per year`) |>
        dplyr::mutate(`Tidal range (m)` = 3) # Added as constant for aligned axis in a graph

    arrow::write_parquet(grazing_land_reduced_selection, reduced_land_section_path)

    # Enumerate a lot of possibility for a simple smooth plot + some contour building
    # Enumerate possibilities of grazing land use against
    grazing_land_full_pairings <- tibble::tibble(
        `Productive grazing land (ha)` = seq(0, total_site_area_rounded_up_to, 0.01)
    ) |>
        tidyr::crossing(`Reference grazing revenue per hectare per year` = cattle_revenue_per_hectare_options) |>
        dplyr::mutate(`Fraction of site for grazing` = `Productive grazing land (ha)` / total_site_area) |>
        dplyr::mutate(`Annualised Site revenue ($ / ha / yr)` = `Fraction of site for grazing` * `Reference grazing revenue per hectare per year`) |>
        dplyr::mutate(`Tidal range (m)` = 3) # Added as constant for aligned axis in a graph

    arrow::write_parquet(grazing_land_full_pairings, enumerated_grazing_land_path)
} else {
    grazing_land_reduced_selection <- arrow::read_parquet(reduced_land_section_path)
    grazing_land_full_pairings <- arrow::read_parquet(enumerated_grazing_land_path)

}

########################################################################################################
# Run BlueCAM in various prior belief scenarios to estimate carbon abatement for financial calculations
########################################################################################################
# Set the bluecam sheet target and a mapping from BlueCAM climate specification and BlueCam excel sheet
target_blue_cam_sheet <- "Tropical Monsoon"

bluecam_climate_sheet_to_stpi <- c(
    "Tropical Monsoon" = "Tropical - monsoon",
    "Tropical humid" = "Tropical - humid",
    "Temperate" = "Temperate - with mangroves",
    "Subtropical" = "Subtropical",
    "Arid & Semi arid" = "Arid - semi-arid"
)

stpi_climate <- bluecam_climate_sheet_to_stpi[[target_blue_cam_sheet]]


# Read in the description of the tidal areas
land_use_area_path <- file.path(manualdata_dir, "CEA_LandUseAreas.xlsx")
land_use_area_data <- readxl::read_excel(land_use_area_path)

# Load in the stpi mapping table
stpi_mapping_table_path <- file.path(manualdata_dir, "BlueCAMModellingDocumentTable2.xlsx")
stpi_range_cols <- c("STPI range of Carbon Estimation Area (CEA) - lower",
                     "STPI range of Carbon Estimation Area (CEA) - upper")

stpi_mapping_table_data <- readxl::read_xlsx(stpi_mapping_table_path) |>
    dplyr::mutate(across(
        dplyr::all_of(stpi_range_cols),
        function(x) if_else(abs(x) >= 100, Inf * x, x)
    ))

relevant_stpi_mapping_table <- stpi_mapping_table_data |>
    dplyr::filter(Climate == stpi_climate)


# For each flooding scenario, we'll enter results into BlueCam and record the values
# Open Excel Document using excel interface
blue_cam_model_excel_original_path <- file.path(synthesised_data_dir, "The blue carbon accounting model (BlueCAM).xlsx")
blue_cam_model_excel_copy_path <- stringr::str_replace(blue_cam_model_excel_original_path, ".xlsx", " - copy.xlsx")
file.copy(blue_cam_model_excel_original_path, blue_cam_model_excel_copy_path)



# Tutorial examples for interacting with excel client:
# https://www.r-bloggers.com/2021/07/rdcomclient-read-and-write-excel-and-call-vba-macro-in-r/

# Set constants used in multiple place
# Differs from permanence period
permancence_period_years <- 25
reporting_time_step_years <- 25
number_of_ceas <- land_use_area_data |> nrow()
starting_carbon_abatement_carryover <- 0

# Now
if (reestimate_carbon_sequestration) {
  xlApp <- COMCreate("Excel.Application")
  xlApp[["Visible"]] <- TRUE

  bluecamworkbook <- xlApp$Workbooks()$Open(blue_cam_model_excel_copy_path)
  Sys.sleep(1)

  monsoonEvaluationsWorkSheet <- bluecamworkbook$Worksheets(target_blue_cam_sheet)

  # cea parameter table
  cea_parameter_starting_cell <- c(2, 10) # "J2"
  number_of_cea_cols <- 11
  number_of_cea_rows <- number_of_ceas

  # time period parameter table
  time_period_parameter_starting_cell <- c(2, 1) # "A2"
  number_of_time_rows <- 1
  number_of_time_cols <- 5

  # fuel use parameter table
  fuel_use_starting_cell <- c(2, 7) # "G2"
  number_of_fuel_cols <- 2
  number_of_fuel_rows <- 4


  carryover_starting_cell <- c(2, 41) # "AO2"
  number_of_carryover_cols <- 1
  number_of_carryover_rows <- 1

  # Results output table
  results_starting_cell <- c(2, 43)


  # Greenhouse gases table output
  ghgs_results_starting_cell <- c(2, 22)

  # Vegetation table output
  vegetation_results_starting_cell <- c(2, 31)

  # Soil results output
  soil_results_starting_cell <- c(2, 35)

  # Example template dataframes from the data
  template_cea_parameters_df <- extract_cea_input_data(
      monsoonEvaluationsWorkSheet,
      number_of_ceas)

  template_time_period_parameters_df <- extract_data_frame_values(
      monsoonEvaluationsWorkSheet,
      time_period_parameter_starting_cell,
      number_of_time_cols,
      number_of_time_rows)

  template_fuel_use_df <- extract_data_frame_values(
      monsoonEvaluationsWorkSheet,
      fuel_use_starting_cell,
      number_of_fuel_cols,
      number_of_fuel_rows)

  initial_time_period_df <- template_time_period_parameters_df
  initial_time_period_df[[1, 1]] <- date_string_to_excel_number("2021-07-01")
  initial_time_period_df[[1, 2]] <- date_string_to_excel_number("2046-07-01")
  initial_time_period_df[[1, 3]] <- "25 year permanence period"
  initial_time_period_df[[1, 4]] <- "No"
  initial_time_period_df[[1, 5]] <- 0.1

  # Load CEA information from excel sheet
  starting_cea_data_frame <- convert_manual_cea_data_to_bluecam_format(
      land_use_area_data, template_cea_parameters_df
      ) |>
    dplyr::mutate(`CEA number` = clean_cea_names(`CEA number`))

  # Set up initial worksheet state
  set_up_bluecam_initial_spreadsheet_constants(
    monsoonEvaluationsWorkSheet,
    initial_time_period_df,
    starting_cea_data_frame,
    starting_carbon_abatement_carryover
  )

  # Now iterate over tidal heights
  # Reporting period timestep
  # Step through reporting timesteps
  first_timestep <- FALSE
  scenario_carbon_results <- list()

  for (current_tidal_range in tidal_range_hypotheses) {
    # Copy out the initial dataframe case to use for this loop
    new_estimated_cea_data <- starting_cea_data_frame

    # Calculate the constants used in STPI value
    current_to_actual_tidal_range <- (current_tidal_range - tidal_range_onsite)
    implied_new_actual_ahd_hat <- heighest_astronomical_tide_ahd + current_to_actual_tidal_range / 2
    implied_new_mean_tidal_height <- calculate_mean_tidal_height(implied_new_actual_ahd_hat, current_tidal_range) # Shouldn't change

    first_timestep <- TRUE
    time_period_steps <- seq(from = 0, to = permancence_period_years, by = reporting_time_step_years) |> tail(-1)

    for (time_period_step in time_period_steps) {
      # Create the new cea data with new frames and so-on
      # Updates tidal height, CEA area, New CEA or first, Tidal introduction
      new_estimated_cea_data <- estimate_new_cea_dataframe(
        new_estimated_cea_data,
        current_tidal_range,
        raster_tibble,
        implied_new_mean_tidal_height,
        implied_new_actual_ahd_hat,
        relevant_stpi_mapping_table,
        first_timestep
      )

      total_cea_area_in_scenario <- sum(new_estimated_cea_data$`CEA area (ACEAi) (ha)`)
      # Update the excel spreadsheet
      update_excel_results(monsoonEvaluationsWorkSheet, current_tidal_range, new_estimated_cea_data)

      Sys.sleep(0.1)

      # Extract the results for the current scenario
      scenario_carbon_results[[as.character(current_tidal_range)]] <- extract_all_results_details(
        monsoonEvaluationsWorkSheet,
        number_of_ceas
      ) |>
        lapply(add_cea_extract_loop_constants, current_tidal_range, time_period_step, total_cea_area_in_scenario)

      # Update the CEA to set it for the next year if doing multiple timesteps; CUrrent reporting period becomes the last one and etc
      new_estimated_cea_data <- prepare_for_next_time_step(new_estimated_cea_data, reporting_time_step_years)
    }
  }

  # Close excel now that its no longer needed
  xlApp$quit()
}

# Format overall results now for later carbon assessments:
# There are assumptions involved in using the largest total site area from the largest possible CEAs calculated
# It is essentially assuming it is not possible to arrange an alternate use for land in carbon-emissions us
# i.e we aren't handing partial edge cases where maybe mixed land use is feasible with some land set aside for abatement
# and other bits set aside for livestock.
overall_tidal_results_file_path <- file.path(
  synthesised_data_dir,
  "overall_carbon_sequestration_estimates.xlsx"
)
cea_tidal_results_file_path <- file.path(
  synthesised_data_dir,
  "cea_carbon_sequestration_estimates.xlsx"
)

if (reestimate_carbon_sequestration) {
  overall_results_over_tidal_differences <- extract_and_merge_scenario_subdataset(scenario_carbon_results, "overall") |>
    dplyr::mutate(`Net abatement amount (Ar) (Tonnes CO2e)` = unname(`Net abatement amount (Ar) (Tonnes CO2e)`)) |>
    dplyr::mutate(`Average Tonnes of CO2 abatement per Hectare per Year` = `Net abatement amount (Ar) (Tonnes CO2e)` / total_site_area / permancence_period_years)

  writexl::write_xlsx(overall_results_over_tidal_differences, overall_tidal_results_file_path)

  cea_results_over_tidal_differences <- extract_and_merge_scenario_subdataset(scenario_carbon_results, "cea") |>
    dplyr::left_join(raster_tibble |>
      dplyr::select(-raster_values), by = c("CEA number" = "cea_names")) |>
    dplyr::rename("Original DEM CEA area (Hectares)" = "Hectares") |>
    dplyr::mutate(`Measurement per Hectare per Year` = `Measurement` / permancence_period_years)

  writexl::write_xlsx(cea_results_over_tidal_differences, cea_tidal_results_file_path)
} else {
  overall_results_over_tidal_differences <- readxl::read_xlsx(overall_tidal_results_file_path)
  cea_results_over_tidal_differences <- readxl::read_xlsx(cea_tidal_results_file_path)
}

if (replot_carbon_sequestration) {
    cea_total_carbon_abatement_series <- cea_results_over_tidal_differences |>
      dplyr::filter(measure_type == "Total") |>
      dplyr::group_by(`CEA number`, `Tidal range`) |>
      dplyr::summarise(
        `Total carbon abated` = sum(Measurement),
        `Average Tonnes of CO2 abatement per Hectare per Year` = sum(Measurement) / max(`Original DEM CEA area (Hectares)`)
      )

    # Plot the results for each overall against the tidal range
    subscripted_nett_label <- expression(paste("Net abatement amount (tonnes CO"[2], "e)"))
    overall_carbon_abatement_expectations <- overall_results_over_tidal_differences |>
      typical_pointed_ggplot_line_plot(
        "Tidal range",
        "Net abatement amount (Ar) (Tonnes CO2e)",
        NULL
      ) +
      scale_color_grey() +
      theme(legend.position = "none") +
      ggplot2::labs(
        x = "Tidal range (m)",
        y = subscripted_nett_label
      )

    subscripted_average_label <- expression(paste("Average abatement (tonnes CO"[2], "e ha"^-1, " yr"^-1, ")"))
    overall_carbon_abatement_expectations_per_area_year <- overall_results_over_tidal_differences |>
      typical_pointed_ggplot_line_plot(
        "Tidal range",
        "Average Tonnes of CO2 abatement per Hectare per Year",
        NULL
      ) +
      ggplot2::labs(
        x = "Tidal range (m)",
        y = subscripted_average_label
      ) +
      scale_color_grey() +
      theme(legend.position = "none")

    # Calculate "break-even" carbon rates needed to match the grazing land rates
    # Calculate full sequence
    grazing_land_reduced_rate <- grazing_land_reduced_selection |>
      dplyr::filter(
        `Scenario for hectares of usable land` == "Current (56 ha)",
        `Reference grazing revenue per hectare per year` == cattle_revenue_per_hectare_per_year
      ) |>
      dplyr::pull(`Annualised Site revenue`)

    break_even_carbon_price_for_scenario <- overall_results_over_tidal_differences |>
      dplyr::mutate(`ACCU break even price` = grazing_land_reduced_rate / `Average Tonnes of CO2 abatement per Hectare per Year`) |>
      dplyr::select(`Tidal range`, `Average Tonnes of CO2 abatement per Hectare per Year`, `ACCU break even price`)

    # Combined the graphs to scaled
    combined_carbon_plots <- cowplot::plot_grid(
        overall_carbon_abatement_expectations,
        overall_carbon_abatement_expectations_per_area_year,
        labels = c("a.", "b."),
        label_size = 16, label_y = 1.01
    )

    combined_carbon_plot_path <- file.path(figures_dir, "carbon_sink_estimates.png")
    cowplot::save_plot(
        combined_carbon_plot_path,
        combined_carbon_plots,
        base_width = 13,
        base_height = 7
    )

    # CEA level results
    cea_carbon_abatement_expectations <- cea_total_carbon_abatement_series |>
        typical_pointed_ggplot_line_plot("Tidal range", "Total carbon abated", "CEA number") +
        ggplot2::labs(x = "Tidal range (m)", y = subscripted_nett_label) +
        ggplot2::scale_colour_viridis_d() +
        ggplot2::theme(legend.title = element_blank())

    cea_carbon_abatement_path <- file.path(figures_dir, "cea_level_carbon_estimates.png")
    ggsave(
        cea_carbon_abatement_path,
        cea_carbon_abatement_expectations,
        width = 10
    )

    # Possible ACCU prices values
    tidal_range_carbon_price_scenarios <- calculate_scenario_annual_costings(
      overall_results_over_tidal_differences, accu_considered_matrix_prices
    )

    # Heatmap of the total land value
    colour_scale_limits_viridis_c <- tidal_range_carbon_price_scenarios$`Annualised Site revenue` |>
      min_max_vec()

    # Small vertical lines intercept
    onsite_tidal_ranges <- tibble::tribble(
      ~`Tidal measurement`, ~`Tidal range`,
      "Nearest port", 3.58,
      "Measured tides", 1.859
    )

    # Label expressions
    land_revenue_fill_expression <- expression(paste("Revenue ($ ha"^-1, " yr"^-1, ")"))
    carbon_price_expression <- expression(paste("Carbon price ($ AUD tCO"[2], "e"^-1, ")"))

    # Build a potential contours dataset
    land_revenue_steps_by_hectares <- grazing_land_full_pairings |>
      dplyr::filter((`Productive grazing land (ha)` %% 20) == 0,
                    `Reference grazing revenue per hectare per year` == cattle_revenue_per_hectare) |>
      dplyr::pull(`Annualised Site revenue ($ / ha / yr)`)

    land_revenue_by_steps <- seq(0, 1000, 200)

    carbon_site_revenue_plot <- build_underlying_heatmap(
      tidal_range_carbon_price_scenarios,
      "Tidal range", "Carbon price per tonne", "Annualised Site revenue"
    ) +
      ggplot2::labs(
        x = "Tidal range (m)", y = carbon_price_expression,
        fill = land_revenue_fill_expression
      ) +
        ggplot2::scale_fill_viridis_c(limits = colour_scale_limits_viridis_c) +
        ggplot2::geom_vline(
            data = onsite_tidal_ranges,
            aes(xintercept = `Tidal range`, linetype = `Tidal measurement`),
            colour = "black",
            lwd = 1.1
      )

    carbon_site_tiles_with_contour <- carbon_site_revenue_plot +
        geom_contour(aes(z = `Annualised Site revenue`),
                     breaks = land_revenue_by_steps,
                     lwd = 1.1,
                     colour = "grey"
                     ) +
        directlabels::geom_dl(aes(label = ..level.., z = `Annualised Site revenue`),
                              method = "bottom.pieces",
                              stat = "contour", breaks = land_revenue_by_steps,
                              hjust = 0.5
                              )
    carbon_cattle_revenue_heatmap_path <- file.path(figures_dir, "tidal_to_carbon_revenue_with_contours.png")
    ggplot2::ggsave(carbon_cattle_revenue_heatmap_path, carbon_site_tiles_with_contour)

    cattle_revenue_plot <- grazing_land_reduced_selection |>
        dplyr::filter(`Reference grazing revenue per hectare per year` == cattle_revenue_per_hectare_per_year) |>
        dplyr::mutate(
            `Scenario for hectares of usable land` = factor(
                `Scenario for hectares of usable land`,
                levels = grazing_land_scenario_name)) |>
        ggplot2::ggplot(aes(x = `Tidal range (m)`, y = `Scenario for hectares of usable land`, fill = `Annualised Site revenue`)) +
        ggplot2::geom_tile(colour = "black") +
        ggplot2::theme_cowplot() +
        ggplot2::labs(
            y = "Grazing land scenario",
            fill = land_revenue_fill_expression,
            x = ""
            ) + # Apply scale_colour_brewer function
        ggplot2::scale_fill_viridis_c(limits = colour_scale_limits_viridis_c) +
        ggplot2::theme(
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank()
            )

    combined_revenue_plots <- plot_grid(
      cattle_revenue_plot + ggplot2::theme(legend.position = "none"),
      carbon_site_revenue_plot,
      labels = c("a.", "b. "),
      rel_widths = c(0.4, 1),
      label_y = 1.01
    )

    combined_revenue_plot_path <- file.path(figures_dir, "heatplot_of_accu_tidal_range_land_value_pairs.png")
    save_plot(
        combined_revenue_plot_path,
        combined_revenue_plots,
        base_width = 13,
        base_height = 8
    )

}
########################################################################################
# Evaluation of the perfect value of information and sample value of information       #
########################################################################################
# Investigate BNStruct for Bayesian network construction and analysis in the future
# Classic discrete calculation of the models
tidal_range_trunc_norm <- truncated_pnorm_func_generator(tidal_range_hypotheses_bounds)
inundation_bound_trunc_norm <- truncated_pnorm_func_generator(inundation_range_of_values)

calc_tidal_range_belief <- discretized_scenario_belief_generator(
  tidal_range_hypotheses,
  step_size_in_tidal_height,
  tidal_range_trunc_norm
)

calc_inundation_range_belief <- discretized_scenario_belief_generator(
  inundation_value_sequence,
  inundation_step_size,
  inundation_bound_trunc_norm
)

# Dimensions for various scenarios
tidal_range_actual_array_dim <- c(1, n_tidal_range_opt)
inundation_actual_array_dim <- c(n_inundation_opt, 1)

sample_dist_array_dim <- c(n_inundation_opt, n_tidal_range_opt, 1, 1)
actual_dist_array_dim <- c(1, 1, n_inundation_opt, n_tidal_range_opt)

full_dist_array_dim <- c(n_inundation_opt, n_tidal_range_opt, n_inundation_opt, n_tidal_range_opt)

full_indundation_array_dim <- c(n_inundation_opt, 1, n_inundation_opt, 1)
full_tidal_range_array_dim <- c(1, n_tidal_range_opt, 1, n_tidal_range_opt)

# Note that generic apply functions for the right margins in different scenarios in defined functions
# apply_and_retain_actual_dims
# apply_and_retain_sample_dims

# Prior beliefs
# Tidal range
prior_tidal_range_scale <- 1
tidal_range_prior_distribution <- calc_tidal_range_belief(tidal_range_onsite, prior_tidal_range_scale)

# Indundation conditional on the tidal range (matrix)
dem_inundation_estimates <- overall_results_over_tidal_differences |>
  dplyr::select(`Tidal range`, `Total hectares in CEAs`) |>
  dplyr::arrange((`Tidal range`))

prior_indundation_scale <- 3
inundation_range_prior_distribution <- sapply(
  dem_inundation_estimates$`Total hectares in CEAs`,
  function(x) calc_inundation_range_belief(x, prior_indundation_scale)
)

# Combine the two for the joint prior distribution
prior_joint_distribution <- (inundation_range_prior_distribution *
                                 tide_vector_to_joint_array(
                                     tidal_range_prior_distribution,
                                     tidal_range_actual_array_dim,
                                     n_inundation_opt))

prior_inundation_dist <- prior_joint_distribution |> rowSums()

# TEsts for sense
recovers_tidal_range_prior <- all.equal(prior_joint_distribution |> colSums(), tidal_range_prior_distribution)

# NUMERIC ISSUES MIGHT BE A LARGER FACTOR
recovers_inundation_conditional <- all.equal(
  replace_matrix_nans(prior_joint_distribution / tide_vector_to_joint_array(tidal_range_prior_distribution, tidal_range_actual_array_dim, n_inundation_opt)),
  inundation_range_prior_distribution
)

# Plot the prior beliefs
tidal_range_prior_plot <- simple_belief_df(tidal_range_prior_distribution, tidal_range_hypotheses) |>
  dplyr::rename("Tidal range (m)" = "target_value") |>
  simple_univariate_belief_bar("Tidal range (m)", "belief")

inundation_prior_plot <- simple_belief_df(prior_inundation_dist, inundation_value_sequence) |>
  dplyr::rename("Inundated land (Hectares)" = "target_value") |>
  simple_univariate_belief_bar("Inundated land (Hectares)", "belief")


# Joint distribution heat map
joint_prior_plot <- joint_straight_to_heatmap(
  prior_joint_distribution,
  inundation_value_sequence,
  tidal_range_hypotheses,
  "Inundated land (Hectares)",
  "Tidal range (m)"
)

# Condition distribution heat map
conditional_prior_plot <- joint_straight_to_heatmap(
  inundation_range_prior_distribution,
  inundation_value_sequence,
  tidal_range_hypotheses,
  "Inundated land (Hectares)",
  "Tidal range (m)"
)

# Combine all of these to show the full prior distributions
combined_prior_belief_plots <- plot_grid(
  tidal_range_prior_plot, conditional_prior_plot,
  inundation_prior_plot, joint_prior_plot,
  labels = c(
    "A) Specified tidal range prior",
    "B) Specified conditional inundation prior",
    "C) Implied inundation prior",
    "D) Implied joint distribution prior"
  ),
  label_y = 1.01, label_x = -0.1,
  nrow = 2
)

combined_prior_beliefs_path <- file.path(figures_dir, "prior_beliefs.png")
save_plot(
    combined_prior_beliefs_path,
    combined_prior_belief_plots,
    base_height = 6, base_width = 11
)

# Three scenarios specify outcomes
sampling_scenario_order <- c(
  "Low complexity",
  "Moderate complexity",
  "High complexity",
  "high tidal uncertainty, confident inundation",
  "low tidal uncertainty, uncertain inundation"
)

scenario_tidal_measurement_bias <- c(1, 0, 0, 0, 0)
scenario_tidal_measurement_uncertainty <- c(0.7, 0.2, 0.2, 0.5, 0.1)
scenario_inundation_bias <- c(0, 0, 0, 0, 0)
scenario_inundation_uncertainty <- c(2, 2, 1, 1, 4)

# Prepare generic values based on joint distribution
# Key assumption - given tidal range implies the inundation as its linked to land height
# did this as linear weighting of land flagged as usable (i.e not flooded) in each case
# It's assuming deviation in each case is uniformly random which is inaccurate
# overall site area as 78.87309
# grazing area as 56.16916
# reference cattle revenue per year as 976.8
complete_scenario_data_path <- file.path(synthesised_data_dir, "combined_scenario_grazing_comparison.xlsx")
if (recalculate_voi){

    actual_cea_values_in_tidal_scenario <- (
        overall_results_over_tidal_differences$`Total hectares in CEAs` |>
            array(dim = tidal_range_actual_array_dim))

    inundation_sequence_matrix <- (inundation_value_sequence |>
      array(dim = inundation_actual_array_dim))

    carbon_abatement_in_tidal_scenario <- unname(overall_results_over_tidal_differences$`Net abatement amount (Ar) (Tonnes CO2e)`)

    reweighting_factors_of_cea_weight <- (1 / actual_cea_values_in_tidal_scenario[one_array(n_inundation_opt), ]) * inundation_sequence_matrix[, one_array(n_tidal_range_opt)]

    fraction_of_site_flooded <- pmin(inundation_sequence_matrix / total_site_area, 1)
    fraction_site_site_not_flooded <- 1 - fraction_of_site_flooded

    # Combined the reweighted carbon sinks with the scenario reweights
    reweighted_scenario_carbon_sink <- array(carbon_abatement_in_tidal_scenario, dim = tidal_range_actual_array_dim)[one_array(n_inundation_opt), ] * reweighting_factors_of_cea_weight

    # Uses theoretical inundation to calculate the cattle revenue inputs and so-on
    decision_ordering <- c("Use for grazing land", "Use for carbon sequestration")

    cattle_revenue_scenario_results <- list()
    cattle_prior_decision_payoff <- list()
    for (revenue_option in cattle_revenue_per_hectare_options) {
      cattle_revenue_key <- paste(revenue_option)

      cattle_revenue_scenario_results[[cattle_revenue_key]] <- list()

      # calculate the values needed
      cattle_inundation_scenario_revenue <- (fraction_site_site_not_flooded *
        grazing_site_rea *
        revenue_option)

      cattle_full_scenario_revenue_matrix <- array(cattle_inundation_scenario_revenue,
        dim = inundation_actual_array_dim
      )[, one_array(n_tidal_range_opt)]

      cattle_revenue_joint_decision_payoff <- sum(cattle_full_scenario_revenue_matrix * prior_joint_distribution)

      expected_cattle_payoff_in_every_sample_scenario <- array(
        cattle_full_scenario_revenue_matrix,
        dim = actual_dist_array_dim
      )[one_array(n_inundation_opt), one_array(n_tidal_range_opt), , ]

      # Save the results out
      cattle_revenue_scenario_results[[cattle_revenue_key]][["scenario_revenue"]] <- (
          cattle_inundation_scenario_revenue
      )

      cattle_revenue_scenario_results[[cattle_revenue_key]][["revenue_matrix"]] <- (
          cattle_full_scenario_revenue_matrix
      )

      cattle_revenue_scenario_results[[cattle_revenue_key]][["joint_payoff"]] <- (
          cattle_revenue_joint_decision_payoff
      )

      cattle_revenue_scenario_results[[cattle_revenue_key]][["scenario_payoff"]] <- (
        expected_cattle_payoff_in_every_sample_scenario
      )

      cattle_prior_decision_payoff[[cattle_revenue_key]] <- cattle_revenue_joint_decision_payoff
    }

    # Hopefully belows adjustment will cut runtime by >30% as would move one expensive calculation out of innermost loop
    latest_real_carbon_price_estimate <- 33.8
    accu_price_sequence_with_latest <- sort(c(accu_considered_matrix_prices, latest_real_carbon_price_estimate))

    # Calculate expected payoff for carbon in prior belief scenario at carbon price
    carbon_decision_payoff <- list()
    carbon_scenario_revenue <- list()
    carbon_scenario_revenue_sample_dist <- list()
    for (current_carbon_price in accu_price_sequence_with_latest) {
        # Key for storing results
        current_price_key <- paste(current_carbon_price)
        carbon_revenue_for_scenario <- reweighted_scenario_carbon_sink * current_carbon_price / permancence_period_years

        carbon_payoff_at_price <- sum(prior_joint_distribution * carbon_revenue_for_scenario)

        carbon_decision_payoff[[current_price_key]] <- carbon_payoff_at_price
        carbon_scenario_revenue[[current_price_key]] <- carbon_revenue_for_scenario
        carbon_scenario_revenue_sample_dist[[current_price_key]] <- (
            array(carbon_revenue_for_scenario,
                  dim = c(1, 1, dim(carbon_revenue_for_scenario)))
            [one_array(n_inundation_opt), one_array(n_tidal_range_opt), , ])
    }

    # Calculate expected payoff for cattle in prior belief scenario at cattle revenue estimate
    # Done slightly earlier with other cattle payoff calcs

    # Calculate the value of perfect information across each pairing of possible carbon pricing + cattle pricing
    decision_summary_in_price_scenario <- list()
    best_prior_decision_in_scenario <- list()
    evpi_in_scenario <- list()
    best_prior_payoff_in_scenario <- list()

    for (current_carbon_price in accu_price_sequence_with_latest) {
        # Key for storing results
        current_price_key <- paste(current_carbon_price)

        # Pull out pre-calculated carbon scenario data
        carbon_revenue_for_scenario <- carbon_scenario_revenue[[current_price_key]]
        carbon_prior_payoff <- carbon_decision_payoff[[current_price_key]]

        # Set up list for extra decision summary vairables
        decision_summary_in_price_scenario[[current_price_key]] <- list()
        best_prior_decision_in_scenario[[current_price_key]] <- list()
        evpi_in_scenario[[current_price_key]] <- list()
        best_prior_payoff_in_scenario[[current_price_key]] <- list()


        for (revenue_option in cattle_revenue_per_hectare_options) {
            cattle_revenue_key <- paste(revenue_option)

            # Log a result
            flog.info(glue::glue("Starting pre-calc for: ${current_price_key} ACCU, ${cattle_revenue_key} Cattle"))

            # Pull out the pre-calculated cattle scenario data
            cattle_full_scenario_revenue_matrix <- cattle_revenue_scenario_results[[cattle_revenue_key]][["revenue_matrix"]]

            # Could separate out the cattle payment loop and carbon payment loop as well for prior payments
            full_reveneue_pricing_comparisons <- array(
                c(
                    cattle_full_scenario_revenue_matrix,
                    carbon_revenue_for_scenario
                ),
                dim = c(dim(cattle_full_scenario_revenue_matrix), 2)
            )

            # Expected Decision
            cattle_prior_payoff <- cattle_prior_decision_payoff[[cattle_revenue_key]]

            # break-even price comparison
            break_even_carbon_price_in_prior <- (cattle_prior_payoff / carbon_prior_payoff * current_carbon_price)

            # Value of perfect information
            # Could move EVPI outside of the scenario loops
            best_decision_payoff <- (apply(
                full_reveneue_pricing_comparisons,
                c(1, 2), max
            ) * prior_joint_distribution) |> sum()
            best_decision_index_group <- apply(full_reveneue_pricing_comparisons, c(1, 2), function(x) decision_ordering[which.max(x)])

            # Raw split of decisions + prob_weighted split of decisions
            best_decision_scenario_count <- data.frame(
                best_decision = c(best_decision_index_group),
                decision_weight = c(prior_joint_distribution),
                count = 1 / length(best_decision_index_group)
            )

            # Finalise outputs for key metrics
            combined_prior_payoffs_vec <- c(cattle_prior_payoff, carbon_prior_payoff)
            prior_value_best_payoff <- max(combined_prior_payoffs_vec)
            value_of_perfect_information <- best_decision_payoff - prior_value_best_payoff
            best_prior_decision <- decision_ordering[which.max(combined_prior_payoffs_vec)]

            # Store the various output values from the calculations - regression test showed it's fine
            decision_summary_in_price_scenario[[current_price_key]][[cattle_revenue_key]] <- (
                best_decision_scenario_count |>
                    dplyr::group_by(best_decision) |>
                    dplyr::summarise(
                        `Prob weighted decision rate` = sum(decision_weight),
                        `Raw scenario decision rate` = sum(count)
                    ) |>
                    dplyr::mutate(
                        `Carbon price ($ / tonne)` = current_carbon_price,
                        `Reference cattle revenue` = revenue_option,
                        `Break even carbon price on prior` = break_even_carbon_price_in_prior
                    )
            )

            best_prior_decision_in_scenario[[current_price_key]][[cattle_revenue_key]] <- best_prior_decision
            evpi_in_scenario[[current_price_key]][[cattle_revenue_key]] <- value_of_perfect_information
            best_prior_payoff_in_scenario[[current_price_key]][[cattle_revenue_key]] <- prior_value_best_payoff
        }
    }


    # Go into the samples then to work out sample information


    # Actual outcome determines payoff not sample estimate -> cast dimension to actual and then replicate across all sample values
    # loop for one specific value
    scenario_key_graphs <- list()
    scenario_key_data <- list()

    for (scenario_id in (seq(sampling_scenario_order))) {
      scenario_name_key <- sampling_scenario_order[scenario_id]

      cur_tidal_measurement_bias <- scenario_tidal_measurement_bias[scenario_id]
      cur_tidal_measurement_uncertainty <- scenario_tidal_measurement_uncertainty[scenario_id]
      cur_inundation_bias <- scenario_inundation_bias[scenario_id]
      cur_inundation_uncertainty <- scenario_inundation_uncertainty[scenario_id]

      # Get the two condition sample matrices
      sample_tidal_beliefs_given_tidal_actual <- sapply(
        tidal_range_hypotheses,
        function(x) {
          calc_tidal_range_belief(
            x + cur_tidal_measurement_bias,
            cur_tidal_measurement_uncertainty
          )
        }
      )

      sample_inundation_beliefs_given_inundation_actual <- sapply(
        inundation_value_sequence,
        function(x) {
          calc_inundation_range_belief(
            x + cur_inundation_bias,
            cur_inundation_uncertainty
          )
        }
      )

      # Plot both as heat maps
      tidal_range_sample_heatmap <- joint_straight_to_heatmap(
        sample_tidal_beliefs_given_tidal_actual,
        tidal_range_hypotheses,
        tidal_range_hypotheses,
        "Sampled tidal range (m)",
        "Actual tidal range (m)"
      ) +
        geom_abline(intercept = 0, slope = 1, colour = "red")

      inundation_sample_heatmap <- joint_straight_to_heatmap(
        sample_inundation_beliefs_given_inundation_actual,
        inundation_value_sequence,
        inundation_value_sequence,
        "Sampled inundation (hectares)",
        "Actual inundation (hectares)"
      ) +
        geom_abline(intercept = 0, slope = 1, colour = "red")

      sample_distributions_of_outcomes <- plot_grid(tidal_range_sample_heatmap, inundation_sample_heatmap,
        labels = c(
          "A) Sample distribution given actual tidal range",
          "B) Sample distribution of inundation given actual inundation"
        )
      )

      scenario_joint_heatmap_path <- file.path(
          figures_dir,
          paste(scenario_name_key, "conditional_sample_distribution.png", sep = "_"))
      save_plot(scenario_joint_heatmap_path, sample_distributions_of_outcomes)


      # Now expand out every inundation, tidal range pair and then our belief from that join distribution
      # 83 121  83 121 - swapping over to array logic
      # Build the sample distribution join
      inundation_belief_reshape <- sample_inundation_beliefs_given_inundation_actual |>
        array(dim = full_indundation_array_dim)

      tidal_range_belief_reshape <- sample_tidal_beliefs_given_tidal_actual |>
        array(dim = full_tidal_range_array_dim)

      # Dependent on assumption from the prior belief
      full_conditioned_sample_distribution <- (
          inundation_belief_reshape[,
                                    one_array(n_tidal_range_opt),
                                    ,
                                    one_array(n_tidal_range_opt)] *

          tidal_range_belief_reshape[one_array(n_inundation_opt),
                                     ,
                                     one_array(n_inundation_opt),
                                     ])

      # Should all be ones for this test
      # conditional_sample_distribution_test <- full_conditioned_sample_distribution |> apply(MARGIN=c(3, 4), sum)
      # all.equal(conditional_sample_distribution_test, array(1, dim=c(n_inundation_opt, n_tidal_range_opt)))
      full_joint_sample_actual_distribution <- (full_conditioned_sample_distribution *
                                                    array(prior_joint_distribution, dim = actual_dist_array_dim)[
                                                        one_array(n_inundation_opt), one_array(n_tidal_range_opt), , ])

      # Recover the prior distribution joint values - should pass
      # all.equal(prior_joint_distribution, apply(full_joint_sample_actual_distribution, MARGIN=c(3, 4), sum))

      # Recover's actual joint

      # Work out the chance of seeing some sample result for inundation and tidal range
      joint_sample_distribution_array <- full_joint_sample_actual_distribution |>
        apply_and_retain_sample_dims(sum) |>
        array(dim = sample_dist_array_dim)

      implied_sample_tidal_range_prob <- joint_sample_distribution_array |> apply(MARGIN = c(2), FUN = sum)
      implied_inundation_tidal_range_prob <- joint_sample_distribution_array |> apply(MARGIN = c(1), FUN = sum)

      sample_conditional_contrast <- joint_sample_distribution_array[, , 1, 1] /
        array(implied_sample_tidal_range_prob, dim = tidal_range_actual_array_dim)[one_array(n_inundation_opt), ]

      # Some nice sample distribution plots
      expected_sample_results_joint_heatmap <- joint_straight_to_heatmap(
        joint_sample_distribution_array[, , 1, 1],
        inundation_value_sequence,
        tidal_range_hypotheses,
        "Sampled inundation (hectares)",
        "Sampled Tidal Height"
      )

      expected_cond_sample_results_joint_heatmap <- joint_straight_to_heatmap(
        sample_conditional_contrast,
        inundation_value_sequence,
        tidal_range_hypotheses,
        "Sampled inundation (hectares)",
        "Sampled Tidal Height"
      )


      inundation_estimate_plot <- simple_belief_df(
        joint_sample_distribution_array |> apply(MARGIN = c(1), FUN = sum),
        inundation_value_sequence
      ) |>
        dplyr::rename("Estimated inundated land (Hectares)" = "target_value") |>
        simple_univariate_belief_bar("Estimated inundated land (Hectares)", "belief")

      tidal_range_estimate_plot <- simple_belief_df(
        joint_sample_distribution_array |> apply(MARGIN = c(2), FUN = sum),
        tidal_range_hypotheses
      ) |>
        dplyr::rename("Estimated tidal range (m)" = "target_value") |>
        simple_univariate_belief_bar("Estimated tidal range (m)", "belief")

      sample_distributions_of_outcomes <- cowplot::plot_grid(tidal_range_sample_heatmap, inundation_sample_heatmap,
        tidal_range_estimate_plot, inundation_estimate_plot,
        expected_sample_results_joint_heatmap,
        expected_cond_sample_results_joint_heatmap,
        labels = c(
          "A) Tidal: Sample given actual",
          "B) Inundation: Sample given actual",
          "C) Tidal: sample dist",
          "D) Inundation: Sample dist",
          "E) Tidal+Inundation: Joint sample dist",
          "F) Indundation+Tidal: Sample given sample"
        ),
        ncol = 2,
        label_size = 16,
        label_y = 1.02,
        label_x = -0.1,
        hjust = -0.5
      )

      implied_joint_sample_distribution_path <- file.path(
          figures_dir,
          paste(scenario_name_key, "sample_distribution_summary.png", sep = "_")
          )

      save_plot(
          implied_joint_sample_distribution_path,
          sample_distributions_of_outcomes,
          base_height = 9, base_width = 12
      )

      # Combined prior sample
      posterior_probability_distribution <- replace_matrix_nans(
          full_joint_sample_actual_distribution /
              joint_sample_distribution_array[, , one_array(n_inundation_opt), one_array(n_tidal_range_opt)])

      # Check all conditional distributions sum to 1
      # all.equal(posterior_probability_distribution |> apply(MARGIN=c(1, 2), FUN=sum), array(1, dim=c(n_inundation_opt, n_tidal_range_opt)))

      # For every theoretical ACCU price and cattle revenue price, calculate utility, EVPI, and EVSI

      # Reshape sample joint distribution
      reshaped_joint_sample_distribution <- array(joint_sample_distribution_array, dim = sample_dist_array_dim)
      complete_full_reshaped_joint_sample_dist_array <- reshaped_joint_sample_distribution[, , one_array(n_inundation_opt), one_array(n_tidal_range_opt)]

      # Use the calculated distributions to now do all of the value of information estimates
      scenario_key_metrics <- list()

      flog.info(glue::glue("Starting calculations for against all payoff options"))
      for (current_carbon_price in accu_price_sequence_with_latest) {
        flog.info(glue::glue("Starting calculations for ${current_price_key}ACCU"))
        # Key for storing results
        current_price_key <- paste(current_carbon_price)

        # Expand out the carbon revenue to match the sample scenario shape
        carbon_payoff_sample_scenarios <- carbon_scenario_revenue_sample_dist[[current_price_key]]

        # Add the next layer of access to my lists
        scenario_key_metrics[[current_price_key]] <- list()
        for (cattle_revenue_per_hectare in cattle_revenue_per_hectare_options) {
          # Key for storing results
          cattle_revenue_key <- paste(cattle_revenue_per_hectare)

          # Pull out the pre-calculated cattle scenario data
          expected_cattle_payoff_in_every_sample_scenario <- cattle_revenue_scenario_results[[cattle_revenue_key]][["scenario_payoff"]]

          # best prior decision payoff
          best_prior_decision_payoff <- best_prior_payoff_in_scenario[[current_price_key]][[cattle_revenue_key]]

          # Do value of sample information calculations
          # For each sample scenario, best decision after calculations
          carbon_sample_scenario_expected_payoff <- (
              (carbon_payoff_sample_scenarios * posterior_probability_distribution) |>
                  apply(c(1, 2), sum)
              )
          cattle_sample_scenario_expected_payoff <- (
              (expected_cattle_payoff_in_every_sample_scenario * posterior_probability_distribution) |>
                  apply(c(1, 2), sum))

          sample_overall_payoff_across_multiple_samples <- (
              (pmax(carbon_sample_scenario_expected_payoff, cattle_sample_scenario_expected_payoff) *
                   joint_sample_distribution_array[, , 1, 1]) |>
                  sum())

          expected_value_of_sample_information <- sample_overall_payoff_across_multiple_samples - best_prior_decision_payoff

          # Store the final values of the calculations
          key_voi_metrics_for_scenario <- tibble(
            `Cattle revenue ($ / He / yr)` = cattle_revenue_per_hectare,
            `Carbon price ($ / tonne)` = current_carbon_price,
            `Best Prior decision landuse` = best_prior_decision_in_scenario[[current_price_key]][[cattle_revenue_key]],
            `Expected annualised payoff on prior decision` = best_prior_decision_payoff,
            `Value of perfect information` = evpi_in_scenario[[current_price_key]][[cattle_revenue_key]],
            `Value of sample information` = expected_value_of_sample_information,
            `Sampling Scenario` = scenario_name_key
          )

    #       scenario_key_metrics_cached used to key  a value for a pre-post check
          scenario_key_metrics[[current_price_key]][[cattle_revenue_key]] <- (
            key_voi_metrics_for_scenario
          )
        }
      }

      # Combine some results for each pricing scenario now
      combined_scenario_results <- scenario_key_metrics |>
        lapply(dplyr::bind_rows) |>
        dplyr::bind_rows()

      # Visualise the combined results
      colour_scale_limits_viridis_c <- tidal_range_carbon_price_scenarios$`Annualised Site revenue` |>
        min_max_vec()

      value_of_information_scale_bounds <- c(combined_scenario_results$`Value of perfect information`, combined_scenario_results$`Value of sample information`, 0) |>
        min_max_vec()

      long_form_value_of_information <- combined_scenario_results |>
        dplyr::select(
          `Carbon price ($ / tonne)`,
          `Value of perfect information`,
          `Value of sample information`
        ) |>
        pivot_longer(-`Carbon price ($ / tonne)`, names_to = "Measure", values_to = "Value")

      voi_heatmap <- long_form_value_of_information |>
        ggplot2::ggplot(aes(x = `Carbon price ($ / tonne)`, y = Measure, fill = Value)) +
          ggplot2::geom_tile(colour = "Black") +
          ggplot2::theme_cowplot() +
          ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 7), labels = scales::number_format()) +
          ggplot2::scale_fill_viridis_c()

      scenario_voi_heatmap_path <- file.path(figures_dir, paste(scenario_name_key, "heatmap_of_voi_values.png", sep = "_"))
      ggplot2::ggsave(scenario_voi_heatmap_path, voi_heatmap)

      combined_scenario_results_output_path <- file.path(
          synthesised_data_dir,
          paste(scenario_name_key, "event_scenarios_first_case.xlsx", sep = "_"))

      writexl::write_xlsx(
        combined_scenario_results,
        combined_scenario_results_output_path
      )

      # Share out the results
      scenario_key_data[[scenario_name_key]] <- combined_scenario_results
      scenario_key_graphs[[scenario_name_key]] <- voi_heatmap
    }

    # Combine and plot full scenario shared results
    # Payoff in and certainty of decision in every scenario
    # "Middle complexity" -> "Moderate complexity"
    full_scenario_voi_data <- scenario_key_data |>
      bind_rows()

    full_scenario_voi_data |>
      writexl::write_xlsx(
          complete_scenario_data_path
      )

    } else {
    full_scenario_voi_data <- readxl::read_excel(
        complete_scenario_data_path
    )
}

if (redo_voi_outcome_plots){
    #
    perfect_information_long_form <- full_scenario_voi_data |>
        dplyr::filter(`Cattle revenue ($ / He / yr)` == cattle_revenue_per_hectare_per_year) |>
        dplyr::select(
            `Carbon price ($ / tonne)`,
            `Value of perfect information`
        ) |>
        tidyr::pivot_longer(`Value of perfect information`, names_to = "Sampling Scenario", values_to = "Financial Value ($)")

    sample_scenario_long_form <- full_scenario_voi_data |>
        dplyr::filter(`Cattle revenue ($ / He / yr)` == cattle_revenue_per_hectare_per_year) |>
        dplyr::select(
            `Carbon price ($ / tonne)`,
            `Value of sample information`,
            `Sampling Scenario`
        ) |>
        dplyr::mutate(`Sampling Scenario` = paste("Value of ", `Sampling Scenario`, " sample information", sep = "")) |>
        dplyr::rename("Financial Value ($)" = "Value of sample information")


    voi_cases_to_exclude <- c("high tidal uncertainty, confident inundation", "low tidal uncertainty, uncertain inundation")

    combined_cases_scenario_evaluation <- dplyr::bind_rows(
        sample_scenario_long_form,
        perfect_information_long_form
    ) |>
        dplyr::mutate(`Measure type` = case_when(str_detect(`Sampling Scenario`, "sample") ~ "Sample", TRUE ~ "Perfect"))


    # Gross way to do this, should have just changed it earlier
    combined_cases_scenario_evaluation_prettier <- combined_cases_scenario_evaluation |>
        dplyr::mutate(
            `Sampling Scenario` =
                str_replace(`Sampling Scenario`, "Value of ", "")
        ) |>
        dplyr::mutate(
            `Sampling Scenario` =
                str_replace(`Sampling Scenario`, " sample information", "")
        ) |>
        dplyr::filter(!(`Sampling Scenario` %in% voi_cases_to_exclude))

    value_per_year_expression <- expression(paste("Value of information ($ yr"^-1, ")"))

    value_of_information_plot_from_results <- combined_cases_scenario_evaluation_prettier |>
        ggplot2::ggplot(ggplot2::aes(
            x = `Carbon price ($ / tonne)`,
            y = `Financial Value ($)`,
            color = `Sampling Scenario`
        )) +
        ggplot2::geom_point(size = 2.4) +
        labs(x = carbon_price_expression, y = value_per_year_expression, colour = "Scenario") +
        ggplot2::geom_line(lwd = 1.1) +
        cowplot::theme_cowplot() +
        ggplot2::scale_x_continuous(
            breaks = scales::pretty_breaks(n = 7),
            labels = scales::dollar_format()
        ) +
        ggplot2::scale_y_continuous(
            breaks = scales::pretty_breaks(n = 7),
            labels = scales::dollar_format()
        ) +
        viridis::scale_color_viridis(discrete = TRUE)


    value_of_information_scenario_path <- file.path(figures_dir, "output_voi_evaluation.png")
    ggsave(value_of_information_scenario_path, value_of_information_plot_from_results)

    long_form_voi_data_path <- file.path(synthesised_data_dir, "voi_long_form.xlsx")
    combined_cases_scenario_evaluation_prettier |>
        writexl::write_xlsx(long_form_voi_data_path)

    # Now think about how to do a good comparison at different cattle revenues
    # Weighted % of time carbon use is better than cattle
    combined_decision_rate_values <- decision_summary_in_price_scenario |>
        lapply(dplyr::bind_rows) |>
        dplyr::bind_rows()

    targeted_decision_for_plot <- "Use for carbon sequestration"

    preferred_carbon_decision_rates <- combined_decision_rate_values |>
        dplyr::filter(best_decision == targeted_decision_for_plot,
                      `Carbon price ($ / tonne)` != latest_real_carbon_price_estimate) |>
        build_underlying_heatmap(
            "Carbon price ($ / tonne)",
            "Reference cattle revenue",
            "Prob weighted decision rate"
        ) +
        ggplot2::labs(
            y = "Cattle Revenue Per Hectare Per Year",
            fill = "% of prior scenarios where carbon is better"
        ) +
        scale_fill_viridis_c(labels = scales::percent_format(),
                             breaks = scales::pretty_breaks(n = 5))

    preferred_carbon_rates_path <- file.path(figures_dir, 'rate_of_carbon_as_preferred_decision.png')
    ggplot2::ggsave(preferred_carbon_rates_path, preferred_carbon_decision_rates)

    # Increasing utility of sample information?
    full_scenario_voi_data_with_relative <- full_scenario_voi_data  |>
        dplyr::mutate(
            `Relative value of sample info to prior` = (`Value of sample information` /
                                                            `Expected annualised payoff on prior decision`))

    sample_information_value_under_many_scenarios <- full_scenario_voi_data_with_relative |>
        dplyr::filter(`Carbon price ($ / tonne)` != latest_real_carbon_price_estimate)|>
        build_underlying_heatmap(
            "Carbon price ($ / tonne)",
            "Cattle revenue ($ / He / yr)",
            "Value of sample information"
        ) +
        ggplot2::labs(
            y = "Cattle Revenue Per Hectare Per Year",
            fill = "Expected Value of Sample Information ($ / yr)"
        ) +
        scale_fill_viridis_c(labels = scales::dollar_format(),
                             breaks = scales::pretty_breaks(n = 5)) +
        facet_wrap(~`Sampling Scenario`)

    sample_info_value_path <- file.path(figures_dir, 'sample_information_against_cattle_and_carbon.png')
    ggplot2::ggsave(sample_info_value_path, sample_information_value_under_many_scenarios)


    sample_information_value_under_many_scenarios <- full_scenario_voi_data_with_relative |>
        dplyr::filter(`Carbon price ($ / tonne)` != latest_real_carbon_price_estimate) |>
        build_underlying_heatmap(
            "Carbon price ($ / tonne)",
            "Cattle revenue ($ / He / yr)",
            "Relative value of sample info to prior"
        ) +
        ggplot2::labs(
            y = "Cattle Revenue Per Hectare Per Year",
            fill = "relative % size of prior payoff and value of sample information"
        ) +
        scale_fill_viridis_c(labels = scales::percent_format(),
                             breaks = scales::pretty_breaks(n = 5)) +
        facet_wrap(~`Sampling Scenario`)

    relative_sample_info_path <- file.path(figures_dir, 'relative_sample_information_against_cattle_and_carbon.png')
    ggplot2::ggsave(relative_sample_info_path, sample_information_value_under_many_scenarios)

}

