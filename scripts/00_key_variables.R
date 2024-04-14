
# Load in the variable yaml file


# Set a stage to rerun the analysis from to save time
considered_analysis_stages <- c(
    "Initial shapefile + land height plots.",
    "BlueCAM carbon sequestration calculation.",
    "BlueCAM carbon sequestration plots.",
    "Prior belief characterisation",
    "Value of Information Analysis"
)

stage_to_start_from <- factor(
    considered_analysis_stages[1],
    levels = considered_analysis_stages,
    ordered = TRUE
)
