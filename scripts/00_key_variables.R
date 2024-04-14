
# Load in the variable yaml file


# Set a stage to rerun the analysis from to save time
considered_analysis_stages <- c(
    "Initial shapefile + land height plots.",
    "BlueCAM carbon sequestration calculation.",
    "BlueCAM carbon sequestration plots.",
    "Prior belief characterisation",
    "Value of Information Analysis",
    "Final voi plots"
)

stage_to_start_from <- factor(
    considered_analysis_stages[1],
    levels = considered_analysis_stages,
    ordered = TRUE
)

analysis_stages_vectors <- factor(
    considered_analysis_stages,
    levels = considered_analysis_stages,
    ordered = TRUE
)


library('yaml')


# Define a custom handler for the 'analysis_argument' tag
handle_analysis_argument <- function(x) {
    # Custom handling code for new yaml type
    # For example, return a list with a specific structure or perform type conversion
    return(list(
        name = x[['name']],
        description = x[['description']],
        value = x[['value']]))
}

handle_argument_list <- function(x){
    argument_entry_names <- lapply(x, function(x) x[[1]][['name']]) |> unlist()
    names(x) <- argument_entry_names

    new_list <- list()
    for (entry_name in names(x)){
        new_list[[entry_name]] <- x[[entry_name]][[1]]
    }

    return(new_list)
}

# Register the custom handler with the yaml.load function
handlers <- list(
    "analysis_argument" = handle_analysis_argument,
    "argument_list" = handle_argument_list
)

# Load in the project specific yaml
modelling_config_yaml_path <- file.path(documentation_dir, 'modelling_config.yaml')

# Load the YAML content with the custom handler
yaml_data <- yaml.load_file(modelling_config_yaml_path, handlers = handlers)

# Separate parameters
model_params <- yaml_data$analysis_variable_arguments
