# Load configuration script
# Assumes config file is in the working directory
source(here::here('scripts', '01_configuration.R'))

# Run the import of packages needed for the project
flog.info("Importing relevant packages for analysis")
source(here::here('scripts', '0x_import.R'))

# Set up the database connections and load in the data coming from the database / disk
flog.info("Loading data from the databases")
source(here::here('scripts', '02_data_loading.R'))

# Clean and modify the data in any crucially needed ways for downstream analysis
flog.info("Cleaning data for downstream analysis")
source(here::here('scripts', '03_data_cleaning.R'))

# Do the analysis of the data itself, exploratory, models, plots
flog.info("Performing analysis, visualisation, plotting etc of the data")
source(here::here('scripts', "04_data_analysis.R"))

