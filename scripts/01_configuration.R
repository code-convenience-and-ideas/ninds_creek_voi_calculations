# Miniminal imports of functions and packages
# This sets up path variables for a report and then also defines any really commonly used and shared functions for analysis
if (!require("pacman")){
  install.packages("pacman")
  library("pacman")
}

p_load("futile.logger")  # A nice logging library for storing results, errors and summaries
p_load("import")  # Allows greaer control of imports in R
p_load("sessioninfo")  # Summarise the information on the current session for debugging

source(here::here('scripts', '01_configuration_functions.R'))


#-- Set up paths

work_dir <- here::here()

scripts_dir <- file.path(work_dir, "scripts")

logs_dir <- file.path(work_dir, "logs")

data_dir <- file.path(work_dir, "data")
manualdata_dir <- file.path(data_dir, "manualData")
processeddata_dir <- file.path(data_dir, "processedData")
rawdata_dir <- file.path(data_dir, "rawData")
synthesised_data_dir <- file.path(data_dir, "summariesReportsSynthesisedData")

documentation_dir <- file.path(work_dir, "documentation")

reports_dir <- file.path(work_dir, "reports")

figures_dir <- file.path(work_dir, "figures")

other_dir <- file.path(work_dir, "other")

models_dir <- file.path(work_dir, "models")

# Start Logger and configure relevant options
default_log_file_path <- file.path(logs_dir, 'standard.log')

flog.logger("standard_logger",
            futile.logger::DEBUG,
            appender=flog.appender(appender.tee(default_log_file_path)))

# Set the level of logging to perform e.g INFO and above, debugging, warnings, etc
flog.threshold(futile.logger::INFO, name="standard_logger")

# Capture errors and traceback in the main logger
options(error = function() {
    flog.error(geterrmessage(), name = "standard_logger") ; traceback() ; stop()
    })

flog.debug(sessionInfo())

flog.info("Defining standard functions used across multiple scripts")


