########################################################################################
#                                                                                      #
# Description: A single line summary of the project and its purposed                   #
# Author: Alexander Baker                                                              #
# Contact: alex.baker@workcoverqld.com.au                                              #
# Manager: Ben Saunders																   #
# Project: 																			   #
#                                                                                      #
########################################################################################
try_updating_the_packages <- FALSE

# Use the pacman package to load packages, if not present it installs them
if (!require("pacman")){
  install.packages("pacman", repos='https://cran.csiro.au/')
  library("pacman")
}
p_load('vctrs', update=try_updating_the_packages)  # Underlying R vctrs package other depend on, needs to be loading first to allow updates
p_load('rlang', update=try_updating_the_packages)  # Underlying R vctrs package other depend on, needs to be loading first to allow updates
p_load("tidyverse", update=try_updating_the_packages) # Nice suite of tools for fast and readable data manipulation
p_load("ggplot2", update=try_updating_the_packages) # nice static plotting library
p_load("cowplot", update=try_updating_the_packages) # Adds some nice panelling options to use with ggplot2 for figures
p_load("plotly", update=try_updating_the_packages)  # Allows creation of interactive html graphics
p_load("crosstalk", update=try_updating_the_packages)  # For Making Widgets interact
p_load("odbc", update=try_updating_the_packages)  # R ODBC driver interface to connect to sql servers
p_load("DBI", update=try_updating_the_packages)  # Interfance to read and write
p_load("keyring", update=try_updating_the_packages)  # For securely storring and accessing passwords in the windows keyring
p_load("ggpubr", update=try_updating_the_packages)  # Some nice aggregate, publication quality plots
p_load("stringr", update=try_updating_the_packages)  # Nice string handling functions for analysis
p_load("data.table", update=try_updating_the_packages)  # Used for easier aggregation syntax
p_load("R.utils", update=try_updating_the_packages)  # Accesses gzip and some other utilities
p_load("arrow", update=try_updating_the_packages)  # Library for reading and writing the apache parquet format - fast binary columnar storage
p_load("gam", update=try_updating_the_packages)  # Generalised additive models
p_load("lubridate", update=try_updating_the_packages)  # Nice date handling of
p_load("testthat", update=try_updating_the_packages)  # Testing framework for our 
p_load('mockr', update=try_updating_the_packages)  # Mock up fake tests and scenarios
p_load("covr", update=try_updating_the_packages)  # Code coverage testing to understanding how much is covered and tested
p_load('yardstick', update=try_updating_the_packages)  # Measure definitions for variable models
p_load('doParallel', update=try_updating_the_packages)  # Used for registering multiple cores for parallel work
p_load('english', update=try_updating_the_packages)  # Contains ordinal names and the like for nice human readable renaming of vectors
p_load("dampack", update=try_updating_the_packages)
p_load('sn', update=try_updating_the_packages)
p_load("truncnorm", update=try_updating_the_packages)
# p_load("voi")
p_load('RSQLite', update=try_updating_the_packages)  # Use just for temporary database for a memory test
p_load('rstan', update=try_updating_the_packages) 
p_load('hrbrthemes', update=try_updating_the_packages)  # ipsum theme
p_load('RColorBrewer', update=try_updating_the_packages)  # Colour brewer scale
p_load('writexl', update=try_updating_the_packages)
p_load('directlabels', update=try_updating_the_packages)


# Shape file and .tiff processing
p_load("tiff", update=try_updating_the_packages)
p_load('rgdal', update=try_updating_the_packages)
p_load('sf', update=try_updating_the_packages)
p_load('glob', update=try_updating_the_packages)
p_load('maptools', update=try_updating_the_packages)
p_load('raster', update=try_updating_the_packages)
p_load('stars', update=try_updating_the_packages)
p_load('tmap', update=try_updating_the_packages)
p_load('purrr', update=try_updating_the_packages)

# Package for interacting with excell document interactively
# NEED RTOOLS + DEVTOOLS FOR GITHUBINSTALL
p_load('devtools')
# https://cran.r-project.org/bin/windows/Rtools/

# p_load("RDCOMClient")
# library("RDCOMClient")
pacman::p_load_gh('omegahat/RDCOMClient')
# devtools::install_github("omegahat/RDCOMClient")

# Load Session Info and dump it to file for record of latest run of code
initial_session_info <- utils::sessionInfo()

writeLines(utils::capture.output(initial_session_info), "sessionInfo.txt")

