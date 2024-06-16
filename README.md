# Value of Information Analysis for the landuse options on a cattle site candidate for mangrove restoration
## Description



## Re-running the analysis
* Clone the repository from GitHub. 
* Make sure you meet the installation / Setup guide below
* Open the analysis.Rproj . (Your working path should then be the cloned folder)
* Open and run '0x_main.R' which will call the other scripts as necessary

## Installation / Setup
### Software pre-requesites
* Make sure you have an R version >= 4.1 installed.
* Make you sure you have the renv R package installed
* Run `renv::activate()` and this should load the dependencies defined in the project

### Data Pre-requisites
The below datasets are needed for reproducing the analysis and are the data used as the basis for some of the calculations.

* Download the Blue Carbon Account model calculator spreadsheet.
    * Save this file as the following: /data/summariesReportsSynthesisedData/The blue carbon accounting model (BlueCAM).xlsx
    * (Please note, the we used the Version 1 with the date set at 2021-12-09)
    
* Download the elevation relief .tif file used for DEM analysis.
    * Save the file as the following: /data/rawData/DEMRasterClippedToProjectNindCreek.tif
    
* Download the Carbon Estimating Area boundary shape files. This setout the CEA boundaries used for calculating site-area and for individual carbon-sequestration estimates.
    * Save these files to /data/rawData/CEAs_V3
    * It expects three subfolders there with ./Fig2a, ./Fig2b, ./Fig2C which each contain shape files.
    
* Download two manually produced datasets.
    * A copy of the land-use descriptions from the paper: data/manualData/CEA_LandUseAreas.xlsx
    * A copy of the BlueCam Reference Document Table2 default eco-system STPI ranges at: data/manualData/BlueCAMModellingDocumentTable2.xlsx

### Parameter configuration
* There is a yaml file: documentation/modelling_config.yaml
    * This sets some of key reference constants used in the calculations. This is your first port of call if you want to
    tweak some of the parameters of the analysis.


## Notes on the approach taken.
### Methods

### Improvement Avenues
