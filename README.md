# Neighborhood Socioeconomic Status and Dog-Mediated Rabies: Disparities in Incidence and Surveillance Effort in a Latin American City

This repository contains scripts used in the analysis presented in the manuscript titled _Neighborhood Socioeconomic Status and Dog-Mediated Rabies: Disparities in Incidence and Surveillance Effort in a Latin American City_. A description of the repository's contents are provided below.

# Script files
Scripts used in the analysis can be found in the **scripts** folder. A description of each script file can be found below.

## Data cleaning scripts
* **dataprep_SEScases.R** - assign SES to cases based on block SES
* **dataprep_SESlocalities.R** - determine locality-level SES as the median SES of blocks falling within each locality
* **dataprep_disttofacilities.R** - assign SES to samples based on locality-level SES
* **dataprep_disttofacilities.R** - calculate Euclidean distance between locality centroids associated with each sample and the closest health facility

## Statistical analysis scripts
* **stat_cochranarmitage.R** -
* **stat_cochranarmitage_passiveonly.R** - 
* **stat_chisquared.R** - 
* **stat_cohenskappa.R** - 
* **mapgam_samplepositivity.R** - 

## Mapping and plotting scripts
* **map_rabiescases_districts.R** -
* **map_rabiessamples.R** - 
* **map_sesblocksandhealthfacilities.R** - 
* **plot_rabiescasesandsamples.R** - 
* **plot_rabiescasesandsamples_passiveonly.R** -
* **plot_samplepositivityandSES.R** - 
* **plot_samplepositivityandSES_passiveonly.R** -
* **stackedrowcharts_SESdistributions.R** -


# Data files
Non-sensitive data used in the analysis can be found in the **data** folder. Note that rabies case locations, which included many residential addresses, were removed to protect the privacy of local residents.
* **AQPSESblocks.rds** - block polygons with SES
* **AQPSESlocalities.rds** - locality polygons with SES
* **AQPsamples_SES.rds** - samples with test result and locality-level SES assignment
* **AQPsamples_centroids.csv** - samples with locality centroids
* **AQPsamples_active.rds** - samples filtered for those collected via active surveillance
* **AQPsamples_passive.rds** - samples filtered for those collected via passive surveillance
* **healthfaciliites.rds** - the locations of health facilities
