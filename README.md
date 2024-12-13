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
* **stat_cochranarmitage.R** - run Cochran-Armitage tests to test for trend between locality-level SES and sample positivity
* **stat_cochranarmitage_passiveonly.R** - same as above but run on passive samples only
* **stat_chisquared.R** - perform chi-squared tests to compare SES distributions between cases and houses and between samples collected via active vs. passive surveillance
* **stat_cohenskappa.R** - calculate cohen's kappa to assess agreement between block and locality SES
* **mapgam_samplepositivity.R** - fit crude and adjusted spatial GAMs to find the spatial OR of sample positivity; also map spatial OR's obtained from the crude and adjusted models
* 
## Mapping and plotting scripts
*Note maps and plots were additionally edited and formatted using Powerpoint*
* **map_rabiescases_districts.R** - map jittered locations of rabies cases and district boundaries (Fig 1)
* **map_rabiessamples.R** - create a choropleth map of sample submission volume by locality (Fig 1)
* **map_sesblocksandhealthfacilities.R** - map blocks shaded according to SES and the locations of health facilities (Fig 1)
* **plot_rabiescasesandsamples.R** - plot temporal trends in the number of cases and samples by SES (Fig 2)
* **plot_rabiescasesandsamples_passiveonly.R** - same as above but exclude active samples (SFig S3)
* **plot_samplepositivityandSES.R** - plot sample positivity vs. SES (Fig 3)
* **plot_samplepositivityandSES_passiveonly.R** - same as above but exclude active samples (SFig S4)
* **stackedrowcharts_SESdistributions.R** - create stacked row charts illustrating differences in the SES distributions of cases vs. houses (SFig S1)


# Data files
Non-sensitive data used in the analysis can be found in the **data** folder. Note that rabies case locations, which included many residential addresses, were removed to protect the privacy of local residents.
* **AQPSESblocks.rds** - block polygons with SES
* **AQPSESlocalities.rds** - locality polygons with SES
* **AQPsamples_SES.rds** - samples with test result and locality-level SES assignment
* **AQPsamples_centroids.csv** - samples with locality centroids
* **AQPsamples_active.rds** - samples filtered for those collected via active surveillance
* **AQPsamples_passive.rds** - samples filtered for those collected via passive surveillance
* **healthfaciliites.rds** - the locations of health facilities
