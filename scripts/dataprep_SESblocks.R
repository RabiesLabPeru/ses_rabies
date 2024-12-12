library(tidyverse)
library(sf)

##-----------------------------------------------------------------------------
# 0. User-defined functions
##-----------------------------------------------------------------------------

# Function to read all KML layers
ReadSESKML <- function(kmlfile){
  lyr <- st_layers(kmlfile)$name
  mykml <- st_read(kmlfile, layer = lyr[1]) %>% 
    mutate(BlockID = Name, SES = lyr[1])
  if(length(lyr) > 1){
    for (i in 2:length(lyr)) {
      tmp <- st_read(kmlfile, layer = lyr[i]) %>% 
        mutate(BlockID = Name, SES = lyr[i])
      mykml <- bind_rows(mykml, tmp)
    }
  }
  mykml <- select(mykml, BlockID, SES)
  print(table(mykml$SES, useNA = "ifany"))
  return(mykml)
}

##-----------------------------------------------------------------------------
# 1. Read SES blocks by district
##-----------------------------------------------------------------------------

setwd("~/RabiesLabPeru/spatial_data/Spatial_data/Economic_income_AQP/Economic_income_distrito/")

# Load SES blocks by district
asa <- ReadSESKML("Ecom_dto_Alto_Selva_Alegre_16set2022.kml")
cay <- ReadSESKML("Ecom_dto_Cayma_01febrero2023.kml")
cer <- ReadSESKML("Ecom_dto_Cerro_Colorado_10may2023.kml")
cha <- ReadSESKML("Ecom_dto_Characato_06mar2023.kml")
hun <- ReadSESKML("Ecom_dto_Hunter_13set2022.kml")
jlb <- ReadSESKML("Ecom_dto_JLByR_20set2022.kml")
mar <- ReadSESKML("Ecom_dto_Mariano_Melgar_01feb2023.kml")
mir <- ReadSESKML("Ecom_dto_Miraflores_28set2022.kml")
mol <- ReadSESKML("Ecom_dto_Mollebaya_10nov2022.kml")
pau <- ReadSESKML("Ecom_dto_Paucarpata_10may2023.kml")
ped <- ReadSESKML("Ecom_dto_Pedregal_03oct2022.kml")
que <- ReadSESKML("Ecom_dto_Quequenha_10nov2022.kml") 
sab <- ReadSESKML("Ecom_dto_Sabandia_10nov2022.kml")
sac <- ReadSESKML("Ecom_dto_Sachaca_03oct2022.kml")
soc <- ReadSESKML("Ecom_dto_Socabaya_06mar2023.kml")
tia <- ReadSESKML("Ecom_dto_Tiabaya_06mar2023.kml")
uch <- ReadSESKML("Ecom_dto_Uchumayo_11nov2022.kml")  
yan <- ReadSESKML("Ecom_dto_Yanahuara_03oct2022.kml")
yur <- ReadSESKML("Ecom_dto_Yura_06mar2023.kml")

# Bind AQP districts
aqp <- bind_rows(asa, cay, cer, cha, hun, jlb, mar, mir, mol, pau,
                 que, sab, sac, soc, tia, uch, yan, yur)
plot(st_geometry(aqp))

# Clean SES variable
table(aqp$SES, useNA = "ifany")
aqp$SES[aqp$SES %in% c("NA", "N.A")] <- "Undef"
table(aqp$SES, useNA = "ifany")

table(ped$SES, useNA = "ifany")
ped$SES[ped$SES == "NA"] <- "Undef"
table(ped$SES, useNA = "ifany")

# Save AQP SES blocks data
setwd("~/RabiesLabPeru/Rabies data/data_cleaned/")
write_rds(aqp, "AQPSESblocks_4.3.2024.rds")
write_rds(ped, "PedregalSESblocks_4.3.2024.rds")
