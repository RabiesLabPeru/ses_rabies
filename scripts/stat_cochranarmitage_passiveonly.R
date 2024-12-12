library(tidyverse)
library(DescTools)

setwd("~/Rabies SES/")

##-----------------------------------------------------------------------------
# 1. Load and prep positivity data
##-----------------------------------------------------------------------------

# Load samples obtained vai passive surveillance
samps <- readRDS("data/AQPsamples_passive_10.25.2024.rds") %>%
  mutate(twoyear = case_when(year %in% 2015:2016 ~ "2015-2016",
                             year %in% 2017:2018 ~ "2017-2018",
                             year %in% 2019:2020 ~ "2019-2020",
                             year %in% 2021:2022 ~ "2021-2022"))

# Split samples into two-year intervals
samps15 <- filter(samps, twoyear == "2015-2016")
samps17 <- filter(samps, twoyear == "2017-2018")
samps19 <- filter(samps, twoyear == "2019-2020")
samps21 <- filter(samps, twoyear == "2021-2022")

##-----------------------------------------------------------------------------
# 2a. Apply Cochran-Armitage test for samples aggregated across all years
##-----------------------------------------------------------------------------

table(samps$RESULTADO.FINAL, useNA = "ifany")  # no NA's
allyears_tab <- table(samps$SES, as.numeric(samps$RESULTADO.FINAL == "POSITIVO"))
allyears_tab <- allyears_tab[-6,]  # Remove SES == Undef
allyears_tab
CochranArmitageTest(allyears_tab, alternative = "one.sided")
# USING PASSIVE SAMPLES:
# Z = -10.452, dim = 5, p-value < 2.2e-16
# alternative hypothesis: one.sided
# USING ALL SAMPLES:
# Z = -9.6091, dim = 5, p-value < 2.2e-16
# alternative hypothesis: one.sided
CochranArmitageTest(allyears_tab, alternative = "two.sided")

##-----------------------------------------------------------------------------
# 2b. Sensitivity analysis: repeat C-A after including and reassigning undef 
#     --> E
##-----------------------------------------------------------------------------
allyears_tab2 <- table(samps$SES, as.numeric(samps$RESULTADO.FINAL == "POSITIVO"))
allyears_tab2[5,] <- allyears_tab2[5,]  + allyears_tab2[6,] 
allyears_tab2 <- allyears_tab2[-6,]
CochranArmitageTest(allyears_tab2, alternative = "one.sided")
# USING PASSIVE SAMPLES:
# Z = -10.503, dim = 5, p-value < 2.2e-16
# alternative hypothesis: one.sided
# USING ALL SAMPLES:
# Z = -9.5583, dim = 5, p-value < 2.2e-16
# alternative hypothesis: one.sided

##-----------------------------------------------------------------------------
# 3. Apply Cochran-Armitage test for samples stratified into two-year intervals
##-----------------------------------------------------------------------------

# 2021-2022
tab2021 <- table(samps21$SES, as.numeric(samps21$RESULTADO.FINAL == "POSITIVO"))
tab2021 <- tab2021[-6,]  # Remove SES == Undef
tab2021
CochranArmitageTest(tab2021, alternative = "one.sided")
# Z = -3.5313, dim = 5, p-value = 0.0002068
# alternative hypothesis: one.sided