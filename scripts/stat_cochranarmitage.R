library(tidyverse)
library(DescTools)

setwd("~/Rabies SES/")

##-----------------------------------------------------------------------------
# 1. Load and prep positivity data
##-----------------------------------------------------------------------------

# Load samples SES data and remove all SES = NA
samps <- readRDS("data/AQPsamples_SES_6.24.2024.rds") %>%
  filter(! is.na(SES_median)) %>%
  mutate(year = format(date, "%Y")) %>%
  rename(SES = SES_median) %>%
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
# Z = -9.5583, dim = 5, p-value < 2.2e-16
# alternative hypothesis: one.sided

##-----------------------------------------------------------------------------
# 3. Apply Cochran-Armitage test for samples stratified into two-year intervals
##-----------------------------------------------------------------------------

# 2015-2016
tab2015 <- table(samps15$SES, as.numeric(samps15$RESULTADO.FINAL == "POSITIVO"))
tab2015 <- tab2015[-6,]  # Remove SES == Undef
tab2015
CochranArmitageTest(tab2015, alternative = "one.sided")
# Z = -5.2584, dim = 5, p-value = 7.266e-08
# alternative hypothesis: one.sided

# 2017-2018
tab2017 <- table(samps17$SES, as.numeric(samps17$RESULTADO.FINAL == "POSITIVO"))
tab2017 <- tab2017[-6,]  # Remove SES == Undef
tab2017
CochranArmitageTest(tab2017, alternative = "one.sided")
# Z = -5.1252, dim = 5, p-value = 1.486e-07
# alternative hypothesis: one.sided

# 2019-2020
tab2019 <- table(samps19$SES, as.numeric(samps19$RESULTADO.FINAL == "POSITIVO"))
tab2019 <- tab2019[-6,]  # Remove SES == Undef
tab2019
CochranArmitageTest(tab2019, alternative = "one.sided")
# Z = -4.425, dim = 5, p-value = 4.822e-06
# alternative hypothesis: one.sided

# 2021-2022
tab2021 <- table(samps21$SES, as.numeric(samps21$RESULTADO.FINAL == "POSITIVO"))
tab2021 <- tab2021[-6,]  # Remove SES == Undef
tab2021
CochranArmitageTest(tab2021, alternative = "one.sided")
# Z = -2.2616, dim = 5, p-value = 0.01186
# alternative hypothesis: one.sided

##-----------------------------------------------------------------------------
# 4. Sensitivity analysis (reassign undef --> E) for samples stratified into \
# two-year intervals
##-----------------------------------------------------------------------------

# 2015-2016
tab2015s <- table(samps15$SES, as.numeric(samps15$RESULTADO.FINAL == "POSITIVO"))
tab2015s[5,] <- tab2015s[5,]  + tab2015s[6,] 
tab2015s <- tab2015s[-6,]
tab2015s
CochranArmitageTest(tab2015s, alternative = "one.sided")
# Z = -5.2086, dim = 5, p-value = 9.513e-08
# alternative hypothesis: one.sided

# 2017-2018
tab2017s <- table(samps17$SES, as.numeric(samps17$RESULTADO.FINAL == "POSITIVO"))
tab2017s[5,] <- tab2017s[5,]  + tab2017s[6,] 
tab2017s <- tab2017s[-6,]
tab2017s
CochranArmitageTest(tab2017s, alternative = "one.sided")
# Z = -5.1967, dim = 5, p-value = 1.014e-07
# alternative hypothesis: one.sided

# 2019-2020
tab2019s <- table(samps19$SES, as.numeric(samps19$RESULTADO.FINAL == "POSITIVO"))
tab2019s[5,] <- tab2019s[5,]  + tab2019s[6,] 
tab2019s <- tab2019s[-6,]
tab2019s
CochranArmitageTest(tab2019s, alternative = "one.sided")
# Z = -4.2807, dim = 5, p-value = 9.317e-06
# alternative hypothesis: one.sided

# 2021-2022
tab2021s <- table(samps21$SES, as.numeric(samps21$RESULTADO.FINAL == "POSITIVO"))
tab2021s[5,] <- tab2021s[5,]  + tab2021s[6,] 
tab2021s <- tab2021s[-6,]
tab2021s
CochranArmitageTest(tab2021s, alternative = "one.sided")
# Z = -2.2389, dim = 5, p-value = 0.01258
# alternative hypothesis: one.sided