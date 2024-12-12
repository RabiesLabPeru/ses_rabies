library(tidyverse)

setwd("~/Rabies SES/")

##-----------------------------------------------------------------------------
# 1. Load samples data 
##-----------------------------------------------------------------------------

# Load samples data
samples_active <- read_rds("data/AQPsamples_active_10.25.2024.rds")
samples_passive <- read_rds("data/AQPsamples_passive_10.25.2024.rds")
samples_all <- bind_rows(samples_active, samples_passive)

##-----------------------------------------------------------------------------
# 2. CHI-SQUARED TESTS FOR CASE VS HOUSE DISTRIBUTIONS
##-----------------------------------------------------------------------------

housecount_ses <- c(15369, 32289, 39843, 39729, 36242, 5955)  # A, B, C, D, E, Undef
casecount_ses <- c(6, 21, 46, 120, 129, 23)

# Get counts for contingency table
house_adv <- sum(housecount_ses[1:3])
house_dis <- sum(housecount_ses[4:5])
house_dis2 <- sum(housecount_ses[4:6])  # For sensitivity analysis - include undef
case_adv <- sum(casecount_ses[1:3])
case_dis <- sum(casecount_ses[4:5])
case_dis2 <- sum(casecount_ses[4:6])

# Make contingency tables
mat <- matrix(c(house_adv, house_dis, case_adv, case_dis), ncol = 2)
mat2 <- matrix(c(house_adv, house_dis2, case_adv, case_dis2), ncol = 2)

# Perform chi-squared tests
chisq.test(mat)
# X-squared = 121.74, df = 1, p-value < 2.2e-16
chisq.test(mat2)
# X-squared = 126.91, df = 1, p-value < 2.2e-16

##-----------------------------------------------------------------------------
# 3. CHI-SQUARED TESTS FOR ACTIVE VS. PASSIVE SAMPLES
##-----------------------------------------------------------------------------


# Active 2021
samples_active %>%
  filter(year == 2021) %>%
  mutate(disadv = ifelse(SES %in% c("D", "E", "Undef"), 1, 0)) %>%
  group_by(disadv) %>%
  reframe(count = n())
# Disadvantaged count = 74
# Not disadvantaged count = 16

# Passive 2021
samples_passive %>%
  filter(year == 2021) %>%
  mutate(disadv = ifelse(SES %in% c("D", "E", "Undef"), 1, 0)) %>%
  group_by(disadv) %>%
  reframe(count = n())
# Disadvantaged count = 98
# Not disadvantaged count = 48

# Chi-squared test for 2021
mat_21 <- matrix(c(74, 16, 98, 48), ncol = 2)
chisq.test(mat_21)
# X-squared = 5.6811, df = 1, p-value = 0.01715

# Active 2022
samples_active %>%
  filter(year == 2022) %>%
  mutate(disadv = ifelse(SES %in% c("D", "E", "Undef"), 1, 0)) %>%
  group_by(disadv) %>%
  reframe(count = n())
# Disadvantaged count = 49
# Not disadvantaged count = 14

# Passive 2022
samples_passive %>%
  filter(year == 2022) %>%
  mutate(disadv = ifelse(SES %in% c("D", "E", "Undef"), 1, 0)) %>%
  group_by(disadv) %>%
  reframe(count = n())
# Disadvantaged count = 98
# Not disadvantaged count = 71

# Chi-squared test for 2022
mat_22 <- matrix(c(49, 14, 98, 71), ncol = 2)
chisq.test(mat_22)
# X-squared = 6.913, df = 1, p-value = 0.008557