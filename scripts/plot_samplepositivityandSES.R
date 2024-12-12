library(tidyverse)
library(ggthemes)
library(ggpubr)
library(hrbrthemes)
library(sf)
library(scales)

##-----------------------------------------------------------------------------
# 0. User-defined functions
##-----------------------------------------------------------------------------

my_econ_theme <- function(){
  theme_economist_white(gray_bg=FALSE) +
    theme(axis.text=element_text(size=18),
          axis.title=element_text(size=20,face="bold"),
          legend.title=element_text(size=20),
          legend.text = element_text(size=18),
          plot.title = element_text(size=26, face = "bold"),
          axis.ticks.x = element_blank(),
          legend.position = "right")
  
} 

my_econ_theme_ticks <- function(){
  theme_economist_white(gray_bg=FALSE) +
    theme(axis.text=element_text(size=18),
          axis.title=element_text(size=20,face="bold"),
          legend.title=element_text(size=20),
          legend.text = element_text(size=18),
          plot.title = element_text(size=26, face = "bold"),
          legend.position = "right")
  
} 

# Modified one-way table function to show proportions
# tpt shows levels in decreasing frequency
# tpt2 shows levels in standard order

tb <- function(x){
  t <- rev(sort(table(x, useNA = "ifany")))
  return(t)
}

tb2 <- function(x){
  t <- table(x, useNA = "ifany")
  return(t)
}

ptb <- function(x){
  pt <- round(prop.table(tb(x))*100, 1)
  return(pt)
}

ptb2 <- function(x){
  pt <- round(prop.table(tb2(x))*100, 1)
  return(pt)
}

tpt <- function(x){
  t <- tb(x)
  pt <- ptb(x)
  tpt <- paste0(t, " (", pt,")")
  names(tpt) <- names(t)
  return(noquote(tpt))
}

tpt2 <- function(x){
  t <- tb2(x)
  pt <- ptb2(x)
  tpt <- paste0(t, " (", pt,")")
  names(tpt) <- names(t)
  return(noquote(tpt))
}

##-----------------------------------------------------------------------------
# 1. Load and clean sample data
##-----------------------------------------------------------------------------

setwd("~/Rabies SES/")

# Load samples SES data and remove all SES = NA
samps <- readRDS("data/AQPsamples_SES_6.24.2024.rds") %>%
  filter(! is.na(SES_median)) %>%
  mutate(year = format(date, "%Y")) %>%
  rename(SES = SES_median) %>%
  mutate(twoyear = case_when(year %in% 2015:2016 ~ "2015-2016",
                             year %in% 2017:2018 ~ "2017-2018",
                             year %in% 2019:2020 ~ "2019-2020",
                             year %in% 2021:2022 ~ "2021-2022"))
table(samps$year)
table(samps$twoyear)
table(samps$RESULTADO.FINAL)

# Find sample positivity by two-year interval and SES
positivity <- samps %>%
  filter(RESULTADO.FINAL %in% c("POSITIVO", "NEGATIVO")) %>%
  group_by(twoyear, SES) %>%
  summarise(n = n(),
            p = sum(RESULTADO.FINAL == "POSITIVO")/n,
            me = sqrt(p*(1-p)/n),
            positivity = p*100,
            pos_lower = max(positivity - me*100, 0),
            pos_upper = positivity + me*100)  

# Find positivity across all years
positivity_allyears <- samps %>%
  filter(RESULTADO.FINAL %in% c("POSITIVO", "NEGATIVO")) %>%
  group_by(SES) %>%
  summarise(n = n(),
            p = sum(RESULTADO.FINAL == "POSITIVO")/n,
            me = sqrt(p*(1-p)/n),
            positivity = p*100,
            pos_lower = max(positivity - me*100, 0),
            pos_upper = positivity + me*100)  

##-----------------------------------------------------------------------------
# 2. Plot positivity across all years and label with number of samples
##-----------------------------------------------------------------------------

pdf("figures/R_output/barplot_positivity.pdf", width = 7, height = 4)
ggplot(positivity_allyears, aes(x = SES, y = positivity, fill = SES)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_text(aes(label = n), position=position_dodge(width=0.9), vjust=-0.25, size = 6) +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  my_econ_theme() +  
  scale_y_continuous(expand = c(0,0), limits = c(0, 32))  +
  xlab("SES") +
  ylab("Sample positivity (%)")  

# Bar plot with hrbrtheme
ggplot(positivity_allyears, aes(x = SES, y = positivity, fill = SES)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_text(aes(label = n), position=position_dodge(width=0.9), vjust=-0.25, size = 5) +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  theme_ipsum() +  
  scale_y_continuous(expand = c(0,0), limits = c(0, 36))  +
  xlab("SES") +
  ylab("Sample positivity (%)")  

dev.off()


# Bar plot with margin of error bars - all years
pdf("figures/R_output/barplot_positivity_me.pdf", width = 7, height = 4.5)

ggplot(positivity_allyears, aes(x = SES, y = positivity, fill = SES)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_errorbar(aes(ymin=pos_lower, ymax=pos_upper), width=.2,
                position=position_dodge(.9)) +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 36))  +
  my_econ_theme() +    
  xlab("Year") +
  ylab("Sample positivity (%)")

ggplot(positivity_allyears, aes(x = SES, y = positivity, fill = SES)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_errorbar(aes(ymin=pos_lower, ymax=pos_upper), width=.2,
                position=position_dodge(.9)) +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 35))  +
  theme_ipsum() +  
  xlab("Year") +
  ylab("Sample positivity (%)")

dev.off()

# Bar plot with margin of error bars - two year positivity
pdf("figures/R_output/barplot_twoyearpositivity_me.pdf", width = 7, height = 4.5)

positivity2 <- filter(positivity, n>5)
ggplot(positivity2, aes(x = twoyear, y = positivity, fill = SES)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_errorbar(aes(ymin=pos_lower, ymax=pos_upper), width=.2,
                position=position_dodge(.9)) +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 45),
                     breaks = pretty_breaks())  +
  my_econ_theme() + 
  xlab("Year") +
  ylab("Sample positivity (%)")

ggplot(positivity2, aes(x = twoyear, y = positivity, fill = SES)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_errorbar(aes(ymin=pos_lower, ymax=pos_upper), width=.2,
                position=position_dodge(.9)) +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 45), 
                     breaks = pretty_breaks())  +
  theme_ipsum() +  
  xlab("Year") +
  ylab("Sample positivity (%)")

dev.off()
##-----------------------------------------------------------------------------
# 3. OLD: plot positivity with margin of error
##-----------------------------------------------------------------------------

# Plot positivity across all years
ggplot(positivity_allyears, aes(x = SES, y = positivity, fill = SES)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_errorbar(aes(ymin=pos_lower, ymax=pos_upper), width=.2,
                position=position_dodge(.9)) +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  my_econ_theme() +  #  scale_y_continuous(expand = c(0,0), limits = c(0, NA))  +
  xlab("Year") +
  ylab("Sample positivity (%)")

# Plot twoyear positivity
ggplot(positivity, aes(x = twoyear, y = positivity, fill = SES)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_errorbar(aes(ymin=pos_lower, ymax=pos_upper), width=.2,
                position=position_dodge(.9)) +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  my_econ_theme() +  #  scale_y_continuous(expand = c(0,0), limits = c(0, NA))  +
  xlab("Year") +
  ylab("Sample positivity (%)")

# Exclude cell sizes < 5 (2 undef samples in 2017-2018 and 1 undef sample in  
# 2019-2020)
positivity2 <- filter(positivity, n>5)
ggplot(positivity2, aes(x = twoyear, y = positivity, fill = SES)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_errorbar(aes(ymin=pos_lower, ymax=pos_upper), width=.2,
                position=position_dodge(.9)) +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  my_econ_theme() +  #  scale_y_continuous(expand = c(0,0), limits = c(0, NA))  +
  xlab("Year") +
  ylab("Sample positivity (%)")

# Line plot...looks like a mess
ggplot(positivity, aes(x = twoyear, y = positivity)) +
  geom_line(aes(group = SES, color = SES)) +
  geom_errorbar(aes(ymin=pos_lower, ymax=pos_upper), width=.2,
                position=position_dodge(.9)) +
  scale_color_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  my_econ_theme() +  #  scale_y_continuous(expand = c(0,0), limits = c(0, NA))  +
  xlab("Year") +
  ylab("Sample positivity (%)")
