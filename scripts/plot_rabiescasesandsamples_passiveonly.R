library(tidyverse)
library(ggthemes)
library(ggpubr)
library(hrbrthemes)
library(ggraph)
library(sf)
library(scales)
library(Cairo)

#extrafont::loadfonts()
#import_roboto_condensed()
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
# 1. Load data
##-----------------------------------------------------------------------------

# Load cases with those obtained via active surveillance removed
setwd("~/Rabies SES/")
cases <- readRDS("data/AQPrabiescases_passive_10.25.2024.rds") %>%
  filter(! is.na(SES))
table(cases$SES)

# Convert cases SES to factor
#cases$SES[cases$SES == "Undefined"] <- "Undef"
#cases$SES <- factor(cases$SES, levels = c("A", "B", "C", "D", "E", 
#                                          "Undef"), ordered = T)
#table(cases$SES)

# Load samples data with those obtained via active surveillance removed
samps <- readRDS("data/AQPsamples_passive_10.25.2024.rds")
table(samps$SES)
tpt2(samps$SES)

##-----------------------------------------------------------------------------
# 2. Plots of rabies CASES by SES and year
##-----------------------------------------------------------------------------

# Prepare data by number of cases by SES and year
cases_year <- st_drop_geometry(cases) %>%
  group_by(year, SES, .drop = FALSE) %>%
  summarise(count = n())

pdf("figures/R_output/lineplot_cases_excludeactive.pdf", width = 7, height = 4)
# Line plot with the same info
ggplot(cases_year) +
  geom_line(aes(x = year, y = count, color = SES), linewidth = 1.5) +
  geom_point(aes(x = year, y = count, color = SES), size = 4) +
  scale_color_manual(values = c("#2c7bb6", "#abd9e9", "#f5f57f", "#fdae61", 
                               "#d7191c", "grey")) +
  my_econ_theme_ticks() +
  scale_x_continuous(breaks = 2015:2022) +
  xlab("Year") +
  ylab("Number of cases")

# Line plot with hrbrtheme
ggplot(cases_year) +
  geom_vline(xintercept = 2021, size = 1.4, linetype = "dashed") +
  geom_line(aes(x = year, y = count, color = SES), size = 1.5) +
  geom_point(aes(x = year, y = count, color = SES), size = 4) +
  scale_color_manual(values = c("#2c7bb6", "#abd9e9", "#f5f57f", "#fdae61", 
                                "#d7191c", "grey")) +
  theme_ipsum() +
  scale_x_continuous(breaks = 2015:2022) +
  #scale_y_continuous(expand = c(0,0), limits = c(0, NA))  +
  xlab("Year") +
  ylab("Number of cases")

dev.off()

##-----------------------------------------------------------------------------
# 3. Plots of rabies SAMPLES by SES and year
##-----------------------------------------------------------------------------

# Prepare data by number of cases by SES and year
samp_year <- samps %>%
  group_by(year, SES, .drop = FALSE) %>%
  summarise(count = n())
samp_year$year <- as.numeric(samp_year$year)
pdf("figures/R_output/lineplot_samples_excludeactive.pdf", width = 7, height = 4)

# Line plot
ggplot(samp_year) +
  geom_line(aes(x = year, y = count, color = SES), linewidth = 1.5) +
  geom_point(aes(x = year, y = count, color = SES), size = 4) +
  scale_color_manual(values = c("#2c7bb6", "#abd9e9", "#f5f57f", "#fdae61", 
                                "#d7191c", "grey")) +
  my_econ_theme_ticks() +
  xlab("Year") +
  ylab("Number of samples")


# Line plot with hrbrtheme
ggplot(samp_year) +
  geom_line(aes(x = year, y = count, color = SES), linewidth = 1.5) +
  geom_point(aes(x = year, y = count, color = SES), size = 4) +
  scale_color_manual(values = c("#2c7bb6", "#abd9e9", "#f5f57f", "#fdae61", 
                                "#d7191c", "grey")) +
  theme_ipsum() +
  scale_x_continuous(breaks = 2015:2022) +
  xlab("Year") +
  ylab("Number of samples")

dev.off()
