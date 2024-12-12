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

# Load cases SES data and remove single SES = NA
setwd("~/Rabies SES/")
cases <- readRDS("data/AQPrabiescases_SES_4.3.2024.rds") %>%
  filter(! is.na(SES))
table(cases$SES)

# Convert cases SES to factor
#cases$SES[cases$SES == "Undefined"] <- "Undef"
#cases$SES <- factor(cases$SES, levels = c("A", "B", "C", "D", "E", 
#                                          "Undef"), ordered = T)
#table(cases$SES)

# Load samples SES data and remove all SES = NA
samps <- readRDS("data/AQPsamples_SES_6.24.2024.rds") %>%
  filter(! is.na(SES_median)) %>%
  mutate(year = format(date, "%Y")) %>%
  rename(SES = SES_median)
table(samps$SES)
tpt2(samps$SES)

# Convert sample SES to factor
#samps$SES <- factor(samps$SES, levels = c("A", "B", "C", "D", "E", 
#                                          "Undef"), ordered = T)
#tpt2(samps$SES)

# Αlso consider samples without removing NA's
samps2 <- readRDS("data/AQPsamples_SES_6.24.2024.rds") %>%
  mutate(year = format(date, "%Y")) %>%
  rename(SES = SES_median)
tpt2(samps2$SES)

##-----------------------------------------------------------------------------
# 2. Plots of rabies CASES by SES and year
##-----------------------------------------------------------------------------

# Prepare data by number of cases by SES and year
cases_year <- st_drop_geometry(cases) %>%
  group_by(year, SES, .drop = FALSE) %>%
  summarise(count = n())

pdf("figures/R_output/lineplot_cases.pdf", width = 7, height = 4)
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

# OLD: BAR PLOTS AND OTHER PLOT TYPES
ggplot(cases_year) +
  geom_bar(aes(x = year, y = count, fill = SES), stat = "identity") +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  my_econ_theme() +
  scale_x_continuous(breaks = 2015:2022) +
  scale_y_continuous(expand = c(0,0), limits = c(0, NA))  +
  xlab("Year") +
  ylab("Number of cases")

ggplot(cases_year) +
  geom_bar(aes(x = year, y = count, fill = SES), stat = "identity",
           position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  my_econ_theme() +
  scale_x_continuous(breaks = 2015:2022) +
  scale_y_continuous(expand = c(0,0), limits = c(0, NA))  +
  xlab("Year") +
  ylab("Number of cases")

# Proportion bar plot with hrbrtheme
ggplot(cases_year) +
  geom_bar(aes(x = year, y = count, fill = SES), stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  theme_ipsum() +
  scale_x_continuous(breaks = 2015:2022) +
  scale_y_continuous(expand = c(0,0), limits = c(0, NA))  +
  xlab("Year") +
  ylab("Proportion of cases")

# Proportion bar plot
ggplot(cases_year) +
  geom_bar(aes(x = year, y = count, fill = SES), stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  my_econ_theme() +
  scale_x_continuous(breaks = 2015:2022) +
  scale_y_continuous(expand = c(0,0), limits = c(0, NA))  +
  xlab("Year") +
  ylab("Proportion of cases")

# Area plot
ggplot(cases_year, aes(x=year, y=count, fill=SES)) + 
  geom_area(alpha=1 , size=.5, colour="white") +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +  theme_ipsum() + 
  scale_x_continuous(breaks = 2015:2022) +
  scale_y_continuous(breaks = pretty_breaks())  +
  xlab("Year") +
  ylab("Number of of cases")

##-----------------------------------------------------------------------------
# 3. Plots of rabies SAMPLES by SES and year
##-----------------------------------------------------------------------------

# Prepare data by number of cases by SES and year
samp_year <- samps %>%
  group_by(year, SES, .drop = FALSE) %>%
  summarise(count = n())
samp_year$year <- as.numeric(samp_year$year)
pdf("figures/R_output/lineplot_samples.pdf", width = 7, height = 4)

# Line plot
ggplot(samp_year) +
  geom_line(aes(x = year, y = count, color = SES), size = 1.5) +
  geom_point(aes(x = year, y = count, color = SES), size = 4) +
  scale_color_manual(values = c("#2c7bb6", "#abd9e9", "#f5f57f", "#fdae61", 
                                "#d7191c", "grey")) +
  my_econ_theme_ticks() +
  xlab("Year") +
  ylab("Number of samples")


# Line plot with hrbrtheme
ggplot(samp_year) +
  geom_line(aes(x = year, y = count, color = SES), size = 1.5) +
  geom_point(aes(x = year, y = count, color = SES), size = 4) +
  scale_color_manual(values = c("#2c7bb6", "#abd9e9", "#f5f57f", "#fdae61", 
                                "#d7191c", "grey")) +
  theme_ipsum() +
  scale_x_continuous(breaks = 2015:2022) +
  xlab("Year") +
  ylab("Number of samples")

dev.off()


# OLD: BAR PLOTS AND OTHER PLOT TYPES
# Stacked bar plot
ggplot(samp_year) +
  geom_bar(aes(x = year, y = count, fill = SES), stat = "identity") +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  my_econ_theme() +
  scale_x_continuous(breaks = 2014:2023) +
  scale_y_continuous(expand = c(0,0), limits = c(0, NA))  +
  xlab("Year") +
  ylab("Number of samples")

# Non-stacked bar plot
ggplot(samp_year) +
  geom_bar(aes(x = year, y = count, fill = SES), stat = "identity",
           position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  my_econ_theme() +
  scale_x_continuous(breaks = 2014:2023) +
  scale_y_continuous(expand = c(0,0), limits = c(0, NA))  +
  xlab("Year") +
  ylab("Number of samples")

# Proportion bar plot
ggplot(samp_year) +
  geom_bar(aes(x = year, y = count, fill = SES), stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  my_econ_theme() +
  scale_x_continuous(breaks = 2014:2023) +
  scale_y_continuous(expand = c(0,0), limits = c(0, NA))  +
  xlab("Year") +
  ylab("Proportion of samples")

# Area plot
ggplot(samp_year, aes(x=year, y=count, fill=SES)) + 
  geom_area(alpha=1 , size=.5, colour="white") +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +  theme_ipsum() + 
  scale_x_continuous(breaks = 2014:2023) +
  scale_y_continuous(breaks = pretty_breaks())  +
  xlab("Year") +
  ylab("Number of of samples")

# Proportion bar plot with hrbr theme
ggplot(samp_year) +
  geom_bar(aes(x = year, y = count, fill = SES), stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  theme_ipsum() +
  scale_x_continuous(breaks = 2014:2023) +
  scale_y_continuous(expand = c(0,0), limits = c(0, NA))  +
  xlab("Year") +
  ylab("Proportion of samples")

##-----------------------------------------------------------------------------
# 4. Barplots of rabies SAMPLES (without excluding NAs) by SES and year
##-----------------------------------------------------------------------------

# Prepare data by number of cases by SES and year
samp_year2 <- samps2 %>%
  group_by(year, SES) %>%
  summarise(count = n())

ggplot(samp_year2) +
  geom_line(aes(x = year, y = count, color = SES), size = 1.5) +
  geom_point(aes(x = year, y = count, color = SES), size = 4) +
  scale_color_manual(values = c("#2c7bb6", "#abd9e9", "#f5f57f", "#fdae61", 
                                "#d7191c", "grey")) +
  my_econ_theme_ticks() +
  scale_x_continuous(breaks = 2014:2023) +
  xlab("Year") +
  ylab("Number of samples")

ggplot(samp_year2) +
  geom_bar(aes(x = year, y = count, fill = SES), stat = "identity") +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  my_econ_theme() +
  scale_x_continuous(breaks = 2015:2022) +
  scale_y_continuous(expand = c(0,0), limits = c(0, NA))  +
  xlab("Year") +
  ylab("Number of samples")

ggplot(samp_year2) +
  geom_bar(aes(x = year, y = count, fill = SES), stat = "identity",
           position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  my_econ_theme() +
  scale_x_continuous(breaks = 2015:2022) +
  scale_y_continuous(expand = c(0,0), limits = c(0, NA))  +
  xlab("Year") +
  ylab("Number of samples")

ggplot(samp_year2) +
  geom_bar(aes(x = year, y = count, fill = SES), stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  my_econ_theme() +
  scale_x_continuous(breaks = 2015:2022) +
  scale_y_continuous(expand = c(0,0), limits = c(0, NA))  +
  xlab("Year") +
  ylab("Proportion of samples")


##-----------------------------------------------------------------------------
# 5. Barplots of rabies SAMPLE POSITIVITY by SES and year
##-----------------------------------------------------------------------------

# Make sure there are no NA's in the RESULTADO.FINAL variable
table(samps$RESULTADO.FINAL, useNA = "ifany")  # samples with SES = NA excluded
# NEGATIVO NO SE PROCESO      POSITIVO 
#     1125             2            90 
table(samps2$RESULTADO.FINAL, useNA = "ifany")  # all samples
# NEGATIVO NO SE PROCESO      POSITIVO 
#     2069             2           186 
# -- we'll remove the instances of "no se proceso" when cleaning the data below

# Find sample positivity by existence of PDL code and year and plot
pos_year2 <- samps2 %>%
  filter(RESULTADO.FINAL %in% c("POSITIVO", "NEGATIVO")) %>%
  mutate(PDL = ifelse(!is.na(PDL), "Known", "Missing")) %>%
  group_by(year, PDL) %>%
  summarise(positivity = sum(RESULTADO.FINAL == "POSITIVO")/n()*100,
            count = n()) 
table(pos_year2$PDL)
ggplot(pos_year2, aes(x = year, y = positivity, fill = PDL)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9)) +
  geom_text(aes(label = count), position=position_dodge(width=0.9), vjust=-0.25, size = 6) +
  scale_fill_manual(values = c("darkorchid", "darkgrey")) +
  my_econ_theme() +
  scale_x_continuous(breaks = 2014:2023) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 20))  +
  xlab("Year") +
  ylab("Sample positivity")

# Find sample positivity by SES and year and plot
pos_yearses <- samps %>%
  filter(RESULTADO.FINAL %in% c("POSITIVO", "NEGATIVO")) %>%
  group_by(year, SES) %>%
  summarise(positivity = sum(RESULTADO.FINAL == "POSITIVO")/n()*100,
            count = n())  
ggplot(pos_yearses, aes(x = year, y = positivity, fill = SES)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_text(aes(label = count), position=position_dodge(width=0.9), vjust=-0.25, size = 4) +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  my_econ_theme() +
  scale_x_continuous(breaks = 2014:2023) +
#  scale_y_continuous(expand = c(0,0), limits = c(0, NA))  +
  xlab("Year") +
  ylab("Sample positivity (%)")

# Collapse SES categories and plot again
samps_collapsedses <- samps
samps_collapsedses$SES <- as.character(samps_collapsedses$SES)
samps_collapsedses$SES[samps_collapsedses$SES %in% c("A", "B")] <- "High"
samps_collapsedses$SES[samps_collapsedses$SES == "C"] <- "Medium"
samps_collapsedses$SES[samps_collapsedses$SES %in% c("D", "E", "Undef")] <- "Low"
samps_collapsedses$SES <- factor(samps_collapsedses$SES, ordered = T,
                                 levels = c("High", "Medium", "Low"))
table(samps_collapsedses$SES)
samps_collapsedses %>%
  filter(RESULTADO.FINAL %in% c("POSITIVO", "NEGATIVO")) %>%
  group_by(year, SES) %>%
  summarise(positivity = sum(RESULTADO.FINAL == "POSITIVO")/n()*100,
            count = n()) %>%
  filter(year != 2014) %>% # Remove 2014 (no positive samples that year)
ggplot(., aes(x = year, y = positivity, fill = SES))+
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_text(aes(label = count), position=position_dodge(width=0.9), vjust=-0.25, size = 4) +
  scale_fill_manual(values = c("#2c7bb6", "#ffffbf", "#d7191c")) +
  my_econ_theme() +
  scale_x_continuous(breaks = 2015:2023) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 23))  +
  xlab("Year") +
  ylab("Sample positivity (%)")

# Collapse SES categories and plot again (but remove undefined SES)
samps_collapsedses <- samps
samps_collapsedses$SES <- as.character(samps_collapsedses$SES)
samps_collapsedses$SES[samps_collapsedses$SES %in% c("A", "B")] <- "High"
samps_collapsedses$SES[samps_collapsedses$SES == "C"] <- "Medium"
samps_collapsedses$SES[samps_collapsedses$SES %in% c("D", "E")] <- "Low"
samps_collapsedses$SES <- factor(samps_collapsedses$SES, ordered = T,
                                 levels = c("High", "Medium", "Low"))
samps_collapsedses %>%
  filter(RESULTADO.FINAL %in% c("POSITIVO", "NEGATIVO") & !is.na(SES)) %>%
  group_by(year, SES) %>%
  summarise(positivity = sum(RESULTADO.FINAL == "POSITIVO")/n()*100,
            count = n()) %>%
  filter(year != 2014) %>% # Remove 2014 (no positive samples that year)
ggplot(., aes(x = year, y = positivity, fill = SES))+
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_text(aes(label = count), position=position_dodge(width=0.9), vjust=-0.25, size = 4) +
  scale_fill_manual(values = c("#2c7bb6", "#ffffbf", "#d7191c")) +
  my_econ_theme() +
  scale_x_continuous(breaks = 2015:2023) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 23))  +
  xlab("Year") +
  ylab("Sample positivity (%)")

# Plot sample positivity as a line plot
samps_collapsedses %>%
  filter(RESULTADO.FINAL %in% c("POSITIVO", "NEGATIVO") & !is.na(SES)) %>%
  group_by(year, SES) %>%
  summarise(positivity = sum(RESULTADO.FINAL == "POSITIVO")/n()*100,
            count = n()) %>%
  filter(year != 2014) %>% # Remove 2014 (no positive samples that year)
  ggplot(., aes(x = year, y = positivity, color = SES))+
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  #geom_text(aes(label = count), position=position_dodge(width=0.9), vjust=-0.25, size = 4) +
  scale_color_manual(values = c("#2c7bb6", "#f5f57f", "#d7191c")) +
  my_econ_theme_ticks() +
  scale_x_continuous(breaks = 2015:2023) +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 23))  +
  xlab("Year") +
  ylab("Sample positivity (%)")

# Sample positivity by SES all years
samps %>%
  group_by(SES) %>%
  summarise(positivity = sum(RESULTADO.FINAL == "POSITIVO")/n()*100,
            count = n()) %>%
ggplot(., aes(x = SES, y = positivity, fill = SES)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_text(aes(label = count), position=position_dodge(width=0.9), vjust=-0.25, size = 6) +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", 
                               "#d7191c", "grey")) +
  my_econ_theme() +  
  scale_y_continuous(expand = c(0,0), limits = c(0, 17))  +
  xlab("SES") +
  ylab("Sample positivity (%)")  

##-----------------------------------------------------------------------------
# 6. Do positive sample counts match the number of cases reported for each 
#    year?
##-----------------------------------------------------------------------------
cases %>%
  group_by(year) %>%
  reframe(count = n())

samples %>%
  filter(RESULTADO.FINAL == "POSITIVO") %>%
  group_by(ANIO) %>%
  reframe(count = n())

filter(samples, ANIO == 2014 & RESULTADO.FINAL == "POSITIVO")
