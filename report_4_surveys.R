### ------------------------------------------------------------------------ ###
### analyse surveys ####
### ------------------------------------------------------------------------ ###

### load packages
suppressPackageStartupMessages(library(icesTAF))
taf.libPaths()
suppressPackageStartupMessages(library(icesAdvice))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(foreach))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(FLCore))

### load additional functions
source("utilities.R")

if (!exists("verbose")) verbose <- FALSE

mkdir("report/surveys/")

### ------------------------------------------------------------------------ ###
### load indices ####
### ------------------------------------------------------------------------ ###
idx_full <- readRDS("data/model_input_idx_all.RDS")

### ------------------------------------------------------------------------ ###
### catch curves ####
### ------------------------------------------------------------------------ ###
idx_cc <- idx_full
idx_cc$Q1SWBeam <- trim(idx_cc$Q1SWBeam, age = 1:10)
idx_cc$`FSP-7e` <- trim(idx_cc$`FSP-7e`, age = 1:10)
idx_cc$`FSP-7e-biomass` <- trim(idx_cc$`FSP-7e-biomass`, age = 1:10)
idx_cc$`Q1SWBeam-biomass` <- trim(idx_cc$`Q1SWBeam-biomass`, age = 1:10)
names(idx_cc) <- c("UK-FSP (numbers)", "Q1SWBeam (numbers)", 
                   "UK-FSP (biomass)", "Q1SWBeam (biomass)")


### raw values
p <- plot_catch_curve(input = idx_cc, cohort = FALSE, standardize = FALSE,
                      y_label = "numbers or biomass")
if (isTRUE(verbose)) p
ggsave(file = "report/surveys/plots_data_survey_cc_raw.png", plot = p, 
       width = 15, height = 13.4, units = "cm", dpi = 300, type = "cairo-png")
### with sum
p <- plot_catch_curve(input = idx_cc, cohort = FALSE, standardize = FALSE,
                      y_label = "numbers or biomass", total = TRUE)
if (isTRUE(verbose)) p
ggsave(file = "report/surveys/plots_data_survey_cc_raw_total.png", plot = p, 
       width = 22, height = 15, units = "cm", dpi = 300, type = "cairo-png")


### means standardised
p <- plot_catch_curve(input = idx_cc, cohort = FALSE, standardize = TRUE,
                      y_label = "numbers or biomass (means standardised)")
if (isTRUE(verbose)) p
ggsave(file = "report/surveys/plots_data_survey_cc_std.png", plot = p, 
       width = 15, height = 13.4, units = "cm", dpi = 300, type = "cairo-png")

### means standardised and by cohort
p <- plot_catch_curve(input = idx_cc, cohort = TRUE, standardize = TRUE,
                      y_label = "numbers or biomass (means standardised)")
if (isTRUE(verbose)) p
ggsave(file = "report/surveys/plots_data_survey_cc_cohort_std.png", plot = p, 
       width = 15, height = 13.4, units = "cm", dpi = 300, type = "cairo-png")

### cohorts on log-scale
p <- plot_catch_curve(input = idx_cc[1:2], cohort = TRUE, standardize = FALSE,
                      y_label = "log numbers", log = TRUE, rm_ages = 1)
ggsave(file = "report/surveys/plots_data_survey_cc_cohort_log.png", plot = p, 
       width = 15, height = 10, units = "cm", dpi = 300, type = "cairo-png")

### ------------------------------------------------------------------------ ###
### survey correlations ####
### ------------------------------------------------------------------------ ###

idx_tmp <- idx_full[c("FSP-7e", "Q1SWBeam")]
idx_tmp$Q1SWBeam <- trim(idx_tmp$Q1SWBeam, age = 2:9)
idx_tmp$`FSP-7e` <- trim(idx_tmp$`FSP-7e`, age = 2:8)
index(idx_tmp$`FSP-7e`)[, ac(2008)] <- NA
names(idx_tmp) <- c("UK-FSP (numbers)", "Q1SWBeam (numbers)")


### survey internal correlations
p <- idx_cor(idx = idx_tmp, ncol = 4)
if (isTRUE(verbose)) p
ggsave(file = "report/surveys/plots_data_survey_cor.png", plot = p,
       width = 15, height = 15, units = "cm", dpi = 300, type = "cairo-png")


### ------------------------------------------------------------------------ ###
### Q1SWBeam ####
### ------------------------------------------------------------------------ ###

p <- as.data.frame(idx_full$Q1SWBeam@index) %>%
  filter(age <= 10) %>%
  mutate(age_label = paste0("age: ", age)) %>%
  mutate(age_label = factor(age_label, levels = paste0("age: ", 1:10))) %>%
  ggplot(aes(x = year, y = data)) +
  geom_line() +
  facet_wrap(~ age_label, scales = "free_y", 
             strip.position = "right", ncol = 1) +
  labs(x = "year", y = "Index (numbers)") +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p
ggsave(file = "report/surveys/plots_data_idx_Q1SWBeam_numbers.png",  plot = p,
       width = 10, height = 15, units = "cm", dpi = 300, type = "cairo-png")

### ------------------------------------------------------------------------ ###
### FSP ####
### ------------------------------------------------------------------------ ###

### numbers at age
p <- as.data.frame(idx_full$`FSP-7e`@index) %>%
  filter(age <= 10) %>%
  mutate(age_label = paste0("age: ", age)) %>%
  #mutate(age_label = age) %>%
  mutate(age_label = factor(age_label, levels = paste0("age: ", 1:10))) %>%
  ggplot(aes(x = year, y = data)) +
  geom_line() +
  facet_wrap(~ age_label, scales = "free_y", 
             strip.position = "right", ncol = 1) +
  labs(x = "year", y = expression("Number"~"hr"^{-1}~"m beam"^{-1})) +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p
ggsave(file = "report/surveys/plots_data_idx_FSP_numbers.png",  plot = p,
       width = 10, height = 15, units = "cm", dpi = 300, type = "cairo-png")

### index biomass
p <- as.data.frame(FLQuants(
  total = quantSums(idx_full$`FSP-7e-biomass`@index),
  age2_8 = quantSums(idx_full$`FSP-7e-biomass`@index[ac(2:8)]))) %>%
  mutate(qname = factor(qname, levels = c("total", "age2_8"), 
                        labels = c("Total biomass", "Biomass ages 2-8"))) %>%
  ggplot(aes(x = year, y = data)) +
  geom_line() +
  facet_wrap(~ qname, ncol = 1) +
  ylim(c(0, NA)) +
  labs(x = "Year", y = expression("kg"~"hr"^{-1}~"m beam"^{-1})) +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p
ggsave(file = "report/surveys/plots_data_idx_FSP_biomass.png",  plot = p,
       width = 10, height = 6, units = "cm", dpi = 300, type = "cairo-png")

### catch curves
idx_cc_fsp <- idx_full[c("FSP-7e", "FSP-7e-biomass")]
idx_cc_fsp$`FSP-7e` <- trim(idx_cc_fsp$`FSP-7e`, age = 1:10)
idx_cc_fsp$`FSP-7e-biomass` <- trim(idx_cc_fsp$`FSP-7e-biomass`, age = 1:10)
names(idx_cc) <- c("UK-FSP (numbers)", "UK-FSP (biomass)")
p <- plot_catch_curve(input = idx_cc_fsp, cohort = FALSE, standardize = FALSE,
                      y_label = "numbers or biomass") +
  theme(legend.position = "none")
if (isTRUE(verbose)) p
ggsave(file = "report/surveys/plots_data_survey_cc_raw_fsp.png", plot = p, 
       width = 15, height = 10, units = "cm", dpi = 300, type = "cairo-png")

