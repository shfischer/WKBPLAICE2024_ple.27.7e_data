### ------------------------------------------------------------------------ ###
### survey data ####
### ------------------------------------------------------------------------ ###

library(icesTAF)
taf.libPaths()
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggrastr)
library(icesDatras)
library(mapdata)
library(foreign)
library(FLCore)
library(foreach)

source("utilities.R")

mkdir("data/surveys")
mkdir("data/surveys/plots")

if (!exists("verbose")) verbose <- FALSE

### ------------------------------------------------------------------------ ###
### map - get data from DATRAS for Q1SWBeam ####
### ------------------------------------------------------------------------ ###

### get data from DATRAS
### catch by length
if (isTRUE(verbose)) {
  Q1_HL <- lapply(2006:2023, getHLdata, survey = "BTS", quarter = 1)
  Q1_HL <- bind_rows(Q1_HL)
  Q1_HL <- Q1_HL %>%
    filter(Survey == "BTS" & Quarter == 1 & Country == "GB" & Ship == "74E9")
  saveRDS(Q1_HL, "data/surveys/DATRAS_Q1SWBeam_HL.rds")
}
Q1_HL <- readRDS("data/surveys/DATRAS_Q1SWBeam_HL.rds")

### Haul info
if (isTRUE(verbose)) {
  Q1_HH <- lapply(2006:2023, getHHdata, survey = "BTS", quarter = 1)
  Q1_HH <- bind_rows(Q1_HH)
  Q1_HH <- Q1_HH %>% 
    filter(Survey == "BTS" & Quarter == 1 & Country == "GB" & Ship == "74E9") %>%
    mutate(Latitude = (HaulLat + ShootLat)/2,
           Longitude = (HaulLong + ShootLong)/2)
  saveRDS(Q1_HH, "data/surveys/DATRAS_Q1SWBeam_HH.rds")
}
Q1_HH <- readRDS("data/surveys/DATRAS_Q1SWBeam_HH.rds")


### add haul information
Q1 <- Q1_HL %>%
  select(-RecordType) %>%
  full_join(Q1_HH %>% select(-RecordType))
if (isTRUE(verbose)) head(Q1)

### SpecCode 127143 -> plaice

### combine haul info and species info
Q1_ple <- Q1_HL %>% filter(SpecCode == 127143) %>%
  select(-RecordType, -SpecCode) %>%
  full_join(Q1_HH %>% select(-RecordType))
### summarise catch by haul
Q1_ple <- Q1_ple %>% 
  filter(Survey == "BTS" & Quarter == 1 & Country == "GB" & Ship == "74E9") %>%
  filter(HaulVal == "V") %>%  # only valid hauls
  group_by(Year, HaulNo, Latitude, Longitude, Gear, HaulDur, StatRec) %>%
  mutate(Weight = CatCatchWgt/TotalNo) %>%
  summarise(Numbers = sum(HLNoAtLngt),
            Weight = sum(Weight)) %>%
  ### standardise to catch per hour
  mutate(Numbers = Numbers/HaulDur*60,
         Weight = Weight/HaulDur*60) %>%
  ### average between gears (BT4P/BT4S - left/right)
  ### if only one gear available, use this gear's values
  ungroup() %>%
  group_by(Year, HaulNo, Latitude, Longitude, HaulDur, StatRec) %>%
  summarise(Numbers = mean(Numbers, na.rm = TRUE),
            Weight = mean(Weight, na.rm = TRUE))

### data for maps
coastline <- map_data("worldHires")
StatRec <- read.dbf("boot/initial/data/surveys/StatRec_map_Areas_Full_20170124.dbf")
StatRec7e <- subset(StatRec, Area_27 %in% c("7.e"))
StatRec7e_names <- as.character(StatRec7e$ICESNAME)

Q1_ple_data <- Q1_ple %>%
  mutate(Numbers = ifelse(is.na(Numbers), 0, Numbers),
         Weight = ifelse(is.na(Weight), 0, Weight),
         Occurence = Numbers != 0)

### numbers
p <- ggplot() +
  geom_rect(data = StatRec7e,
            mapping = aes(xmin = WEST, xmax = EAST, ymin = SOUTH, ymax = NORTH),
            fill = "grey90", colour = "grey50", alpha = 0.8, linewidth = 0.1) +
  ### add coastline
  geom_polygon(data = subset(coastline, lat <= 65 & lat >= 40 &
                               long >= -20 & long <= 10),
               aes(x = long, y = lat, group = group), fill = "grey",
               colour = "black", linewidth = 0.3) +
  geom_point(data = Q1_ple %>% 
               filter(StatRec %in% StatRec7e_names),
             aes(x = Longitude, y = Latitude, size = Numbers),
             fill = NA, shape = 21, stroke = 0.4) +
  scale_size("Numbers/hr", range = c(1, 4)) + 
  geom_point(data = Q1_ple %>% 
               filter(StatRec %in% StatRec7e_names) %>%
               filter(is.na(Numbers)) %>%
               mutate(Numbers = 0),
             aes(x = Longitude, y = Latitude, size = Numbers),
             shape = 4, fill = NA, stroke = 0.3, size = 0.4,
             show.legend = FALSE) +
  facet_wrap(~ Year) + 
  labs(x = "Longitude", y = "Latitude") +
  theme_bw(base_size = 8) +
  theme(panel.background = element_rect(fill = "gray97")) +
  coord_cartesian(xlim = c(-7.5, -1.5), ylim = c(47.5, 51), expand = FALSE) +
  scale_x_continuous(breaks = seq(from = -8, to = 2, by = 2)) +
  scale_y_continuous(breaks = seq(from = 48, to = 52, by = 1))
if (isTRUE(verbose)) p
ggsave("data/surveys/plots/map_Q1SWBeam_numbers.png", 
       width = 20, height = 12, units = "cm", dpi = 300, plot = p)
### rasterised version for PDF
p <- ggplot() +
  geom_rect(data = StatRec7e,
            mapping = aes(xmin = WEST, xmax = EAST, ymin = SOUTH, ymax = NORTH),
            fill = "grey90", colour = "grey50", alpha = 0.8, linewidth = 0.1) +
  ### add coastline
  rasterise(geom_polygon(data = subset(coastline, lat <= 65 & lat >= 40 &
                                         long >= -20 & long <= 10),
                         aes(x = long, y = lat, group = group), fill = "grey",
                         colour = "black", linewidth = 0.3), dpi = 600) +
  geom_point(data = Q1_ple %>% 
               filter(StatRec %in% StatRec7e_names),
             aes(x = Longitude, y = Latitude, size = Numbers),
             fill = NA, shape = 21, stroke = 0.4) +
  scale_size("Numbers/hr", range = c(1, 4)) + 
  geom_point(data = Q1_ple %>% 
               filter(StatRec %in% StatRec7e_names) %>%
               filter(is.na(Numbers)) %>%
               mutate(Numbers = 0),
             aes(x = Longitude, y = Latitude, size = Numbers),
             shape = 4, fill = NA, stroke = 0.3, size = 0.4,
             show.legend = FALSE) +
  facet_wrap(~ Year) + 
  labs(x = "Longitude", y = "Latitude") +
  theme_bw(base_size = 8) +
  theme(panel.background = element_rect(fill = "gray97")) +
  coord_cartesian(xlim = c(-7.5, -1.5), ylim = c(47.5, 51), expand = FALSE) +
  scale_x_continuous(breaks = seq(from = -8, to = 2, by = 2)) +
  scale_y_continuous(breaks = seq(from = 48, to = 52, by = 1))
ggsave("data/surveys/plots/map_Q1SWBeam_numbers.pdf", 
       width = 16, height = 10, units = "cm", plot = p)

### biomass
p <- ggplot() +
  geom_rect(data = StatRec7e,
            mapping = aes(xmin = WEST, xmax = EAST, ymin = SOUTH, ymax = NORTH),
            fill = "grey90", colour = "grey50", alpha = 0.8, linewidth = 0.1) +
  ### add coastline
  geom_polygon(data = subset(coastline, lat <= 65 & lat >= 40 &
                               long >= -20 & long <= 10),
               aes(x = long, y = lat, group = group), fill = "grey",
               colour = "black", linewidth = 0.3) +
  geom_point(data = Q1_ple %>% 
               filter(StatRec %in% StatRec7e_names),
             aes(x = Longitude, y = Latitude, size = Weight/1000),
             fill = NA, shape = 21, stroke = 0.4) +
  scale_size("kg/hr", range = c(1, 4)) + 
  geom_point(data = Q1_ple %>% 
               filter(StatRec %in% StatRec7e_names) %>%
               filter(is.na(Numbers)) %>%
               mutate(Numbers = 0),
             aes(x = Longitude, y = Latitude, size = Weight/1000),
             shape = 4, fill = NA, stroke = 0.3, size = 0.4,
             show.legend = FALSE) +
  facet_wrap(~ Year) + 
  labs(x = "Longitude", y = "Latitude") +
  theme_bw(base_size = 8) +
  theme(panel.background = element_rect(fill = "gray97")) +
  coord_cartesian(xlim = c(-7.5, -1.5), ylim = c(47.5, 51), expand = FALSE) +
  scale_x_continuous(breaks = seq(from = -8, to = 2, by = 2)) +
  scale_y_continuous(breaks = seq(from = 48, to = 52, by = 1))
if (isTRUE(verbose)) p
ggsave("data/surveys/plots/map_Q1SWBeam_biomass.png", 
       width = 20, height = 12, units = "cm", dpi = 300, plot = p)
### rasterised version for pdf
p <- ggplot() +
  geom_rect(data = StatRec7e,
            mapping = aes(xmin = WEST, xmax = EAST, ymin = SOUTH, ymax = NORTH),
            fill = "grey90", colour = "grey50", alpha = 0.8, linewidth = 0.1) +
  ### add coastline
  rasterise(geom_polygon(data = subset(coastline, lat <= 65 & lat >= 40 &
                                         long >= -20 & long <= 10),
                         aes(x = long, y = lat, group = group), fill = "grey",
                         colour = "black", linewidth = 0.3), dpi = 600) +
  geom_point(data = Q1_ple %>% 
               filter(StatRec %in% StatRec7e_names),
             aes(x = Longitude, y = Latitude, size = Weight/1000),
             fill = NA, shape = 21, stroke = 0.4) +
  scale_size("kg/hr", range = c(1, 4)) + 
  geom_point(data = Q1_ple %>% 
               filter(StatRec %in% StatRec7e_names) %>%
               filter(is.na(Numbers)) %>%
               mutate(Numbers = 0),
             aes(x = Longitude, y = Latitude, size = Weight/1000),
             shape = 4, fill = NA, stroke = 0.3, size = 0.4,
             show.legend = FALSE) +
  facet_wrap(~ Year) + 
  labs(x = "Longitude", y = "Latitude") +
  theme_bw(base_size = 8) +
  theme(panel.background = element_rect(fill = "gray97")) +
  coord_cartesian(xlim = c(-7.5, -1.5), ylim = c(47.5, 51), expand = FALSE) +
  scale_x_continuous(breaks = seq(from = -8, to = 2, by = 2)) +
  scale_y_continuous(breaks = seq(from = 48, to = 52, by = 1))
ggsave("data/surveys/plots/map_Q1SWBeam_biomass.pdf", 
       width = 16, height = 10, units = "cm", plot = p)

### ------------------------------------------------------------------------ ###
### UK-FSP ####
### ------------------------------------------------------------------------ ###

### original filenames
### TotalBioData_PLEv2.csv -> FSP_biomass.csv
### DataFor_MeanNbyAgeP2_PLEv2.csv -> FSP_numbers.csv

### load data
FSP_numbers <- read.csv("boot/initial/data/surveys/FSP_numbers.csv")
FSP_biomass <- read.csv("boot/initial/data/surveys/FSP_biomass.csv")

### format
FSP_numbers <- FSP_numbers %>%
  select(-X_TYPE_, -X_FREQ_) %>%
  pivot_longer(cols = starts_with("AAge"),
               names_prefix = "AAge", 
               names_to = "age", values_to = "numbers") %>%
  mutate(age = as.numeric(as.character(age)))
FSP_biomass <- FSP_biomass %>%
  select(-X_TYPE_, -X_FREQ_, -TBio_index) %>%
  pivot_longer(cols = starts_with("WAAge"),
               names_prefix = "WAAge", 
               names_to = "age", values_to = "biomass") %>%
  mutate(age = as.numeric(as.character(age)))
### average weight at age
FSP_weight <- full_join(FSP_numbers,
                        FSP_biomass) %>%
  mutate(weight = biomass/numbers)


### bubble plot of numbers
p <- FSP_numbers %>%
  mutate(numbers = ifelse(numbers > 0, numbers, NA)) %>%
  ggplot(aes(x = year, y = age, size = numbers)) +
  geom_point(shape = 21) +
  #scale_size(expression(Numbers h[1])) + 
  scale_size(bquote(Numbers~h^-1~m~beam^-1), range = c(0, 6)) + 
  labs(x = "Year", y = "Age (years)") +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p
ggsave("data/surveys/plots/FSP_bubbles_numbers.png", 
       width = 12, height = 7, units = "cm", dpi = 300, plot = p)
ggsave("data/surveys/plots/FSP_bubbles_numbers.pdf", 
       width = 12, height = 7, units = "cm", plot = p)

### plot numbers and biomass
cols <- scales::hue_pal()(length(1:10))
cols <- cols[c(seq(from = 1, to = length(cols), by = 3),
               seq(from = 2, to = length(cols), by = 3),
               seq(from = 3, to = length(cols), by = 3))]
p <- full_join(FSP_numbers %>% mutate(type = "Numbers"),
          FSP_biomass %>% mutate(type = "Biomass") %>%
            rename(numbers = biomass)) %>%
  filter(age <= 10) %>%
  mutate(age = factor(age, levels = sort(unique(age)))) %>%
  ggplot(aes(x = year, y = numbers, colour = age)) +
  geom_line() +
  geom_text(aes(label = age), show.legend = FALSE) +
  scale_colour_manual("Age (years)", values = cols) + 
  facet_wrap(~ type, scales = "free_y") + 
  labs(x = "Year", y = bquote(Numbers/Biomass~h^-1~m~beam^-1)) +
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave("data/surveys/plots/FSP_numbers_biomass.png", 
       width = 20, height = 12, units = "cm", dpi = 300, plot = p)
ggsave("data/surveys/plots/FSP_numbers_biomass.pdf", 
       width = 16, height = 8, units = "cm", plot = p)

### weight at age
p <- FSP_weight %>%
  pivot_longer(c(numbers, biomass, weight), 
               names_to = "unit") %>%
  mutate(unit = factor(unit, 
                       levels = c("numbers", "biomass", "weight"),
                       labels = c("Numbers~h^-1~m~beam^-1",
                                  "kg~h^-1~m~beam^-1",
                                  "Weight~at~age~(kg)"))) %>%
  filter(age <= 10) %>%
  mutate(age = factor(age, levels = sort(unique(age)))) %>%
  ggplot(aes(x = year, y = value, colour = age)) +
  geom_line(linewidth = 0.3) +
  geom_text(aes(label = age), show.legend = FALSE, size = 2) +
  facet_wrap(~ unit, scales = "free_y", ncol = 1, labeller = label_parsed, 
             strip.position = "left") +
  scale_colour_manual("Age (years)", values = cols) +
  labs(x = "Year", y = "Weight at age (kg)") +
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"),
        axis.title.y = element_blank(), 
        strip.placement = "outside", 
        strip.background = element_blank(),
        strip.text = element_text(size = 8))
if (isTRUE(verbose)) p
ggsave("data/surveys/plots/FSP_weight_at_age.png", 
       width = 10, height = 10, units = "cm", dpi = 300, plot = p)
ggsave("data/surveys/plots/FSP_weight_at_age.pdf", 
       width = 10, height = 10, units = "cm", plot = p)
  

### ------------------------------------------------------------------------ ###
### Q1SWBeam ####
### ------------------------------------------------------------------------ ###

### V2i = full area, 2022 index numbers cover smaller area (11/13 strata)
### V2ii = reduced area in all years (11 strata)
### => use V2i

### load data
Q1SWBeam_numbers <- read.csv("boot/initial/data/surveys/Q1SWBEAM_IndexV2i.csv")

### format
Q1SWBeam_numbers <- Q1SWBeam_numbers %>%
  select(-X_TYPE_, -X_FREQ_, -Area, -count)
Q1SWBeam_numbers <- full_join(Q1SWBeam_numbers %>%
            select(Year, starts_with("AAge")) %>%
            pivot_longer(-Year, names_to = "Age", values_to = "Numbers") %>%
            mutate(Age = gsub(x = Age, pattern = "AAge", replacement = "")) %>%
            mutate(Age = as.numeric(as.character(Age))),
          Q1SWBeam_numbers %>%
            select(Year, starts_with("WWAge")) %>%
            pivot_longer(-Year, names_to = "Age", values_to = "Biomass") %>%
            mutate(Age = gsub(x = Age, pattern = "WWAge_", replacement = "")) %>%
            mutate(Age = as.numeric(as.character(Age)))) %>%
  mutate(Weight = Biomass/Numbers)


### bubble plot of numbers
p <- Q1SWBeam_numbers %>%
  mutate(Numbers = ifelse(Numbers > 0, Numbers, NA)) %>%
  ggplot(aes(x = Year, y = Age, size = Numbers)) +
  geom_point(shape = 21) +
  #scale_size(expression(Numbers h[1])) + 
  scale_size(bquote(Numbers~km^-2), range = c(0, 6)) + 
  labs(x = "Year", y = "Age (years)") +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p
ggsave("data/surveys/plots/Q1SWBeam_bubbles_numbers.png", 
       width = 12, height = 7, units = "cm", dpi = 300, plot = p)
ggsave("data/surveys/plots/Q1SWBeam_bubbles_numbers.pdf", 
       width = 12, height = 7, units = "cm", plot = p)

### plot numbers and biomass
cols <- scales::hue_pal()(length(1:10))
cols <- cols[c(seq(from = 1, to = length(cols), by = 3),
               seq(from = 2, to = length(cols), by = 3),
               seq(from = 3, to = length(cols), by = 3))]
p <- Q1SWBeam_numbers %>%
  pivot_longer(-1:-2) %>%
  filter(Age <= 10) %>%
  mutate(Age = factor(Age, levels = sort(unique(Age)))) %>%
  filter(name %in% c("Biomass", "Numbers")) %>%
  ggplot(aes(x = Year, y = value, colour = Age)) +
  geom_line() +
  geom_text(aes(label = Age), show.legend = FALSE) +
  scale_colour_manual("Age (years)", values = cols) + 
  facet_wrap(~ name, scales = "free_y") + 
  labs(x = "Year", y = bquote(Numbers/Biomass~km^-2)) +
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave("data/surveys/plots/Q1SWBeam_numbers_biomass.png", 
       width = 20, height = 12, units = "cm", dpi = 300, plot = p)
ggsave("data/surveys/plots/Q1SWBeam_numbers_biomass.pdf", 
       width = 16, height = 8, units = "cm", plot = p)

### weight at age
p <- Q1SWBeam_numbers %>%
  pivot_longer(-1:-2) %>%
  filter(Age <= 10) %>%
  mutate(Age = factor(Age, levels = sort(unique(Age)))) %>%
  mutate(name = factor(name, 
                       levels = c("Numbers", "Biomass", "Weight"),
                       labels = c("Numbers~km^-2",
                                  "kg~km^-2",
                                  "Weight~at~age~(kg)"))) %>%
  ggplot(aes(x = Year, y = value, colour = Age)) +
  geom_line(linewidth = 0.3) +
  geom_text(aes(label = Age), show.legend = FALSE, size = 2) +
  facet_wrap(~ name, scales = "free_y", ncol = 1, labeller = label_parsed, 
             strip.position = "left") +
  scale_colour_manual("Age (years)", values = cols) +
  labs(x = "Year", y = "Weight at age (kg)") +
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"),
        axis.title.y = element_blank(), 
        strip.placement = "outside", 
        strip.background = element_blank(),
        strip.text = element_text(size = 8))
if (isTRUE(verbose)) p
ggsave("data/surveys/plots/Q1SWBeam_weight_at_age.png", 
       width = 10, height = 10, units = "cm", dpi = 300, plot = p)
ggsave("data/surveys/plots/Q1SWBeam_weight_at_age.pdf", 
       width = 10, height = 10, units = "cm", plot = p)

### ------------------------------------------------------------------------ ###
### age correlations ####
### ------------------------------------------------------------------------ ###

FSP_qnt <- as.FLQuant(FSP_numbers %>%
                        select(year, age, data = numbers) %>%
                        filter(age <= 10))

p <- idx_cor(FLIndices("UK-FSP" = FLIndex(index = FSP_qnt)))
if (isTRUE(verbose)) p
ggsave("data/surveys/plots/FSP_correlations.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)
ggsave("data/surveys/plots/FSP_correlations.pdf", 
       width = 15, height = 10, units = "cm", plot = p)

Q1SWBeam_qnt <- as.FLQuant(Q1SWBeam_numbers %>%
                        select(year = Year, age = Age, data = Numbers) %>%
                        filter(age <= 10))

p <- idx_cor(FLIndices("Q1SWBeam" = FLIndex(index = Q1SWBeam_qnt)))
if (isTRUE(verbose)) p
ggsave("data/surveys/plots/Q1SWBeam_correlations.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)
ggsave("data/surveys/plots/Q1SWBeam_correlations.pdf", 
       width = 15, height = 10, units = "cm", plot = p)

### ------------------------------------------------------------------------ ###
### final data ####
### ------------------------------------------------------------------------ ###

### save
saveRDS(Q1SWBeam_numbers, file = "data/OM/idx_Q1SWBeam.rds")
saveRDS(FSP_weight, file = "data/OM/idx_FSP.rds")

