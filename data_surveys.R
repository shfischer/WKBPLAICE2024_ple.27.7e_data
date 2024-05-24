### ------------------------------------------------------------------------ ###
### survey data ####
### ------------------------------------------------------------------------ ###

library(icesTAF)
taf.libPaths()
library(tidyr)
library(dplyr)
library(ggplot2)
library(icesDatras)
library(mapdata)
library(foreign)

mkdir("data/surveys")
mkdir("data/surveys/plots")

if (!exists("verbose")) verbose <- FALSE

### ------------------------------------------------------------------------ ###
### map - get data from DATRAS for Q1SWBeam ####
### ------------------------------------------------------------------------ ###

### get data from DATRAS
### catch by length
Q1_HL <- lapply(2006:2023, getHLdata, survey = "BTS", quarter = 1)
Q1_HL <- bind_rows(Q1_HL)
Q1_HL <- Q1_HL %>%
  filter(Survey == "BTS" & Quarter == 1 & Country == "GB" & Ship == "74E9")
saveRDS(Q1_HL, "data/surveys/DATRAS_Q1SWBeam_HL.rds")
Q1_HL <- readRDS("data/surveys/DATRAS_Q1SWBeam_HL.rds")

### Haul info
Q1_HH <- lapply(2006:2023, getHHdata, survey = "BTS", quarter = 1)
Q1_HH <- bind_rows(Q1_HH)
Q1_HH <- Q1_HH %>% 
  filter(Survey == "BTS" & Quarter == 1 & Country == "GB" & Ship == "74E9") %>%
  mutate(Latitude = (HaulLat + ShootLat)/2,
         Longitude = (HaulLong + ShootLong)/2)
saveRDS(Q1_HH, "data/surveys/DATRAS_Q1SWBeam_HH.rds")
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
       width = 20, height = 14, units = "cm", dpi = 300, plot = p)


