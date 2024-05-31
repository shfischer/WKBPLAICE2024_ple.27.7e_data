### ------------------------------------------------------------------------ ###
### Maturity data from Q1SWBeam survey ####
### ------------------------------------------------------------------------ ###

library(icesTAF)
library(tidyr)
library(stringr)
library(ggplot2)

mkdir("data/maturity")
mkdir("data/maturity/plots")

if (!exists("verbose")) verbose <- FALSE


### ------------------------------------------------------------------------ ###
### data ####
### ------------------------------------------------------------------------ ###
### data provided in Access data base

library(RODBC)
### on Windows, needs a Microsoft Access Database Engine
### e.g. https://www.microsoft.com/en-us/download/details.aspx?id=54920


DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
MDBPATH <- "boot/initial/data/maturity/Q1BeamWesternChannel.accdb"
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)
channel <- odbcDriverConnect(PATH)


### ------------------------------------------------------------------------ ###
### data extract ####
### ------------------------------------------------------------------------ ###

mat <- read.csv("boot/initial/data/maturity/Cefas_maturity.csv")
table(mat$fldMainSpeciesCode)
mat_ <- mat %>% 
  filter(fldMainSpeciesCode == "PLE") %>%
  filter(!is.na(fldFishMaturity) & fldFishMaturity != "")
### keep only Q1SWBeam data - CEND XX/YY
mat_ <- mat_ %>%
  filter(grepl(x = fldCruiseName, pattern = "^CEND"))
mat_ <- mat_ %>%
  mutate(year = gsub(x = fldCruiseName, pattern = "\\D", replacement = "")) %>%
  mutate(year = str_sub(year, -2, -1)) %>%
  mutate(year = as.numeric(year) + 2000)

table(mat$fldCruiseName)
table(mat_$year)
#table(mat_$fldCruiseName[mat_$year == 2023])
table(mat_$fldCruiseName, mat_$year)
table(mat_$fldFishMaturity)
table(mat_$fldCruiseName, mat_$fldFishMaturity)
table(mat_$year, mat_$fldFishMaturity)

### cleaning
summary(mat_)
table(mat_$fldFishLength)
mat_ <- mat_ %>%
  filter(fldFishLength < 1000)

### maturity - immature or mature
mat_ <- mat_ %>% 
  mutate(maturity = case_when(
    fldFishMaturity %in% c(1, "I") ~ "immature",
    fldFishMaturity %in% c(2, 3, 4, 5, "M", "H", "R", "S") ~ "mature",
    fldFishMaturity %in% c(6, "U") ~ "unknown",
    .default = "unknown"
  ))
table(mat_$maturity)
### remove unknown/abnormal/NA
mat_ <- mat_%>%
  filter(maturity %in% c("immature", "mature"))
### proportion mature
mat_smry <- mat_ %>%
  select(year, length = fldFishLength, maturity) %>%
  mutate(tmp = 1) %>%
  group_by(year, length, maturity) %>%
  summarise(count = sum(tmp, na.rm = TRUE)) %>%
  mutate(count = ifelse(!is.na(count), count, 0)) %>%
  pivot_wider(names_from = maturity, values_from = count) %>%
  mutate(immature = ifelse(!is.na(immature), immature, 0),
         mature = ifelse(!is.na(mature), mature, 0)) %>%
  mutate(total = immature + mature) %>%
  mutate(prop_mature = mature/total) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(prop_length = total/sum(total))

###

### plot
p <- mat_smry %>%
  ggplot(aes(x = length/10, y = prop_mature, alpha = prop_length)) +
  geom_point(size = 0.4) +
  facet_wrap(~ year) +
  scale_alpha("Proportion of\nmeasured\nindividuals") +
  labs(x = "Length (cm)", y = "Proportion mature") +
  xlim(c(0, 60)) + 
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave("data/maturity/plots/Q1SWBeam_maturity_prop.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)

mat_ %>%
  filter(year == 2021 & fldFishLength <= 170)


### ------------------------------------------------------------------------ ###
### fit model ####
### ------------------------------------------------------------------------ ###
### logistic regression

glm_2023 <- glm(prop_mature ~ length, data = mat_smry %>% filter(year == 2023), 
                family = binomial(link = "logit"), weights = total)
### length at 50% maturity
lrPerc <- function(cf, p) (log(p/(1-p))-cf[[1]])/cf[[2]]
lrPerc(coef(glm_2023), 0.5)

### for all years
range(mat_smry$year)

yrs <- unique(mat_smry$year)
names(yrs) <- yrs
glms <- lapply(yrs, function(yr) {
  glm(prop_mature ~ length, data = mat_smry %>% filter(year == yr), 
      family = binomial(link = "logit"), weights = total)
})
L50_all <- sapply(glms, function(x) {
  lrPerc(coef(x), 0.5)
})
L50_all <- data.frame(year = as.numeric(names(L50_all)), L50 = L50_all)

### predict values for all years
lngths_pred <- 0:600
pred <- lapply(yrs, function(yr) {#browser()
  data.frame(length = lngths_pred,
             prop_mature = predict(glms[[as.character(yr)]], 
                                   data.frame(length = lngths_pred), 
                                   type = "response"),
             year = yr)
})
pred <- do.call(rbind, pred)

### add fit to plot
### plot
p <-  ggplot() +
  geom_point(data = mat_smry,
             mapping = aes(x = length/10, y = prop_mature, alpha = prop_length),
             size = 0.4) +
  geom_line(data = pred,
            mapping = aes(x = length/10, y = prop_mature),
            colour = "red", linewidth = 0.3) +
  facet_wrap(~ year) +
  scale_alpha("Proportion of\nmeasured\nindividuals") +
  xlim(c(0, 60)) + 
  labs(x = "Length (cm)", y = "Proportion mature") +
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave("data/maturity/plots/Q1SWBeam_maturity_prop_fit.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)

### plot L50 time series
p <- L50_all %>%
  ggplot(aes(x = year, y = L50/10)) +
  geom_line() +
  labs(x = "Year", y = "L50 (cm)") +
  ylim(c(0, NA)) +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p
ggsave("data/maturity/plots/Q1SWBeam_fit_L50.png", 
       width = 10, height = 6, units = "cm", dpi = 300, plot = p)

