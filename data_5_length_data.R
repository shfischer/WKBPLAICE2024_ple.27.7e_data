### ------------------------------------------------------------------------ ###
### length data ####
### ------------------------------------------------------------------------ ###
### from InterCatch and accessions

## Before: many InterCatch data files: boot/data/InterCatch/*
##         boot/data/accessions/discards_alk.csv
##         boot/data/accessions/landings_alk.csv
##         
## After:  some temporary InterCatch files: boot/data/InterCatch/*
##         historical time series updateded: boot/data/InterCatch/*
##         some data plots: data/plots_*.png

### ------------------------------------------------------------------------ ###
### prepare ####
### ------------------------------------------------------------------------ ###

### load packages
suppressPackageStartupMessages(library(icesAdvice))
suppressPackageStartupMessages(library(icesTAF))
taf.libPaths()
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(Cairo))
suppressPackageStartupMessages(library(FLCore))
suppressPackageStartupMessages(library(icesDatras))

### create folder to store data
mkdir("data/length/")

### load additional functions
source("utilities.R")

### set WG year
year <- 2023
data_year <- year - 1

if (!exists("verbose")) verbose <- FALSE

### ------------------------------------------------------------------------ ###
### revisions to previous years? ####
### ------------------------------------------------------------------------ ###
revs <- list.dirs("boot/data/InterCatch/length/", recursive = FALSE, 
                  full.names = FALSE)
revs <- revs[grep(x = revs, pattern = "[0-9]{4}_revision")]
yrs_dirs <- c("", revs)
yrs <- c(data_year, as.numeric(gsub(x = revs, pattern = "_revision", 
                                    replacement = "")))
names(yrs_dirs) <- yrs

### ------------------------------------------------------------------------ ###
### preprocessing - before raising ####
### ------------------------------------------------------------------------ ###
### InterCatch
### 8. Extract and View Imported Stock/Year Data
### "Check Catch"
### click "Export CATONs with details" button -> download StockOverview.txt
### click "Export Numbers and Mean Weights at age" button -> download zip file

### unzip files from InterCatch export (before raising)
. <- lapply(yrs_dirs, function(x) {
  taf.unzip(zipfile = paste0("boot/data/InterCatch/length/", x, 
                             "/before_raising/",
                             "Numbers_at_age_and_mean_weights_at_age.zip"), 
      exdir = paste0("boot/data/InterCatch/length/", x, "/before_raising"))
})

### ------------------------------------------------------------------------ ###
### load overview
StockOverview <- lapply(yrs_dirs, function(x) {
  tmp <- read.table(paste0("boot/data/InterCatch/length/", x,
                           "/before_raising/StockOverview.txt"),
                    sep = "\t", header = TRUE, as.is = TRUE)
  tmp$Season <- gsub(x = tmp$Season,
                     pattern = "[0-9]{4}",
                     replacement = "annual")
  return(tmp)
})
### load also previous years
StockOverview_hist <- read.csv(paste0("boot/data/InterCatch/length/",
                                      "before_raising/",
                                      "StockOverview_hist.csv"),
                               as.is = TRUE)
StockOverview_hist$Season <- gsub(x = StockOverview_hist$Season,
                                  pattern = "[0-9]{4}",
                                  replacement = "annual")
### remove values for current year, in case they are already in there
StockOverview_hist <- StockOverview_hist %>%
  filter(!Year %in% yrs)
### combine
StockOverview_all <- Reduce(rbind, append(StockOverview, 
                                          list(StockOverview_hist)))

### set yearly catch to "annual"
StockOverview_all$Season <- gsub(x = StockOverview_all$Season,
                                 pattern = "[0-9]{4}",
                                 replacement = "annual")
### save entire InterCatch history
### including current year
write.csv(StockOverview_all, 
          file = paste0("boot/data/InterCatch/length/before_raising/",
                        "StockOverview_hist.csv"),
          row.names = FALSE)
### load
# StockOverview_all <- read.csv(paste0("boot/data/InterCatch/length/",
#                                      "before_raising/StockOverview_hist.csv"),
#                               as.is = TRUE)

### first: summarise submitted data
### spread out seasons
subm <- StockOverview_all %>%
  select(Year, Country, Season, Fleets, Catch.Cat., Catch..kg) %>%
  mutate(Catch..kg = Catch..kg/1000) %>%
  tidyr::spread(key = Season, value = Catch..kg)
### split into catch categories
subm <- split(subm, subm$Catch.Cat.)
subm_cat <- names(subm)
names(subm_cat) <- subm_cat
### modify season names to include catch category
subm <- lapply(subm_cat, function(x) {
  tmp_cat <- switch(x,
                    "BMS landing" = "BMS_Land",
                    "Discards" = "Disc",
                    "Landings" = "Land",
                    "Logbook Registered Discard" = "LogBDisc"
                    )
  tmp_season <- names(subm[[x]])[grep(x = names(subm[[x]]), 
                                 pattern = "^[1234]$|annual")]
  tmp <- paste0(tmp_cat, "-", tmp_season)
 
  names(subm[[x]])[grep(x = names(subm[[x]]), 
                        pattern = "^[1234]$|annual")] <- tmp
  subm[[x]]$Catch.Cat. <- NULL ### remove catch category column
  return(subm[[x]])
})
subm <- subm[c("Landings", "Discards", "BMS landing", 
               "Logbook Registered Discard")]
subm <- Reduce(function(...) full_join(..., by = c("Year", "Country", "Fleets")), 
               subm)

### overview of submitted data
if (isTRUE(verbose))
  View(subm %>% filter(Year == data_year))

if (isTRUE(verbose))
  StockOverview_all %>%
    group_by(Year, Catch.Cat., Country) %>%
    summarise(catch = sum(Catch..kg)) %>%
    filter(Year == data_year)
### catch by country and year
if (isTRUE(verbose))
  StockOverview_all %>% 
  group_by(Year, Catch.Cat.) %>%
  summarize(catch = sum(Catch..kg)) %>%
  print(n = Inf)

### ------------------------------------------------------------------------ ###
### submitted sample data
### load data for current year
samples <- lapply(yrs_dirs, function(x) {
  tmp <- read.table(paste0("boot/data/InterCatch/length/", x, 
                           "/before_raising/NumbersAtAgeLength.txt"),
                    sep = "\t", header = TRUE, skip = 2, as.is = TRUE)
  tmp$Season <- gsub(x = tmp$Season, pattern = "[0-9]{4}",
                     replacement = "annual")
  return(tmp)
})
### load also previous years
samples_hist <- read.csv(paste0("boot/data/InterCatch/length/", 
                                "before_raising/NumbersAtAgeLength_hist.csv"), 
                         as.is = TRUE)
samples_hist$Season <- gsub(x = samples_hist$Season, pattern = "[0-9]{4}",
                            replacement = "annual")
### remove values for current year, in case they are already in there
samples_hist <- samples_hist %>%
  filter(!Year %in% yrs)

### combine
samples_all <- Reduce(bind_rows, append(samples, 
                                          list(samples_hist)))

### replace year with "annual" as season
samples_all$Season <- gsub(x = samples_all$Season, pattern = "[0-9]{4}",
                           replacement = "annual")

### remove duplicates
samples_all <- samples_all %>% select(-X)
samples_all <- unique(samples_all)

### save entire history including current year
write.csv(samples_all, 
          file = paste0("boot/data/InterCatch/length/before_raising/",
                        "NumbersAtAgeLength_hist.csv"),
          row.names = FALSE)

### load
# samples_all <- read.csv(file = paste0("boot/data/InterCatch/before_rais",
#                                       "ing/NumbersAtAgeLength_hist.csv"))


### countries that submitted age distributions
if (isTRUE(verbose))
  aggregate(x = Catch.Cat. ~ Country + Year, data = samples_all,
            FUN = unique)
### in all years only UK data, but for Landings & Discards
### 2021: BE & FR & UKE: D & L
### 2022: BE, FR, UKE

### spread
### spread out seasons
subm_samples <- samples_all %>%
  #mutate(dummy = "X") %>%
  select(Year, Country, Season, Fleets, Catch.Cat.) %>%
  tidyr::spread(key = Season, value = Season)

### split into catch categories
subm_samples <- split(subm_samples, subm_samples$Catch.Cat.)
subm_cat <- names(subm_samples)
subm_cat <- c("Discards", "Landings")
names(subm_cat) <- subm_cat
names(subm_samples) <- names(subm_cat)
### modify season names to include catch category
subm_samples <- lapply(subm_cat, function(x) {#browser()
  tmp <- subm_samples[[x]]
  names(tmp)[grep(x = names(tmp), pattern = "^[0-9]{1,9}")] <- 
    paste0(substr(x = x, start = 1, stop = 4), "_sample-",
           names(tmp)[grep(x = names(tmp), pattern = "[0-9]{1,9}")])
  names(tmp)[grep(x = names(tmp), pattern = "^annual$")] <- 
    paste0(substr(x = x, start = 1, stop = 4), "_sample-annual")
  tmp$Catch.Cat. <- NULL ### remove catch category column
  return(tmp)
})
subm_samples <- subm_samples[rev(names(subm_samples))]
subm_samples <- Reduce(function(...) 
  full_join(..., by = c("Year", "Country", "Fleets")), 
  subm_samples)

### overview of submitted data
if (isTRUE(verbose))
  View(subm_samples %>% filter(Year == data_year))

### merge catch and samples
data_submitted <- full_join(subm, subm_samples, 
                            by = c("Year", "Country", "Fleets"))
data_submitted <- data_submitted %>%
  select(Year:`Land-annual`, `Land_sample-1`:`Land_sample-4`, 
         `Land_sample-annual`,
         `Disc-1`:`Disc-annual`, `Disc_sample-1`:`Disc_sample-4`, 
         `Disc_sample-annual`,
         `BMS_Land-1`:`LogBDisc-annual`)

if (isTRUE(verbose))
  View(data_submitted)
if (isTRUE(verbose))
  View(data_submitted[data_submitted$Year == data_year, ])


### ------------------------------------------------------------------------ ###
### discard rates
data_disc <- data_submitted %>%
  mutate("Disc_rate-1" = `Disc-1` / (`Disc-1` + `Land-1`),
         "Disc_rate-2" = `Disc-2` / (`Disc-2` + `Land-2`),
         "Disc_rate-3" = `Disc-3` / (`Disc-3` + `Land-3`),
         "Disc_rate-4" = `Disc-4` / (`Disc-4` + `Land-4`),
         "Disc_rate-annual" = `Disc-annual` / (`Disc-annual` + `Land-annual`)) %>%
  dplyr::select(Year:`Disc-annual`, `Disc_rate-1`:`Disc_rate-annual`, 
                `Disc_sample-1`:`LogBDisc-annual`)

### find missing discards
data_disc <- data_disc %>%
  mutate(`Disc_rate-1` = ifelse(!is.na(`Land-1`) & is.na(`Disc-1`), 
                                "X", round(`Disc_rate-1`, 2)),
         `Disc_rate-2` = ifelse(!is.na(`Land-2`) & is.na(`Disc-2`), 
                                "X", round(`Disc_rate-2`, 2)),
         `Disc_rate-3` = ifelse(!is.na(`Land-3`) & is.na(`Disc-3`), 
                                "X", round(`Disc_rate-3`, 2)),
         `Disc_rate-4` = ifelse(!is.na(`Land-4`) & is.na(`Disc-4`), 
                                "X", round(`Disc_rate-4`, 2)),
         `Disc_rate-annual` = ifelse(!is.na(`Land-annual`) & is.na(`Disc-annual`), 
                                "X", round(`Disc_rate-annual`, 2)))

### export, can then be used as guidance for allocations in InterCatch
. <- lapply(yrs, function(x) {
  write.csv(data_disc %>% filter(Year == x), 
          file = paste0("data/length/IC_", x, ".csv"), row.names = FALSE, na = "")
})

if (isTRUE(verbose))
  data_disc %>% filter(Year == max(Year)) %>% 
    select(Year, Country, Fleets, "Disc_rate-1", "Disc_rate-2", "Disc_rate-3",
           "Disc_rate-3", "Disc_rate-4", "Disc_rate-annual")

### ------------------------------------------------------------------------ ###
### find fleets where discard rates can be borrowed from other seasons
pos <- apply(data_disc, 1, function(x) {
  rates <- x[paste0("Disc_rate-", c(1:4, "annual"))]
  if (isTRUE(any(!is.na(as.numeric(rates)))) & isTRUE(any(rates == "X"))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})
groups_season <- data_disc[pos, ] %>% 
  filter(Year == data_year) %>%
  select(Year:Fleets, `Disc_rate-1`:`Disc_rate-annual`)
if (isTRUE(verbose))
  groups_season

### ------------------------------------------------------------------------ ###
### find fleets where discard rates can be borrowed from other countries

groups_country <- data_disc %>%
  select(Year:Fleets, `Disc_rate-1`:`Disc_rate-annual`) %>%
  mutate(Fleets = substr(toupper(Fleets), start = 1, stop = 12)) %>%
  filter(Year == data_year) %>%
  group_by(Fleets) %>%
  mutate(keep = ifelse(
    isTRUE(any(!is.na(as.numeric(c(`Disc_rate-1`, `Disc_rate-2`, `Disc_rate-3`, 
                                   `Disc_rate-4`, `Disc_rate-annual`))))) &
    isTRUE(any(c(`Disc_rate-1`, `Disc_rate-2`, `Disc_rate-3`, 
                 `Disc_rate-4`, `Disc_rate-annual`) == "X")) &
    isTRUE(length(`Disc_rate-1`) > 1),
    TRUE, FALSE)) %>%
  filter(keep == TRUE) %>%
  select(-keep) %>%
  arrange(Fleets, Country)
if (isTRUE(verbose))
  groups_country %>% print(width = Inf)

### ------------------------------------------------------------------------ ###
### visualize length distributions

### reshape data
samples_all <- samples_all[, which(names(samples_all) != "X")]
samples_df <- gather(samples_all, key = "length", value = "numbers", 
                     16:ncol(samples_all))
samples_df$sex <- gsub(x = samples_df$length, pattern = "[0-9]", replacement = "")
samples_df$sex <- gsub(x = samples_df$sex, pattern = "Lngt", replacement = "")
samples_df$length <- as.numeric(gsub(x = samples_df$length, 
                                pattern = "UndeterminedLngt|MaleLngt|FemaleLngt",
                                replacement = ""))
samples_df$Catch.Cat.[samples_df$Catch.Cat. == "L"] <- "Landings"
samples_df$Catch.Cat.[samples_df$Catch.Cat. == "D"] <- "Discards"
### factor for facetting
samples_df$facet <- with(samples_df, paste0(Country, " - ", Catch.Cat., 
                                            " -  Q", Season, "\n", Fleets))
samples_df <- samples_df %>%
  mutate(Catch.Cat. = factor(Catch.Cat., levels = c("Discards", "Landings")))
### extract some sample info
samples_df_info <- samples_df %>% group_by(facet, Year, Catch.Cat.) %>%
  summarise(length_max = max(length, na.rm = TRUE), 
            n_max = max(numbers, na.rm = TRUE),
            NumAgeMeasurement = mean(NumAgeMeasurement),
            NumLengthMeasurements = mean(NumLengthMeasurements),
            NumSamplesAge = mean(NumSamplesAge),
            NumSamplesLength = mean(NumSamplesLength),
            Country = unique(Country)) %>%
  mutate(Catch.Cat. = factor(Catch.Cat., levels = c("Discards", "Landings")))

### plot
p <- samples_df %>%
  filter(Year == data_year) %>%
  ggplot(aes(x = length, y = numbers)) +
  geom_bar(stat = "identity", aes(fill = Catch.Cat.)) +
  facet_wrap(~ facet, scale = "free_y") +
  theme_bw(base_size = 8) + 
  geom_text(data = samples_df_info %>%
              filter(Year == data_year),
            aes(x = length_max*0.95, y = n_max*0.95,
                label = paste0("length samples: ", NumSamplesLength, "\n",
                               "length readings: ", NumLengthMeasurements)),
            hjust = 1, vjust = 1, size = 2.5) +
  labs(x = "length [mm]", y = "catch numbers") +
  ylim(0, NA) +
  scale_fill_discrete("")
if (isTRUE(verbose)) p
ggsave(file = paste0("data/length/plots_length_samples_sep_", data_year, ".png"),
       width = 30, height = 20,  plot = p,
       units = "cm", dpi = 300, type = "cairo")
p <- samples_df %>%
  filter(Year == data_year & 
           Country %in% c("Belgium", "France")) %>%
  ggplot(aes(x = length, y = numbers)) +
  geom_bar(stat = "identity", aes(fill = Catch.Cat.)) +
  facet_wrap(~ facet, scale = "free_y", ncol = 3) +
  theme_bw(base_size = 8) + 
  geom_text(data = samples_df_info %>%
              filter(Year == data_year & 
                       Country %in% c("Belgium", "France")),
            aes(x = length_max*0.95, y = n_max*0.95,
                label = paste0("length samples: ", NumSamplesLength, "\n",
                               "length readings: ", NumLengthMeasurements)),
            hjust = 1, vjust = 1, size = 2.5) +
  labs(x = "length [mm]", y = "catch numbers") +
  ylim(0, NA) +
  scale_fill_discrete("")
if (isTRUE(verbose)) p
ggsave(file = paste0("data/length/plots_length_samples_sep_", data_year, 
                     "_BE_FR.png"),
       width = 15, height = 8,  plot = p,
       units = "cm", dpi = 300, type = "cairo")
p <- samples_df %>%
  filter(Year == data_year & 
           Country %in% c("UK (England)") &
           Catch.Cat. == "Discards") %>%
  ggplot(aes(x = length, y = numbers)) +
  geom_bar(stat = "identity", aes(fill = Catch.Cat.)) +
  facet_wrap(~ facet, scale = "free_y", ncol = 3) +
  theme_bw(base_size = 8) + 
  geom_text(data = samples_df_info %>%
              filter(Year == data_year & 
                       Country %in% c("UK (England)") &
                       Catch.Cat. == "Discards"),
            aes(x = length_max*0.95, y = n_max*0.95,
                label = paste0("length samples: ", NumSamplesLength, "\n",
                               "length readings: ", NumLengthMeasurements)),
            hjust = 1, vjust = 1, size = 2.5) +
  labs(x = "length [mm]", y = "catch numbers") +
  ylim(0, NA) +
  scale_fill_discrete("")
if (isTRUE(verbose)) p
ggsave(file = paste0("data/length/plots_length_samples_sep_", data_year, 
                     "_UKE_D.png"),
       width = 15, height = 14,  plot = p,
       units = "cm", dpi = 300, type = "cairo")
p <- samples_df %>%
  filter(Year == data_year & 
           Country %in% c("UK (England)") &
           Catch.Cat. == "Landings") %>%
  ggplot(aes(x = length, y = numbers)) +
  geom_bar(stat = "identity", aes(fill = Catch.Cat.)) +
  scale_fill_manual("", 
                    values = c(Discards = "#F8766D", Landings = "#00BFC4")) +
  facet_wrap(~ facet, scale = "free_y", ncol = 3) +
  theme_bw(base_size = 8) + 
  geom_text(data = samples_df_info %>%
              filter(Year == data_year & 
                       Country %in% c("UK (England)") &
                       Catch.Cat. == "Landings"),
            aes(x = length_max*0.95, y = n_max*0.95,
                label = paste0("length samples: ", NumSamplesLength, "\n",
                               "length readings: ", NumLengthMeasurements)),
            hjust = 1, vjust = 1, size = 2.5) +
  labs(x = "length [mm]", y = "catch numbers") +
  ylim(0, NA)# +
  #scale_fill_discrete("")
if (isTRUE(verbose)) p
ggsave(file = paste0("data/length/plots_length_samples_sep_", data_year, 
                     "_UKE_L.png"),
       width = 15, height = 16,  plot = p,
       units = "cm", dpi = 300, type = "cairo")


### ------------------------------------------------------------------------ ###
### groups for length allocations ####
### ------------------------------------------------------------------------ ###

### ------------------------------------------------------------------------ ###
### find fleets where length samples from some quarters can be used for others

### landings
pos <- apply(data_disc, 1, function(x) {#browser()
  landings <- as.numeric(x[paste0("Land-", c(1:4, "annual"))])
  lsample <- as.numeric(x[paste0("Land_sample-", c(1:4, "annual"))])
  if (isTRUE(any(!is.na(lsample))) & 
      any(!is.na(landings) & is.na(lsample))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})
groups_season <- data_disc[pos, ] %>% 
  filter(Year == data_year) %>%
  select(Year:`Land_sample-annual`)
if (isTRUE(verbose)) groups_season

### discards
pos <- apply(data_disc, 1, function(x) {
  # if (x["Fleets"] == "OTB_DEF_>=120_0_0_all" & x["Year"] == 2019 &
  #     x["Country"] == "UK (England)") browser()
  ### use discard rate because raised discards also need age distributions
  discards <- x[paste0("Disc_rate-", c(1:4, "annual"))]
  dsample <- x[paste0("Disc_sample-", c(1:4, "annual"))]
  if (isTRUE(any(!is.na(dsample))) & 
      any(!is.na(discards) & is.na(dsample))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})
groups_season <- data_disc[pos, ] %>% 
  filter(Year == data_year) %>%
  select(Year:Fleets, `Disc-1`:`Disc_sample-annual`)
if (isTRUE(verbose)) groups_season

### ------------------------------------------------------------------------ ###
### find fleets where samples can be borrowed from other countries

### landings
groups_country <- data_disc %>%
  mutate(Fleets2 = substr(toupper(Fleets), start = 1, stop = 12)) %>%
  select(Year, Country, Fleets2, `Land-1`:`Land_sample-4`, 
         `Land_sample-annual`) %>%
  filter(Year == data_year) %>%
  group_by(Fleets2) %>%
  mutate(keep = ifelse(
    isTRUE(any(!is.na(as.numeric(c(`Land-1`, `Land-2`, `Land-3`, 
                                   `Land-4`, `Land-annual`))))) &
    isTRUE(any(!is.na(c(`Land_sample-1`, `Land_sample-2`, `Land_sample-3`, 
                 `Land_sample-4`, `Land_sample-annual`)))) &
    isTRUE(length(`Land_sample-1`) > 1),
    TRUE, FALSE)) %>%
  filter(keep == TRUE) %>%
  select(-keep) %>%
  arrange(Fleets2, Country)
if (isTRUE(verbose)) groups_country %>% print(width = Inf)

### discards
groups_country <- data_disc %>%
  mutate(Fleets2 = substr(toupper(Fleets), start = 1, stop = 12)) %>%
  select(Year, Country, Fleets2, `Disc-1`:`Disc_sample-annual`) %>%
  filter(Year == data_year) %>%
  group_by(Fleets2) %>%
  mutate(keep = ifelse(
    isTRUE(any(!is.na(c(`Disc_rate-1`, `Disc_rate-2`, `Disc_rate-3`, 
                                   `Disc_rate-4`, `Disc_rate-annual`)))) &
    isTRUE(any(!is.na(c(`Disc_sample-1`, `Disc_sample-2`, `Disc_sample-3`, 
                 `Disc_sample-4`, `Disc_sample-annual`)))) &
    isTRUE(length(`Disc_sample-1`) > 1),
    TRUE, FALSE)) %>%
  filter(keep == TRUE) %>%
  select(-keep) %>%
  arrange(Fleets2, Country)
if (isTRUE(verbose)) groups_country %>% print(width = Inf)

### ------------------------------------------------------------------------ ###
### data after raising ####
### ------------------------------------------------------------------------ ###

### ------------------------------------------------------------------------ ###
### extract InterCatch tables & update
### use catch data (included landings, discards, etc)
### step: "14. Aggregate and Export Stock Data"
### select: ALL, ALL, ALL, ALL, ALL, Plusgroup: 1990 (last), 
###         SeasonType: Quarter, ALL, AreaType: Div, ALL
### click on "Aggregate" - wait...
### click "Export of Files"
### Download zip file and save in boot/initial/data/InterCatch/length/catch/
### select "Final Export"



### unzip data & extract tables for all catch categories
. <- lapply(yrs_dirs, function(x) {
  ### unzip 
  taf.unzip(zipfile = paste0("boot/data/InterCatch/length/", x, "/catch/",
                             "/*.zip"), 
            exdir = paste0("boot/data/InterCatch/length/", x, "/catch/"), 
            unzip = "unzip")
  ### extract tables with age and sample data
  extract_tables(file_path = paste0("boot/data/InterCatch/length/", x, 
                                    "/catch/"))
})

### read table 2
### ignore table 1 because it should be identical to table 1 from the age 
### allocations
table2_new <- lapply(yrs_dirs, function(x) {
  tmp <- read.table(paste0("boot/data/InterCatch/length/", x, 
                           "/catch/table2.txt"), 
                    sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  tmp$Season[!tmp$Season %in% 1:4] <- "annual"
  return(tmp)
})

### load historical values
table2_hist <- read.csv(paste0("boot/data/InterCatch/length/catch/", 
                               "table2_hist.csv"), 
                        as.is = TRUE, stringsAsFactors = FALSE)
table2_hist$Season[!table2_hist$Season %in% 1:4] <- "annual"
### remove values for current year, in case they are already in there
table2 <- table2_hist %>% filter(!Year %in% yrs)
### combine
table2 <- Reduce(bind_rows, append(list(table2), table2_new))

### save to file
write.csv(table2, file = paste0("boot/data/InterCatch/length/catch/",
                                "table2_hist.csv"), 
          row.names = FALSE)
if (isTRUE(verbose))
  table2 <- read.csv(paste0("boot/data/InterCatch/length/catch/",
                            "table2_hist.csv"), as.is = TRUE)
# table2 <- read.csv("boot/data/InterCatch/length/catch/table2_hist.csv")

### ------------------------------------------------------------------------ ###
### length frequencies ####
### ------------------------------------------------------------------------ ###

### annual data
if (isTRUE(verbose))
  table2 %>% group_by(Year, AgeOrLength) %>%
    summarise(numbers = sum(CANUM))

### check for BMS landings and Logbook registered discards
if (isTRUE(verbose))
  table2 %>%
    filter(!CatchCategory %in% c("Discards", "Landings")) %>%
    group_by(Year, CatchCategory) %>%
    filter(CATON > 0) %>% select(Year, Country, CatchCategory, Fleet, Season,
                                 AgeOrLength, CATON) %>% 
    summarise(catch = sum(CATON))


p <- ggplot(data = table2 %>% 
              filter(CatchCategory %in% c("Discards", "Landings")), 
            aes(x = AgeOrLength/10, y = CANUM, fill = CatchCategory)) +
  geom_bar(stat = "identity", width =  0.85) +
  facet_wrap(~ Year) +
  scale_fill_discrete("") +
  labs(y = "catch numbers", x = "length [cm]") +
  xlim(c(0, NA)) + 
  theme_custom2
if (isTRUE(verbose)) p
ggsave(file = "data/length/plots_length_distribution_annual.png", plot = p, 
       width = 15, height = 10, units = "cm", dpi = 300, type = "cairo")

### ------------------------------------------------------------------------ ###
### ALK (commercial, from accessions) ####
### ------------------------------------------------------------------------ ###

### load ALKS (historical and current)
alks <- read.csv("boot/data/accessions/ALKs.csv", 
                  stringsAsFactors = FALSE, as.is = TRUE)
names(alks)[1] <- "year"
alks <- alks %>% gather(key = "age", value = "count", X1:X26) %>%
  mutate(age = as.numeric(gsub(x = age, pattern = "X", replacement = "")))


### plot ALKs
p <- ggplot(data = alks %>% filter(count > 0), 
            aes(x = age, y = length, size = count, colour = data)) +
  geom_point(alpha = 0.2, shape = 1) +
  labs(x = "age [years]", y = "length [cm]") +
  ylim(0, NA) + xlim(0, NA) +
  facet_wrap(~ year) +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p
ggsave(file = paste0("data/length/plots_ALK_", data_year, ".png"), plot = p,
       width = 15, height = 10, units = "cm", dpi = 300, type = "cairo")

### ------------------------------------------------------------------------ ###
### ALKs from Q1SWBeam survey ####
### ------------------------------------------------------------------------ ###
### available from DATRAS

### only load data from DATRAS in verbose mode
### data is also provided in TAF's bootstrap directory
if (isTRUE(verbose)) {
  ### get data from DATRAS
  #Q1 <- getCAdata(survey = "BTS", year = 2021, quarter = 1)
  Q1 <- lapply(2006:data_year, getCAdata, survey = "BTS", quarter = 1)
  Q1 <- lapply(Q1, function(x) {
    x$LngtCode <- as.character(x$LngtCode)
    return(x)
  })
  Q1 <- do.call(bind_rows, Q1)
  ### select plaice
  Q1_ple <- Q1 %>%
    filter(Survey == "BTS" &
             Ship == "74E9" & ### Cefas Endeavour - name changed in 2023...
             Quarter == 1 &
             Country == "GB" &
             SpecCode == 127143 & ### plaice
             !is.na(Age)
    ) %>%
    select(Year, Age, LngtClass, CANoAtLngt) %>%
    group_by(Year, Age, LngtClass) %>%
    summarise(CANoAtLngt = sum(CANoAtLngt))
  saveRDS(object = Q1_ple, 
          file = "boot/initial/data/DATRAS/Q1SWBeam_ple_ALK.rds")
  # saveRDS(object = Q1_ple, 
  #         file = "boot/data/DATRAS/Q1SWBeam_ple_ALK.rds")
}

### load ALK from bootstrap directory
Q1_ple <- readRDS("boot/data/DATRAS/Q1SWBeam_ple_ALK.rds")
saveRDS(object = Q1_ple, file = "data/length/Q1SWBeam_ple.rds")

### plot ALKs
p <- ggplot(data = Q1_ple, 
            aes(x = Age, y = LngtClass, size = CANoAtLngt)) +
  geom_point(alpha = 0.2, shape = 1) +
  labs(x = "age [years]", y = "length [cm]") +
  ylim(0, NA) + xlim(0, NA) +
  facet_wrap(~ Year) +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p
ggsave(file = paste0("data/length/plots_ALK_Q1SWBeam_", data_year, ".png"), 
       plot = p,
       width = 15, height = 10, units = "cm", dpi = 300, type = "cairo")

### ------------------------------------------------------------------------ ###
### combine ALKs: commercial & Q1SWBeam ####
### ------------------------------------------------------------------------ ###

ALK_combined <- bind_rows(
  alks %>%
    group_by(year, age, length) %>%
    summarise(count = sum(count, na.rm = TRUE)) %>%
    filter(count > 0) %>%
    mutate(data = "catch"),
  Q1_ple %>%
    rename(year = Year, age = Age, length = LngtClass, count = CANoAtLngt) %>%
    group_by(year, age, length) %>%
    filter(count > 0) %>%
    mutate(data = "Q1SWBeam")) %>%
  ungroup()

### number of fish age per year and data source
if (isTRUE(verbose))
  ALK_combined %>%
    group_by(year, data) %>%
    summarise(count = sum(count)) %>%
    print(n = Inf)

### plot last X years
p <- ggplot(data = ALK_combined %>% 
              filter(year >= 2016) %>%
              group_by(year, data, age, length) %>%
              summarise(count = sum(count)) %>%
              ungroup() %>%
              group_by(year, data) %>%
              mutate(freq = count/sum(count)), 
            aes(x = age, y = length, size = freq, colour = data)) +
  geom_point(alpha = 0.5, shape = 21) +
  labs(x = "age [years]", y = "length [cm]") +
  ylim(0, NA) + xlim(0, NA) +
  scale_size_continuous("freq") +
  scale_colour_discrete("") +
  facet_wrap(~ year) +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p
ggsave(file = paste0("data/length/plots_ALK_combined.png"), plot = p,
       width = 15, height = 10, units = "cm", dpi = 300, type = "cairo")

### ------------------------------------------------------------------------ ###
### fit von Bertalanffy growth model to combined ALK ####
### ------------------------------------------------------------------------ ###

### von Bertalanffy growth function
vB_length <- function(L_inf, k, t, t_0 = 0){
  L_inf * (1 - exp(-k * (t - t_0)))
}

### weight commercial and survey data equally
ALK_fit <- ALK_combined %>%
  ### standardise counts per data source
  group_by(year, data) %>%
  mutate(freq = count/sum(count)) %>%
  ungroup() %>%
  group_by(year, data, age, length) %>%
  summarise(freq = sum(freq)) %>%
  ungroup() %>%
  ### combine both data source and standardise again
  group_by(year, age, length) %>%
  summarise(freq = sum(freq))
ALK_fit <- as.data.frame(ALK_fit)
df_i <- ALK_fit %>% filter(year == 2020)
nls(length ~ vB_length(L_inf, k, t = age, t_0),
    data = df_i,
    weight = df_i$freq,
    start = list(L_inf = 50, k = 0.1, t_0 = 0))

### fit model by year
yrs_alk <- as.list(unique(ALK_fit$year))
names(yrs_alk) <- yrs_alk
yrs_alk <- append(yrs_alk, list("last5" = c(2017:2021)))

model_fits <- lapply(yrs_alk, function(x) {
  df_i <- ALK_fit %>%
    filter(year %in% x)
  df_i <- as.data.frame(df_i)
  nls(length ~ vB_length(L_inf, k, t = age, t_0),
      data = df_i, weight = df_i$freq,
      start = list(L_inf = 50, k = 0.1, t_0 = 0))
})
### get parameters
if (isTRUE(verbose)) {
  sapply(model_fits, function(x) x$m$getPars()[["L_inf"]])
  sapply(model_fits, function(x) x$m$getPars()[["k"]])
  sapply(model_fits, function(x) x$m$getPars()[["t_0"]])
}
vB_pars <- data.frame(
  year = names(model_fits),
  k = sapply(model_fits, function(x) x$m$getPars()[["k"]]),
  Linf = sapply(model_fits, function(x) x$m$getPars()[["L_inf"]]),
  t0 = sapply(model_fits, function(x) x$m$getPars()[["t_0"]]))
write.csv(x = vB_pars, file = "data/length/vB_pars.csv", row.names = FALSE)

### get estimates
ages <- seq(0, 26, 0.1)
length_predicted <- lapply(names(model_fits),
  function(x) {
    model_tmp <- model_fits[[x]]
    lengths_tmp <- vB_length(
      L_inf = summary(model_tmp)$parameters["L_inf", "Estimate"],
      k = summary(model_tmp)$parameters["k", "Estimate"],
      t_0 = summary(model_tmp)$parameters["t_0", "Estimate"], 
      t = ages)
    data.frame(age = ages,
               length = lengths_tmp,
               year = x)
})
length_predicted_years <- do.call(rbind, 
                                  length_predicted[-length(length_predicted)])
length_predicted_last5 <- lapply(yrs_alk$last5, function(x) {
  length_predicted[[length(length_predicted)]] %>% mutate(year = x)
})
length_predicted_last5 <- do.call(rbind, length_predicted_last5)

### add curves to plot
p <- ggplot() +
  geom_point(data = ALK_combined %>% 
               group_by(year, data, age, length) %>%
               summarise(count = sum(count)) %>%
               ungroup() %>%
               group_by(year, data) %>%
               mutate(freq = count/sum(count)), 
             aes(x = age, y = length, size = freq, colour = data),
             alpha = 0.5, shape = 21) +
  ### curve by year
  geom_line(data = length_predicted_years %>%
              mutate(year = as.numeric(as.character(year))),
            aes(x = age, y = length)) +
  ### last 5 years
  geom_line(data = length_predicted_last5 %>%
              mutate(year = as.numeric(as.character(year))),
            aes(x = age, y = length),
            linetype = "dashed") +
  labs(x = "age [years]", y = "length [cm]") +
  ylim(0, NA) + xlim(0, NA) +
  scale_size_continuous("freq") +
  scale_colour_discrete("") +
  facet_wrap(~ year) +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p
ggsave(file = paste0("data/length/plots_ALK_combined_curves.png"), plot = p,
       width = 15, height = 10, units = "cm", dpi = 300, type = "cairo")



### ------------------------------------------------------------------------ ###
### Length-based indicators (LBI) ####
### ------------------------------------------------------------------------ ###

### annual catch length frequencies
freq_catch <- table2 %>%
  filter(CatchCategory %in% c("Discards", "Landings")) %>%
  group_by(Year, AgeOrLength) %>%
  summarise(numbers = sum(CANUM))

### ------------------------------------------------------------------------ ###
### calculate Lc (length of first capture) 
### definition: first length class where abundance is >= half max abundance
### by year
Lc_yr <- freq_catch %>% 
  summarise(Lc = min(AgeOrLength[numbers >= max(numbers)/2]))
### total
Lc_total <- freq_catch %>% 
  group_by(AgeOrLength) %>%
  summarise(numbers = sum(numbers)) %>%
  summarise(Lc = min(AgeOrLength[numbers >= max(numbers)/2]))
### average of annual values (last 5 years)
Lc <- freq_catch %>%
  filter(Year >= !!(data_year - 4)) %>%
  group_by(Year, AgeOrLength) %>%
  summarise(numbers = sum(numbers)) %>%
  summarise(Lc = min(AgeOrLength[numbers >= max(numbers)/2])) %>%
  summarise(Lc = mean(Lc)/10) %>%
  as.numeric()

### illustrate Lc for 2021
p <- table2 %>%
  filter(Year == data_year & CANUM > 0) %>%
  mutate(CANUM == ifelse(is.na(CANUM), 0, CANUM)) %>%
  mutate(CatchCategory2 = ifelse(CatchCategory == "Landings", 
                                 "Landings", "Discards")) %>%
  mutate(CatchCategory2 = factor(CatchCategory2, 
                                 levels = c("Discards", "Landings"))) %>%
  ggplot(aes(x = AgeOrLength/10, y = CANUM/1000, fill = CatchCategory2)) +
  geom_col() +
  scale_fill_discrete("") +
  facet_wrap(~ Year) + 
  labs(x = "Length (cm)", y = "Raised catch numbers (thousands)") +
  ylim(c(0, NA)) +
  theme_bw(base_size = 8) +
  theme(legend.position = c(0.8, 0.8), 
        legend.key = element_blank(),
        legend.key.width = unit(0.6, "lines"),
        legend.background = element_blank())
if (isTRUE(verbose)) p
ggsave(file = paste0("data/length/plots_length_dist_2022_for_Lc.png"), 
       plot = p,
       width = 15, height = 10, units = "cm", dpi = 300, type = "cairo")

### illustrate Lc by year
p <- table2 %>%
  filter(CANUM > 0) %>%
  mutate(CANUM == ifelse(is.na(CANUM), 0, CANUM)) %>%
  mutate(CatchCategory2 = ifelse(CatchCategory == "Landings", 
                                 "Landings", "Discards")) %>%
  mutate(CatchCategory2 = factor(CatchCategory2, 
                                 levels = c("Discards", "Landings"))) %>%
  ggplot(aes(x = AgeOrLength/10, y = CANUM/1000, fill = CatchCategory2)) +
  geom_col() +
  scale_fill_discrete("") +
  geom_vline(data = Lc_yr %>% mutate(type = "L[c]"), 
             aes(xintercept = Lc/10, linetype = type, color = type)) + 
  scale_colour_manual("", values = c("L[c]" = "black"),
                      labels = scales::parse_format()) + 
  scale_linetype_manual("", values = c("L[c]" = "dotted"),
                        labels = scales::parse_format()) + 
  facet_wrap(~ Year) + 
  labs(x = "Length (cm)", y = "Raised catch numbers (thousands)") +
  ylim(c(0, NA)) + xlim(c(0, NA)) +
  theme_bw(base_size = 8) +
  theme(legend.position = c(0.8, 0.1), 
        legend.key = element_blank(),
        legend.key.width = unit(0.2, "lines"),
        legend.key.height = unit(1, "lines"),
        legend.background = element_blank(),
        legend.box = "horizontal")
if (isTRUE(verbose)) p
ggsave(file = paste0("data/length/plots_Lc_yr.png"), 
       plot = p,
       width = 15, height = 10, units = "cm", dpi = 300, type = "cairo")


### ------------------------------------------------------------------------ ###
### calculate mean length above Lc ###
Lmean_yr <- full_join(freq_catch, Lc_yr) %>%
  ### select only length classes above Lc
  filter(AgeOrLength >= Lc) %>%
  ### mean of length classes, weighted by catch numbers at length
  summarise(Lmean = weighted.mean(x = AgeOrLength, w = numbers))
Lmean <- freq_catch %>%
  mutate(AgeOrLength = AgeOrLength/10) %>%
  mutate(Lc = Lc) %>%
  filter(AgeOrLength >= Lc) %>%
  summarise(Lmean = weighted.mean(x = AgeOrLength, w = numbers),
            Lc = unique(Lc))
Lref <- Lmean %>%
  pivot_longer(c(Lmean, Lc)) %>%
  mutate(name = factor(name, levels = c("Lmean", "Lc"),
                       labels = c("L[mean]", "L[c]")))

### illustrate mean catch length above Lc by year
p <- table2 %>%
  filter(CANUM > 0) %>%
  mutate(CANUM == ifelse(is.na(CANUM), 0, CANUM)) %>%
  mutate(CatchCategory2 = ifelse(CatchCategory == "Landings", 
                                 "Landings", "Discards")) %>%
  mutate(CatchCategory2 = factor(CatchCategory2, 
                                 levels = c("Discards", "Landings"))) %>%
  ggplot(aes(x = AgeOrLength/10, y = CANUM/1000, fill = CatchCategory2)) +
  geom_col() +
  scale_fill_discrete("") +
  geom_vline(data = Lref, 
             aes(xintercept = value, colour = name, linetype = name)) + 
  scale_colour_manual("", 
                      values = c("L[c]" = "black", "L[mean]" = "black"),
                      labels = scales::parse_format()) + 
  scale_linetype_manual("", 
                        values = c("L[c]" = "dotted", "L[mean]" = "solid"),
                        labels = scales::parse_format()) + 
  facet_wrap(~ Year) + 
  labs(x = "Length (cm)", y = "Raised catch numbers (thousands)") +
  ylim(c(0, NA)) + xlim(c(0, NA)) +
  theme_bw(base_size = 8) +
  theme(legend.position = c(0.8, 0.1), 
        legend.key = element_blank(),
        legend.key.width = unit(0.2, "lines"),
        legend.key.height = unit(1, "lines"),
        legend.background = element_blank(),
        legend.box = "horizontal")
if (isTRUE(verbose)) p
ggsave(file = paste0("data/length/plots_Lmean_yr.png"), 
       plot = p,
       width = 15, height = 10, units = "cm", dpi = 300, type = "cairo")



### ------------------------------------------------------------------------ ###
### calculate MSY proxy reference length: LF=M

### use Linf from fishbase
#Linf <- 54.4

### approximation: 0.95*Lmax
#Linf <- max(table2$AgeOrLength[table2$Year == 2020])/10*0.95

Linf <- vB_pars[vB_pars == "last5", "Linf"] # average of last 5 years
LFeM <- 0.25 * Linf + 0.75 * Lc

Lref2 <- Lmean %>%
  mutate(LFeM = LFeM) %>%
  pivot_longer(c(Lmean, Lc, LFeM)) %>%
  mutate(name = factor(name, levels = c("Lmean", "Lc", "LFeM"),
                       labels = c("L[mean]", "L[c]", "L[F==M]")))

### illustrate mean catch length above Lc by year relative to LF=M
p <- table2 %>%
  filter(CANUM > 0) %>%
  mutate(CANUM == ifelse(is.na(CANUM), 0, CANUM)) %>%
  mutate(CatchCategory2 = ifelse(CatchCategory == "Landings", 
                                 "Landings", "Discards")) %>%
  mutate(CatchCategory2 = factor(CatchCategory2, 
                                 levels = c("Discards", "Landings"))) %>%
  ggplot(aes(x = AgeOrLength/10, y = CANUM/1000, fill = CatchCategory2)) +
  geom_col() +
  scale_fill_discrete("") +
  geom_vline(data = Lref2, 
             aes(xintercept = value, colour = name, linetype = name)) + 
  scale_colour_manual("", 
                      values = c("L[c]" = "black", 
                                 "L[mean]" = "black",
                                 "L[F==M]" = "red"),
                      labels = scales::parse_format()) + 
  scale_linetype_manual("", 
                        values = c("L[c]" = "dotted", 
                                   "L[mean]" = "solid",
                                   "L[F==M]" = "solid"),
                        labels = scales::parse_format()) + 
  facet_wrap(~ Year) + 
  labs(x = "Length (cm)", y = "Raised catch numbers (thousands)") +
  ylim(c(0, NA)) + xlim(c(0, NA)) +
  theme_bw(base_size = 8) +
  theme(legend.position = c(0.8, 0.1), 
        legend.key = element_blank(),
        legend.key.width = unit(0.2, "lines"),
        legend.key.height = unit(1, "lines"),
        legend.background = element_blank(),
        legend.box = "horizontal")
if (isTRUE(verbose)) p
ggsave(file = paste0("data/length/plots_Lmean_yr_LFeM.png"), 
       plot = p,
       width = 15, height = 10, units = "cm", dpi = 300, type = "cairo")


### ------------------------------------------------------------------------ ###
### L vs LF=M


Lmean <- Lmean %>%
  mutate(LFeM = LFeM) %>%
  mutate(f = Lmean/LFeM)
if (isTRUE(verbose)) Lmean

Lref3 <- as.data.frame(cbind(Lc, LFeM)) %>%
  pivot_longer(c(Lc, LFeM)) %>%
  mutate(name = factor(name, levels = c("Lc", "LFeM"),
                       labels = c("L[c]", "L[F==M]")))
Lref3 <- Lref3 %>%
  filter(name == "L[F==M]") %>%
  mutate(name = factor(name))

p <- ggplot() +
  geom_hline(data = Lref3, 
             aes(yintercept = value, linetype = name, colour = name)) +
  geom_line(data = Lmean,
            aes(x = Year, y = Lmean),
            color = "#ed6028") +
  scale_linetype_manual("", 
                        values = c("L[F==M]" = "solid"),
                        labels = scales::parse_format()) + 
  scale_colour_manual("",
                      values = c("L[F==M]" = "#679dfe"),
                      labels = scales::parse_format()) +
  scale_x_continuous(breaks = c(2014, 2016, 2018, 2020)) +
  coord_cartesian(ylim = c(25, 40), 
                  xlim = c(min(Lmean$Year, na.rm = TRUE) - 1,
                           max(Lmean$Year, na.rm = TRUE) + 1),
                  expand = FALSE) +
  labs(x = "", y = "Mean catch length (cm)", title = "Length indicator") +
  theme_bw(base_size = 8) +
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.key.height = unit(0.5, "lines"),
        plot.title = element_text(face = "bold", colour = "#ed6028"))
if (isTRUE(verbose)) p
ggsave(file = paste0("data/length/plots_Lmean_ICES_style.png"), 
       plot = p,
       width = 10, height = 6, units = "cm", dpi = 300, type = "cairo")

### ------------------------------------------------------------------------ ###
### save values used for advice

write.csv(Lmean, "data/length/length_smry.csv")
saveRDS(Lmean, "data/length/length_smry.rds")

### ------------------------------------------------------------------------ ###
### save length distributions ####
### ------------------------------------------------------------------------ ###

length_dist <- table2 %>% 
  filter(CatchCategory %in% c("Discards", "Landings")) %>%
  select(Stock, Year, CatchCategory, Length = AgeOrLength, CANUM, WECA) %>%
  group_by(Stock, Year, CatchCategory, Length) %>%
  summarise(WECA = weighted.mean(x = WECA, w = CANUM),
            CANUM = sum(CANUM))
write.csv(length_dist, "data/length_dist.csv", row.names = FALSE)


length_dist %>%
  mutate(biomass = WECA * CANUM) %>%
  ungroup() %>%
  group_by(Year) %>%
  summarise(biomass = sum(biomass))
