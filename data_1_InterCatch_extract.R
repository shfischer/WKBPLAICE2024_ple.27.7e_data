## Preprocess data, write TAF data tables

## Before: many InterCatch data files: boot/data/InterCatch/*
##         
## After:  some temporary InterCatch files: boot/data/InterCatch/*
##         historical time series updateded: boot/data/InterCatch/*
##         some data plots: data/plots_*.png
##         data/discard_ratio.txt
##         data/catch_history_7e.csv


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

### create folder to store data
mkdir("data")

### load additional functions
source("utilities.R")

### set WG year
year <- 2024
data_year <- year - 1

if (!exists("verbose")) verbose <- FALSE

### ------------------------------------------------------------------------ ###
### revisions to previous years? ####
### ------------------------------------------------------------------------ ###
revs <- list.dirs("boot/data/InterCatch/", recursive = FALSE, 
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
### save both in boot/initial/data/InterCatch/before_raising/

### unzip files from InterCatch export (before raising)
. <- lapply(yrs_dirs, function(x) {
  taf.unzip(zipfile = paste0("boot/data/InterCatch/", x, 
                             "/before_raising/",
                             "Numbers_at_age_and_mean_weights_at_age.zip"), 
            exdir = paste0("boot/data/InterCatch/", x, "/before_raising"))
})

### ------------------------------------------------------------------------ ###
### load overview
StockOverview <- lapply(yrs_dirs, function(x) {
  read.table(paste0("boot/data/InterCatch/", x,
                                   "/before_raising/StockOverview.txt"),
                            sep = "\t", header = TRUE, as.is = TRUE)
})
### load also previous years
StockOverview_hist <- read.csv(paste0("boot/data/InterCatch/",
                                      "before_raising/",
                                      "StockOverview_hist.csv"),
                               as.is = TRUE)
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

### catch per country and year
if (isTRUE(verbose))
  StockOverview_all %>%
    group_by(Year, Country, Catch.Cat.) %>%
    summarise(catch = sum(Catch..kg)) %>%
    print(n = Inf)
if (isTRUE(verbose))
  StockOverview_all %>%
  group_by(Year, Catch.Cat.) %>%
  summarise(catch = sum(Catch..kg)) %>%
  print(n = Inf)
  
### save entire InterCatch history
### including current year
write.csv(StockOverview_all, 
          file = paste0("boot/data/InterCatch/before_raising/",
                        "StockOverview_hist.csv"),
          row.names = FALSE)
### load
# StockOverview_all <- read.csv(paste0("boot/data/InterCatch/",
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
    group_by(Country, Year, Catch.Cat.) %>%
    summarise(catch = sum(Catch..kg)) %>%
    filter(Year == data_year)

### ------------------------------------------------------------------------ ###
### submitted sample data

### load data for current year
samples <- lapply(yrs_dirs, function(x) {
  tmp <- read.table(paste0("boot/data/InterCatch/", x, "/before_raising/",
                           "NumbersAtAgeLength.txt"),
                    sep = "\t", header = TRUE, skip = 2, as.is = TRUE)
  tmp$Season <- gsub(x = tmp$Season, pattern = "[0-9]{4}",
                     replacement = "annual")
  return(tmp)
})
### load also previous years
samples_hist <- read.csv(paste0("boot/data/InterCatch/", 
                                "before_raising/NumbersAtAgeLength_hist.csv"), 
                         as.is = TRUE)
### replace year with "annual" as season
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
samples_all <- unique(samples_all)

### save entire history including current year
write.csv(samples_all, 
          file = paste0("boot/data/InterCatch/before_raising/",
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
### 2020: also Belgian landings & discards
### 2021: UK D/L + Belgium D/L + France L
### 2022: UK & BE D/L, France L
### 2023: UK & BE D/L, France L

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
          file = paste0("data/IC_", x, ".csv"), row.names = FALSE, na = "")
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
### visualize age distributions

### reshape data
samples_all <- samples_all[, which(names(samples_all) != "X")]
samples_df <- gather(samples_all, key = "age", value = "numbers", 
                     16:ncol(samples_all))
samples_df$sex <- gsub(x = samples_df$age, pattern = "[0-9]", replacement = "")
samples_df$age <- as.numeric(gsub(x = samples_df$age, 
                                  pattern = "UndeterminedAge|MaleAge|FemaleAge",
                                  replacement = ""))
samples_df$Catch.Cat.[samples_df$Catch.Cat. == "L"] <- "Landings"
samples_df$Catch.Cat.[samples_df$Catch.Cat. == "D"] <- "Discards"
### factor for facetting
samples_df$facet <- with(samples_df, paste0(Country, " - ", Catch.Cat., 
                                            " -  Q", Season, "\n", Fleets))
### extract some sample info
samples_df_info <- samples_df %>% group_by(facet, Year) %>%
  summarise(age_max = max(age, na.rm = TRUE), 
            n_max = max(numbers, na.rm = TRUE),
            NumAgeMeasurement = mean(NumAgeMeasurement),
            NumLengthMeasurements = mean(NumLengthMeasurements),
            NumSamplesAge = mean(NumSamplesAge),
            NumSamplesLength = mean(NumSamplesLength),
            Country = unique(Country))

### plot
p <- ggplot(data = samples_df %>% filter(Year == data_year), 
            aes(x = age, y = numbers)) +
  geom_bar(stat = "identity", aes(fill = Catch.Cat.)) +
  facet_wrap(~ facet, scale = "free_y", ncol = 4) +
  theme_bw(base_size = 7) + 
  geom_text(data = samples_df_info %>% filter(Year == data_year),
            aes(x = age_max*0.95, y = n_max*0.95,
                label = paste0("length samples: ", NumSamplesLength, "\n",
                               "length readings: ", NumLengthMeasurements, "\n",
                               "age samples: ", NumSamplesAge, "\n",
                               "age readings: ", NumAgeMeasurement)),
            hjust = 1, vjust = 1, size = 1.5) +
  labs(x = "age [years]", y = "catch numbers") +
  ylim(0, NA) +
  scale_fill_discrete("") +
  theme(legend.position = c(0.9, 0.05),
        legend.background = element_blank())
if (isTRUE(verbose)) p
ggsave(file = paste0("data/plots_age_samples_sep_", data_year, ".png"),
       width = 15, height = 20,  plot = p,
       units = "cm", dpi = 300, type = "cairo")

### ------------------------------------------------------------------------ ###
### visualize age distributions (2)

### reshape data
samples_all <- samples_all[, which(names(samples_all) != "X")]
samples_df <- gather(samples_all, key = "age", value = "numbers", 
                     16:ncol(samples_all))
samples_df$sex <- gsub(x = samples_df$age, pattern = "[0-9]", replacement = "")
samples_df$age <- as.numeric(gsub(x = samples_df$age, 
                                  pattern = "UndeterminedAge|MaleAge|FemaleAge",
                                  replacement = ""))
samples_df$Catch.Cat.[samples_df$Catch.Cat. == "L"] <- "Landings"
samples_df$Catch.Cat.[samples_df$Catch.Cat. == "D"] <- "Discards"
### factor for facetting
samples_df$facet <- with(samples_df, paste0(Country, 
                                            " -  Q", Season, "\n", Fleets))
### sort facets
samples_df$facet <- factor(samples_df$facet,
  levels = unique(with(samples_df, 
                       facet[order(Country, Fleets, Season)])))
### get sample info for submitted sample data
samples_df_info <- samples_df %>% group_by(facet, Year) %>%
  summarise(
    age_max = max(age, na.rm = TRUE),
    age_max_manual = 27,
    n_max = max(numbers, na.rm = TRUE),
    land_n = mean(NumLengthMeasurements[Catch.Cat. == "Landings"]),
    land_sample = mean(NumSamplesLength[Catch.Cat. == "Landings"]),
    disc_n = mean(NumLengthMeasurements[Catch.Cat. == "Discards"]),
    disc_sample = mean(NumSamplesLength[Catch.Cat. == "Discards"]),
    #Country = unique(Country),
    #Season = unique(Season),
    Fleets = unique(Fleets)
  )
### summarise sampling details
samples_df_info$label <- unlist(lapply(seq(nrow(samples_df_info)), function(x) {
  #browser()
  tmp <- samples_df_info[x, ]
  res <- ""
  if (!is.na(tmp$land_sample)) {
    res <- paste0(res, paste0("landings:\nsamples: ", tmp$land_sample, "\n",
                              "readings: ", tmp$land_n, "\n"))
  }
  if (!is.na(tmp$disc_sample)) {
    res <- paste0(res, paste0("discards:\nsamples: ", tmp$disc_sample, "\n",
                              "readings: ", tmp$disc_n, "\n"))
  }
  return(res)
}))
### remove empty facets
samples_df_info <- samples_df_info %>% filter(n_max > 0)

### create dummy elements for nicer plotting
### get bar with same width in plot, irrespective of Landings/Discards
samples_df2 <- bind_rows(
  samples_df,
  with(samples_df[samples_df$Year == data_year, ],
       expand.grid(Catch.Cat. = unique(Catch.Cat.),
                   age = unique(age),
                   facet = unique(facet),
                   Year = 2017,
                   stringsAsFactors = FALSE)))
### plot
p <- ggplot() +
  geom_col(data = samples_df2[samples_df2$Year == data_year, ],
           aes(x = age, y = numbers, fill = Catch.Cat.), 
           position = position_dodge(preserve = "single"), width = 2) +
  facet_wrap(~ facet, scale = "free_y", ncol = 3) +
  theme_bw(base_size = 8) + 
  geom_text(data = samples_df_info[samples_df_info$Year == data_year, ],
            aes(x = age_max_manual*0.95, y = n_max*0.95,
                label = label),
            hjust = 1, vjust = 1, size = 2, colour = "grey30") +
  labs(x = "age [years]", y = "numbers") +
  theme(strip.text = element_text(size = 5)) +
  ylim(0, NA)
if (isTRUE(verbose)) p
ggsave(file = paste0("data/plots_age_samples_", data_year, ".png"),
       width = 15, height = 20, plot = p,
       units = "cm", dpi = 300, type = "cairo")

### ------------------------------------------------------------------------ ###
### groups for age allocations ####
### ------------------------------------------------------------------------ ###

### ------------------------------------------------------------------------ ###
### find fleets where age samples from some quarters can be used for others

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
  select(Year:Fleets, `Disc-1`:`Disc_sample-4`)
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
  select(Year, Country, Fleets2, `Disc-1`:`Disc_sample-4`) %>%
  filter(Year == data_year) %>%
  group_by(Fleets2) %>%
  mutate(keep = ifelse(
    isTRUE(any(!is.na(c(`Disc_rate-1`, `Disc_rate-2`, `Disc_rate-3`, 
                                   `Disc_rate-4`, `Disc_rate-annual`)))) &
    isTRUE(any(!is.na(c(`Disc_sample-1`, `Disc_sample-2`, `Disc_sample-3`, 
                 `Disc_sample-4`)))) &
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
### step: "14. Aggregate and Export Stock Data"
### select: ALL, ALL, ALL, ALL, ALL, Plusgroup: 15, SeasonType: Quarter, ALL
###         AreaType: Div, ALL
### click on "Aggregate" - wait...
### click "Export of Files"
### Download zip file and save in boot/initial/data/InterCatch/catch/
### repeat for landings and discards (including BMS and logbook)
### do catch last, and select "Final Export"
### three data sets (zip files): catch, discards & landings

### unzip data & extract tables for all catch categories
. <- lapply(yrs_dirs, function(y) {
  lapply(c("landings", "discards", "catch"), function(x) {
    ### unzip 
    file_in <- list.files(path = paste0("boot/data/InterCatch/", y, "/", x),
                          pattern = ".zip", full.names = TRUE)
    file_in <- gsub(x = file_in, pattern = "//", replacement = "/")
    file_out <- paste0("./boot/data/InterCatch/", y, "/", x)
    file_out <- gsub(x = file_out, pattern = "//", replacement = "/")
    taf.unzip(zipfile = file_in, exdir = file_out)
    ### extract tables with age and sample data
    extract_tables(file_path = paste0("boot/data/InterCatch/", y, "/", x, 
                                      "/"))
  })
})

### use only tables from catch/, because they include catch, split into 
### discards & landings
### read tables
table1_new <- lapply(yrs_dirs, function(x) {
  tmp <- read.table(paste0("boot/data/InterCatch/", x, "/catch/table1.txt"), 
                    sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  tmp$Season[!tmp$Season %in% 1:4] <- "annual"
  return(tmp)
})
table2_new <- lapply(yrs_dirs, function(x) {
  tmp <- read.table(paste0("boot/data/InterCatch/", x, "/catch/table2.txt"), 
                    sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  tmp$Season[!tmp$Season %in% 1:4] <- "annual"
  return(tmp)
})
### load historical values
table1_hist <- read.csv(paste0("boot/data/InterCatch/table1_hist.txt"), 
                        as.is = TRUE, stringsAsFactors = FALSE)
table2_hist <- read.csv(paste0("boot/data/InterCatch/table2_hist.txt"), 
                        as.is = TRUE, stringsAsFactors = FALSE)
### remove values for current year, in case they are already in there
table1 <- table1_hist %>% filter(!Year %in% yrs)
table2 <- table2_hist %>% filter(!Year %in% yrs)
### combine
table1 <- Reduce(bind_rows, append(list(table1), table1_new))
table2 <- Reduce(bind_rows, append(list(table2), table2_new))
### save to file
write.csv(table1, file = "boot/data/InterCatch/table1_hist.txt", 
          row.names = FALSE)
write.csv(table2, file = "boot/data/InterCatch/table2_hist.txt", 
          row.names = FALSE)

# table1 <- read.csv("boot/data/InterCatch/table1_hist.txt", as.is = TRUE)
# table2 <- read.csv("boot/data/InterCatch/table2_hist.txt", as.is = TRUE)
# StockOverview_all <- read.csv(paste0("boot/data/InterCatch/",
#                                      "before_raising/StockOverview_hist.csv"),
#                               as.is = TRUE)

### ------------------------------------------------------------------------ ###
### landings and discards by country (raised) ####
### ------------------------------------------------------------------------ ###
### based on data after raising/allocations

if (isTRUE(verbose)) {
  ### catch per year
  table1 %>% group_by(Year) %>%
    summarise(CATON = sum(CATON/1000)) %>%
    print(n = Inf)
  ### catch per year by catch catgory
  table1 %>% group_by(Year, CatchCategory) %>%
    summarise(CATON = sum(CATON/1000)) %>%
    print(n = Inf)
  ### per country
  table1 %>% group_by(Year, Country, CatchCategory) %>%
    summarise(CATON = sum(CATON/1000)) %>% print(n = Inf)
}

### combine UK
table1$Country2 <- as.character(table1$Country)
table1$Country2[grepl(x = table1$Country2, 
                pattern = "^UK*")] <- "UK"

### proportion per country
if (isTRUE(verbose))
  table1 %>% 
    group_by(Year, CatchCategory, Country) %>%
    summarise(catch = sum(CATON/1000)) %>%
    mutate(country_rel = catch / sum(catch)) %>%
    print(n = Inf)

### proportion per country UK combined
if (isTRUE(verbose))
  table1 %>% 
    group_by(Year, CatchCategory, Country2) %>%
    summarise(catch = sum(CATON/1000)) %>%
    mutate(country_rel = catch / sum(catch) * 100) %>%
    print(n = Inf)

### plot
p <- table1 %>%
  filter(CatchCategory %in% c("Landings", "Discards")) %>%
  ggplot() +
  geom_bar(aes(x = Year, y = CATON/1000, fill = Country), 
           stat = "identity") +
  facet_wrap(~ CatchCategory) +
  theme_custom2 +
  labs(x = "year", y = "catch [tonnes]") + 
  geom_text(data = table1 %>% 
              filter(CatchCategory %in% c("Landings", "Discards")) %>%
              group_by(Year, CatchCategory) %>%
              summarise(CATON = sum(CATON)),
            aes(x = Year, y = CATON/1000 + 50,
                label = round(CATON/1000, 0)),
            size = 2)
if (isTRUE(verbose)) p
### save plot
ggsave(file = "data/plots_data_landings_country.png", 
       width = 15, height = 7, units = "cm", dpi = 300, plot = p,
       type = "cairo")

### ------------------------------------------------------------------------ ###
### landings by fleet ####
### ------------------------------------------------------------------------ ###

### create new object for storing fleets
fleets <- StockOverview_all

### abbreviate fleet names
# unique(fleets$Fleets)
fleets$Fleets <- as.character(fleets$Fleets)
fleets$Fleets2 <- substr(x = fleets$Fleets, start = 1, stop = 7) 
# unique(fleets$Fleets2)

### aggregate by fleet
df_fleet <- aggregate(Catch..kg ~ Fleets2 + Year, 
                      data = fleets[fleets$Catch.Cat. == "Landings", ],
                      FUN = sum)

### plot
p <- ggplot() +
  geom_bar(data = df_fleet, aes(x = Fleets2, y = Catch..kg/1000, 
                                fill = Fleets2), 
           stat = "identity", show.legend = FALSE) +
  scale_fill_discrete("Fleet") +
  geom_text(data = df_fleet, aes(x = Fleets2, y = Catch..kg/1000 + 30, 
                                 label = round(Catch..kg/1000, 1)),
            size = 1.5, family = "serif") +
  facet_wrap(~ Year, ncol = 2) +
  theme_custom2 +
  labs(x = "fleet", y = "reported landings [tonnes]") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
if (isTRUE(verbose)) p
### save plot
ggsave(file = paste0("data/plots_data_landings_fleet.png"), 
       width = 13.4, height = 15, units = "cm", dpi = 300, plot = p,
       type = "cairo")

### plot last 2 years only
p <- ggplot() +
  geom_bar(data = df_fleet[df_fleet$Year %in% 
                             tail(sort(unique(df_fleet$Year)), 2), ],
           aes(x = Fleets2, y = Catch..kg/1000, fill = Fleets2), 
           stat = "identity", show.legend = FALSE) +
  scale_fill_discrete("Fleet") +
  geom_text(data = df_fleet[df_fleet$Year %in% 
                              tail(sort(unique(df_fleet$Year)), 2), ], 
            aes(x = Fleets2, y = Catch..kg/1000 + 30,
                label = round(Catch..kg/1000, 1)),
            size = 1.5, family = "serif") +
  facet_wrap(~ Year, ncol = 2) +
  theme_custom2 +
  labs(x = "fleet", y = "reported landings [tonnes]") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
if (isTRUE(verbose)) p
### save plot
ggsave(file = paste0("data/plots_data_landings_fleet_last.png"), 
       width = 13.4, height = 8, units = "cm", dpi = 300, plot = p, 
       type = "cairo")

### ------------------------------------------------------------------------ ###
### contributions by gear group (for advice sheet) ####
### ------------------------------------------------------------------------ ###
### use raised data

gears <- table1

if (isTRUE(verbose)) 
  gears %>%
    group_by(Year, CatchCategory, Fleet) %>%
    summarise(catch = sum(CATON)) %>%
    print(n = Inf)

### abbreviate fleets
gears <- gears %>%
  mutate(gear = substr(Fleet, start = 1, stop = 3)) %>%
  mutate(gear = ifelse(gear == "C-A", "MIS", gear)) %>%
  rowwise() %>%
  mutate(gear_group = switch(gear,
                             "TBB" = "beam trawl",
                             "OTB" = "otter trawl",
                             "OTT" = "otter trawl",
                             "GNS" = "gill nets",
                             "GTR" = "gill nets",
                             "GND" = "gill nets",
                             "other"
                             )) %>%
  ungroup()

if (isTRUE(verbose))
  gears %>% 
    group_by(Year, CatchCategory, gear_group) %>%
    summarise(catch = sum(CATON)) %>%
    mutate(gear_contribution = catch / sum(catch)) %>%
    mutate(ICES_rounded = icesRound(gear_contribution * 100)) %>%
    filter(Year == data_year)

if (isTRUE(verbose))
  ### catch per catch category
  table1 %>% group_by(Year, CatchCategory) %>%
    summarise(catch = round(sum(CATON)/1000)) %>%
    print(n = Inf)
if (isTRUE(verbose))
  ### annual catch
  table1 %>% group_by(Year) %>%
    summarise(catch = round(sum(CATON)/1000))

### plot catch per gear
p <- ggplot(data = gears %>% 
              filter(CatchCategory %in% c("Landings", "Discards")), 
            aes(x = Country2, y  = CATON/1000, fill = gear)) +
  geom_bar(stat = "identity") +
  facet_grid(CatchCategory ~ Year, scales = "free", space = "free_y") +
  theme_custom2 +
  labs(x = "Country", y = "catch [tonnes]") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1))
if (isTRUE(verbose)) p
ggsave(file = "data/plots_data_landings_gear.png", plot = p, 
       width = 15, height = 8, units = "cm", dpi = 300,
       type = "cairo")
### last five years only
p <- ggplot(data = gears %>% 
              filter(CatchCategory %in% c("Landings", "Discards") &
                       Year %in% tail(sort(unique(Year)), 5)), 
            aes(x = Country2, y  = CATON/1000, fill = gear)) +
  geom_bar(stat = "identity") +
  facet_grid(CatchCategory ~ Year, scales = "free", space = "free_y") +
  theme_custom2 +
  labs(x = "Country", y = "catch [tonnes]") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1))
if (isTRUE(verbose)) p
ggsave(file = "data/plots_data_landings_gear_last5.png", plot = p, 
       width = 15, height = 8, units = "cm", dpi = 300,
       type = "cairo")

### ------------------------------------------------------------------------ ###
### total age distribution by catch category ####
### ------------------------------------------------------------------------ ###

### numbers
p <- ggplot(data = table2 %>% 
              filter(CatchCategory %in% c("Discards", "Landings")), 
            aes(x = AgeOrLength, y = CANUM, fill = CatchCategory)) +
  geom_bar(stat = "identity", width =  0.85) +
  facet_wrap(~ Year) +
  theme_custom2 +
  labs(y = "catch numbers", x = "age [years]")
if (isTRUE(verbose)) p
ggsave(file = "data/plots_data_catch_age_structure.png", plot = p, 
       width = 15, height = 8, units = "cm", dpi = 300,
       type = "cairo")
### weight
p <- ggplot(data = table2 %>% 
              mutate(catch = CANUM * WECA) %>%
              filter(CatchCategory %in% c("Discards", "Landings")), 
            aes(x = AgeOrLength, y = catch/(1e+6), fill = CatchCategory)) +
  geom_bar(stat = "identity", width =  0.85) +
  facet_wrap(~ Year) +
  theme_custom2 +
  labs(y = "catch [tonnes]", x = "age [years]")
if (isTRUE(verbose)) p
ggsave(file = "data/plots_data_catch_age_structure_weigth.png", plot = p, 
       width = 15, height = 8, units = "cm", dpi = 300,
       type = "cairo")

### ------------------------------------------------------------------------ ###
### sampling coverage ####
### ------------------------------------------------------------------------ ###

### proportion covered by sampled fleets in comparison to total (raised) catch
### separately for discards and landings
if (isTRUE(verbose)) 
  table1 %>% group_by(Year, CatchCategory, SampledOrEstimated) %>%
    summarise(catch = sum(CATON)/1000) %>%
    mutate(contribution = catch / sum(catch)) %>%
    print(n = Inf)

### proportion covered in submitted data (before raising)
if (isTRUE(verbose)) 
  table1 %>%
    group_by(Year, CatchCategory, CATONRaisedOrImported, SampledOrEstimated) %>%
    summarise(catch = sum(CATON)/1000) %>%
    mutate(contribution = catch / sum(catch)) %>%
    print(n = Inf)

### ------------------------------------------------------------------------ ###
### more numbers for report ####
### ------------------------------------------------------------------------ ###

### proportion of landings with associated discards
if (isTRUE(verbose)) 
  table1 %>%
    filter(CATONRaisedOrImported == "Imported_Data") %>%
    select(Year, Country, Fleet, Season, CatchCategory, CATON) %>%
    group_by(Year, Country, Fleet, Season, CatchCategory) %>%
    tidyr::spread(CatchCategory, CATON) %>%
    mutate(d = ifelse(is.na(Discards), "no_discards", "discards")) %>%
    group_by(Year, d) %>%
    summarise(catch = sum(Landings)) %>%
    tidyr::spread(d, catch) %>%
    mutate(prop_dis_rep = discards/(no_discards + discards)) %>%
    print(n = Inf)

### proportion covered in submitted data (before raising)
if (isTRUE(verbose)) 
  table1 %>%
    group_by(Year, CatchCategory, CATONRaisedOrImported, SampledOrEstimated) %>%
    summarise(catch = sum(CATON)) %>%
    mutate(contribution = catch / sum(catch)) %>%
    print(n = Inf)

### ------------------------------------------------------------------------ ###
### discard ratio calculation for all past years ####
### ------------------------------------------------------------------------ ###

  ### ---------------------------------------------------------------------- ###
  ### ratio of total reported discards/landings
  
  reported <- table1 %>% 
    filter(CATONRaisedOrImported != "Raised_Discards") %>%
    group_by(Year, CatchCategory) %>%
    summarise(catch = sum(CATON)) %>%
    tidyr::spread(key = CatchCategory, value = catch) %>%
    transmute(Ratio = Discards / (Discards + Landings))

  ### ---------------------------------------------------------------------- ###
  ### mean of ratios
  
  ### old approach
  ### calculate discard ratio per fleet (for those for which there is data)
  ### and average
  
  dat_old <- table1 %>% 
    filter(CATONRaisedOrImported != "Raised_Discards") %>%
    group_by(Year, Country, CatchCategory, Fleet) %>%
    summarise(catch = sum(CATON)) %>%
    tidyr::spread(key = CatchCategory, value = catch) %>%
    mutate(Ratio = Discards / (Discards + Landings)) %>%
    ungroup() %>% group_by(Year) %>%
    summarise(Ratio = mean(Ratio, na.rm = TRUE))
  
  ### ---------------------------------------------------------------------- ###
  ### weighted mean
  ### ---------------------------------------------------------------------- ###
  
  dat_weighted <- table1 %>% 
    filter(CATONRaisedOrImported != "Raised_Discards") %>%
    group_by(Year, Country, CatchCategory, Fleet) %>%
    summarise(catch = sum(CATON)) %>%
    tidyr::spread(key = CatchCategory, value = catch) %>%
    mutate(catch = Discards + Landings, 
           Ratio = Discards / (Discards + Landings)) %>%
    ungroup() %>% group_by(Year) %>%
    summarise(Ratio = weighted.mean(x = Ratio, w = catch, na.rm = TRUE))
  
  ### ---------------------------------------------------------------------- ###
  ### InterCatch raised discards
  ### ---------------------------------------------------------------------- ###
  
  raised <- table1 %>% group_by(Year, CatchCategory) %>%
    summarise(catch = sum(CATON)) %>%
    tidyr::spread(key = CatchCategory, value = catch) %>%
    transmute(Ratio = Discards / (Discards + Landings))
  
  ### ---------------------------------------------------------------------- ###
  ### InterCatch raised discards INCL migration
  ### ---------------------------------------------------------------------- ###
  
  ### load catch and landings
  vpa_catch <- readFLQuant(input_file = "boot/data/vpa_files/PLE7ECA.dat")
  vpa_landings <- readFLQuant(input_file = "boot/data/vpa_files/PLE7ELA.dat")
  vpa_discards <- vpa_catch - vpa_landings
  
  ### calculate rate
  raised_migration <- vpa_discards / vpa_catch
  raised_migration <- as.data.frame(raised_migration)
  
  ### select required columns
  raised_migration <- raised_migration[, c("year", "data")]
  names(raised_migration) <- c("Year", "Ratio")
  
  ### keep only data >= 2012
  raised_migration <- raised_migration[raised_migration$Year >= 2012, ]
  
  ### ---------------------------------------------------------------------- ###
  ### aggregate calculations and plot

  ratios <- bind_rows(mutate(reported, Method = "reported"),
                      mutate(dat_old, Method = "fleet mean"),
                      mutate(dat_weighted, Method = "weighted fleet mean"),
                      mutate(raised, Method = "raised"),
                      mutate(raised_migration, 
                             Method = ("raised incl.\nmigration")))
  
  ### change order of methods
  ratios$Method <- factor(ratios$Method,
                          levels = unique(ratios$Method))
  
  ### save for now, updated in script data_2_migration_weights.R ...
  write.csv(x = ratios, file = "data/discard_rates.csv", row.names = FALSE)
  

### ------------------------------------------------------------------------ ###
### 7e catch summary ####
### ------------------------------------------------------------------------ ###

smry7e <- table1 %>%
    group_by(Year, CatchCategory) %>%
    mutate(CatchCategory = ifelse(CatchCategory == "Landings", "Landings",
                                  "Discards")) %>%
    summarise(catch = sum(CATON)/1000)
write.csv(smry7e, file = "data/catch_history_7e.csv", row.names = FALSE)  
View(smry7e)  
