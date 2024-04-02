### ------------------------------------------------------------------------ ###
### take values from InterCatch
### smooth weights at age
### update VPA files
### ------------------------------------------------------------------------ ###

## Before: many InterCatch data files: bootstrap/data/InterCatch/*
##         VPA files in bootstrap/data/vpa_files/:
##           PLE7ECA.DAT PLE7ECN.DAT PLE7ECW.DAT PLE7EDA.DAT PLE7EDN.DAT
##           PLE7EDW.DAT PLE7EIND_C.DAT PLE7EIND_L.DAT PLE7ELA.DAT PLE7ELN.DAT
##           PLE7ELW.DAT PLE7EMO.DAT PLE7ENM.DAT PLE7EPF.DAT PLEEPM.DAT 
##           PLE7ESW_C.DAT PLE7EDW_L.DAT
##           PLE7ETU_full.dat
## After: VPA files in data/vpa_files:
##           PLE7ECA.DAT PLE7ECN.DAT PLE7ECW.DAT PLE7EDA.DAT PLE7EDN.DAT
##           PLE7EDW.DAT PLE7EIND_C.DAT PLE7EIND_L.DAT PLE7ELA.DAT PLE7ELN.DAT
##           PLE7ELW.DAT PLE7EMO.DAT PLE7ENM.DAT PLE7EPF.DAT PLEEPM.DAT 
##           PLE7ESW_C.DAT PLE7EDW_L.DAT
##           PLE7ETU_full.dat
##         updated files with migration history in
##           bootstrap/data/InterCatch/migration/*.RDS and *.RData
##         some data plots: data/plots_*.png

### packages
suppressPackageStartupMessages(library(TAF))
taf.libPaths()
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(Cairo))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(FLCore))

### load additional functions
source("utilities.R")

if (!exists("verbose")) verbose <- FALSE

### ------------------------------------------------------------------------ ###
### load InterCatch data ####
### ------------------------------------------------------------------------ ###

### load landings data from InterCatch
laton <- readFLQuant(input_file = "bootstrap/data/InterCatch/landings/caton.txt")
lanum <- readFLQuant(input_file = "bootstrap/data/InterCatch/landings/canum.txt")
wela <- readFLQuant(input_file = "bootstrap/data/InterCatch/landings/weca.txt")

### load discards data from InterCatch
daton <- readFLQuant(input_file = "bootstrap/data/InterCatch/discards/caton.txt")
danum <- readFLQuant(input_file = "bootstrap/data/InterCatch/discards/canum.txt")
weda <- readFLQuant(input_file = "bootstrap/data/InterCatch/discards/weca.txt")

### load catch data from InterCatch
caton <- readFLQuant(input_file = "bootstrap/data/InterCatch/catch/caton.txt")
canum <- readFLQuant(input_file = "bootstrap/data/InterCatch/catch/canum.txt")
weca <- readFLQuant(input_file = "bootstrap/data/InterCatch/catch/weca.txt")

### get year
year <- dims(caton)$maxyear
data_year <- year

### ------------------------------------------------------------------------ ###
### load historical InterCatch data ####
### ------------------------------------------------------------------------ ###
### check if data for previous years exist, e.g. because of revisions
###
rev_dirs <- list.files(path = "bootstrap/data/InterCatch/", 
                       pattern = paste0(2012:(data_year - 1), "_revision", 
                                        collapse = "|"))
rev_years <- as.numeric(gsub(x = rev_dirs, pattern = "\\D", replacement = ""))
names(rev_dirs) <- rev_years

### proceed if data detected
if (length(rev_years) > 0) {
  
  data_year <- c(rev_years, data_year)
  
  ### load data
  hist_data <- lapply(rev_dirs, function(y) {
    
    path_tmp <- paste0("bootstrap/data/InterCatch/", y, "/")
    list(
      ### landings
      laton = readFLQuant(paste0(path_tmp, "landings/caton.txt")),
      lanum = readFLQuant(paste0(path_tmp, "landings/canum.txt")),
      wela = readFLQuant(paste0(path_tmp, "landings/weca.txt")),
      ### discards
      daton = readFLQuant(paste0(path_tmp, "discards/caton.txt")),
      danum = readFLQuant(paste0(path_tmp, "discards/canum.txt")),
      weda = readFLQuant(paste0(path_tmp, "discards/weca.txt")),
      ### catch
      caton = readFLQuant(paste0(path_tmp, "catch/caton.txt")),
      canum = readFLQuant(paste0(path_tmp, "catch/canum.txt")),
      weca = readFLQuant(paste0(path_tmp, "catch/weca.txt"))
    )
    
  })
  ### add data from current year
  hist_data[[length(hist_data) + 1]] <- list(laton = laton, lanum = lanum, 
                                           wela = wela, daton = daton, 
                                           danum = danum, weda = weda,
                                           caton = caton, canum = canum, 
                                           weca = weca)
  names(hist_data)[length(hist_data)] <- tail(data_year, 1)
  ### re-structure list
  names_tmp <- names(hist_data[[1]])
  hist_data <- lapply(names_tmp, function(x) {
    lapply(hist_data, "[[", x)
  })
  names(hist_data) <- names_tmp
  ### standardise ages
  hist_data[c(2:3, 5:6, 8:9)] <- lapply(
    hist_data[c(2:3, 5:6, 8:9)], lapply, trim_age, max = 15, min = 1, fill = 0)
  
  ### function for combining years
  comb_yrs <- function(list, data_year) {
    #browser()
    res <- window(tail(x, 1)[[1]], start = min(data_year), end = max(data_year))
    for (yr in data_year) {
      res[dimnames(list[[ac(yr)]])$age, ac(yr)] <- list[[ac(yr)]]
    }
    return(res)
  }
  ### coerce all years into single FLQuant
  hist_data <- lapply(hist_data, 
                      function(list, years = data_year) {
                        res <- window(list[[1]], start = min(data_year), end = max(data_year))
                        for (yr in data_year) {
                          res[, ac(yr)] <- list[[ac(yr)]]
                        }
                        return(res)
                      })
  
  ### extract list elements
  laton <- hist_data$laton
  lanum <- hist_data$lanum
  wela <- hist_data$wela
  daton <- hist_data$daton
  danum <- hist_data$danum
  weda <- hist_data$weda
  caton <- hist_data$caton
  canum <- hist_data$canum
  weca <- hist_data$weca
  
}

### ------------------------------------------------------------------------ ###
### load migration data ####
### ------------------------------------------------------------------------ ###
migration_path <- "bootstrap/data/InterCatch/migration/"
### ------------------------------------------------------------------------ ###
### total catch tonnage ####

### laton
migration_laton <- read.csv(file = paste0(migration_path, 
                                          "TotRemovalsFirstQVIIe.txt"))
### daton
migration_daton <- read.csv(file = paste0(migration_path, 
                                          "TotRemovalsFirstQVIIe_discards.txt"))
### load landings history
migration_laton_hist <- readRDS(file = paste0(migration_path,
                                              "TotRemovalsFirstQVIIe_historical.RDS"))
### load discard history
migration_daton_hist <- readRDS(file = paste0(migration_path,
                                              "TotRemovalsFirstQVIIe_discards_historical.RDS"))
### add/update
migration_laton_hist <- window(migration_laton_hist, end = max(data_year))
migration_daton_hist <- window(migration_daton_hist, end = max(data_year))
migration_laton_hist[, ac(max(data_year))] <- unlist(migration_laton)
migration_daton_hist[, ac(max(data_year))] <- unlist(migration_daton)

### ------------------------------------------------------------------------ ###
### catch numbers at age ####

### lanum
migration_lanum <- read.csv2(file = paste0(migration_path, 
                                           "TotRemovalsN_FirstQVIIe.txt"))
### convert to FLQuant
migration_lanum <- FLQuant(as.numeric(as.character(unlist(migration_lanum))),
                           dimnames = list(age = 0:c(nrow(migration_lanum) - 1), 
                                           year = max(data_year)), 
                           units = "thousands")
### danum
migration_danum <- read.csv2(file = paste0(migration_path, 
                                           "TotRemovalsN_FirstQVIIe_discards.txt"))
### convert to FLQuant
migration_danum <- FLQuant(as.numeric(as.character(unlist(migration_danum))),
                           dimnames = list(age = 0:c(nrow(migration_danum) - 1), 
                                           year = max(data_year)), 
                           units = "thousands")

### load historical landings data
migration_lanum_hist <- readRDS(file = paste0(migration_path,
                                              "TotRemovalsN_FirstQVIIe_historical.RDS"))
### load historical discard data
migration_danum_hist <- readRDS(file = paste0(migration_path, "TotRemovalsN_",
                                              "FirstQVIIe_discards_historical",
                                              ".RDS"))

### add/update
migration_lanum_hist <- window(migration_lanum_hist, end = max(data_year))
migration_danum_hist <- window(migration_danum_hist, end = max(data_year))
migration_lanum_hist[, ac(max(data_year))] <- 
  migration_lanum[trimws(dimnames(migration_lanum_hist)$age), ]
migration_danum_hist[, ac(max(data_year))] <- 
  migration_danum[trimws(dimnames(migration_danum_hist)$age), ]

### ------------------------------------------------------------------------ ###
### weights at age ####

### wela
migration_wela <- read.csv2(file = paste0(migration_path, "Q1_weca.txt"),
                            as.is = TRUE)
### convert to FLQuant
migration_wela <- FLQuant(migration_wela$data,
                           dimnames = list(age = migration_wela$age, 
                                           year = unique(migration_wela$year)), 
                           units = "kg")

### weda
migration_weda <- read.csv2(file = paste0(migration_path, 
                                          "Q1_weca_discards.txt"),
                            as.is = TRUE)
### convert to FLQuant
migration_weda <- FLQuant(migration_weda$data,
                          dimnames = list(age = migration_weda$age, 
                                          year = unique(migration_weda$year)), 
                          units = "kg")

### load historical data
migration_wela_hist <- readRDS(file = paste0(migration_path,
                                             "Q1_weca_historical.RDS"))
migration_weda_hist <- readRDS(file = paste0(migration_path,
                                             "Q1_weca_discards_historical.RDS"))

### standardise units
if (units(migration_wela) == "grams") {
  
  migration_wela <- migration_wela / 1000
  units(migration_wela) <- "kg"
  
}
### standardise units
if (units(migration_weda) == "grams") {
  
  migration_weda <- migration_weda / 1000
  units(migration_weda) <- "kg"
  
}

### add/update
migration_wela_hist <- window(migration_wela_hist, end = max(data_year))
migration_weda_hist <- window(migration_weda_hist, end = max(data_year))
migration_wela_hist[, ac(max(data_year))] <- 
  migration_wela[trimws(dimnames(migration_wela_hist)$age), ]
migration_weda_hist[, ac(max(data_year))] <- 
  migration_weda[trimws(dimnames(migration_weda_hist)$age), ]

### ------------------------------------------------------------------------ ###
### calculate migration catch (landings + discards) ####
### ------------------------------------------------------------------------ ###

### total catch
migration_caton_hist <- migration_laton_hist + migration_daton_hist
units(migration_caton_hist) <- units(migration_laton_hist)

### catch numbers
migration_canum_hist <- migration_lanum_hist + migration_danum_hist
units(migration_canum_hist) <- units(migration_lanum_hist)

### catch weight 
### weighted mean of discards and landings, weighted by catch numbers
### replace NAs with 0 for weighting
### numbers
if (any(is.na(migration_lanum_hist)))
  migration_lanum_hist[is.na(migration_lanum_hist)][] <- 0
if (any(is.na(migration_danum_hist)))
  migration_danum_hist[is.na(migration_danum_hist)][] <- 0
### weights
if (any(is.na(migration_wela_hist)))
  migration_wela_hist[is.na(migration_wela_hist)][] <- 0
if (any(is.na(migration_weda_hist)))
  migration_weda_hist[is.na(migration_weda_hist)][] <- 0
### weighted mean
migration_weca_hist <- (migration_lanum_hist * migration_wela_hist + 
    migration_danum_hist * migration_weda_hist) / 
  (migration_lanum_hist + migration_danum_hist)
### remove NAs from 0/0
if (any(is.na(migration_weca_hist)))
  migration_weca_hist[is.na(migration_weca_hist)][] <- 0

### check SOP
if (isTRUE(verbose)) {
  SOP_factor <- quantSums(migration_weca_hist * migration_canum_hist) /
    migration_caton_hist
  ### replace NA with 1
  SOP_factor[is.na(SOP_factor)] <- 1
  SOP_factor
  ### SOP correction for weights
  # migration_weca_hist <- migration_weca_hist /
  #   FLQuant(rep(c(SOP_factor), each = dims(migration_weca_hist)$age),
  #           dimnames = dimnames(migration_weca_hist))
}
### correct SOP later, when InterCatch and migration are combined
units(migration_weca_hist) <- units(migration_wela_hist)

# quantSums(migration_wela * migration_lanum)
# migration_laton
# 
# quantSums(migration_weda * migration_danum)
# migration_daton

### ------------------------------------------------------------------------ ###
### save migration time series as FLQuant objects ####
### ------------------------------------------------------------------------ ###

### laton
saveRDS(object = migration_laton_hist,
        file = paste0(migration_path, "TotRemovalsFirstQVIIe_historical.RDS"))
### lanum
saveRDS(object = migration_lanum_hist,
        file = paste0(migration_path, "TotRemovalsN_FirstQVIIe_historical.RDS"))
### wela
saveRDS(object = migration_wela_hist,
        file = paste0(migration_path, "Q1_weca_historical.RDS"))

### daton
saveRDS(object = migration_daton_hist,
        file = paste0(migration_path,
                      "TotRemovalsFirstQVIIe_discards_historical.RDS"))
### danum
saveRDS(object = migration_danum_hist,
        file = paste0(migration_path,
                      "TotRemovalsN_FirstQVIIe_discards_historical.RDS"))
### weda
saveRDS(object = migration_weda_hist,
        file = paste0(migration_path, "Q1_weca_discards_historical.RDS"))

### caton
saveRDS(object = migration_caton_hist,
        file = paste0(migration_path,
                      "TotRemovalsFirstQVIIe_catch_historical.RDS"))
### canum
saveRDS(object = migration_canum_hist,
        file = paste0(migration_path,
                      "TotRemovalsN_FirstQVIIe_catch_historical.RDS"))
### weca
saveRDS(object = migration_weca_hist,
        file = paste0(migration_path, "Q1_weca_catch_historical.RDS"))

### all together
save(migration_laton_hist, migration_daton_hist, migration_caton_hist,
     migration_lanum_hist, migration_danum_hist, migration_canum_hist,
     migration_wela_hist, migration_weda_hist,migration_weca_hist,
     file = paste0(migration_path, "migration_data.RData"))

### ------------------------------------------------------------------------ ###
### use common units: tonnes, thousand, kg for InterCatch data ####
### ------------------------------------------------------------------------ ###

if (units(lanum) == "numbers") {
  lanum <- lanum/1000
  units(lanum) <- "thousands"
  danum <- danum/1000
  units(danum) <- "thousands"
  canum <- canum/1000
  units(canum) <- "thousands"
}

if (units(wela) == "grams") {
  wela <- wela/1000
  units(wela) <- "kg"
  weda <- weda/1000
  units(weda) <- "kg"
  weca <- weca/1000
  units(weca) <- "kg"
}

### laton, daton & caton are already in tonnes

### ------------------------------------------------------------------------ ###
### extract migration data for requested year(s) ####
### ------------------------------------------------------------------------ ###

### for landings
migration_laton <- migration_laton_hist[, ac(data_year)]
migration_lanum <- migration_lanum_hist[, ac(data_year)]
migration_wela <- migration_wela_hist[, ac(data_year)]

### for discards
migration_daton <- migration_daton_hist[, ac(data_year)]
migration_danum <- migration_danum_hist[, ac(data_year)]
migration_weda <- migration_weda_hist[, ac(data_year)]

### for catch
migration_caton <- migration_caton_hist[, ac(data_year)]
migration_canum <- migration_canum_hist[, ac(data_year)]
migration_weca <- migration_weca_hist[, ac(data_year)]

### ------------------------------------------------------------------------ ###
### use same age structure for InterCatch and migration data ####
### -> inflate migration FLQuants
### ------------------------------------------------------------------------ ###

### remove age 0 from InterCatch data (rarely provided, unreliable)
if (any(identical(dims(lanum)$min, 0),
        identical(dims(danum)$min, 0),
        identical(dims(canum)$min, 0))) {
  lanum <- lanum[setdiff(dimnames(lanum)$age, 0), ]
  wela <- wela[setdiff(dimnames(wela)$age, 0), ]
  danum <- danum[setdiff(dimnames(danum)$age, 0), ]
  weda <- weda[setdiff(dimnames(weda)$age, 0), ]
  canum <- canum[setdiff(dimnames(canum)$age, 0), ]
  weca <- weca[setdiff(dimnames(weca)$age, 0), ]
}

### add older ages
if (dims(lanum)$max == 15 & dims(wela)$max == 15 &
    dims(migration_lanum)$max < 15 & dims(migration_wela)$max < 15) {
  
  ### catch
  migration_canum <- expand_age(migration_canum, max_age = 15, value = 0)
  migration_weca <- expand_age(migration_weca, max_age = 15, value = 0)
  ### discards
  migration_danum <- expand_age(migration_danum, max_age = 15, value = 0)
  migration_weda <- expand_age(migration_weda, max_age = 15, value = 0)
  ### landings
  migration_lanum <- expand_age(migration_lanum, max_age = 15, value = 0)
  migration_wela <- expand_age(migration_wela, max_age = 15, value = 0)
}

### replace NA with 0
if (any(is.na(migration_canum))) migration_canum[is.na(migration_canum)] <- 0
if (any(is.na(migration_weca))) migration_weca[is.na(migration_weca)] <- 0
if (any(is.na(migration_lanum))) migration_lanum[is.na(migration_lanum)] <- 0
if (any(is.na(migration_wela))) migration_wela[is.na(migration_wela)] <- 0
if (any(is.na(migration_danum))) migration_danum[is.na(migration_danum)] <- 0
if (any(is.na(migration_weda))) migration_weda[is.na(migration_weda)] <- 0

### inflate InterCatch data
### maximum age
if (dims(lanum)$max < 15) lanum <- expand_age(lanum, max_age = 15, value = 0)
if (dims(wela)$max  < 15) wela  <- expand_age(wela, max_age = 15, value = 0)
if (dims(danum)$max < 15) danum <- expand_age(danum, max_age = 15, value = 0)
if (dims(weda)$max  < 15) weda  <- expand_age(weda, max_age = 15, value = 0)
if (dims(canum)$max < 15) canum <- expand_age(canum, max_age = 15, value = 0)
if (dims(weca)$max  < 15) weca  <- expand_age(weca, max_age = 15, value = 0)
### minimum age
if (dims(lanum)$min > 1) lanum <- trim_age(lanum, min = 1, fill = 0)
if (dims(wela)$min  > 1) wela  <- trim_age(wela, min = 1, fill = 0)
if (dims(danum)$min > 1) danum <- trim_age(danum, min = 1, fill = 0)
if (dims(weda)$min  > 1) weda  <- trim_age(weda, min = 1, fill = 0)
if (dims(canum)$min > 1) canum <- trim_age(canum, min = 1, fill = 0)
if (dims(weca)$min  > 1) weca  <- trim_age(weca, min = 1, fill = 0)

### ------------------------------------------------------------------------ ###
### combine landings & migration correction ####
### ------------------------------------------------------------------------ ###

### landings
combined_laton <- laton + migration_laton
combined_lanum <- lanum + migration_lanum
combined_wela <- (lanum * wela + migration_lanum * migration_wela) / 
  (lanum + migration_lanum)

### discards
combined_daton <- daton + migration_daton
combined_danum <- danum + migration_danum
combined_weda <- (danum * weda + migration_danum * migration_weda) / 
  (danum + migration_danum)

### catch
combined_caton <- caton + migration_caton
combined_canum <- canum + migration_canum
combined_weca <- (canum * weca + migration_canum * migration_weca) / 
  (canum + migration_canum)

### ------------------------------------------------------------------------ ###
### fit 2nd degree polynomial model to weights~age ####
### ------------------------------------------------------------------------ ###
### individually for every year (if more than one year provided)

### templates for smoothed weights
landings_catch_weights <- landings_stock_weights <- discards_catch_weights <- 
  discards_stock_weights <- catch_catch_weights <- catch_stock_weights <- 
  combined_weca %=% 0

for (yr in data_year) {
  
  ### separately for all catch categories
  res_landings <- fit_polynomial(
    canum = combined_lanum[, ac(yr)], weca = combined_wela[, ac(yr)], 
    caton = combined_laton[, ac(yr)], 
    fit_age = -15, SOP_data_ages = 15, plot_diagnostics = FALSE, x = 0, y = 1.5)
  res_discards <- fit_polynomial(
    canum = combined_danum[, ac(yr)], weca = combined_weda[, ac(yr)], 
    caton = combined_daton[, ac(yr)], 
    fit_age = -15, SOP_data_ages = 15, plot_diagnostics = FALSE, x = 0, y = 1.5)
  res_catch <- fit_polynomial(
    canum = combined_canum[, ac(yr)], weca = combined_weca[, ac(yr)], 
    caton = combined_caton[, ac(yr)], 
    fit_age = -15, SOP_data_ages = 15, plot_diagnostics = FALSE, x = 0, y = 1.5)
  
  ### plot
  p <- res_landings$plot
  if (isTRUE(verbose)) p
  ggsave(file = paste0("data/plots_smoothed_weights_",
                       yr, "_landings.png"), type = "cairo-png",
         width = 15, height = 10, units = "cm", dpi = 300, plot = p)
  p <- res_catch$plot
  if (isTRUE(verbose)) p
  ggsave(file = paste0("data/plots_smoothed_weights_",
                       yr, "_catch.png"), type = "cairo-png",
         width = 15, height = 10, units = "cm", dpi = 300, plot = p)
  p <- res_discards$plot
  if (isTRUE(verbose)) p
  ggsave(file = paste0("data/plots_smoothed_weights_",
                       yr, "_discards.png"), type = "cairo-png",
         width = 15, height = 10, units = "cm", dpi = 300, plot = p)
  
  ### extract values
  landings_catch_weights[, ac(yr)] <- res_landings$catch_weights
  landings_stock_weights[, ac(yr)] <- res_landings$stock_weights
  discards_catch_weights[, ac(yr)] <- res_discards$catch_weights
  discards_stock_weights[, ac(yr)] <- res_discards$stock_weights # nonsense...
  catch_catch_weights[, ac(yr)] <- res_catch$catch_weights
  catch_stock_weights[, ac(yr)] <- res_catch$stock_weights
  
}

### ------------------------------------------------------------------------ ###
### set plusgroup
### ------------------------------------------------------------------------ ###

### set plusgroup for catch and stock weights
landings_catch_weights <- setPlusGroupWeca(
  number = combined_lanum, weight = landings_catch_weights, plusgroup = 10)
landings_stock_weights <- setPlusGroupWeca(
  number = combined_lanum, weight = landings_stock_weights, plusgroup = 10)
discards_catch_weights <- setPlusGroupWeca(
  number = combined_danum, weight = discards_catch_weights, plusgroup = 10)
discards_stock_weights <- setPlusGroupWeca(
  number = combined_danum, weight = discards_stock_weights, plusgroup = 10)
catch_catch_weights <- setPlusGroupWeca(
  number = combined_canum, weight = catch_catch_weights, plusgroup = 10)
catch_stock_weights <- setPlusGroupWeca(
  number = combined_canum, weight = catch_stock_weights, plusgroup = 10)

### set plusgroup for canum
landings_numbers <- setPlusGroupSum(number = combined_lanum, plusgroup = 10)
discards_numbers <- setPlusGroupSum(number = combined_danum, plusgroup = 10)
catch_numbers <- setPlusGroupSum(number = combined_canum, plusgroup = 10)


### ------------------------------------------------------------------------ ###
### add/update values to VPA files
### ------------------------------------------------------------------------ ###

### copy files from last year into data folder
file.copy(from = paste0("bootstrap/data/vpa_files/",
                        list.files("bootstrap/data/vpa_files/", 
                                   pattern = "*.DAT$")),
          to = "data/", overwrite = TRUE)

### catch numbers
update_vpa(file = "data/PLE7ECN.DAT", 
           update = round(catch_numbers, 2))
### landings numbers
update_vpa(file = "data/PLE7ELN.DAT", 
           update = round(landings_numbers, 2))
### discards numbers
update_vpa(file = "data/PLE7EDN.DAT", 
           update = round(discards_numbers, 2))

### catch weights
update_vpa(file = "data/PLE7ECW.DAT", 
           update = round(catch_catch_weights, 3))
### landings weights at age
update_vpa(file = "data/PLE7ELW.DAT", 
           update = round(landings_catch_weights, 3))
### discard weights at age
update_vpa(file = "data/PLE7EDW.DAT", 
           update = round(discards_catch_weights, 3))

### stock weights (landings)
update_vpa(file = "data/PLE7ESW_L.DAT", 
           update = round(landings_stock_weights, 3))
### stock weights (catch)
update_vpa(file = "data/PLE7ESW_C.DAT", 
           update = round(catch_stock_weights, 3))


### total catch
update_vpa(file = "data/PLE7ECA.DAT", 
           update = round(combined_caton, 2))
### total landings
update_vpa(file = "data/PLE7ELA.DAT", 
           update = round(combined_laton, 2))
### total discards
update_vpa(file = "data/PLE7EDA.DAT", 
           update = round(combined_daton, 2))


### update year range only in remaining files
update_vpa(file = "data/PLE7EMO.DAT",
           update = caton, year_range_only = TRUE)
update_vpa(file = "data/PLE7ENM.DAT",
           update = caton, year_range_only = TRUE)
update_vpa(file = "data/PLE7EPF.DAT",
           update = caton, year_range_only = TRUE)
update_vpa(file = "data/PLE7EPM.DAT",
           update = caton, year_range_only = TRUE)

### ------------------------------------------------------------------------ ###
### compare time series & plotting ####
### ------------------------------------------------------------------------ ###

### catch history combined
laton_hist <- readFLQuant(input_file = "data/PLE7ELA.DAT")
caton_hist <- readFLQuant(input_file = "data/PLE7ECA.DAT")

### for 7e only
laton_7e <- laton_hist - migration_laton_hist
daton_7e <- (caton_hist - laton_hist - migration_daton_hist)[, ac(2012:max(data_year))]

### 7d component
laton_7d <- migration_laton_hist
daton_7d <- migration_daton_hist[migration_daton_hist != 0]
names(dimnames(daton_7d))[1] <- "age"

### convert into data frame
caton_7e_7d <- rbind(cbind(as.data.frame(laton_7e), 
                           Division = "27.7e", CatchCategory = "landings"),
                     cbind(as.data.frame(laton_7d), 
                           Division = "27.7d", CatchCategory = "landings"),
                     cbind(as.data.frame(daton_7e), 
                           Division = "27.7e", CatchCategory = "discards"),
                     cbind(as.data.frame(daton_7d), 
                           Division = "27.7d", CatchCategory = "discards"))
caton_7e_7d$Division <- factor(as.character(caton_7e_7d$Division),
                               levels = c("27.7d", "27.7e"))
caton_7e_7d$CatchCategory <- factor(as.character(caton_7e_7d$CatchCategory),
                               levels = c("landings", "discards"))
### plot
p <- ggplot(data = caton_7e_7d,
            aes(x = year, y = data, fill = Division)) +
  geom_bar(stat = "identity") +
  theme_custom2 +
  labs(x = "Year", y = "Catch [tonnes]") +
  facet_grid(~ CatchCategory, scales = "free_x", space = "free") +
  scale_x_continuous(breaks = seq(1987, to = 2017, by = 10))
if (isTRUE(verbose)) p
ggsave(file = "data/plots_data_migration_contribution.png",
       type = "cairo-png", width = 15, height = 8, units = "cm", dpi = 300,
       plot = p)

### ------------------------------------------------------------------------ ###
### plot combined discard rate ####
### ------------------------------------------------------------------------ ###

### load catch and landings
vpa_data <- FLQuants(
  catch =  readFLQuant(input_file = "data/PLE7ECA.dat"),
  landings = readFLQuant(input_file = "data/PLE7ELA.dat"),
  discards = FLQuant())
vpa_data$discards <- with(vpa_data, catch - landings)
vpa_data <- window(vpa_data, start = 2012)
vpa_data <- vpa_data[c("landings", "discards")]

### format and calculate discard rate
vpa_data2 <- as.data.frame(vpa_data) %>%
  tidyr::spread(key = qname, value = data) %>%
  mutate("discard_rate" = (discards / (landings + discards))*100,
         landings = NULL, discards = NULL,
         quant = "discard rate [%]") %>%
  bind_rows(as.data.frame(vpa_data) %>%
              mutate(quant = "catch [tonnes]"))
vpa_data2$qname <- factor(as.character(vpa_data2$qname),
                          levels = rev(levels(vpa_data2$qname)))

p <- ggplot(vpa_data2) +
  geom_bar(aes(x = year, y = data, fill = qname), 
           stat = "identity") +
  geom_point(aes(x = year, y = discard_rate)) + 
  geom_line(aes(x = year, y = discard_rate)) + 
  facet_wrap(~ quant, scales = "free") +
  scale_fill_discrete("") +
  ylim(0, NA) +
  theme_custom2 +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(y = "")
if (isTRUE(verbose)) p
ggsave(file = "data/plots_data_catch_rate_incl_migration.png", 
       type = "cairo-png", width = 15, height = 8, units = "cm", dpi = 300,
       plot = p)

### ------------------------------------------------------------------------ ###
### update discard rates with migration data ####
### ------------------------------------------------------------------------ ###

### load discard rates
ratios <- read.csv(file = "data/discard_rates.csv", as.is = TRUE)
ratios <- ratios[ratios$Method != "raised incl.\nmigration", ]

### get new discard rates
disc_new <- vpa_data2 %>% 
  filter(!is.na(vpa_data2$discard_rate)) %>%
  select(year, discard_rate) %>%
  rename(Year = year, Ratio = discard_rate) %>%
  mutate(Ratio = Ratio/100, Method = "raised incl.\nmigration")
ratios <- bind_rows(ratios, disc_new)
### update file
write.csv(x = ratios, file = "data/discard_rates.csv", row.names = FALSE)

### plot
p <- ggplot(data = ratios, aes(x = Year, y = Ratio, fill = Method)) +
  geom_bar(stat = "identity", 
           position = position_dodge()) + 
  geom_text(aes(label = sprintf("%.02f", round(Ratio,2))),
            position = position_dodge(width = 1), size = 1.5, 
            family = "serif", angle = 45) +
  theme_custom2 +
  labs(x = "year", y = "Discard ratio")      
if (isTRUE(verbose)) p
ggsave(file = "data/plots_discard_ratios_new.png", plot = p,
       width = 15, height = 8, units = "cm", dpi = 300) 

### ---------------------------------------------------------------------- ###
### mean of ratios in past XXX years ####

ratio <- mean(ratios$Ratio[ratios$Method == "raised incl.\nmigration"])
if (isTRUE(verbose)) ratio

### save ratio
write.table(sprintf("%.100f", ratio), 
            file = "data/discard_ratio.txt",
            row.names = FALSE, col.names = FALSE)
# sprintf("%.50f", raised_migration$Ratio[raised_migration$Year == 2015])
# sprintf("%.50f", raised_migration$Ratio[raised_migration$Year == 2016])

