### NOT USED IN 2024 ####

### ------------------------------------------------------------------------ ###
### prepare data for rfb rule ####
### ------------------------------------------------------------------------ ###

mkdir("data/rfb")

### ------------------------------------------------------------------------ ###
### FSP biomass index ####
### ------------------------------------------------------------------------ ###

### copy csv file with FSP biomass at age
file.copy(from = "boot/data/tuning/FSP_biomass.csv", 
          to = "data/rfb/FSP_biomass.csv")

### ------------------------------------------------------------------------ ###
### length data from InterCatch ####
### ------------------------------------------------------------------------ ###

file.copy(from = "boot/data/InterCatch/length/catch/table2_hist.csv",
          to = "data/rfb/InterCatch_length.csv")


### ------------------------------------------------------------------------ ###
### catch data ####
### ------------------------------------------------------------------------ ###

### summary file
advice <- read.csv("boot/data/advice/advice_history.csv")

### load catch data
migration_caton_hist <- readRDS("boot/data/InterCatch/migration/TotRemovalsFirstQVIIe_catch_historical.RDS")
migration_laton_hist <- readRDS("boot/data/InterCatch/migration/TotRemovalsFirstQVIIe_historical.RDS")
migration_daton_hist <- readRDS("boot/data/InterCatch/migration/TotRemovalsFirstQVIIe_discards_historical.RDS")
stk <- readRDS("data/model_input_stk_d.RDS")

years <- advice$year
advice$ICES_discards_7e <- c((discards(stk) - migration_daton_hist)[, ac(years)])
advice$ICES_discards_stock <- c(discards(stk)[, ac(years)])
advice$ICES_landings_stock <- c(landings(stk)[, ac(years)])
advice$ICES_catch_7e <- c((catch(stk) - migration_caton_hist)[, ac(years)])
advice$ICES_catch_stock <- c(catch(stk)[, ac(years)])
advice$ICES_landings_7e <- c((landings(stk) - migration_laton_hist)[, ac(years)])

### save
write.csv(advice, file = "data/rfb/advice_history.csv", row.names = FALSE)

