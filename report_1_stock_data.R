### ------------------------------------------------------------------------ ###
### create tables and plots with stock data ####
### ------------------------------------------------------------------------ ###


## Before: model/stock.rds
##         model/stk_int.rds
##         model/diags.rds
##         data/model_input_idx.RDS
##         model/retro.rds
##         boot/data/previous_results/diags_2017.rds
##         boot/data/previous_results/stock_2017.rds
## After:  many of plots and tables in report/

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
suppressPackageStartupMessages(library(FLXSA))
suppressPackageStartupMessages(library(FLash))

### load additional functions
source("utilities.R")

if (!exists("verbose")) verbose <- FALSE

mkdir("report/stock_data/")

### ------------------------------------------------------------------------ ###
### load data ####
### ------------------------------------------------------------------------ ###

idx <- readRDS("data/model_input_idx.RDS") ### tuning information
stk_input_d <- readRDS("data/model_input_stk_d.RDS")

### current assessment year
year <- range(stk_input_d)[["maxyear"]] + 1

### last data year
year_data <- year - 1

### ------------------------------------------------------------------------ ###
### plot input data ####
### ------------------------------------------------------------------------ ###

### landings age structure
p <- plot_age_structure(stock = stk_input_d, slots = "landings.n", 
                        years = (year_data - 15):year_data)
if (isTRUE(verbose)) p
ggsave(file = "report/stock_data/plots_data_landings_age_structure.png",  
       plot = p,
       width = 15, height = 15, units = "cm", dpi = 300, type = "cairo-png")

### plot stock and landings wts
p <- plot_weights(stock = stk_input_d, slots = c("stock.wt", "landings.wt"))
if (isTRUE(verbose)) p
ggsave(file = "report/stock_data/plots_data_weights_at_age.png", plot = p,
       width = 15, height = 8, units = "cm", dpi = 300, type = "cairo-png")


### TO DO: catch curves of landings and discards



### ------------------------------------------------------------------------ ###
### Landings numbers ####
### ------------------------------------------------------------------------ ###

### extract and round 
L_age <- round(t(catch.n(stk_input_d)[, drop = TRUE]))
### add year
L_age <- cbind(year = as.numeric(as.character(rownames(L_age))), L_age)
### change last age to plusgroup
colnames(L_age)[ncol(L_age)] <- paste0(colnames(L_age)[ncol(L_age)], "+")
### add sum
L_age <- cbind(L_age, total = round(c(quantSums(catch.n(stk_input_d)))))
### save file
write.csv(file = "report/tables_input_landings_numbers_at_age.csv",
          row.names = FALSE, x = L_age)

### ------------------------------------------------------------------------ ###
### landings weights at age ####
### ------------------------------------------------------------------------ ###

### extract and round 
WtL_age <- round(t(landings.wt(stk_input_d)[, drop = TRUE]), 3)
### add year
WtL_age <- cbind(year = as.numeric(as.character(rownames(WtL_age))), WtL_age)
### change last age to plusgroup
colnames(WtL_age)[ncol(WtL_age)] <- paste0(colnames(WtL_age)[ncol(WtL_age)], "+")
### save file
write.csv(file = "report/tables_input_landings_weights_at_age.csv",
          row.names = FALSE, x = WtL_age)

### ------------------------------------------------------------------------ ###
### stock weights at age ####
### ------------------------------------------------------------------------ ###

### extract and round F-at-age values
WtS_age <- round(t(stock.wt(stk_input_d)[, drop = TRUE]), 3)
### add year
WtS_age <- cbind(year = as.numeric(as.character(rownames(WtS_age))), WtS_age)
### change last age to plusgroup
colnames(WtS_age)[ncol(WtS_age)] <- paste0(colnames(WtS_age)[ncol(WtS_age)], "+")
### save file
write.csv(file = "report/tables_input_stock_weights_at_age.csv",
          row.names = FALSE, x = WtS_age)

