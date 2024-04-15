### ------------------------------------------------------------------------ ###
### load data from VPA files and create FLR input objects ####
### ------------------------------------------------------------------------ ###


## Before: VPA input files in data/:
##           PLE7EIND_L.DAT
##           PLE7ELA.DAT, PLE7ELN.DAT, PLE7ELW.DAT, PLE7ESW_L.DAT, PLE7ENM.DAT, 
##           PLE7EMO.DAT, PLE7EPF.DAT, PLE7EPM.DAT, 
##           PLE7ETU_full.dat

## After: data/model_input_stk.RDS
##        data/model_input_idx.RDS
##        data/model_input_discards.RDS


### ------------------------------------------------------------------------ ###
### load data for XSA assessment ####
### ------------------------------------------------------------------------ ###

### load packages
suppressPackageStartupMessages(library(icesTAF))
taf.libPaths()
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(FLCore))

### load additional functions
source("utilities.R")

### ------------------------------------------------------------------------ ###
### load stock data ####
### ------------------------------------------------------------------------ ###

### read data
stock <- readFLStock(paste0("data/PLE7EIND_L.DAT"),
                     no.discards = TRUE, quiet = TRUE)

### define fbar age range
range(stock)[c("minfbar", "maxfbar")] <- c(3, 6)

### set units
units(stock)[1:17] <- as.list(c(rep(c("tonnes", "thousands", "kg"), 4),
                                "NA", "NA", "f", "NA", "NA"))
units <- units(stock)

### set plusgroup
stock <- setPlusGroup(stock, 10)

# summary(stock)

### ------------------------------------------------------------------------ ###
### SOP correction ####
### ------------------------------------------------------------------------ ###
SOP_factor <- landings(stock)/catch(stock)
# SOP_factor

### correct wts
landings.wt(stock) <- catch.wt(stock) <- landings.wt(stock) * 
  rep(c(SOP_factor), each = dims(stock)$age)

# computeLandings(stock)/landings(stock)
### update total catch with imported catch values
catch(stock) <- computeCatch(stock)

### set units again
units(stock) <- units

### ------------------------------------------------------------------------ ###
### read and process survey indices ####
### ------------------------------------------------------------------------ ###

### load indices
idx_all <- readFLIndices("data/PLE7ETU_full.dat")
# summary(idx_all)

### keep only required indices
idx <- idx_all[c("FSP-7e", "Q1SWBeam")]

### trim ages and years according to WGCSE 
### remove data for FSP-7e for year 2008
index(idx[["FSP-7e"]])[,"2008"] <- NA
idx[["FSP-7e"]] <- idx[["FSP-7e"]][ac(2:8),]

### adapt Q1SWBeam: ages 2-9
idx[["Q1SWBeam"]] <- idx[["Q1SWBeam"]][ac(2:9),]

### last age is not a plusgroup
range(idx[["FSP-7e"]])[["plusgroup"]] <- NA
range(idx[["Q1SWBeam"]])[["plusgroup"]] <- NA

# summary(idx)

### ------------------------------------------------------------------------ ###
### discards ####
### ------------------------------------------------------------------------ ###
### done in data_6_discards.R


### ------------------------------------------------------------------------ ###
### save model input files ####
### ------------------------------------------------------------------------ ###

saveRDS(object = stock, file = "data/model_input_stk.RDS")
saveRDS(object = idx, file = "data/model_input_idx.RDS")
saveRDS(object = idx_all, file = "data/model_input_idx_all.RDS")
# saveRDS(object = discards, file = "data/model_input_discards.RDS")

