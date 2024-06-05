### ------------------------------------------------------------------------ ###
### OM data ####
### ------------------------------------------------------------------------ ###
### preparation of data for operating model (OM)
### - FLStock
### FLIndices


### packages
library(icesTAF)
taf.libPaths()
library(tidyr)
library(dplyr)
library(FLCore)

### load additional functions
source("utilities.R")

if (!exists("verbose")) verbose <- FALSE

### ------------------------------------------------------------------------ ###
### load data ####
### ------------------------------------------------------------------------ ###

### catch
catch <- readRDS("data/OM/catch.rds")

### stock weights
stock_weights <- readRDS("data/OM/stock_weights.rds")

### natural mortality
M_Then <- readRDS("data/OM/M_Then.rds")
M_Lorenzen_Linf <- readRDS("data/OM/M_Lorenzen_Linf.rds")
M_Gislason <- readRDS("data/OM/M_Gislason.rds")

### maturity
maturity <- readRDS("data/OM/maturity.rds")


### ------------------------------------------------------------------------ ###
### baseline OM - stock ####
### ------------------------------------------------------------------------ ###

stk <- FLStock(catch$CN)

### catch
catch.n(stk)[]  <- catch$CN
catch.wt(stk)[] <- catch$CW
catch(stk)[]    <- catch$CA
landings.n(stk)[]  <- catch$LN
landings.wt(stk)[] <- catch$LW
landings(stk)[]    <- catch$LA
discards.n(stk)[]  <- catch$DN
discards.wt(stk)[] <- catch$DW
discards.wt(stk)[is.na(discards.wt(stk))] <- 0
discards(stk)[]    <- catch$DA

### stock weights
stock.wt(stk)[] <- stock_weights

### M
m(stk)[] <- M_Then

### maturity
mat(stk)[] <- maturity

### proportion of F and M before spawning - 0
harvest.spwn(stk) <- 0
m.spwn(stk) <- 0

### stock description
name(stk) <- "ple.27.7e"
desc(stk) <- "ple.27.7e from WKBPLAICE 2024 - baseline OM"

### fbar range 3-6
range(stk)[["minfbar"]] <- 3
range(stk)[["maxfbar"]] <- 6

### units
units(stk)[c("catch", "discards", "landings", "stock")] <- "t"
units(stk)[c("catch.n", "discards.n", "landings.n", "stock.n")] <- "1000"
units(stk)[c("catch.wt", "discards.wt", "landings.wt", "stock.wt")] <- "kg"
units(stk)["harvest"] <- "f"

### baseline OM - assume 50% discard survival

discards.n(stk) <- discards.n(stk) * 0.5 ### discard survival
discards(stk) <- discards(stk) * 0.5
catch.n(stk) <- landings.n(stk) + discards.n(stk) ### update catch
catch(stk) <- landings(stk) + discards(stk) ### 
catch.wt(stk) <- (landings.wt(stk)*landings.n(stk) + discards.wt(stk) * discards.n(stk))/(landings.n(stk) + discards.n(stk)) ### adapt catch weights
#catch.wt(stk)[is.na(catch.wt(stk))] <- 0

# (discards.n(stk) + landings.n(stk))/catch.n(stk)
# (discards.n(stk)*discards.wt(stk) + landings.n(stk)*landings.wt(stk))/(catch.n(stk)*catch.wt(stk))
# quantSums(discards.n(stk)*discards.wt(stk) + landings.n(stk)*landings.wt(stk))/quantSums(catch.n(stk)*catch.wt(stk))
# quantSums(discards.n(stk)*discards.wt(stk) + landings.n(stk)*landings.wt(stk))/quantSums(catch.n(stk)*catch.wt(stk))

### save
saveRDS(stk, file = "data/OM/stk_baseline.rds")

### ------------------------------------------------------------------------ ###
### indices ####
### ------------------------------------------------------------------------ ###

Q1SWBeam <- readRDS("data/OM/idx_Q1SWBeam.rds")
FSP <- readRDS("data/OM/idx_FSP.rds")

### Q1SWBeam
Q1SWBeam_idx <- as.FLQuant(Q1SWBeam %>%
                  select(year = Year, age = Age, data = Numbers) %>%
                  filter(age >= 2 & age <= 9))
Q1SWBeam_wt <- as.FLQuant(Q1SWBeam %>%
                        select(year = Year, age = Age, data = Weight) %>%
                        filter(age >= 2 & age <= 9))

idx_Q1 <- FLIndex(index = Q1SWBeam_idx)
catch.n(idx_Q1)[] <- Q1SWBeam_idx
catch.wt(idx_Q1)[] <- Q1SWBeam_wt

range(idx_Q1)[["plusgroup"]] <- NA
range(idx_Q1)[["startf"]] <- 0
range(idx_Q1)[["endf"]] <- 0.25

units(idx_Q1)["index"] <- c("numbers/km2")
units(idx_Q1)["catch.n"] <- c("numbers/km2")
units(idx_Q1)["catch.wt"] <- c("kg")


### UK-FSP
FSP_idx <- as.FLQuant(FSP %>%
                        select(year = year, age = age, data = numbers) %>%
                        filter(age >= 2 & age <= 9))
FSP_wt <- as.FLQuant(FSP %>%
                        select(year = year, age = age, data = weight) %>%
                        filter(age >= 2 & age <= 9))

idx_FSP <- FLIndex(index = FSP_idx)
catch.n(idx_FSP)[] <- FSP_idx
catch.wt(idx_FSP)[] <- FSP_wt

range(idx_FSP)[["plusgroup"]] <- NA
range(idx_FSP)[["startf"]] <- 0.75
range(idx_FSP)[["endf"]] <- 0.80

units(idx_FSP)["index"] <- c("numbers/h m beam")
units(idx_FSP)["catch.n"] <- c("numbers/h m beam")
units(idx_FSP)["catch.wt"] <- c("kg")


### combine both indices
idcs <- FLIndices("UK-FSP" = idx_FSP,
                  "Q1SWBeam" = idx_Q1)

### save
saveRDS(idcs, file = "data/OM/idcs.rds")

