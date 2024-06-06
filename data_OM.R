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
                        filter(age >= 2 & age <= 8))
FSP_wt <- as.FLQuant(FSP %>%
                        select(year = year, age = age, data = weight) %>%
                        filter(age >= 2 & age <= 8))

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

### ------------------------------------------------------------------------ ###
### alternative plusgroups ####
### ------------------------------------------------------------------------ ###

stk_pg9 <- setPlusGroup(stk, 9)
stk_pg8 <- setPlusGroup(stk, 8)
saveRDS(stk_pg9, file = "data/OM/stk_baseline_pg9.rds")
saveRDS(stk_pg8, file = "data/OM/stk_baseline_pg8.rds")

idcs_pg9 <- idcs
idcs_pg9$Q1SWBeam <- idcs_pg9$Q1SWBeam[ac(2:8), ]
idcs_pg8 <- idcs
idcs_pg8$`UK-FSP` <- idcs_pg8$`UK-FSP`[ac(2:7), ]
idcs_pg8$Q1SWBeam <- idcs_pg8$Q1SWBeam[ac(2:7), ]
saveRDS(idcs_pg9, file = "data/OM/idcs_pg9.rds")
saveRDS(idcs_pg8, file = "data/OM/idcs_pg8.rds")

### ------------------------------------------------------------------------ ###
### 0% discard survival ####
### ------------------------------------------------------------------------ ###

stk_d100 <- stk

### catch
catch.n(stk_d100)[]  <- catch$CN
catch.wt(stk_d100)[] <- catch$CW
catch(stk_d100)[]    <- catch$CA
landings.n(stk_d100)[]  <- catch$LN
landings.wt(stk_d100)[] <- catch$LW
landings(stk_d100)[]    <- catch$LA
discards.n(stk_d100)[]  <- catch$DN
discards.wt(stk_d100)[] <- catch$DW
discards.wt(stk_d100)[is.na(discards.wt(stk_d100))] <- 0
discards(stk_d100)[]    <- catch$DA

# (discards.n(stk_d100) + landings.n(stk_d100))/catch.n(stk_d100)
# (discards.n(stk_d100)*discards.wt(stk_d100) + landings.n(stk_d100)*landings.wt(stk_d100))/(catch.n(stk_d100)*catch.wt(stk_d100))
# quantSums(discards.n(stk_d100)*discards.wt(stk_d100) + landings.n(stk_d100)*landings.wt(stk_d100))/quantSums(catch.n(stk_d100)*catch.wt(stk_d100))
# quantSums(discards.n(stk_d100)*discards.wt(stk_d100) + landings.n(stk_d100)*landings.wt(stk_d100))/quantSums(catch.n(stk_d100)*catch.wt(stk_d100))

### save
saveRDS(stk_d100, file = "data/OM/stk_d100.rds")

### ------------------------------------------------------------------------ ###
### 100% discard survival ####
### ------------------------------------------------------------------------ ###

stk_d0 <- stk_d100

discards.n(stk_d0) <- 0 ### no discards
discards.wt(stk_d0) <- 0
discards(stk_d0) <- 0
catch.n(stk_d0) <- landings.n(stk_d0) ### update catch
catch.wt(stk_d0) <- landings.wt(stk_d0)
catch(stk_d0) <- landings(stk_d0)

# (discards.n(stk_d0) + landings.n(stk_d0))/catch.n(stk_d0)
# (discards.n(stk_d0)*discards.wt(stk_d0) + landings.n(stk_d0)*landings.wt(stk_d0))/(catch.n(stk_d0)*catch.wt(stk_d0))
# quantSums(discards.n(stk_d0)*discards.wt(stk_d0) + landings.n(stk_d0)*landings.wt(stk_d0))/quantSums(catch.n(stk_d0)*catch.wt(stk_d0))
# quantSums(discards.n(stk_d0)*discards.wt(stk_d0) + landings.n(stk_d0)*landings.wt(stk_d0))/quantSums(catch.n(stk_d0)*catch.wt(stk_d0))

### save
saveRDS(stk_d0, file = "data/OM/stk_d0.rds")

### ------------------------------------------------------------------------ ###
### M scenarios ####
### ------------------------------------------------------------------------ ###

### M -50%
stk_M_low <- stk
m(stk_M_low) <- m(stk_M_low) * (1 - 0.5)
saveRDS(stk_M_low, file = "data/OM/stk_M_low.rds")

### M +50%
stk_M_high <- stk
m(stk_M_high) <- m(stk_M_high) * (1 + 0.5)
saveRDS(stk_M_high, file = "data/OM/stk_M_high.rds")

### M Lorenzen L=Linf
stk_M_Lorenzen_Linf <- stk
M_Lorenzen_Linf <- readRDS("data/OM/M_Lorenzen_Linf.rds")
m(stk_M_Lorenzen_Linf)[] <- M_Lorenzen_Linf
saveRDS(stk_M_Lorenzen_Linf, file = "data/OM/stk_M_Lorenzen_Linf.rds")

### M Gislason
stk_M_Gislason <- stk
M_Gislason <- readRDS("data/OM/M_Gislason.rds")
m(stk_M_Gislason)[] <- M_Gislason
saveRDS(stk_M_Gislason, file = "data/OM/stk_M_Gislason.rds")

### ------------------------------------------------------------------------ ###
### Migration - removed ####
### ------------------------------------------------------------------------ ###

stk_no_migration <- stk

### catch
catch.n(stk_no_migration)[]  <- catch$CN_7e
catch.wt(stk_no_migration)[] <- catch$CW_7e
catch(stk_no_migration)[]    <- catch$CA_7e
landings.n(stk_no_migration)[]  <- catch$LN_7e
landings.wt(stk_no_migration)[] <- catch$LW_7e
landings(stk_no_migration)[]    <- catch$LA_7e
discards.n(stk_no_migration)[]  <- catch$DN_7e
discards.wt(stk_no_migration)[] <- catch$DW_7e
discards.wt(stk_no_migration)[is.na(discards.wt(stk_no_migration))] <- 0
discards(stk_no_migration)[]    <- catch$DA_7e

### then consider 50% discard survival
discards.n(stk_no_migration) <- discards.n(stk_no_migration) * 0.5 ### discard survival
discards(stk_no_migration) <- discards(stk_no_migration) * 0.5
catch.n(stk_no_migration) <- landings.n(stk_no_migration) + discards.n(stk_no_migration) ### update catch
catch(stk_no_migration) <- landings(stk_no_migration) + discards(stk_no_migration) ### 
catch.wt(stk_no_migration) <- (landings.wt(stk_no_migration)*landings.n(stk_no_migration) + discards.wt(stk_no_migration) * discards.n(stk_no_migration))/(landings.n(stk_no_migration) + discards.n(stk_no_migration)) ### adapt catch weights

# (discards.n(stk_no_migration) + landings.n(stk_no_migration))/catch.n(stk_no_migration)
# (discards.n(stk_no_migration)*discards.wt(stk_no_migration) + landings.n(stk_no_migration)*landings.wt(stk_no_migration))/(catch.n(stk_no_migration)*catch.wt(stk_no_migration))
# quantSums(discards.n(stk_no_migration)*discards.wt(stk_no_migration) + landings.n(stk_no_migration)*landings.wt(stk_no_migration))/quantSums(catch.n(stk_no_migration)*catch.wt(stk_no_migration))
# quantSums(discards.n(stk_no_migration)*discards.wt(stk_no_migration) + landings.n(stk_no_migration)*landings.wt(stk_no_migration))/quantSums(catch.n(stk_no_migration)*catch.wt(stk_no_migration))

### save
saveRDS(stk_no_migration, file = "data/OM/stk_no_migration.rds")

