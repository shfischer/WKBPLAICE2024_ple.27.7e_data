### packages
library(icesTAF)
taf.libPaths()
library(tidyr)
library(dplyr)
library(ggplot2)
library(FLCore)
library(foreach)
library(XLConnect)

### load additional functions
source("utilities.R")

if (!exists("verbose")) verbose <- FALSE

### ------------------------------------------------------------------------ ###
### before InterCatch - 1980-2011 ####
### ------------------------------------------------------------------------ ###
### load data from IBPWCFlat2 2015
### - full history of landings, with and without migration data


sheet <- loadWorkbook(paste0("boot/initial/data/historical/ple7e1980_2014 with revised migration.xlsx"))

yrs <- 1980:2014

### total catch - 7e & 7d migration
hist_LA <- lapply(yrs, function(year) {
  try({
    tmp <- readWorksheet(
      sheet, 
      sheet = paste0("TOTINT+migration(", year, ")"), header = FALSE, 
      startRow = 15, endRow = 15, 
      startCol = which(LETTERS == "C"), endCol = which(LETTERS == "D"))
    names(tmp) <- c("7e", "7d")
    tmp$year <- year
    return(tmp)
  })
})
hist_LA <- do.call(rbind, hist_LA)


### catch numbers and weights at age - 7e & 7d migration
hist_LN <- lapply(yrs, function(year) {
  try({
    tmp <- readWorksheet(
      sheet, 
      sheet = paste0("TOTINT+migration(", year, ")"), header = FALSE, 
      startRow = 14, endRow = 29, 
      startCol = which(LETTERS == "N"), endCol = which(LETTERS == "T"),
      drop = 4:5)
    names(tmp) <- c("age", "LN_7e", "LW_7e", "LN_7d", "LW_7d")
    tmp$year <- year
    return(tmp)
  })
})
hist_LN <- do.call(rbind, hist_LN)
hist_LN$age[hist_LN$age == "15+"] <- 15
hist_LN$age <- as.numeric(hist_LN$age)
hist_LN$LN_7e <- hist_LN$LN_7e/1000
hist_LN$LN_7d <- hist_LN$LN_7d/1000

hist_LN$LN_7e[is.na(hist_LN$LN_7e)] <- 0
hist_LN$LN_7d[is.na(hist_LN$LN_7d)] <- 0
hist_LN$LW_7e[is.na(hist_LN$LW_7e)] <- 0
hist_LN$LW_7d[is.na(hist_LN$LW_7d)] <- 0

### combine catch
hist_LN$LN_7de <- hist_LN$LN_7e + hist_LN$LN_7d
### combine weight at age -> weighted average
hist_LN$LW_7de <- with(hist_LN, (LN_7e * LW_7e + LN_7d * LW_7d) / LN_7de)
hist_LN$LW_7de[is.na(hist_LN$LW_7de)] <- 0

### coerce into FLQuants
hist_LA_7e <- hist_LA %>%
  select(year, data = `7e`) %>%
  as.FLQuant(units = "tonnes")
hist_LA_7d <- hist_LA %>%
  select(year, data = `7d`) %>%
  as.FLQuant(units = "tonnes")
hist_LA_7de <- hist_LA_7e + hist_LA_7d
hist_LN_7e <- hist_LN %>%
  select(year, age, data = LN_7e) %>%
  as.FLQuant(units = "1000")
hist_LN_7d <- hist_LN %>%
  select(year, age, data = LN_7d) %>%
  as.FLQuant(units = "1000")
hist_LN_7de <- hist_LN %>%
  select(year, age, data = LN_7de) %>%
  as.FLQuant(units = "1000")
hist_LW_7e <- hist_LN %>%
  select(year, age, data = LW_7e) %>%
  as.FLQuant(units = "kg")
hist_LW_7d <- hist_LN %>%
  select(year, age, data = LW_7d) %>%
  as.FLQuant(units = "kg")
hist_LW_7de <- hist_LN %>%
  select(year, age, data = LW_7de) %>%
  as.FLQuant(units = "kg")

### check SOP
if (isTRUE(verbose)) {
  quantSums(hist_LN_7e * hist_LW_7e)/hist_LA_7e
  summary(quantSums(hist_LN_7e * hist_LW_7e)/hist_LA_7e)
  quantSums(hist_LN_7d * hist_LW_7d)/hist_LA_7d
  summary(quantSums(hist_LN_7d * hist_LW_7d)/hist_LA_7d)
  quantSums(hist_LN_7de * hist_LW_7de)/hist_LA_7de
  summary(quantSums(hist_LN_7de * hist_LW_7de)/hist_LA_7de)
  ### 7e & 7d landings
  hist_LA_7e + hist_LA_7d
  ### -> matches total landings in assessment/advice sheets
  ###    (rounded to nearest tonne, until 2013)
}

### use 10+ plusgroup
hist_LN_7e_pg10  <- setPlusGroupSum(number = hist_LN_7e, plusgroup = 10)
hist_LN_7d_pg10  <- setPlusGroupSum(number = hist_LN_7d, plusgroup = 10)
hist_LN_7de_pg10 <- setPlusGroupSum(number = hist_LN_7de, plusgroup = 10)
hist_LW_7e_pg10  <- setPlusGroupWeca(number = hist_LN_7e, weight = hist_LW_7e, 
                                     plusgroup = 10)
hist_LW_7d_pg10  <- setPlusGroupWeca(weight = hist_LW_7d, number = hist_LN_7d, 
                                     plusgroup = 10)
hist_LW_7de_pg10 <- setPlusGroupWeca(weight = hist_LW_7d, number = hist_LN_7d, 
                                     plusgroup = 10)

### combine data
hist_catch <- FLQuants(
  hist_LA_7e = hist_LA_7e, hist_LA_7d = hist_LA_7d, hist_LA_7de = hist_LA_7de,
  hist_LN_7e = hist_LN_7e, hist_LN_7d = hist_LN_7d, hist_LN_7de = hist_LN_7de,
  hist_LW_7e = hist_LW_7e, hist_LW_7d = hist_LW_7d, hist_LW_7de = hist_LW_7de,
  hist_LA_7e_pg10 = hist_LA_7e, hist_LA_7d_pg10 = hist_LA_7d, 
  hist_LA_7de_pg10 = hist_LA_7de, ### total catch - same as without plusgroup
  hist_LN_7e_pg10 = hist_LN_7e_pg10, hist_LN_7d_pg10 = hist_LN_7d_pg10, 
  hist_LN_7de_pg10 = hist_LN_7de_pg10,
  hist_LW_7e_pg10 = hist_LW_7e_pg10, hist_LW_7d_pg10 = hist_LW_7d_pg10, 
  hist_LW_7de_pg10 = hist_LW_7de_pg10,
)

### save data
dir.create("data/catch/")
saveRDS(hist_catch, file = "data/catch/catch_pre_InterCatch.rds")


# ### plots weights at age
# hist_LN %>%
#   ggplot(aes(x = year, y = LW_7e, colour = as.factor(age))) +
#   geom_line() +
#   theme_bw(base_size = 8)
# 
# 
# 
# 
# 
# ### save data
# dir.create("data/catch/")
# saveRDS(FLQuants(
#   LA_7e = LA_7e, LA_7d = LA_7d,
#   LN_7e = LN_7e, LN_7d = LN_7d,
#   LW_7e = LW_7e, LW_7d = LW_7d
# ), file = "data/catch/catch_pre_InterCatch.rds")
# 
# 




# 
# 
# ### weights at age history from InterCatch
# table2 <- read.csv("boot/data/InterCatch/table2_hist.txt")
# 
# 
# table2 %>%
#   group_by(Year, Season) %>%
#   mutate(catch = CANUM * WECA) %>%
#   summarise(catch = sum(catch)) %>%
#   print(n = Inf)
# 
# 
# ### replicate weca.txt
# weca <- table2 %>%
#   #filter(Year == 2022) %>%
#   group_by(Year, AgeOrLength) %>%
#   summarise(WECA = weighted.mean(x = WECA, w = CANUM, na.rm = TRUE))
# 
# 
# ### for Q1 only
# weca_Q1 <- table2 %>%
#   filter(Season == 1) %>%
#   group_by(Year, AgeOrLength) %>%
#   summarise(WECA = weighted.mean(x = WECA, w = CANUM, na.rm = TRUE))
# 
# bind_rows(weca %>% mutate(Q = "all"),
#           weca_Q1 %>% mutate(Q = "Q1")) %>%
#   ggplot(aes(x = Year, y = WECA, colour = as.factor(AgeOrLength),
#              linetype = Q)) +
#   geom_line() +
#   facet_wrap(~ AgeOrLength) +
#   theme_bw(base_size = 8)
# 

### ------------------------------------------------------------------------ ###
### InterCatch history 2012-2023 ####
### ------------------------------------------------------------------------ ###
### age data from InterCatch

IC_yrs <- 2012:2023
lanum <- danum <- canum <- FLQuant(NA, dimnames = list(age = 0:15, year = IC_yrs))
wela <- weda <- weca <- FLQuant(NA, dimnames = list(age = 0:15, year = IC_yrs))
laton <- daton <- caton <- FLQuant(NA, dimnames = list(age = "all", year = IC_yrs))

### load data from InterCatch files
for(catch in c("catch", "discards", "landings")) {
  for(year in IC_yrs) {
    #browser()
    ### numbers
    canum_tmp <- readFLQuant(
      input_file = paste0("boot/initial/data/InterCatch/",
                          "full_history/", year, "/", catch, "/canum.txt"))
    canum_name <- switch(catch,
                         "catch" = "canum",
                         "discards" = "danum",
                         "landings" = "lanum")
    canum_tmp_new <- get(canum_name)
    canum_tmp_new[dimnames(canum_tmp)$age, dimnames(canum_tmp)$year] <- canum_tmp
    assign(canum_name, canum_tmp_new)
    
    ### tonnage
    caton_tmp <- readFLQuant(
      input_file = paste0("boot/initial/data/InterCatch/",
                          "full_history/", year, "/", catch, "/caton.txt"))
    caton_name <- switch(catch,
                         "catch" = "caton",
                         "discards" = "daton",
                         "landings" = "laton")
    caton_tmp_new <- get(caton_name)
    caton_tmp_new[dimnames(caton_tmp)$age, dimnames(caton_tmp)$year] <- caton_tmp
    assign(caton_name, caton_tmp_new)
    
    ### weights
    weca_tmp <- readFLQuant(
      input_file = paste0("boot/initial/data/InterCatch/",
                          "full_history/", year, "/", catch, "/weca.txt"))
    weca_name <- switch(catch,
                        "catch" = "weca",
                        "discards" = "weda",
                        "landings" = "wela")
    weca_tmp_new <- get(weca_name)
    weca_tmp_new[dimnames(weca_tmp)$age, dimnames(weca_tmp)$year] <- weca_tmp
    assign(weca_name, weca_tmp_new)
    
  }
}

if (isTRUE(verbose))
  as.data.frame(wela) %>%
  ggplot(aes(x = year, y = data, colour = as.factor(age))) +
  geom_line() +
  geom_point() +
  theme_bw(base_size = 8)

### combine
IC_catch <- FLQuants(
  LA = laton, DA = daton, CA = caton,
  LN = lanum, DN = danum, CN = canum,
  LW = wela, DW = weda, CW = weca
)

### save data
saveRDS(IC_catch, file = "data/catch/catch_InterCatch.rds")

### ------------------------------------------------------------------------ ###
### migration data ####
### ------------------------------------------------------------------------ ###

### load data (from WGCSE 2024)
load("boot/initial/data/InterCatch/migration_data.RData")
# "migration_laton_hist" "migration_daton_hist" "migration_caton_hist" 
# "migration_lanum_hist" "migration_danum_hist" "migration_canum_hist" 
# "migration_wela_hist"  "migration_weda_hist" "migration_weca_hist" 

if (isTRUE(verbose)) {
  ### landings tonnage 
  migration_laton_hist[, ac(1980:2014)] / hist_LA_7d
  ### -> identical
  
  ### landings numbers at age
  migration_lanum_hist[ac(1:10), ac(1980:2014)] / hist_LN_7d_pg10[ac(1:10), ]
  all.equal(migration_lanum_hist[ac(1:10), ac(1980:2011)], 
            hist_LN_7d_pg10[ac(1:10), ac(1980:2011)])
  ### -> identical until 2011
  ### -> 2012-2013: negligible changes (difference < 0.001)
  
  ### landings weight at age
  migration_wela_hist[ac(1:10), ac(1980:2014)] / hist_LW_7d_pg10[ac(1:10), ]
  all.equal(migration_wela_hist[ac(1:10), ac(1980:2009)], 
            hist_LW_7d_pg10[ac(1:10), ac(1980:2009)])
  ### identical until 2011
  ### -> 2010-2014: negligible changes
  
  
  
}

### ------------------------------------------------------------------------ ###
### combine historical and recent data ####
### ------------------------------------------------------------------------ ###

### templates
### - 7e & 7d
LA <- DA <- CA <- FLQuant(NA, dimnames = list(age = "all", year = 1980:2023))
LN <- DN <- CN <- FLQuant(NA, dimnames = list(age = 0:15, year = 1980:2023))
LW <- DW <- CW <- FLQuant(NA, dimnames = list(age = 0:15, year = 1980:2023))
### - 7e
LA_7e <- DA_7e <- CA_7e <- LA
LN_7e <- DN_7e <- CN_7e <- LN
LW_7e <- DW_7e <- CW_7e <- LW
### - 7d
LA_7d <- DA_7d <- CA_7d <- LA
LN_7d <- DN_7d <- CN_7d <- LN
LW_7d <- DW_7d <- CW_7d <- LW

### 7e data
### 2012-2023 -> InterCatch
### 1980-2011 -> historical data
LA_7e[, ac(2012:2023)] <- IC_catch$LA
LA_7e[, ac(1980:2011)] <- hist_catch$hist_LA_7e[, ac(1980:2011)]
LN_7e[, ac(2012:2023)] <- IC_catch$LN/1000
LN_7e[, ac(1980:2011)] <- hist_catch$hist_LN_7e[, ac(1980:2011)]
LW_7e[, ac(2012:2023)] <- IC_catch$LW/1000
LW_7e[, ac(1980:2011)] <- hist_catch$hist_LW_7e[, ac(1980:2011)]

DA_7e[, ac(2012:2023)] <- IC_catch$DA
DA_7e[, ac(1980:2011)] <- 0 ### unknown
DN_7e[, ac(2012:2023)] <- IC_catch$DN/1000
DN_7e[is.na(DN_7e)]    <- 0
DW_7e[, ac(2012:2023)] <- IC_catch$DW/1000
DW_7e[is.na(DW_7e)] <- 0

CA_7e[, ac(2012:2023)] <- IC_catch$CA
CA_7e[, ac(1980:2011)] <- hist_catch$hist_LA_7e[, ac(1980:2011)] ### landings
CN_7e[, ac(2012:2023)] <- IC_catch$CN/1000
CN_7e[, ac(1980:2011)] <- hist_catch$hist_LN_7e[, ac(1980:2011)] ### landings
CN_7e[is.na(CN_7e)] <- 0
CW_7e[, ac(2012:2023)] <- IC_catch$CW/1000
CW_7e[, ac(1980:2011)] <- hist_catch$hist_LW_7e[, ac(1980:2011)] ### landings
CW_7e[is.na(CW_7e)] <- 0

### check SOP
if (isTRUE(verbose)) {
  quantSums(CN_7e * CW_7e)/CA_7e
  quantSums(LN_7e * LW_7e)/LA_7e
  quantSums(DN_7e * DW_7e)/DA_7e
}

### 7d data
### use migration data set
LA_7d[] <- migration_laton_hist
DA_7d[] <- migration_daton_hist
CA_7d[] <- migration_caton_hist

LN_7d[ac(1:10), ] <- migration_lanum_hist
LN_7d[is.na(LN_7d)] <- 0
DN_7d[ac(1:10), ] <- migration_danum_hist
DN_7d[is.na(DN_7d)] <- 0
CN_7d[ac(1:10), ] <- migration_canum_hist
CN_7d[is.na(CN_7d)] <- 0

LW_7d[ac(1:10), ] <- migration_wela_hist
LW_7d[is.na(LW_7d)] <- 0
DW_7d[ac(1:10), ] <- migration_weda_hist
DW_7d[is.na(DW_7d)] <- 0
CW_7d[ac(1:10), ] <- migration_weca_hist
CW_7d[is.na(CW_7d)] <- 0

### check SOP
if (isTRUE(verbose)) {
  quantSums(CN_7d * CW_7d)/CA_7d
  quantSums(LN_7d * LW_7d)/LA_7d
  quantSums(DN_7d * DW_7d)/DA_7d
}


