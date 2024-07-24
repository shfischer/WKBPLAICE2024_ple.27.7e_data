### ------------------------------------------------------------------------ ###
### catch data ####
### ------------------------------------------------------------------------ ###

### packages
library(icesTAF)
taf.libPaths()
library(icesAdvice)
library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)
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
#hist_catch <- readRDS("data/catch/catch_pre_InterCatch.rds")

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
#IC_catch <- readRDS("data/catch/catch_InterCatch.rds")

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
LN_7e[is.na(LN_7e)] <- 0
LW_7e[, ac(2012:2023)] <- IC_catch$LW/1000
LW_7e[, ac(1980:2011)] <- hist_catch$hist_LW_7e[, ac(1980:2011)]
LW_7e[is.na(LW_7e)] <- 0

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

### combine 7e & 7d catch
LA <- LA_7e + LA_7d
LN <- LN_7e + LN_7d
LW <- ((LW_7e * LN_7e) + (LW_7d * LN_7d))/(LN_7e + LN_7d)
LW[is.na(LW)] <- 0

CA <- CA_7e + CA_7d
CN <- CN_7e + CN_7d
CW <- ((CW_7e * CN_7e) + (CW_7d * CN_7d))/(CN_7e + CN_7d)
CW[is.na(CW)] <- 0

DA <- DA_7e + DA_7d
DN <- DN_7e + DN_7d
DW <- ((DW_7e * DN_7e) + (DW_7d * DN_7d))/(DN_7e + DN_7d)
DW[is.na(DW)] <- 0

### check SOP
if (isTRUE(verbose)) {
  quantSums(CN * CW)/CA
  quantSums(LN * LW)/LA
  quantSums(DN * DW)/DA
}

### ------------------------------------------------------------------------ ###
### guestimate discards 2002-2011 ####
### ------------------------------------------------------------------------ ###
### use approach developed at WGCSE 2020
### -> 7e discard rate (migration already includes discards)
### 
### take percent estimates from WKFLAT 10 WD
### Figure 19, annual discards by weight from UK
### data for 2001-2009 exists
### for 2010-2011, use mean of before (2009) and after (2012 from InterCatch)
d_yrs <- 2002:2011
# d_rate_2012 <- (DA_7e/CA_7e)[, ac(2012)]
disc_rates <- data.frame(year = d_yrs,
                         rate = c(0.053, 0.047, 0.063, 0.087, 0.075, 0.097, 
                                  0.127, 0.074,
                                  rep(mean(c(0.074, 0.2176186)), 2)))
### calculate total discards per year
DA_7e[, ac(d_yrs)] <- LA_7e[, ac(d_yrs)] * 
  (1/(1 - disc_rates$rate) - 1)
### set discard weights at age: 5-year average
### but remove 0s (missing) for mean
DW_7e_tmp <- DW_7e
DW_7e_tmp[DW_7e_tmp == 0] <- NA
DW_7e[, ac(d_yrs)] <- 
  yearMeans(DW_7e_tmp[, ac(2012:2016)])
### age structure: 5 year average
d_n <- DN_7e[, ac(2012:2016)]
d_n <- d_n / rep(c(apply(d_n, 2, max)), each = dim(d_n)[1])
d_n <- yearMeans(d_n)
### solve for numbers at age
fn <- function(mult, yr) {
  res <- quantSums(d_n * mult * DW_7e[, ac(yr)])
  res <- (res - DA_7e[, ac(yr)])^2
  return(c(res))
}
for (yr in d_yrs) {
  mult_res <- optimize(f = fn, yr = yr, interval = c(0, 1e+9))[["minimum"]]
  DN_7e[, ac(yr)] <- d_n * mult_res
}

### update total catch - 7e
CA_7e[, ac(d_yrs)] <- LA_7e[, ac(d_yrs)] + DA_7e[, ac(d_yrs)]
CN_7e[, ac(d_yrs)] <- LN_7e[, ac(d_yrs)] + DN_7e[, ac(d_yrs)]
CW_7e[, ac(d_yrs)] <- ((LN_7e[, ac(d_yrs)] * LW_7e[, ac(d_yrs)]) + 
                         (DN_7e[, ac(d_yrs)] * DW_7e[, ac(d_yrs)]))/
  (LN_7e[, ac(d_yrs)] + DN_7e[, ac(d_yrs)])

### check SOP
if (isTRUE(verbose)) {
  quantSums(DN_7e * DW_7e)[, ac(d_yrs)]/DA_7e[, ac(d_yrs)]
  quantSums(CN_7e * CW_7e)[, ac(d_yrs)]/CA_7e[, ac(d_yrs)]
}

### update catch (and discards) - 7e & 7d combined
CA[, ac(d_yrs)] <- CA_7e[, ac(d_yrs)] + CA_7d[, ac(d_yrs)]
CN[, ac(d_yrs)] <- CN_7e[, ac(d_yrs)] + CN_7d[, ac(d_yrs)]
CW[, ac(d_yrs)] <- ((CN_7e[, ac(d_yrs)] * CW_7e[, ac(d_yrs)]) + 
                         (CN_7d[, ac(d_yrs)] * CW_7d[, ac(d_yrs)]))/
  (CN_7e[, ac(d_yrs)] + CN_7d[, ac(d_yrs)])
DA[, ac(d_yrs)] <- DA_7e[, ac(d_yrs)] + DA_7d[, ac(d_yrs)]
DN[, ac(d_yrs)] <- DN_7e[, ac(d_yrs)] + DN_7d[, ac(d_yrs)]
DW[, ac(d_yrs)] <- ((DN_7e[, ac(d_yrs)] * DW_7e[, ac(d_yrs)]) + 
                      (DN_7d[, ac(d_yrs)] * DW_7d[, ac(d_yrs)]))/
  (DN_7e[, ac(d_yrs)] + DN_7d[, ac(d_yrs)])

### check SOP
if (isTRUE(verbose)) {
  quantSums(DN_7e * DW_7e)[, ac(d_yrs)]/DA_7e[, ac(d_yrs)]
  quantSums(CN_7e * CW_7e)[, ac(d_yrs)]/CA_7e[, ac(d_yrs)]
  quantSums(DN * DW)[, ac(d_yrs)]/DA[, ac(d_yrs)]
  quantSums(CN * CW)[, ac(d_yrs)]/CA[, ac(d_yrs)]
}

#plot(CA)

### combine data sets
catch <- FLQuants(
  CA = CA, LA = LA, DA = DA,
  CN = CN, LN = LN, DN = DN,
  CW = CW, LW = LW, DW = DW,
  CA_7e = CA_7e, LA_7e = LA_7e, DA_7e = DA_7e,
  CN_7e = CN_7e, LN_7e = LN_7e, DN_7e = DN_7e,
  CW_7e = CW_7e, LW_7e = LW_7e, DW_7e = DW_7e,
  CA_7d = CA_7d, LA_7d = LA_7d, DA_7d = DA_7d,
  CN_7d = CN_7d, LN_7d = LN_7d, DN_7d = DN_7d,
  CW_7d = CW_7d, LW_7d = LW_7d, DW_7d = DW_7d
)
saveRDS(catch, file = "data/catch/catch.rds")
#catch <- readRDS("data/catch/catch.rds")

### ------------------------------------------------------------------------ ###
### plot data ####
### ------------------------------------------------------------------------ ###
dir.create("data/catch/plots")

### total catch
p <- as.data.frame(catch[c("LA", "DA")]) %>%
  mutate(qname = factor(qname, levels = c("DA", "LA"), 
                        labels = c("Discards", "Landings"))) %>%
  ggplot(aes(x = year, y = data, fill = qname)) +
  geom_col() +
  scale_fill_discrete("") + 
  labs(x = "Year", y = "Catch (tonnes)") + 
  theme_bw(base_size = 8) +
  theme(legend.key.width = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave(file = paste0("data/catch/plots/catch_stock.png"),
       width = 15, height = 8,  plot = p,
       units = "cm", dpi = 300, type = "cairo")
### with discard rate
p2 <- as.data.frame(catch$DA/catch$CA * 100) %>%
  mutate(data = ifelse(data == 0, NA, data)) %>%
  ggplot(aes(x = year, y = data, colour = "Discard rate")) +
  geom_line() +
  ylim(c(0, NA)) +
  scale_colour_manual("", values = "black") +
  labs(x = "Year", y = "Discard rate (%)") + 
  theme_bw(base_size = 8)
p_ <- (p + theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())) / p2 +
  plot_layout(heights = c(1, 0.5))
if (isTRUE(verbose)) p_
ggsave(file = paste0("data/catch/plots/catch_stock_discard_rate.png"),
       width = 15, height = 8,  plot = p_,
       units = "cm", dpi = 300, type = "cairo")
ggsave(file = paste0("data/catch/plots/catch_stock_discard_rate.pdf"),
       width = 16, height = 8,  plot = p_,
       units = "cm")

### total catch - by area & total - landings and discards
p <- as.data.frame(catch[c("LA", "DA", "LA_7e", "DA_7e", "LA_7d", "DA_7d")]) %>%
  mutate(catch = ifelse(grepl(x = qname, pattern = "^LA*"), 
                        "Landings", "Discards")) %>%
  mutate(catch = factor(catch, levels = c("Discards", "Landings"))) %>%
  mutate(area = case_match(qname,
                           c("LA", "DA") ~ "Stock",
                           c("LA_7e", "DA_7e") ~ "Division 7.e",
                           c("LA_7d", "DA_7d") ~ "Division 7.d (migration)"
  )) %>%
  mutate(area = factor(area, levels = c("Stock", "Division 7.e", 
                                        "Division 7.d (migration)"))) %>%
  ggplot(aes(x = year, y = data, fill = catch)) +
  geom_col() +
  scale_fill_discrete("") + 
  #facet_grid(area ~ 1, space = "free", scales = "free_y") + 
  facet_wrap(~ area, ncol = 1) + 
  labs(x = "Year", y = "Catch (tonnes)") + 
  theme_bw(base_size = 8) +
  theme(legend.key.width = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave(file = paste0("data/catch/plots/catch_stock_area.png"),
       width = 15, height = 8,  plot = p,
       units = "cm", dpi = 300, type = "cairo")
ggsave(file = paste0("data/catch/plots/catch_stock_area.pdf"),
       width = 16, height = 8,  plot = p,
       units = "cm")

### age structure - numbers
p <- as.data.frame(catch[c("LN", "DN")]) %>%
  mutate(qname = factor(qname, levels = c("DN", "LN"), 
                        labels = c("Discards", "Landings"))) %>%
  ggplot(aes(x = age, y = data, fill = qname)) +
  geom_col() +
  scale_fill_discrete("") + 
  facet_wrap(~ year) + 
  labs(x = "Age (years)", y = "Catch numbers (thousands)") + 
  theme_bw(base_size = 8) +
  theme(legend.key.width = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave(file = paste0("data/catch/plots/catch_age.png"),
       width = 25, height = 15,  plot = p,
       units = "cm", dpi = 300, type = "cairo")
ggsave(file = paste0("data/catch/plots/catch_age.pdf"),
       width = 16, height = 12,  plot = p,
       units = "cm")
### age structure - numbers - 2023 only
p <- as.data.frame(catch[c("LN", "DN")]) %>%
  mutate(qname = factor(qname, levels = c("DN", "LN"), 
                        labels = c("Discards", "Landings"))) %>%
  filter(year == 2023) %>%
  ggplot(aes(x = age, y = data, fill = qname)) +
  geom_col() +
  scale_fill_discrete("") + 
  facet_wrap(~ year) + 
  labs(x = "Age (years)", y = "Catch numbers (thousands)") + 
  theme_bw(base_size = 8) +
  theme(legend.key.width = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave(file = paste0("data/catch/plots/catch_age_2023.png"),
       width = 10, height = 6,  plot = p,
       units = "cm", dpi = 300, type = "cairo")
### age structure - biomass
p <- as.data.frame(FLQuants(LN = catch$LN * catch$LW,
                            DN = catch$DN * catch$DW)) %>%
  mutate(qname = factor(qname, levels = c("DN", "LN"), 
                        labels = c("Discards", "Landings"))) %>%
  ggplot(aes(x = age, y = data, fill = qname)) +
  geom_col() +
  scale_fill_discrete("") + 
  facet_wrap(~ year) + 
  labs(x = "Age (years)", y = "Catch biomass (tonnes)") + 
  theme_bw(base_size = 8) +
  theme(legend.key.width = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave(file = paste0("data/catch/plots/catch_age_biomass.png"),
       width = 25, height = 15,  plot = p,
       units = "cm", dpi = 300, type = "cairo")
ggsave(file = paste0("data/catch/plots/catch_age_biomass.pdf"),
       width = 16, height = 12,  plot = p, units = "cm")
### age structure - standardised numbers as bubbles
p <- as.data.frame(catch$CN) %>%
  group_by(age) %>%
  mutate(data = data/mean(data) - 1) %>%
  mutate(sign = ifelse(data <= 0, "negative", "positive")) %>%
  mutate(data_abs = abs(data)) %>%
  ggplot(aes(x = year, y = age, size = data_abs, fill = sign)) +
  geom_point(shape = 21) + 
  scale_size("Difference\nto mean", range = c(0, 6)) + 
  #scale_y_continuous(breaks = seq(from = 2, to = 12, by = 2)) + 
  scale_fill_discrete("") + 
  theme_bw(base_size = 8) +
  labs(x = "Year", y = "Age (years)")
if (isTRUE(verbose)) p
ggsave(file = paste0("data/catch/plots/catch_age_numbers_bubbles.png"),
       width = 15, height = 8,  plot = p,
       units = "cm", dpi = 300, type = "cairo")
ggsave(file = paste0("data/catch/plots/catch_age_numbers_bubbles.pdf"),
       width = 16, height = 8,  plot = p,
       units = "cm")

### catch weights at age
cols <- scales::hue_pal()(length(0:15))
cols <- cols[c(seq(from = 1, to = length(cols), by = 4),
               seq(from = 2, to = length(cols), by = 4),
               seq(from = 3, to = length(cols), by = 4),
               seq(from = 4, to = length(cols), by = 4))]
p <- as.data.frame(catch[c("CW", "LW", "DW")]) %>%
  mutate(data = ifelse(data == 0, NA, data)) %>%
  mutate(qname = factor(qname, levels = c("CW", "LW", "DW"),
                        labels = c("Catch", "Landings", "Discards"))) %>%
  mutate(age = factor(age, levels = 15:0,
                      labels = c("15+", 14:0))) %>%
  ggplot(aes(x = year, y = data, colour = age)) +
  geom_line(linewidth = 0.3) +
  geom_point(size = 0.3) + 
  scale_colour_manual("Age (years)", values = cols) + 
  facet_wrap(~ qname, ncol = 1) + 
  labs(x = "Year", y = "Weight at age (kg)") + 
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave(file = paste0("data/catch/plots/catch_weights_raw.png"),
       width = 15, height = 8,  plot = p,
       units = "cm", dpi = 300, type = "cairo")
ggsave(file = paste0("data/catch/plots/catch_weights_raw.pdf"),
       width = 16, height = 8,  plot = p,
       units = "cm")

### catch weights at age - growth curve by year
p <- as.data.frame(catch[c("CW", "LW", "DW")]) %>%
  mutate(data = ifelse(data == 0, NA, data)) %>%
  mutate(qname = factor(qname, levels = c("DW", "LW", "CW"),
                        labels = c("Discards", "Landings", "Catch"))) %>%
  ggplot(aes(x = age, y = data, colour = qname)) +
  geom_line(linewidth = 0.3) +
  geom_point(size = 0.3) +
  scale_colour_discrete("") + 
  facet_wrap(~ year) + 
  labs(x = "Age (years)", y = "Weight at age (kg)") + 
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave(file = paste0("data/catch/plots/catch_weights_raw_growth.png"),
       width = 25, height = 15,  plot = p,
       units = "cm", dpi = 300, type = "cairo")
ggsave(file = paste0("data/catch/plots/catch_weights_raw_growth.pdf"),
       width = 16, height = 12,  plot = p,
       units = "cm")

### ------------------------------------------------------------------------ ###
### fit model to weights at age ####
### ------------------------------------------------------------------------ ###

### von Bertalanffy equation for weight
# vB_weight <- function(W_inf, k, t, t_0 = 0) {
#   W_inf * (1 - exp(-k * (t - t_0)))
# }
# df <- as.data.frame(catch$CW_7e[, ac(2023)]) %>%
#   select(age, weight = data) %>%
#   filter(weight > 0)
# nls(weight ~ vB_weight(W_inf, k, t = age, t_0),
#     data = df,
#     weight = df$weight,
#     start = list(W_inf = 2, k = 0.1, t_0 = 0), 
#     control = list(maxiter = 500, minFactor = 1/1e+6, warnOnly = TRUE))
### -> doesn't work reliably, 
###    unrealistic values, e.g. assymptotic weight > 100kg

### follow previous WGCSE approach
### -> fit second degree polynomial model to weights at age
### assume catch weights are mid-year weights, i.e. age + 0.5

ages <- 0:15
years <- 1980:2023
names(years) <- years

### fit model
fits <- lapply(years, function(year) {
  tmp_df <- as.data.frame(catch$CW[, ac(year)]) %>%
    select(age, weight = data) %>%
    mutate(age = age + 0.5) %>%
    mutate(weight = ifelse(weight == 0, NA, weight))
  tmp_fit <- lm(weight ~ poly(age, 2, raw = TRUE), data = tmp_df)
  return(tmp_fit)
})

### predict values for stock and catch weight
fits_data <- lapply(years, function(year) {
  data.frame(
    year = year,
    age = ages,
    stock_weight = predict(fits[[ac(year)]], 
                           newdata = data.frame(age = ages)),
    catch_weight = predict(fits[[ac(year)]], 
                           newdata = data.frame(age = ages + 0.5))
    )
})
fits_data <- fits_data %>% 
  bind_rows() %>%
  mutate(stock_weight = ifelse(stock_weight <= 0, NA, stock_weight),
         catch_weight = ifelse(catch_weight <= 0, NA, catch_weight)) 
df <- fits_data %>%
  full_join(as.data.frame(catch$CW) %>%
              select(age, year, data_weight = data) %>%
              mutate(data_weight = ifelse(data_weight == 0, NA, data_weight))) %>%
  pivot_longer(c(stock_weight, catch_weight, data_weight)) %>%
  mutate(age = ifelse(name %in% c("catch_weight", "data_weight"),
                      age + 0.5, age)) %>%
  mutate(name = factor(name, 
                       levels = c("stock_weight", "catch_weight", "data_weight"),
                      labels = c("stock", "catch", "data")))

### create data.frame with small steps for plotting fit
df_fit <- lapply(years, function(year) {
  data.frame(
    year = year,
    age = seq(-5, 20, 0.1),
    weight = predict(fits[[ac(year)]], 
                           newdata = data.frame(age = seq(-5, 20, 0.1)))
  )
})
df_fit <- df_fit %>%
  bind_rows() %>%
  mutate(weight = ifelse(weight <= 0, NA, weight))

### plot data, fit, and predicted values
p <- df %>%
  ggplot() +
  geom_line(data = df_fit, aes(x = age, y = weight), 
            colour = "black", size = 0.3) + 
  geom_point(aes(x = age, y = value, colour = name, shape = name), 
             size = 0.4) +
  scale_shape_manual("", values = c(stock = 1, catch = 1, data = 4)) +
  scale_colour_discrete("") + 
  facet_wrap(~ year) + 
  labs(x = "Age (years)", y = "Weight at age (kg)") +
  coord_cartesian(xlim = c(0, 15), ylim = c(0, 2)) + 
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave(file = paste0("data/catch/plots/catch_weights_fit.png"),
       width = 25, height = 15,  plot = p,
       units = "cm", dpi = 300, type = "cairo")
ggsave(file = paste0("data/catch/plots/catch_weights_fit.pdf"),
       width = 16, height = 12,  plot = p,
       units = "cm")
### plot data, fit, and predicted values - example year
p <- df %>% filter(year == 2018) %>%
  ggplot() +
  geom_line(data = df_fit %>% filter(year == 2018), 
            aes(x = age, y = weight), 
            colour = "black", size = 0.3) + 
  geom_point(aes(x = age, y = value, colour = name, shape = name), 
             size = 0.6) +
  scale_shape_manual("", values = c(stock = 1, catch = 1, data = 4)) +
  scale_colour_discrete("") + 
  facet_wrap(~ year) + 
  labs(x = "Age (years)", y = "Weight at age (kg)") +
  coord_cartesian(xlim = c(0, 15), ylim = c(0, 2)) + 
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave(file = paste0("data/catch/plots/catch_weights_fit_2018.png"),
       width = 15, height = 8,  plot = p,
       units = "cm", dpi = 300, type = "cairo")
ggsave(file = paste0("data/catch/plots/catch_weights_fit_2018.pdf"),
       width = 10, height = 6,  plot = p,
       units = "cm")

### save
saveRDS(fits_data, file = "data/catch/catch_weights_fitted.rds")

### ------------------------------------------------------------------------ ###
### InterCatch Q1 weights ####
### ------------------------------------------------------------------------ ###
### exploration for stock weights at age -> not used

table2 <- read.csv("boot/initial/data/InterCatch/table2_hist.txt")
WECA_Q1 <- table2 %>%
  filter(Season == 1) %>%
  select(year = Year, age = AgeOrLength, WECA, CANUM) %>%
  mutate(age = ifelse(age < 15, age, 15)) %>% ### plusgroup 15+
  group_by(year, age) %>%
  summarise(WECA = weighted.mean(x = WECA, w = CANUM, na.rm = TRUE))
WECA_Qall <- table2 %>%
  select(year = Year, age = AgeOrLength, WECA, CANUM) %>%
  mutate(age = ifelse(age < 15, age, 15)) %>% ### plusgroup 15+
  group_by(year, age) %>%
  summarise(WECA = weighted.mean(x = WECA, w = CANUM, na.rm = TRUE))

p <- full_join(WECA_Q1 %>% rename("Q1" = WECA),
          WECA_Qall %>% rename("all" = WECA)) %>%
  pivot_longer(c("Q1", "all")) %>%
  mutate(value = value/1000) %>%
  ggplot(aes(x = age, y = value, colour = name)) +
  geom_line(size = 0.4) +
  geom_point(size = 0.3) +
  scale_colour_discrete("Data") +
  facet_wrap(~ year) + 
  labs(x = "Age (years)", y = "Weight at age (kg)") +
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave(file = paste0("data/catch/plots/IC_weights_Q1_comparison.png"),
       width = 25, height = 15,  plot = p,
       units = "cm", dpi = 300, type = "cairo")
ggsave(file = paste0("data/catch/plots/IC_weights_Q1_comparison.pdf"),
       width = 16, height = 8,  plot = p,
       units = "cm")

### ------------------------------------------------------------------------ ###
### stock weights ####
### ------------------------------------------------------------------------ ###
### use fitted values at beginning of year

### for 1980-2011 use fitted values
SW <- catch$CW %=% NA_real_
SW[, ac(1980:2023)] <- as.FLQuant(fits_data %>%
                                    select(year, age, data = stock_weight) %>%
                                    filter(year %in% 1980:2023))
SW[is.na(SW)] <- 0

saveRDS(SW, file = "data/catch/stock_weights.rds")

### ------------------------------------------------------------------------ ###
### prepare data for stock assessment ####
### ------------------------------------------------------------------------ ###
### plusgroup: 10+
###  - few fish caught above (commercial catches and both surveys)
###  - migration data (from 7.d) only available until age 10+
### minimum age: 2
###  - very few fish at age 1 caught in both surveys, 0s in time series
###  - negligible catches at age 1
###  -> mesh size of fishery and survey similar - beam trawls with ~80mm

catch_2_10 <- catch@.Data ### workaround to allow replacing multiple elements...
names(catch_2_10) <- names(catch)

### catch numbers
names_N <- c("CN", "LN", "DN", "CN_7e", "LN_7e", "DN_7e", "CN_7d", "LN_7d", 
             "DN_7d")
catch_2_10[names_N] <- lapply(names_N, function(x) {#browser()
  ### plusgroup
  tmp <- setPlusGroupSum(number = catch_2_10[[x]], plusgroup = 10)
  ### remove ages < 2
  tmp <- tmp[ac(2:10), ]
  ### replace NAs with 0
  tmp[is.na(tmp)] <- 0
  return(tmp)
})

### catch weights
names_W <- c("CW", "LW", "DW", "CW_7e", "LW_7e", "DW_7e", "CW_7d", "LW_7d", 
             "DW_7d")
catch_2_10[names_W] <- lapply(seq_along(names_W), function(x) {#browser()
  ### plusgroup
  tmp <- setPlusGroupWeca(number = catch_2_10[[names_N[[x]]]], 
                          weight = catch_2_10[[names_W[[x]]]], plusgroup = 10)
  ### remove ages < 2
  tmp <- tmp[ac(2:10), ]
  ### replace NAs with 0
  tmp[is.na(tmp)] <- 0
  return(tmp)
})

### stock weights
### use catch numbers for weighted average above plusgroup
SW_2_10 <- setPlusGroupWeca(number = catch$CN, weight = SW, plusgroup = 10)
SW_2_10 <- SW_2_10[ac(2:10), ]
SW_2_10[is.na(SW_2_10)] <- 0

### save
saveRDS(catch_2_10, file = "data/catch/catch_pg.rds")
saveRDS(SW_2_10, file = "data/catch/stock_weights_pg.rds")

### ------------------------------------------------------------------------ ###
### Sum of products (SOP) correction ####
### ------------------------------------------------------------------------ ###
### as before at WGCSE -> "correct" weights

n_ages <- dim(catch_2_10$CN)[[1]] ### number of ages
catch_SOP <- catch_2_10

### calculate SOP error
SOP_CA <- quantSums(catch_SOP$CN * catch_SOP$CW)/catch_SOP$CA
SOP_LA <- quantSums(catch_SOP$LN * catch_SOP$LW)/catch_SOP$LA
SOP_DA <- quantSums(catch_SOP$DN * catch_SOP$DW)/catch_SOP$DA

SOP_CA_7e <- quantSums(catch_SOP$CN_7e * catch_SOP$CW_7e)/catch_SOP$CA_7e
SOP_LA_7e <- quantSums(catch_SOP$LN_7e * catch_SOP$LW_7e)/catch_SOP$LA_7e
SOP_DA_7e <- quantSums(catch_SOP$DN_7e * catch_SOP$DW_7e)/catch_SOP$DA_7e

SOP_CA_7d <- quantSums(catch_SOP$CN_7d * catch_SOP$CW_7d)/catch_SOP$CA_7d
SOP_LA_7d <- quantSums(catch_SOP$LN_7d * catch_SOP$LW_7d)/catch_SOP$LA_7d
SOP_DA_7d <- quantSums(catch_SOP$DN_7d * catch_SOP$DW_7d)/catch_SOP$DA_7d

### "correct" weights
catch_SOP$CW <- catch_SOP$CW / catch_SOP$CW %=% rep(c(SOP_CA), each = n_ages)
catch_SOP$LW <- catch_SOP$LW / catch_SOP$LW %=% rep(c(SOP_LA), each = n_ages)
catch_SOP$DW <- catch_SOP$DW / catch_SOP$DW %=% rep(c(SOP_DA), each = n_ages)

catch_SOP$CW_7e <- catch_SOP$CW_7e / catch_SOP$CW_7e %=% rep(c(SOP_CA_7e), 
                                                             each = n_ages)
catch_SOP$LW_7e <- catch_SOP$LW_7e / catch_SOP$LW_7e %=% rep(c(SOP_LA_7e), 
                                                             each = n_ages)
catch_SOP$DW_7e <- catch_SOP$DW_7e / catch_SOP$DW_7e %=% rep(c(SOP_DA_7e), 
                                                             each = n_ages)

catch_SOP$CW_7d <- catch_SOP$CW_7d / catch_SOP$CW_7d %=% rep(c(SOP_CA_7d), 
                                                             each = n_ages)
catch_SOP$LW_7d <- catch_SOP$LW_7d / catch_SOP$LW_7d %=% rep(c(SOP_LA_7d), 
                                                             each = n_ages)
catch_SOP$DW_7d <- catch_SOP$DW_7d / catch_SOP$DW_7d %=% rep(c(SOP_DA_7d), 
                                                             each = n_ages)

### save
saveRDS(catch_SOP, file = "data/catch/catch_pg_SOP.rds")
#catch_SOP <- readRDS("data/catch/catch_pg_SOP.rds")

### plot correction factors
p <- as.data.frame(FLQuants(
  Catch = SOP_CA, Landings = SOP_LA, Discards = SOP_DA
)) %>%
  mutate(qname = factor(qname, 
                        levels = c("Catch", "Landings", "Discards"))) %>%
  ggplot(aes(x = year, y = data)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey") +
  geom_line(linewidth = 0.3) +
  geom_point(size = 0.3) + 
  facet_wrap(~ qname, ncol = 1) +
  labs(x = "Year", y = "Sum of products (SOP) factors") + 
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave(file = paste0("data/catch/plots/catch_SOP.png"),
       width = 15, height = 8,  plot = p,
       units = "cm", dpi = 300, type = "cairo")
ggsave(file = paste0("data/catch/plots/catch_SOP.pdf"),
       width = 10, height = 6,  plot = p,
       units = "cm")

### ------------------------------------------------------------------------ ###
### plot final catch and stock weights & catch numbers ####
### ------------------------------------------------------------------------ ###

### catch weights at age
cols <- scales::hue_pal()(length(2:10))
cols <- cols[c(seq(from = 1, to = length(cols), by = 4),
               seq(from = 2, to = length(cols), by = 4),
               seq(from = 3, to = length(cols), by = 4),
               seq(from = 4, to = length(cols), by = 4))]
p <- as.data.frame(FLQuants(
  Catch = catch_SOP$CW, Landings = catch_SOP$LW, Discards = catch_SOP$DW,
  Stock = SW_2_10
)) %>%
  mutate(data = ifelse(data == 0, NA, data)) %>%
  mutate(qname = factor(qname, 
                        levels = c("Stock", "Catch", "Landings", "Discards"))) %>%
  mutate(age = factor(age, levels = 10:2,
                      labels = c("10+", 9:2))) %>%
  ggplot(aes(x = year, y = data, colour = age)) +
  geom_line(linewidth = 0.3) +
  geom_point(size = 0.3) + 
  scale_colour_manual("Age (years)", values = cols) + 
  facet_wrap(~ qname, ncol = 1) + 
  labs(x = "Year", y = "Weight at age (kg)") + 
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave(file = paste0("data/catch/plots/catch_stock_weights_pg.png"),
       width = 15, height = 10,  plot = p,
       units = "cm", dpi = 300, type = "cairo")
ggsave(file = paste0("data/catch/plots/catch_stock_weights_pg.pdf"),
       width = 16, height = 10,  plot = p,
       units = "cm")

### catch age structure
p <- as.data.frame(FLQuants(catch_SOP[c("LN", "DN")])) %>%
  mutate(qname = factor(qname, levels = c("DN", "LN"), 
                        labels = c("Discards", "Landings"))) %>%
  ggplot(aes(x = age, y = data, fill = qname)) +
  geom_col() +
  scale_fill_discrete("") + 
  facet_wrap(~ year) + 
  labs(x = "Year", y = "Catch numbers (thousands)") + 
  scale_x_continuous(breaks = seq(2, 10, 2), 
                     labels = c(seq(2, 8, 2), "10+")) +
  theme_bw(base_size = 8) +
  theme(legend.key.width = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave(file = paste0("data/catch/plots/catch_age_pg.png"),
       width = 25, height = 15,  plot = p,
       units = "cm", dpi = 300, type = "cairo")
ggsave(file = paste0("data/catch/plots/catch_age_pg.pdf"),
       width = 16, height = 12,  plot = p,
       units = "cm")

### ------------------------------------------------------------------------ ###
### 7.e InterCatch data - countries & fleets ####
### ------------------------------------------------------------------------ ###

### catch by country
table1 <- read.csv("boot/initial/data/InterCatch/table1_hist.txt")
p <- table1 %>%
  filter(CatchCategory %in% c("Landings", "Discards")) %>%
  mutate(CatchCategory = factor(CatchCategory, 
                                levels = c("Landings", "Discards"))) %>%
#  mutate(Country = factor(Country, levels = rev(sort(unique(Country))))) %>%
  ggplot() +
  geom_bar(aes(x = Year, y = CATON/1000, fill = Country), 
           stat = "identity") +
#  scale_fill_brewer(palette = "Dark2") + 
  geom_text(data = . %>%
              group_by(Year, CatchCategory) %>%
              summarise(CATON = sum(CATON)),
            aes(x = Year, y = CATON/1000 + 50,
                label = round(CATON/1000, 0)),
            size = 1.5) +
  facet_wrap(~ CatchCategory, ncol = 1) +
  labs(x = "Year", y = "Catch in Division 7.e (tonnes)") + 
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p
ggsave(file = paste0("data/catch/plots/InterCatch_countries.png"),
       width = 15, height = 10,  plot = p,
       units = "cm", dpi = 300, type = "cairo")
ggsave(file = paste0("data/catch/plots/InterCatch_countries.pdf"),
       width = 10, height = 6,  plot = p,
       units = "cm")

### catch by gear
p <- table1 %>% 
  filter(CatchCategory %in% c("Landings", "Discards")) %>%
  mutate(CatchCategory = factor(CatchCategory, 
                                levels = c("Landings", "Discards"))) %>%
  ### simplify countries
  mutate(Country2 = ifelse(Country %in% c("UK (Channel Island Guernsey)",
                                          "UK (Channel Island Jersey)",
                                          "UK (England)", "UK(Scotland)"),
                           "UK", Country)) %>%
  mutate(Country2 = ifelse(Country2 %in% c("Ireland", "Netherlands"),
                           "Other", Country2)) %>%
  ### simplify gears
  mutate(Gear = substr(x = Fleet, start = 1, stop = 3)) %>%
  mutate(Gear = ifelse(Gear %in% c("TBB", "OTB", "GNS"), Gear, "Other")) %>%
  select(Year, CatchCategory, Gear, CATON) %>%
  group_by(Year, CatchCategory, Gear) %>%
  summarise(CATON = sum(CATON)) %>%
  mutate(prop = CATON/sum(CATON)) %>%
  mutate(Gear = factor(Gear, levels = rev(c("TBB", "OTB", "GNS", "Other")))) %>%
  ggplot(aes(x = Year, y = prop * 100, fill = Gear)) +
  geom_col() +
  scale_fill_brewer(palette = "Set1") + 
  facet_wrap(~ CatchCategory, ncol = 1) +
  labs(x = "Year", y = "Gear contribution (%)") +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p
ggsave(file = paste0("data/catch/plots/InterCatch_gears.png"),
       width = 15, height = 10,  plot = p,
       units = "cm", dpi = 300, type = "cairo")
ggsave(file = paste0("data/catch/plots/InterCatch_gears.pdf"),
       width = 10, height = 6,  plot = p,
       units = "cm")

### sampling coverage
p <- table1 %>% group_by(Year, CatchCategory, SampledOrEstimated) %>%
  # mutate(CatchCategory = factor(CatchCategory, 
  #                               levels = c("Landings", "Discards"))) %>%
  summarise(catch = sum(CATON)/1000) %>%
  mutate(contribution = catch / sum(catch)) %>%
  filter(SampledOrEstimated == "Sampled_Distribution") %>%
  select(-catch, -SampledOrEstimated) %>%
  ggplot(aes(x = Year, y = contribution * 100, colour = CatchCategory,
             linetype = CatchCategory)) +
  geom_line() +
  scale_colour_discrete("") + 
  scale_linetype_manual("", values = c("dashed", "solid")) +
  labs(x = "Year", y = "Sampling coverage (%)") + 
  scale_y_continuous(limits = c(0, 100)) + 
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"),
        legend.position = "inside",
        legend.position.inside = c(0.2, 0.2),
        legend.background = element_blank(),
        legend.key = element_blank())
if (isTRUE(verbose)) p
ggsave(file = paste0("data/catch/plots/InterCatch_sampling.png"),
       width = 10, height = 6,  plot = p,
       units = "cm", dpi = 300, type = "cairo")
ggsave(file = paste0("data/catch/plots/InterCatch_sampling.pdf"),
       width = 10, height = 6,  plot = p,
       units = "cm")

### plot some age samples - 2023 only
IC_samples <- read.csv("boot/initial/data/InterCatch/NumbersAtAgeLength_hist.csv")
IC_samples_plot <- IC_samples %>%
  filter(Year == 2023) %>%
  select(-X) %>%
  pivot_longer(cols = starts_with("Male") | starts_with("Female") | 
                 starts_with("Undetermined"),
               names_prefix = "FemaleAge|MaleAge|UndeterminedAge", 
               names_to = "age", values_to = "numbers") %>%
  mutate(age = as.numeric(as.character(age))) %>%
  mutate(label = paste0(Country, " - ", Catch.Cat., " - Q", Season, "\n", 
                        Fleets)) %>%
  mutate(numbers = numbers/1000)
### extract some sample info
IC_samples_info <- IC_samples_plot %>% group_by(label, Year) %>%
  summarise(age_max = max(age, na.rm = TRUE), 
            n_max = max(numbers, na.rm = TRUE),
            NumAgeMeasurement = mean(NumAgeMeasurement),
            NumLengthMeasurements = mean(NumLengthMeasurements),
            NumSamplesAge = mean(NumSamplesAge),
            NumSamplesLength = mean(NumSamplesLength),
            Country = unique(Country))
### plot
p <- ggplot(data = IC_samples_plot, 
            aes(x = age, y = numbers)) +
  geom_bar(stat = "identity", aes(fill = Catch.Cat.)) +
  geom_text(data = IC_samples_info,
            aes(x = age_max*0.95, y = n_max*0.95,
                label = paste0("length samples: ", NumSamplesLength, "\n",
                               "length readings: ", NumLengthMeasurements, "\n",
                               "age samples: ", NumSamplesAge, "\n",
                               "age readings: ", NumAgeMeasurement)),
            hjust = 1, vjust = 1, size = 1.5) +
  facet_wrap(~ label, scale = "free_y", ncol = 4) +
  labs(x = "Age (years)", y = "Numbers (thousands)") +
  ylim(0, NA) +
  scale_fill_discrete("") +
  theme_bw(base_size = 7) + 
  theme(legend.position = c(0.9, 0.05),
        legend.background = element_blank(),
        legend.key.width = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave(file = paste0("data/catch/plots/InterCatch_samples_2023.png"),
       width = 25, height = 18,  plot = p,
       units = "cm", dpi = 300, type = "cairo")
ggsave(file = paste0("data/catch/plots/InterCatch_samples_2023.pdf"),
       width = 25, height = 18,  plot = p,
       units = "cm")

### ------------------------------------------------------------------------ ###
### Discard survivability explorations ####
### ------------------------------------------------------------------------ ###
### Morfin et al. (2017), https://doi.org/10.1016/j.jenvman.2017.08.046
### - otter trawls: 58.2%
### Revill et al. (2013), https://doi.org/10.1016/j.fishres.2013.07.005
### - beam trawls: 37.5%

### InterCatch catch history
table1 <- read.csv("boot/initial/data/InterCatch/table1_hist.txt")

### catch by gear
gear_split <- table1 %>% 
  filter(CatchCategory %in% c("Landings", "Discards")) %>%
  mutate(CatchCategory = factor(CatchCategory, 
                                levels = c("Landings", "Discards"))) %>%
  mutate(Gear = substr(x = Fleet, start = 1, stop = 3)) %>%
  mutate(Gear = ifelse(Gear %in% c("TBB", "OTB"), Gear, "Other")) %>%
  select(Year, CatchCategory, Gear, CATON) %>%
  ### keep only TBB and OTB
  filter(Gear %in% c("TBB", "OTB")) %>%
  group_by(Year, CatchCategory, Gear) %>%
  summarise(CATON = sum(CATON)) %>%
  mutate(prop = CATON/sum(CATON)) %>%
  mutate(Gear = factor(Gear, levels = rev(c("TBB", "OTB"))))

### add discard survivability
gear_split_surv <- gear_split %>% 
  filter(CatchCategory == "Discards") %>%
  ungroup() %>%
  full_join(data.frame(Gear = c("OTB", "TBB"),
                       survivability = c(0.582, 0.375)))
gear_split_surv_yr <- gear_split_surv %>%
  group_by(Year) %>%
  summarise(survivability = weighted.mean(x = survivability, w = prop),
            discards = sum(CATON))
### plot rate
p1 <- gear_split_surv_yr %>%
  ggplot(aes(x = Year, y = survivability * 100)) +
  geom_line() + 
  geom_hline(yintercept = mean(gear_split_surv_yr$survivability) * 100,
             linetype = "dashed", show.legend = TRUE) +
  labs(x = "Year", y = "Survival (%)") +
  ylim(c(0, NA)) +
  xlim(c(2002, 2023)) +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p1

### include historical discards
catch_2_10 <- readRDS("data/catch/catch_pg.rds")
gear_split_surv_yr_hist <- 
  gear_split_surv_yr %>%
  full_join(as.data.frame(catch_2_10$DA) %>%
              select(Year = year, discards = data) %>%
              mutate(discards = discards * 1000) %>%
              filter(discards > 0 & Year < 2012)) %>%
  mutate(survivability = ifelse(Year < 2012, mean(survivability, na.rm = TRUE), 
                                survivability)) %>%
  mutate(dead = discards * survivability,
         surviving = discards * (1 - survivability)) %>%
  select(Year, dead, surviving) %>%
  pivot_longer(-Year, names_to = "type", values_to = "discards")
### plot
p2 <- gear_split_surv_yr_hist %>%
  mutate(type = factor(type, levels = c("surviving", "dead"))) %>%
  ggplot(aes(x = Year, y = discards/1000, fill = type)) +
  geom_col() + 
  scale_fill_brewer("Discards", palette = "Dark2") +
  labs(x = "Year", y = "Discards (t)") +
  theme_bw(base_size = 8)
if (isTRUE(verbose)) p2

### combine plots
p <- p1/p2
if (isTRUE(verbose)) p
ggsave(file = paste0("data/catch/plots/Discards_survival.png"),
       width = 15, height = 10,  plot = p,
       units = "cm", dpi = 300, type = "cairo")
ggsave(file = paste0("data/catch/plots/Discards_survival.pdf"),
       width = 10, height = 6,  plot = p,
       units = "cm")

### ------------------------------------------------------------------------ ###
### fbar range ####
### ------------------------------------------------------------------------ ###
catch <- readRDS("data/catch/catch_pg_SOP.rds")
cn <- catch$CN@.Data[,, 1, 1, 1, 1, drop = TRUE]

### function from icesAdvice R package
if (isTRUE(verbose)) {
  ### full time series
  agesFbar(t(cn), plot = TRUE) ### [1] 2.663063 5.456509
  ### last 10 years
  agesFbar(t(cn[, 35:44]), plot = TRUE) ### [1] 2.952658 5.923334
}
### i.e. 3 - 6 (rounded)

### ------------------------------------------------------------------------ ###
### final data for OM ####
### ------------------------------------------------------------------------ ###
### total catch
### catch (landings/discards/catch) weights: raw, as far as available
### stock weights: from model fit, back-calculated to beginning of year
###                including all discards because they are part of the stock


catch <- readRDS("data/catch/catch_pg_SOP.rds")
saveRDS(catch, file = "data/OM/catch.rds")

stock_weights <- readRDS("data/catch/stock_weights_pg.rds")
saveRDS(stock_weights, file = "data/OM/stock_weights.rds")
