### ------------------------------------------------------------------------ ###
### deal with discards ####
### ------------------------------------------------------------------------ ###

## Before: VPA input files in data/:
##           PLE7EIND_C.DAT
##           PLE7E*.DAT

## After: data/model_input_stk_d.RDS

### ------------------------------------------------------------------------ ###
### load data for XSA assessment ####
### ------------------------------------------------------------------------ ###

### load packages
suppressPackageStartupMessages(library(TAF))
taf.libPaths()
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(FLCore))

### load additional functions
source("utilities.R")

if (!exists("verbose")) verbose <- FALSE

### ------------------------------------------------------------------------ ###
### load stock data ####
### ------------------------------------------------------------------------ ###

### read data
stock <- readFLStock(paste0("data/PLE7EIND_C.DAT"), quiet = TRUE)

### define fbar age range
range(stock)[c("minfbar", "maxfbar")] <- c(3, 6)

### set units
units(stock)[1:17] <- as.list(c(rep(c("tonnes", "thousands", "kg"), 4),
                                "NA", "NA", "f", "NA", "NA"))
units <- units(stock)

### set plusgroup
stock <- setPlusGroup(stock, 10)
stock_original <- stock

# summary(stock)

### ------------------------------------------------------------------------ ###
### SOP correction ####
### ------------------------------------------------------------------------ ###

SOP_l <- landings(stock) / quantSums(landings.n(stock) * landings.wt(stock)) 
landings.wt(stock) <- landings.wt(stock) * rep(c(SOP_l), each = dims(stock)$age)
SOP_d <- discards(stock) / quantSums(discards.n(stock) * discards.wt(stock)) 
discards.wt(stock) <- discards.wt(stock) * rep(c(SOP_d), each = dims(stock)$age)
SOP_c <- catch(stock) / quantSums(catch.n(stock) * catch.wt(stock)) 
catch.wt(stock) <- catch.wt(stock) * rep(c(SOP_c), each = dims(stock)$age)

### ------------------------------------------------------------------------ ###
### guestimate discards prior to 2012 ####
### ------------------------------------------------------------------------ ###
### take percent estimates from WKFLAT 10 WD
### Figure 19, annual discards by weight from UK
### data for 2001-2009 exists
### for 2010-2011, use mean of before and after
d_yrs <- 2002:2011
disc_rates <- data.frame(year = d_yrs,
                         rate = c(0.053, 0.047, 0.063, 0.087, 0.075, 0.097, 
                                  0.127, 0.074,
                                  rep(mean(c(0.074, 0.217618838)), 2)))
### calculate total discards per year
discards(stock)[, ac(d_yrs)] <- landings(stock)[, ac(d_yrs)] * 
                                  (1/(1 - disc_rates$rate) - 1)
### set discard weights at age
### 5-year average
discards.wt(stock)[, ac(d_yrs)] <- 
  yearMeans(discards.wt(stock)[, ac(2012:2016)])
### age structure: 5 year average
d_n <- discards.n(stock)[, ac(2012:2016)]
d_n <- d_n / rep(c(apply(d_n, 2, max)), each = dim(d_n)[1])
d_n <- yearMeans(d_n)
### solve for numbers at age
fn <- function(mult, yr) {
  res <- quantSums(d_n * mult * discards.wt(stock)[, ac(yr)])
  res <- (res - discards(stock)[, ac(yr)])^2
  return(c(res))
}
for (yr in d_yrs) {
  mult_res <- optimize(f = fn, yr = yr, interval = c(0, 1e+9))[["minimum"]]
  discards.n(stock)[, ac(yr)] <- d_n * mult_res
}
# computeDiscards(stock) / discards(stock) ### all good
catch(stock[, ac(d_yrs)]) <- computeCatch(stock[, ac(d_yrs)], slot = "all")

### ------------------------------------------------------------------------ ###
### plot discards ####
### ------------------------------------------------------------------------ ###

df_catch <- as.data.frame(FLQuants(
  "discards\nguestimate" = discards(stock)[, ac(2002:2011)],
  "discards" = window(discards(stock), start = 2012),
  "landings" = landings(stock)))
p <- ggplot(data = df_catch,
         aes(x = year, y = data, fill = qname)) +
  geom_col() + theme_custom2 + scale_fill_discrete("") +
  labs(y = "catch [tonnes]")
if (isTRUE(verbose)) p
ggsave(file = "data/plots_discards_timeseries.png", plot = p,
       width = 15, height = 8, units = "cm", dpi = 300, type = "cairo-png")

### ------------------------------------------------------------------------ ###
### save model input files ####
### ------------------------------------------------------------------------ ###

saveRDS(object = stock, file = "data/model_input_stk_d.RDS")
