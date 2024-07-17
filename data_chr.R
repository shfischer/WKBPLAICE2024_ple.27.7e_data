### ------------------------------------------------------------------------ ###
### visualisation of chr rule ####
### ------------------------------------------------------------------------ ###
library(icesTAF)
library(FLCore)
library(tidyr)
library(dplyr)
library(cat3advice)
library(patchwork)
library(ggplot2)

mkdir("chr")

### ------------------------------------------------------------------------ ###
### data ####
### ------------------------------------------------------------------------ ###

stk <- readRDS("data/OM/stk_baseline.rds")
idx <- readRDS("data/OM/idcs.rds")

idxB <- quantSums(catch.n(idx$`UK-FSP`) * catch.wt(idx$`UK-FSP`)) %>%
  as.data.frame() %>%
  select(year, index = data)
catch_df <- full_join(full_join(
  as.data.frame(landings(stk)) %>%
    select(year, landings = data),
  as.data.frame(discards(stk)) %>%
    select(year, discards = data)),
  as.data.frame(catch(stk)) %>%
    select(year, catch = data))

df <- merge(idxB, catch_df, all = TRUE)

hr <- HR(df, units_catch = "tonnes", units_index = "kg/hr m beam")

p <- plot(hr)
p
ggsave("chr/harvest_rate.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)

I <- I(idxB, units = "kg/hr m beam")
p <- plot(I)
p
ggsave("chr/chr_I.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)

b <- b(idxB, units = "kg/hr m beam")
p <- plot(b)
p
ggsave("chr/chr_b.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)

lc <- Lc(data = ple7e_length, pool = 2017:2021)
lc@value <- 0
set.seed(1234)
lmean <- Lmean(data.frame(
  year = 2012:2023,
  length = rlnorm(n = length(2012:2023), sdlog = 0.1),
  numbers = 1), 
               Lc = lc)
f <- f(Lmean = lmean, Lref = Lref(1), units = "cm")

F <- F(hr, f)
p <- plot(F)
p
ggsave("chr/chr_F.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)
