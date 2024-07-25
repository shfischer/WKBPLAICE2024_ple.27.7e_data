### ------------------------------------------------------------------------ ###
### trial SAM ####
### ------------------------------------------------------------------------ ###
library(FLCore)
library(stockassessment)
library(FLfse)
library(tidyr)
library(dplyr)
library(ggplot2)

mkdir("model")

### ------------------------------------------------------------------------ ###
### data ####
### ------------------------------------------------------------------------ ###

stk <- readRDS("data/OM/stk_baseline.rds")
idx <- readRDS("data/OM/idcs.rds")

#idx$Q1SWBeam@index[, ac(2022)] <- NA

### ------------------------------------------------------------------------ ###
### fit SAM ####
### ------------------------------------------------------------------------ ###

fit <- FLR_SAM(stk = stk, idx = idx)
if (isTRUE(verbose)) plot(fit)
saveRDS(fit, file = "model/fit_baseline.rds")

fit$opt$message
fit$opt$convergence

### data plot
png(filename = "model/SAM_data.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
dataplot(fit)
dev.off()
pdf(file = "model/SAM_data.pdf", 
    width = 16/2.54, height = 8/2.54)
dataplot(fit)
dev.off()

### results
png(filename = "model/SAM_fit.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
plot(fit)
dev.off()
pdf(file = "model/SAM_fit.pdf", 
    width = 16/2.54, height = 8/2.54)
par(mar = c(2, 4.5, 0.5, 0.5))
plot(fit)
dev.off()

### catch
png(filename = "model/SAM_fit_catch.png", 
    width = 15, height = 6, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
catchplot(fit)
dev.off()
pdf(file = "model/SAM_fit_catch.pdf", 
    width = 10/2.54, height = 5/2.54)
par(mar = c(2, 4.5, 0.5, 0.5))
catchplot(fit)
dev.off()


### SSB and TSB
png(filename = "model/SAM_fit_SSB.png", 
    width = 15, height = 6, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
ssbplot(fit)
dev.off()
pdf(file = "model/SAM_fit_SSB.pdf", 
    width = 10/2.54, height = 5/2.54)
par(mar = c(2, 4.5, 0.5, 0.5))
ssbplot(fit)
dev.off()
png(filename = "model/SAM_fit_TSB.png", 
    width = 15, height = 6, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
tsbplot(fit)
dev.off()
pdf(file = "model/SAM_fit_TSB.pdf", 
    width = 10/2.54, height = 5/2.54)
par(mar = c(2, 4.5, 0.5, 0.5))
tsbplot(fit)
dev.off()

saveConf(fit$conf, file = "model/SAM_conf.txt", overwrite = TRUE)

### ------------------------------------------------------------------------ ###
### model diagnostics ####
### ------------------------------------------------------------------------ ###

### one-observation-ahead residuals
. <- capture.output(res <- residuals(fit))
if (isTRUE(verbose)) print(tail(.))
if (isTRUE(verbose)) plot(res)

png(filename = "model/SAM_res.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
plot(res)
dev.off()
pdf(file = "model/SAM_res.pdf", 
    width = 16/2.54, height = 8/2.54)
par(mar = c(4, 4.5, 0.5, 0.5))
plot(res)
dev.off()

### between-age correlation by fleet
png(filename = "model/SAM_cor.png", 
    width = 5, height = 15, units = "cm", res = 300, type = "cairo")
par(mar = c(1, 2, 0.5, 0.5))
corplot(res)
dev.off()
pdf(file = "model/SAM_cor.pdf", 
    width = 5/2.54, height = 15/2.54)
par(mar = c(1, 2, 0.5, 0.5))
corplot(res)
dev.off()

### process residuals
. <- capture.output(resp <- procres(fit))
if (isTRUE(verbose)) print(tail(.))
if (isTRUE(verbose)) plot(resp)

png(filename = "model/SAM_resp.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(3, 4.5, 0.5, 0.5))
plot(resp)
dev.off()
pdf(file = "model/SAM_resp.pdf", 
    width = 16/2.54, height = 8/2.54)
par(mar = c(4, 4.5, 0.5, 0.5))
plot(resp)
dev.off()

### retro
retro <- retro(fit, year = 5)
if (isTRUE(verbose)) plot(retro)
if (isTRUE(verbose)) mohn(retro)

png(filename = "model/SAM_retro.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
plot(retro)
dev.off()
pdf(file = "model/SAM_retro.pdf", 
    width = 16/2.54, height = 8/2.54)
par(mar = c(2, 4.5, 0.5, 0.5))
plot(retro)
dev.off()

### survey leave-out runs
lo <- leaveout(fit)
if (isTRUE(verbose)) plot(lo)

png(filename = "model/SAM_lo.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
plot(lo)
dev.off()
pdf(file = "model/SAM_lo.pdf", 
    width = 16/2.54, height = 8/2.54)
par(mar = c(2, 4.5, 0.5, 0.5))
plot(lo)
dev.off()

### simulate data from fitted model and re-estimate from each run
sim <- simstudy(fit, nsim = 100, ncores = 10)
if (isTRUE(verbose)) plot(sim)

png(filename = "model/SAM_sim.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
plot(sim)
dev.off()
pdf(file = "model/SAM_sim.pdf", 
    width = 16/2.54, height = 8/2.54)
par(mar = c(2, 4.5, 0.5, 0.5))
plot(sim)
dev.off()

png(filename = "model/SAM_sim_no_sims.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
plot(fit, partial = FALSE) ### for comparison with simulations
dev.off()

### jitter: start from random inital values
set.seed(12345)
jit <- jit(fit, nojit = 100, ncores = 10)
if (isTRUE(verbose)) plot(jit)

png(filename = "model/SAM_jit.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
plot(jit)
dev.off()
pdf(file = "model/SAM_jit.pdf", 
    width = 16/2.54, height = 8/2.54)
par(mar = c(2, 4.5, 0.5, 0.5))
plot(jit)
dev.off()

### fit to survey
png(filename = "model/SAM_fitplot.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
fitplot(fit)
dev.off()
pdf(file = "model/SAM_fitplot.pdf", 
    width = 16/2.54, height = 16/2.54)
par(mar = c(2, 4.5, 0.5, 0.5))
fitplot(fit)
dev.off()

### F selectivity
png(filename = "model/SAM_fsel.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
fselectivityplot(fit)
dev.off()
pdf(file = "model/SAM_fsel.pdf", 
    width = 16/2.54, height = 8/2.54)
par(mar = c(2, 4.5, 0.5, 0.5))
fselectivityplot(fit)
dev.off()

### stock-recruit plot
png(filename = "model/SAM_SR.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
srplot(fit)
dev.off()
pdf(file = "model/SAM_SR.pdf", 
    width = 16/2.54, height = 8/2.54)
par(mar = c(4, 4.5, 0.5, 0.5))
srplot(fit)
dev.off()

### parameter table - fixed effects
partable(fit)
png(filename = "model/SAM_parplot.png", 
    width = 15, height = 15, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 2, 0.5))
parplot(fit)
dev.off()
pdf(file = "model/SAM_parplot.pdf", 
    width = 16/2.54, height = 8/2.54)
par(mar = c(2, 4.5, 2, 0.5))
parplot(fit)
dev.off()

### table of survey catchabilities
qtable(fit)
png(filename = "model/SAM_fit_surveyQ.png", 
    width = 15, height = 15, units = "cm", res = 300, type = "cairo")
par(mar = c(4.5, 4.5, 2, 0.5))
qtableplot(qtable(fit), exp = TRUE)
dev.off()
pdf(file = "model/SAM_fit_surveyQ.pdf", 
    width = 16/2.54, height = 8/2.54)
par(mar = c(2, 4.5, 2, 0.5))
qtableplot(qtable(fit), exp = TRUE)
dev.off()


### comparison to SAM fit from paper
fit_paper <- readRDS("model/fit_paper.rds")
png(filename = "model/SAM_fit_comparison.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
plot(c("WKBPLAICE" = fit, "Fischer et al. (2023)" = fit_paper))
dev.off()
pdf(file = "model/SAM_fit_comparison.pdf", 
    width = 16/2.54, height = 8/2.54)
par(mar = c(2, 4.5, 0.5, 0.5))
plot(c("WKBPLAICE" = fit, "Fischer et al. (2023)" = fit_paper))
dev.off()

### proportion of biomass by age
stk_fit <- SAM2FLStock(fit, stk)
cols <- scales::hue_pal()(length(2:10))
cols <- cols[c(seq(from = 1, to = length(cols), by = 3),
               seq(from = 2, to = length(cols), by = 3),
               seq(from = 3, to = length(cols), by = 3))]
p <- as.data.frame((stock.n(stk_fit) * stock.wt(stk_fit))) %>%
  mutate(age = factor(age, levels = 10:2, 
                      labels = c("10+", 9:2))) %>%
  ggplot(aes(x = year, y = data, fill = age)) +
  geom_col(position = "fill") +
  scale_fill_manual("Age (years)", values = cols) +
  labs(x = "Year", y = "Stock biomass contribution") +
  theme_bw(base_size = 8) +
  theme(legend.key.size = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave("model/SAM_biomass_contribution.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)
ggsave("model/SAM_biomass_contribution.pdf", 
       width = 10, height = 5, units = "cm", plot = p)

### ------------------------------------------------------------------------ ###
### alternative plusgroups ####
### ------------------------------------------------------------------------ ###

### 9+
stk_pg9 <- readRDS("data/OM/stk_baseline_pg9.rds")
idx_pg9 <- readRDS("data/OM/idcs_pg9.rds")
fit_pg9 <- FLR_SAM(stk = stk_pg9, idx = idx_pg9)

### 8+
stk_pg8 <- readRDS("data/OM/stk_baseline_pg8.rds")
idx_pg8 <- readRDS("data/OM/idcs_pg8.rds")
fit_pg8 <- FLR_SAM(stk = stk_pg8, idx = idx_pg8)

### compare results
png(filename = "model/SAM_fit_comparison_pg.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
plot(c("10+" = fit, "9+" = fit_pg9, "8+" = fit_pg8))
dev.off()
pdf(file = "model/SAM_fit_comparison_pg.pdf", 
    width = 16/2.54, height = 8/2.54)
par(mar = c(2, 4.5, 0.5, 0.5))
plot(c("10+" = fit, "9+" = fit_pg9, "8+" = fit_pg8))
dev.off()

### residuals
. <- capture.output(res_pg9 <- residuals(fit_pg9))
png(filename = "model/SAM_res_pg9.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
plot(res_pg9)
dev.off()
pdf(file = "model/SAM_res_pg9.pdf", 
    width = 16/2.54, height = 8/2.54)
par(mar = c(4, 4.5, 0.5, 0.5))
plot(res_pg9)
dev.off()
. <- capture.output(res_pg8 <- residuals(fit_pg8))
png(filename = "model/SAM_res_pg8.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
plot(res_pg8)
dev.off()
pdf(file = "model/SAM_res_pg8.pdf", 
    width = 16/2.54, height = 8/2.54)
par(mar = c(4, 4.5, 0.5, 0.5))
plot(res_pg8)
dev.off()
### process residuals
. <- capture.output(resp_pg9 <- procres(fit_pg9))
png(filename = "model/SAM_resp_pg9.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(3, 4.5, 0.5, 0.5))
plot(resp_pg9)
dev.off()
pdf(file = "model/SAM_resp_pg9.pdf", 
    width = 16/2.54, height = 8/2.54)
par(mar = c(4, 4.5, 0.5, 0.5))
plot(resp_pg9)
dev.off()
. <- capture.output(resp_pg8 <- procres(fit_pg8))
png(filename = "model/SAM_resp_pg8.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(3, 4.5, 0.5, 0.5))
plot(resp_pg8)
dev.off()
pdf(file = "model/SAM_resp_pg8.pdf", 
    width = 16/2.54, height = 8/2.54)
par(mar = c(4, 4.5, 0.5, 0.5))
plot(resp_pg8)
dev.off()

### ------------------------------------------------------------------------ ###
### Discard survival ####
### ------------------------------------------------------------------------ ###

### 100% survival - 0% discards
stk_d0 <- readRDS("data/OM/stk_d0.rds")
fit_d0 <- FLR_SAM(stk = stk_d0, idx = idx) 

### 0% survival - 100% discards included
stk_d100 <- readRDS("data/OM/stk_d100.rds")
fit_d100 <- FLR_SAM(stk = stk_d100, idx = idx) 

### compare
png(filename = "model/SAM_fit_comparison_d_survival.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
plot(c("50% discard\nsurvival (baseline)" = fit, 
       "0% discard\nsurvival" = fit_d100, 
       "100% discard\nsurvival" = fit_d0))
dev.off()
pdf(file = "model/SAM_fit_comparison_d_survival.pdf", 
    width = 16/2.54, height = 8/2.54)
par(mar = c(2, 4.5, 0.5, 0.5))
plot(c("50% discard\nsurvival (baseline)" = fit, 
       "0% discard\nsurvival" = fit_d100, 
       "100% discard\nsurvival" = fit_d0))
dev.off()

png(filename = "model/SAM_fit_catch_comparison_d_suvival.png", 
    width = 15, height = 10, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
catchplot(c("50% discard\nsurvival (baseline)" = fit, 
            "0% discard\nsurvival" = fit_d100, 
            "100% discard\nsurvival" = fit_d0))
dev.off()
pdf(file = "model/SAM_fit_catch_comparison_d_suvival.pdf", 
    width = 16/2.54, height = 8/2.54)
par(mar = c(2, 4.5, 0.5, 0.5))
catchplot(c("50% discard\nsurvival (baseline)" = fit, 
            "0% discard\nsurvival" = fit_d100, 
            "100% discard\nsurvival" = fit_d0))
dev.off()

### ------------------------------------------------------------------------ ###
### M scenario ####
### ------------------------------------------------------------------------ ###

### M -50%
stk_M_low <- readRDS("data/OM/stk_M_low.rds")
fit_M_low <- FLR_SAM(stk = stk_M_low, idx = idx) 

### M +50%
stk_M_high <- readRDS("data/OM/stk_M_high.rds")
fit_M_high <- FLR_SAM(stk = stk_M_high, idx = idx) 

### M Lorenzen L=Linf
stk_M_Lorenzen_Linf <- readRDS("data/OM/stk_M_Lorenzen_Linf.rds")
fit_M_Lorenzen_Linf <- FLR_SAM(stk = stk_M_Lorenzen_Linf, idx = idx) 

### M Gislason
stk_M_Gislason <- readRDS("data/OM/stk_M_Gislason.rds")
fit_M_Gislason <- FLR_SAM(stk = stk_M_Gislason, idx = idx) 

### compare
png(filename = "model/SAM_fit_comparison_M.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
plot(c("M Then M = 0.18\n(baseline)" = fit, 
       "M-50%" = fit_M_low,
       "M+50%" = fit_M_high,
       "M Lorenzen" = fit_M_Lorenzen_Linf,
       "M Gislason" = fit_M_Gislason
       ))
dev.off()
pdf(file = "model/SAM_fit_comparison_M.pdf", 
    width = 16/2.54, height = 8/2.54)
par(mar = c(2, 4.5, 0.5, 0.5))
plot(c("M Then M = 0.18\n(baseline)" = fit, 
       "M-50%" = fit_M_low,
       "M+50%" = fit_M_high,
       "M Lorenzen" = fit_M_Lorenzen_Linf,
       "M Gislason" = fit_M_Gislason
))
dev.off()


### ------------------------------------------------------------------------ ###
### Migration - removed ####
### ------------------------------------------------------------------------ ###

stk_no_migration <- readRDS("data/OM/stk_no_migration.rds")
fit_no_migration <- FLR_SAM(stk = stk_no_migration, idx = idx) 

### compare
png(filename = "model/SAM_fit_comparison_migration.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
plot(c("Including migration\n(baseline)" = fit, 
       "Excluding migration" = fit_no_migration))
dev.off()
pdf(file = "model/SAM_fit_comparison_migration.pdf", 
    width = 16/2.54, height = 8/2.54)
par(mar = c(2, 4.5, 0.5, 0.5))
plot(c("Including migration\n(baseline)" = fit, 
       "Excluding migration" = fit_no_migration))
dev.off()

png(filename = "model/SAM_fit_catch_comparison_migration.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
catchplot(c("Including migration\n(baseline)" = fit, 
            "Excluding migration" = fit_no_migration))
dev.off()
pdf(file = "model/SAM_fit_catch_comparison_migration.pdf", 
    width = 16/2.54, height = 8/2.54)
par(mar = c(2, 4.5, 0.5, 0.5))
catchplot(c("Including migration\n(baseline)" = fit, 
            "Excluding migration" = fit_no_migration))
dev.off()



### compare models: modeltable()

