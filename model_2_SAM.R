### ------------------------------------------------------------------------ ###
### Exploratory SAM assessment (including discards) ####
### ------------------------------------------------------------------------ ###

## Before: data/model_input_stk.RDS
##         data/model_input_idx.RDS
## After: model/SAM/

### ------------------------------------------------------------------------ ###
### prepare session ####
### ------------------------------------------------------------------------ ###

### load packages
suppressPackageStartupMessages(library(icesTAF))
taf.libPaths()
suppressPackageStartupMessages(library(FLCore))
suppressPackageStartupMessages(library(TMB))
suppressPackageStartupMessages(library(stockassessment))
suppressPackageStartupMessages(library(FLfse))

### load additional functions
source("utilities.R")

### make model directory
mkdir("model/SAM/")

if (!exists("verbose")) verbose <- FALSE

### ------------------------------------------------------------------------ ###
### load data ####
### ------------------------------------------------------------------------ ###

### load input objects
### stock object: FLStock (includes discards)
stk <- readRDS("data/model_input_stk_d.RDS")
### index: FLIndices
idx <- readRDS("data/model_input_idx.RDS")

### SAM model configuration
conf <- list(keyLogFpar = matrix(data = c(rep(-1, 9),
                                          c(0:4, 5, 5, -1, -1),
                                          c(6:10, 11, 11, 11, -1)),
                                 nrow = 3, ncol = 9, byrow = TRUE))


### ------------------------------------------------------------------------ ###
### fit SAM ####
### ------------------------------------------------------------------------ ###

fit <- FLR_SAM(stk = stk, idx = idx, conf = conf)
if (isTRUE(verbose)) plot(fit)

### ------------------------------------------------------------------------ ###
### model diagnostics ####
### ------------------------------------------------------------------------ ###

### one-observation-ahead residuals
. <- capture.output(res <- residuals(fit))
if (isTRUE(verbose)) print(tail(.))
if (isTRUE(verbose)) plot(res)

### process residuals
. <- capture.output(resp <- procres(fit))
if (isTRUE(verbose)) print(tail(.))
if (isTRUE(verbose)) plot(resp)

### retro
retro <- retro(fit, year = 5)
if (isTRUE(verbose)) plot(retro)

### survey leave-out runs
lo <- leaveout(fit)
if (isTRUE(verbose)) plot(lo)

### simulate data from fitted model and re-estimate from each run
sim <- simstudy(fit, nsim = 100)
if (isTRUE(verbose)) plot(sim)

### jitter: start from random initial values
set.seed(12345)
jit <- jit(fit, nojit = 100)
if (isTRUE(verbose)) plot(jit)

### ------------------------------------------------------------------------ ###
### forecast to get intermediate year ####
### ------------------------------------------------------------------------ ###
set.seed(12345)
fit_fc <- forecast(fit = fit, fscale = c(1, 1),
                   ave.years = tail(as.numeric(dimnames(stk)$year), 3),
                   rec.years = as.numeric(dimnames(stk)$year))


### ------------------------------------------------------------------------ ###
### save results ####
### ------------------------------------------------------------------------ ###

### model fit
saveRDS(fit, file = "model/SAM/SAM_fit.rds")

### diagnostics
saveRDS(res, file = "model/SAM/SAM_res.rds")
saveRDS(resp, file = "model/SAM/SAM_resp.rds")
saveRDS(retro, file = "model/SAM/SAM_retro.rds")
saveRDS(lo, file = "model/SAM/SAM_lo.rds")
saveRDS(sim, file = "model/SAM/SAM_sim.rds")
saveRDS(jit, file = "model/SAM/SAM_jit.rds")

### 1-year forecast
saveRDS(fit_fc, file = "model/SAM/SAM_fit_fc.rds")

