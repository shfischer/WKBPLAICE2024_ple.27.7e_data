### ------------------------------------------------------------------------ ###
### figures and tables from SAM assessment ####
### ------------------------------------------------------------------------ ###

## Before: model/SAM/SAM_fit.rds
##         model/SAM/SAM_res.rds
##         model/SAM/SAM_resp.rds
##         model/SAM/SAM_retro.rds
##         model/SAM/SAM_lo.rds
##         model/SAM/SAM_sim.rds
##         model/SAM/SAM_jit.rds
##         model/SAM/SAM_fit_fc.rds
## After:  many of plots and tables in report/SAM/

### load packages
suppressPackageStartupMessages(library(icesTAF))
taf.libPaths()
suppressPackageStartupMessages(library(icesAdvice))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(patchwork))
suppressPackageStartupMessages(library(foreach))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(FLCore))
suppressPackageStartupMessages(library(stockassessment))
suppressPackageStartupMessages(library(FLfse))

### load additional functions
source("utilities.R")

if (!exists("verbose")) verbose <- FALSE


### make model directory
mkdir("report/SAM/")

### ------------------------------------------------------------------------ ###
### load assessment results ####
### ------------------------------------------------------------------------ ###

### model input
stk_input_d <- readRDS("data/model_input_stk_d.RDS")

### SAM model fit
SAM_fit <- readRDS("model/SAM/SAM_fit.rds")

### diagnostics
SAM_res <- readRDS("model/SAM/SAM_res.rds")
SAM_resp <- readRDS("model/SAM/SAM_resp.rds")
SAM_retro <- readRDS("model/SAM/SAM_retro.rds")
SAM_lo <- readRDS("model/SAM/SAM_lo.rds")
SAM_sim <- readRDS("model/SAM/SAM_sim.rds")
SAM_jit <- readRDS("model/SAM/SAM_jit.rds")

### 1-year forecast
SAM_fit_fc <- readRDS("model/SAM/SAM_fit_fc.rds")

### ------------------------------------------------------------------------ ###
### SAM summary ####
### ------------------------------------------------------------------------ ###

if (isTRUE(verbose)) plot(SAM_fit)
png(filename = "report/SAM/SAM_fit.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
plot(SAM_fit)
dev.off()

### catch (observed and modelled)
if (isTRUE(verbose)) catchplot(SAM_fit)
png(filename = "report/SAM/SAM_fit_catch.png", 
    width = 15, height = 6, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
catchplot(SAM_fit)
dev.off()

write.csv(summary(SAM_fit), file = "report/SAM/SAM_fit_smry.csv", 
          row.names = FALSE)

### ------------------------------------------------------------------------ ###
### SAM diagnostics ####
### ------------------------------------------------------------------------ ###

### residuals
if (isTRUE(verbose)) plot(SAM_res)
png(filename = "report/SAM/SAM_res.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
plot(SAM_res)
dev.off()

### process residuals
if (isTRUE(verbose)) plot(SAM_resp)
png(filename = "report/SAM/SAM_resp.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(3, 4.5, 0.5, 0.5))
plot(SAM_resp)
dev.off()

### between-age correlation by fleet
if (isTRUE(verbose)) corplot(SAM_res)
png(filename = "report/SAM/SAM_cor.png", 
    width = 5, height = 15, units = "cm", res = 300, type = "cairo")
par(mar = c(1, 2, 0.5, 0.5))
corplot(SAM_res)
dev.off()

### retro
if (isTRUE(verbose)) plot(SAM_retro)
png(filename = "report/SAM/SAM_retro.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
plot(SAM_retro)
dev.off()

### retro Mohn's rho
if (isTRUE(verbose)) stockassessment::mohn(SAM_retro)
write.csv(as.data.frame(stockassessment::mohn(SAM_retro)), 
          file = "report/SAM/SAM_retro_mohn.csv")

### survey leavout
if (isTRUE(verbose)) plot(SAM_lo)
png(filename = "report/SAM/SAM_lo.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
plot(SAM_lo)
dev.off()

### simstudy: simulate data from fitted model and re-estimate from each run
if (isTRUE(verbose)) plot(SAM_sim)
png(filename = "report/SAM/SAM_sim.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
plot(SAM_sim)
dev.off()

### jitter: start from random initial values
if (isTRUE(verbose)) plot(SAM_jit)
png(filename = "report/SAM/SAM_jit.png", 
    width = 20, height = 12, units = "cm", res = 300, type = "cairo")
par(mar = c(2, 4.5, 0.5, 0.5))
plot(SAM_jit)
dev.off()

### ------------------------------------------------------------------------ ###
### ICES style summary plot ####
### ------------------------------------------------------------------------ ###

### convert SAM into FLStock
stk_new <- SAM2FLStock(SAM_fit)
### 1-year forecast
tab_fc <- print(SAM_fit_fc)

df_catch <- as.data.frame(FLQuants(Landings = landings(stk_new)/1000, 
                                   Discards = discards(stk_new)/1000)) %>%
  mutate(qname = factor(qname, levels = c("Discards", "Landings")))
p_catch <- df_catch %>%
  ggplot(aes(x = year, y = data, fill = qname)) +
  geom_col() +
  scale_fill_manual("", values = c(Landings = "#002b5f", 
                                   Discards = "#28b3e8")) +
  coord_cartesian(ylim = c(0, max(df_catch$data, na.rm = TRUE) * 1.05), 
                  xlim = c(min(df_catch$year, na.rm = TRUE) - 1,
                           max(df_catch$year, na.rm = TRUE) + 1), 
                  expand = FALSE) +
  labs(x = "", y = "Catches in 1000 t", title = "Catches") +
  theme_bw(base_size = 8) +
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.5, "lines"),
        plot.title = element_text(face = "bold", colour = "#002b5f"))
if (isTRUE(verbose)) p_catch
df_rec <- as.data.frame(
  rbind(rectable(SAM_fit)/1000,
        tab_fc[2, c("rec:median", "rec:low", "rec:high")]/1000))
rownames(df_rec)[length(rownames(df_rec))] <- SAM_fit_fc[[2]]$year
df_rec$year <- as.numeric(rownames(df_rec))
p_rec <- df_rec %>%
  filter(year < max(year)) %>%
  ggplot(aes(x = year, y = Estimate, fill = "Recruitment")) +
  geom_col() +
  scale_fill_manual("", values = c("#28b3e8")) +
  geom_col(data = df_rec %>%
             filter(year == max(year)),
           aes(x = year, y = Estimate), fill = "#92defb") +
  geom_errorbar(data = df_rec,
                aes(ymin = Low, ymax = High, x = year),
                size = 0.2) +
  coord_cartesian(ylim = c(0, max(df_rec$High, na.rm = TRUE) * 1.05),
                  xlim = c(min(df_rec$year, na.rm = TRUE) - 1,
                           max(df_rec$year, na.rm = TRUE) + 1),
                  expand = FALSE) +
  labs(x = "", y = "Recruitment in thousands", title = "Recruitment (age 2)") +
  theme_bw(base_size = 8) +
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.5, "lines"),
        plot.title = element_text(face = "bold", colour = "#28b3e8"))
if (isTRUE(verbose)) p_rec

df_fbar <- as.data.frame(fbartable(SAM_fit))
df_fbar$year <- as.numeric(rownames(df_fbar))
df_fbar_refs <- data.frame(ref = c("F[pa]", "F[lim]", "F[MSY]", "F[MSY]~(MSE)"),
                           val = c(0.392, 0.549, 0.241, 0.167))
p_fbar <- df_fbar %>%
  ggplot() +
  geom_ribbon(aes(x = year, y = Estimate, ymin = Low, ymax = High), 
              alpha = 0.7, fill = "#f2a497", show.legend = FALSE) +
  geom_line(aes(x = year, y = Estimate),
            color = "#ed6028") +
  geom_hline(data = df_fbar_refs, 
             aes(yintercept = val, linetype = ref, colour = ref, alpha = ref)) +
  scale_linetype_manual("", 
                        values = c("F[pa]" = 1111, 
                                   "F[lim]" = "dashed", 
                                   "F[MSY]" = "solid",
                                   "F[MSY]~(MSE)" = "dashed"),
                        labels = scales::parse_format()) + 
  scale_colour_manual("",
                      values = c("F[pa]" = "black",
                                 "F[lim]" = "black",
                                 "F[MSY]" = "#679dfe",
                                 "F[MSY]~(MSE)" = "#679dfe"),
                      labels = scales::parse_format()) +
  scale_alpha_manual("", 
                     values = c("F[pa]" = 1, 
                                "F[lim]" = 1, 
                                "F[MSY]" = 0.7,
                                "F[MSY]~(MSE)" = 0.7),
                     labels = scales::parse_format()) +
  coord_cartesian(ylim = c(0, max(0.9, 
                                  max(df_fbar$Estimate, na.rm = TRUE) * 1.05)), 
                  xlim = c(min(df_fbar$year, na.rm = TRUE) - 1,
                           max(df_fbar$year, na.rm = TRUE) + 1), 
                  expand = FALSE) +
  labs(x = "", y = "F (ages 3-6)", title = "Fishing pressure") +
  theme_bw(base_size = 8) +
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.key.height = unit(0.5, "lines"),
        plot.title = element_text(face = "bold", colour = "#ed6028"))
if (isTRUE(verbose)) p_fbar
df_ssb <- as.data.frame(
  rbind(ssbtable(SAM_fit)/1000,
        tab_fc[2, c("ssb:median", "ssb:low", "ssb:high")]/1000))
rownames(df_ssb)[length(rownames(df_ssb))] <- SAM_fit_fc[[2]]$year
df_ssb$year <- as.numeric(rownames(df_ssb))
df_ssb_refs <- data.frame(ref = c("B[pa]", "B[lim]", "MSY~B[trigger]"),
                          val = c(2950/1000, 2100/1000, 2950/1000))
p_ssb <- df_ssb %>%
  ggplot() +
  geom_ribbon(aes(x = year, y = Estimate, ymin = Low, ymax = High), 
              alpha = 0.7, fill = "#94b0a9", show.legend = FALSE) +
  geom_line(aes(x = year, y = Estimate),
            color = "#077c6c") +
  geom_hline(data = df_ssb_refs, 
             aes(yintercept = val, linetype = ref, colour = ref, alpha = ref)) +
  scale_linetype_manual("", 
                        values = c("B[pa]" = 1111, 
                                   "B[lim]" = "dashed", 
                                   "MSY~B[trigger]" = "solid"),
                        labels = scales::parse_format()) + 
  scale_colour_manual("",
                      values = c("B[pa]" = "black",
                                 "B[lim]" = "black",
                                 "MSY~B[trigger]" = "#679dfe"),
                      labels = scales::parse_format()) +
  scale_alpha_manual("", 
                     values = c("B[pa]" = 1, 
                                "B[lim]" = 1, 
                                "MSY~B[trigger]" = 0.7),
                     labels = scales::parse_format()) +
  coord_cartesian(ylim = c(0, max(df_ssb$High, na.rm = TRUE) * 1.05), 
                  xlim = c(min(df_ssb$year, na.rm = TRUE) - 1,
                           max(df_ssb$year, na.rm = TRUE) + 1), 
                  expand = FALSE) +
  labs(x = "", y = "SSB in 1000 t", title = "Spawning Stock Biomass") +
  theme_bw(base_size = 8) +
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.key.height = unit(0.5, "lines"),
        plot.title = element_text(face = "bold", colour = "#097e6e"))
if (isTRUE(verbose)) p_ssb
p <- (p_catch + p_rec)/(p_fbar + p_ssb)
if (isTRUE(verbose)) p
ggsave(file = "report/SAM/plots_SAM_results_ICES_style.png", plot = p,
       width = 15, height = 10, units = "cm", dpi = 300, type = "cairo")

### ------------------------------------------------------------------------ ###
### tables ####
### ------------------------------------------------------------------------ ###

### F at age
write.csv(faytable(SAM_fit), file = "report/SAM/SAM_table_fay.csv")

### N at age
write.csv(ntable(SAM_fit), file = "report/SAM/SAM_table_nay.csv")

### catch at age (modelled)
write.csv(catchtable(SAM_fit), file = "report/SAM/SAM_table_cay.csv")

