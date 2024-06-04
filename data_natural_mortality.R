### ------------------------------------------------------------------------ ###
### natural mortality M ####
### ------------------------------------------------------------------------ ###

mkdir("data/M")
mkdir("data/M/plots")

library(icesTAF)
taf.libPaths()
library(tidyr)
library(dplyr)
library(ggplot2)

if (!exists("verbose")) verbose <- FALSE

### ------------------------------------------------------------------------ ###
### M at age estimators ####
### ------------------------------------------------------------------------ ###

### get von Bertalanffy growth parameters
vB_pars <- read.csv("data/ALKs/vB_pars_q.csv")
### use values estimated with last five years of data
vB_Linf <- tail(vB_pars$Linf, 1)
vB_k <- tail(vB_pars$k, 1)
vB_t0 <- tail(vB_pars$t0, 1)

### von Bertalanffy growth function
vB_length <- function(L_inf, k, t, t_0 = 0){
  L_inf * (1 - exp(-k * (t - t_0)))
}

### Gislason et al. (2010)
### https://doi.org/10.1111/j.1467-2979.2009.00350.x
M_Gislason <- function(L, Linf, k) {
  exp(0.55 - 1.61*log(L) + 1.44*log(Linf) + log(k))
}

### Lorenzen et al. (2022)
### https://doi.org/10.1016/j.fishres.2022.106327
M_Lorenzen <- function(L, Linf, k) {
  exp(0.28 - 1.30*log(L/Linf) + 1.08*log(k))
}
M_Lorenzen_Linf <- function(Linf, k, ...) {
  exp(0.65 + 0.87*log(k))
}
M_Lorenzen_M1 <- function(Linf, k, ...) {
  exp(0.65 + 0.91*log(Linf) + 0.87*log(k))
}
if (isTRUE(verbose)) M_Lorenzen_Linf(Linf = vB_Linf, k = vB_k)
if (isTRUE(verbose)) M_Lorenzen_M1(Linf = vB_Linf, k = vB_k)

# M1 <- M_Lorenzen_M1(Linf = vB_Linf, k = vB_k)
# L1 <- 1
# 
# MLinf <- M_Lorenzen_Linf(Linf = vB_Linf, k = vB_k)
# Linf <- vB_Linf
# 
# df <- data.frame(log_L = c(log(1/vB_Linf), log(vB_Linf)),
#                  log_M = c(log(M1), log(MLinf)))
# 
# lm(data = df, formula = log_M ~ log_L)

### Then et al. 2015
### https://doi.org/10.1093/icesjms/fsu136
M_Then <- function(Linf, k) {
  4.118*(k^0.73)*(Linf^(-0.33))
}
if (isTRUE(verbose)) M_Then(Linf = vB_Linf, k = vB_k)

### table with Ms
ages <- seq(1, 15.5, 0.1)
lengths <- vB_length(L_inf = vB_Linf, k = vB_k, t_0 = vB_t0, t = ages)
df_M <- data.frame(
  age = ages,
  M_default = 0.12,
  M_Gislason = M_Gislason(L = lengths, Linf = vB_Linf, k = vB_k),
  M_Lorenzen_L = M_Lorenzen(L = lengths, Linf = vB_Linf, k = vB_k),
  M_Lorenzen_Linf = M_Lorenzen_Linf(Linf = vB_Linf, k = vB_k),
  Then = M_Then(Linf = vB_Linf, k = vB_k)
  )
p <- df_M %>%
  pivot_longer(-1) %>%
  mutate(name = gsub(x = name, pattern = "M_", replacement = "")) %>%
  mutate(name = factor(name, 
                       levels = c("default", "Gislason", "Lorenzen_L",
                                  "Lorenzen_Linf", "Then")
         )) %>%
  ggplot(aes(x = age, y = value, colour = name, linetype = name)) +
  geom_line() +
  geom_point(data = . %>% 
               filter(age %% 0.5 == 0 & age %% 1 != 0) %>%
               filter(name %in% c("Gislason", "Lorenzen_L")),
             size = 0.6, show.legend = FALSE) + 
  scale_colour_discrete("", labels = c("default", "Gislason",
                                       "Lorenzen", 
                                       expression(Lorenzen~M[L==L[infinity]]),
                                       "Then")) + 
  scale_linetype("", labels = c("default", "Gislason",
                                "Lorenzen", 
                                expression(Lorenzen~M[L==L[infinity]]),
                                "Then")) + 
  xlim(c(0, NA)) +
  ylim(c(0, 0.75)) + 
  labs(x = "Age (years)", y = "Natural mortality M (per year)") +
  theme_bw(base_size = 8) +
  theme(legend.key.height = unit(0.5, "lines"))
if (isTRUE(verbose)) p
ggsave("data/M/plots/M_options.png", 
       width = 15, height = 10, units = "cm", dpi = 300, plot = p)

