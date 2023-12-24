# -----------------------------------
# Analysis code for Southern giant petrel (Macronectes giganteus) predation on 
# living Magellanic penguins (Spheniscus magellanicus) 

# Code provided for peer review and is subject to change

# code author: Eric L. Wagner
# last updated: December 2023
# -----------------------------------

# Load libraries
library(rstanarm)
library(brms)
library(ggplot2)
library(ggnewscale)
library(RColorBrewer)
library(gridExtra)
library(ggpubr)

# Load data
peak1 <- read.csv("PetPeak.csv")
fledge <- read.csv("Fledge.csv")

# Bayesian Poisson GAMM for Days Since Sept 1 Petrel Counts
petrel.brm2 <- brm(Petrels ~ s(DaysSept1, bs = 'cc') + Time + (1|BookYear),
                   data = peak1,
                   family = poisson(), 
                   cores = 4,
                   iter = 5000, 
                   warmup = 1000, 
                   control = list(adapt_delta = 0.99, max_treedepth = 11))

summary(petrel.brm2)
launch_shinystan(petrel.brm2)

# To determine the breaks
conditional_smooths(petrel.brm2)
conditional_effects(petrel.brm2)

# Chick Fledging Weight v Days since 1 Sept. LMER
chick.weight.lmer <- stan_lmer(Weightkg ~ DaysSept1 + (1|BookYear),
                               data = fledge, 
                               prior = normal(0,5),
                               prior_intercept = normal(0,5),
                               prior_covariance = decov(),
                               adapt_delta = 0.99,
                               chains = 3, iter = 5000, warmup = 1000, cores = 3)

summary(chick.weight.lmer, prob = c(0.025, 0.5, 0.975), digits = 2)
launch_shinystan(chick.weight.lmer)
