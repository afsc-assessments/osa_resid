## see
## https://github.com/fishfollower/compResidual#composition-residuals
## for installation instructions

## TMB:::install.contrib("https://github.com/vtrijoulet/OSA_multivariate_dists/archive/main.zip")
## devtools::install_github("fishfollower/compResidual/compResidual")

library(compResidual)
library(ggplot2)
library(cowplot)
library(reshape2)
theme_set(theme_bw())
library(here)
source(here::here("R", "plot_osa_comps.R"))


## example of how it works on simulated data (help file)
o <- rmultinom(100, 25, c(.2,.2,.1,.1,.1,.3))
p <- matrix(rep(25*c(.2,.2,.1,.1,.1,.3), 100), nrow=6)
res <- resMulti(o,p)
plot(res)
colSums(o)
colSums(p)

## GOA pollock info
repfile <- readRDS(here::here('data', 'repfile.RDS'))
datfile <- readRDS(here::here('data', 'datfile.RDS'))


ages <- 3:10
o <- repfile$Survey_1_observed_and_expected_age_comp[,ages]
p <- repfile$Survey_1_observed_and_expected_age_comp[,10+ages]
pearson <- repfile$Survey_1_Pearson_residuals_age_comp[,ages]
years <- datfile$srv_acyrs1
Neff <- datfile$multN_srv1 # this gets rounded
plot_osa_comps(o,p, pearson, ages=ages, years=years, Neff=Neff,
               stock='GOApollock', survey='shelikof')

## obs and expected (p), rows=years, columns=ages
ages <- 1:10
o <- repfile$Survey_2_observed_and_expected_age_comp[,ages]
p <- repfile$Survey_2_observed_and_expected_age_comp[,10+ages]
pearson <- repfile$Survey_2_Pearson_residuals_age_comp[,ages]
years <- datfile$srv_acyrs2
Neff <- datfile$multN_srv2 # this gets rounded
plot_osa_comps(o,p, pearson, ages=ages, years=years, Neff=Neff,
               stock='GOApollock', survey='nmfs_bt')

ages <- 1:10
o <- repfile$Survey_3_observed_and_expected_age_comp[,ages]
p <- repfile$Survey_3_observed_and_expected_age_comp[,10+ages]
pearson <- repfile$Survey_3_Pearson_residuals_age_comp[,ages]
years <- datfile$srv_acyrs3
Neff <- datfile$multN_srv3 # this gets rounded
plot_osa_comps(o,p, pearson, ages=ages, years=years, Neff=Neff,
               stock='GOApollock', survey='adfg')

ages <- 1:10
o <- repfile$Survey_6_observed_and_expected_age_comp[,ages]
p <- repfile$Survey_6_observed_and_expected_age_comp[,10+ages]
pearson <- repfile$Survey_6_Pearson_residuals_age_comp[,ages]
years <- datfile$srv_acyrs6
Neff <- datfile$multN_srv6 # this gets rounded
plot_osa_comps(o,p, pearson, ages=ages, years=years, Neff=Neff,
               stock='GOApollock', survey='summer_at')

ages <- 1:10
o <- repfile$Fishery_observed_and_expected_age_comp[,ages]
p <- repfile$Fishery_observed_and_expected_age_comp[,10+ages]
pearson <- repfile$Fishery_Pearson_residuals_age_comp[,ages]
years <- datfile$fshyrs
Neff <- datfile$multN_fsh
plot_osa_comps(o,p, pearson, ages=ages, years=years, Neff=Neff,
               stock='GOApollock', survey='fishery')

resMulti(t(o[1:2,]), t(p[1:2,]))
resMulti(t(o[1:2,]), t(p[1:2,]))

