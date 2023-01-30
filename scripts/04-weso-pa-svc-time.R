# western screech and barred owls

library(sf)
library(tidyverse)
library(sdmTMB)
library(visreg)
library(RColorBrewer)

proj_crs <- 32610


# dat <- readRDS("WSOW_with_effort_for_spp_T.rds") %>% select(-Hrs_scaled, -NHrs_scaled)
# dat <- readRDS("WSOW_with_all_effort_filled_in.rds") #%>% select(-Hrs_scaled, -NHrs_scaled)
# dat <- readRDS("data/WESO_with_forest_urban_mosaic.rds")

dat <- readRDS("data/WESO_with_landcover.rds")
## for some reason missing from orignal calculation
# dat <- dat %>% mutate(
#   urban_mean = mean(urban, na.rm = TRUE),
#   urban_sd = sd(urban, na.rm = TRUE),
#   urban_scaled = (urban - urban_mean)/urban_sd)
# saveRDS(dat, "data/WESO_with_landcover.rds")
dat <- dat %>% mutate(
  log_BADO = log(BADO_count +1),
  log_GHOW = log(GHOW_count +1),
  GHOW_mean = mean(GHOW_count, na.rm = TRUE),
  GHOW_sd = sd(GHOW_count, na.rm = TRUE),
  GHOW_scaled = (GHOW_count - GHOW_mean)/GHOW_sd)
saveRDS(dat, "data/WESO_with_landcover.rds")


# going to try a different projection
dat <- dat %>% dplyr::select(-x, -X, -y, -Y)

dat <- add_utm_columns(dat, ll_names = c("Longitude", "Latitude"), utm_crs = proj_crs, units = "km") # CRS = 32610

# add m for plotting
dat <- add_utm_columns(dat, ll_names = c("Longitude", "Latitude"), utm_crs = proj_crs, utm_names = c("x", "y"), units = "m") # CRS = 32610

mean(dat$X)
mean(dat$Y)

dat <- dat %>% mutate(X = (X- 600), Y = (Y-4800))

range(dat$X)
range(dat$Y)



## may use poly instead so these not required?
# dat$elev_scaled2 <- dat$elev_scaled^2
# dat$BADO_scaled2 <- dat$BADO_scaled^2
# dat$mosaic_scaled2 <- dat$mosaic_scaled^2
# dat$forest_scaled2 <- dat$forest_scaled^2
# dat$alltrees_scaled <- -dat$alltrees_scaled #don't know why -
# dat$alltrees_scaled2 <- dat$alltrees_scaled^2
# dat$urban2 <- dat$urban^2


dat <- dat %>% filter(!is.na(urban))
dat$year_f <- as.factor(dat$year)
dat$LocID <- as.factor(dat$LocID)
# dat <- dat1

mesh <- make_mesh(dat, xy_cols = c("X", "Y"), cutoff = 30)
plot(mesh)


# --- presence-absence model ####

# start with just effort variables
m1 <- sdmTMB(present ~ 1 +
               year_scaled +
               s(NDist_scaled)  +
               s(Dist_scaled) +
             s(FCounters_scaled),
             spatial_varying = ~ 0 + year_scaled,
             time = "year",
             spatial = "on",
             spatiotemporal = "off",
             family = binomial(link = "logit"),
             mesh = mesh,
             data = dat)
sanity(m1)
m1
# check effects, all essentially linear
visreg::visreg(m1, "NDist_scaled", scale = "response")
visreg::visreg(m1, "Dist_scaled", scale = "response")
visreg::visreg(m1, "FCounters_scaled", scale = "response")


# use linear effort and add habitat change
m2 <- update(m1, present ~ 1 +
               s(elev_scaled)  +
               s(forest_scaled) +
               s(urban_scaled) +
               year_scaled +
               NDist_scaled +
               Dist_scaled +
               FCounters_scaled)

sanity(m2)
m2

visreg::visreg(m2, "Dist_scaled", scale = "response")
visreg::visreg(m2, "elev_scaled", scale = "response")
visreg::visreg(m2, "urban_scaled", scale = "response")
visreg::visreg(m2, "forest_scaled", scale = "response")


# only elevation = nonlinear, urban and forest correlated so try separately, urban by itself not converging
m3 <- update(m1, present ~ 1 +
            s(elev_scaled)  +
            urban_scaled +
            forest_scaled +
            year_scaled +
            NDist_scaled +
            Dist_scaled +
            FCounters_scaled)

sanity(m3)
m3


# add other owls
m4 <- update(m1, present ~ 1 +
               s(log_GHOW) +
               s(log_BADO) +
               s(time_w_BADO_scaled) +
               s(elev_scaled)  +
               urban_scaled +
               forest_scaled +
               year_scaled +
               NDist_scaled +
               Dist_scaled +
               FCounters_scaled)


sanity(m4)
m4
visreg::visreg(m4, "log_GHOW", scale = "response")
visreg::visreg(m4, "log_BADO", scale = "response")
visreg::visreg(m4, "time_w_BADO_scaled", scale = "response")

# only BADO not having an effect, both other effect highly linear


# test interactions with time_w_BADO_scaled
m5 <- update(m1, present ~ 1 +
               log_GHOW +
               log_BADO +
               time_w_BADO_scaled +
               log_GHOW:time_w_BADO_scaled +
               log_BADO:time_w_BADO_scaled +
               s(elev_scaled) +
               urban +
               urban:time_w_BADO_scaled +
               forest +
               forest:time_w_BADO_scaled +
               year_scaled +
               NDist_scaled +
               Dist_scaled +
               FCounters_scaled)


sanity(m5)
m5

visreg::visreg2d(m5, "log_BADO", "time_w_BADO_scaled", scale = "response", col = colorRampPalette(brewer.pal(9,"Purples"))(5),
                 main = "Probability of detecting WESO",
                 xlab = "log(Barred Owl count +1)",
                 ylab = "Years since first Barred Owl")

visreg::visreg2d(m5, "log_GHOW", "time_w_BADO_scaled", scale = "response", col = colorRampPalette(brewer.pal(9,"Purples"))(5),
                 main = "Probability of detecting WESO",
                 xlab = "log(Great Horned Owl count +1)",
                 ylab = "Years since first Barred Owl")

visreg::visreg2d(m5, "forest", "time_w_BADO_scaled", scale = "response", col = colorRampPalette(brewer.pal(9,"Purples"))(5),
                 main = "Probability of detecting WESO",
                 xlab = "Proportion forest",
                 ylab = "Years since first Barred Owl")

visreg::visreg2d(m5, "urban", "time_w_BADO_scaled", scale = "response", col = colorRampPalette(brewer.pal(9,"Purples"))(5),
                 main = "Probability of detecting WESO",
                 xlab = "Proportion urban",
                 ylab = "Years since first Barred Owl"
)

# visreg::visreg(m5, "log_BADO", scale = "response") # included in interaction
visreg::visreg(m5, "elev_scaled", scale = "response")
visreg::visreg(m5, "NDist_scaled", scale = "response")
visreg::visreg(m5, "Dist_scaled", scale = "response")
visreg::visreg(m5, "FCounters_scaled", scale = "response")


# simplify model and remove scaling for ease of illustrating
m6 <- update(m1, present ~ 1 +
               log_BADO +
               log_GHOW + time_w_BADO +
               # GHOW_count*time_w_BADO +
               urban*time_w_BADO +
               # forest +
               # forest*time_w_BADO +
               s(elev_scaled)  +
               year_scaled +
               NDist_scaled +
               Dist_scaled +
               FCounters_scaled)


sanity(m6)
m6


visreg::visreg2d(m6, "log_GHOW", "time_w_BADO", scale = "response", col = colorRampPalette(brewer.pal(9,"Purples"))(5),
                 main = "Probability of detecting WESO",
                 xlab = "log(Great Horned Owl count +1)",
                 ylab = "Years since first Barred Owl")

visreg::visreg2d(m6, "log_BADO", "time_w_BADO", scale = "response", col = colorRampPalette(brewer.pal(9,"Purples"))(5),
                 main = "Probability of detecting WESO",
                 xlab = "log(Barred Owl count +1)",
                 ylab = "Years since first Barred Owl")

visreg::visreg2d(m6, "urban", "time_w_BADO", scale = "response", col = colorRampPalette(brewer.pal(9,"Purples"))(5),
                 main = "Probability of detecting WESO",
                 xlab = "Proportion urban",
                 ylab = "Years since first Barred Owl"
                 )

# # try removing the spatially varying field and adding a spatiotemporal one instead (so, don't need the linear year effect any more)
# m7 <- update(m6,
#              present ~ 1 +
#                log_BADO +
#                log_GHOW +
#                time_w_BADO +
#                urban*time_w_BADO +
#                s(elev_scaled)  +
#                # year_scaled +
#                # (1|year_f) + #doesn't converge
#                # s(year_scaled) + #doesn't converge
#                NDist_scaled +
#                Dist_scaled +
#                FCounters_scaled
#              ,
#              spatial_varying = NULL,
#              spatiotemporal = "RW")
# sanity(m7)
# m7
#
# visreg::visreg2d(m7, "log_GHOW", "time_w_BADO", scale = "response", col = colorRampPalette(brewer.pal(9,"Purples"))(5),
#                  main = "Probability of detecting WESO",
#                  xlab = "log(Great Horned Owl count + 1)",
#                  ylab = "Years since first Barred Owl")
# # visreg::visreg2d(m7, "forest", "time_w_BADO", scale = "response", col = colorRampPalette(brewer.pal(9,"Purples"))(5),
# #                  main = "Probability of detecting WESO",
# #                  xlab = "Proportion forest",
# #                  ylab = "Years since first Barred Owl")
# visreg::visreg2d(m7, "urban", "time_w_BADO", scale = "response", col = colorRampPalette(brewer.pal(9,"Purples"))(5),
#                  main = "Probability of detecting WESO",
#                  xlab = "Proportion urban",
#                  ylab = "Years since first Barred Owl"
# )


tidy(m5, "fixed", conf.int = T)
tidy(m6, "fixed", conf.int = T)
# tidy(m7, "fixed", conf.int = T)

# started with cutoff of 50, but range was estimating near 50, so dropped to 30
tidy(m5, "ran_pars", conf.int = T)
tidy(m6, "ran_pars", conf.int = T)
# tidy(m7, "ran_pars", conf.int = T)


saveRDS(m5, "data/WESO-presence-svc-time-no-st-full.rds")
saveRDS(m6, "data/WESO-presence-svc-time-no-st-final.rds")
# saveRDS(m7, "data/WESO-presence-rw-st-final.rds")



# check residuals


# warning: will take a couple minutes and requires rstan!
# see ?residuals.sdmTMB
set.seed(1)
dat$resid_mcmc <- residuals(
  m5,
  type = "mle-mcmc",
  mcmc_iter = 201,
  mcmc_warmup = 200
)
qqnorm(dat$resid_mcmc)
qqline(dat$resid_mcmc)


set.seed(1)
dat$resid_mcmc <- residuals(
  m6,
  type = "mle-mcmc",
  mcmc_iter = 201,
  mcmc_warmup = 200
)
qqnorm(dat$resid_mcmc)
qqline(dat$resid_mcmc)




