# western screech and barred owls

# library(sf)
library(tidyverse)
library(sdmTMB)
library(visreg)
library(RColorBrewer)

proj_crs <- 32610


# dat <- readRDS("WSOW_with_effort_for_spp_T.rds") %>% select(-Hrs_scaled, -NHrs_scaled)
# dat <- readRDS("WSOW_with_all_effort_filled_in.rds") #%>% select(-Hrs_scaled, -NHrs_scaled)
# dat <- readRDS("data/WESO_with_forest_urban_mosaic.rds")

# dat <- readRDS("data/WESO_with_landcover.rds")
dat <- readRDS("data/WESO_with_all_effort_filled_in.rds")
## for some reason missing from orignal calculation
dat <- dat %>% mutate(
  log_BADO = log(BADO_count +1),
  log_GHOW = log(GHOW_count +1),
  GHOW_mean = mean(GHOW_count, na.rm = TRUE),
  GHOW_sd = sd(GHOW_count, na.rm = TRUE),
  GHOW_scaled = (GHOW_count - GHOW_mean)/GHOW_sd)
# saveRDS(dat, "data/WESO_with_landcover.rds")


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

# dat <- dat %>% filter(Longitude < -115)
dat <- dat %>% filter(!is.na(elev_scaled))
dat$year_f <- as.factor(dat$year)
dat$LocID <- as.factor(dat$LocID)

# dat$Lat_scaled <- scale(dat$Latitude)
# dat <- dat1

mesh <- make_mesh(dat, xy_cols = c("X", "Y"), cutoff = 30)
plot(mesh)

hist(dat$count)

# go straight to full model
mf <- sdmTMB(count ~ 1 +
               # log_GHOW * log_BADO * time_w_BADO +
               log_GHOW + log_BADO + time_w_BADO +
               log_GHOW:log_BADO +
               log_GHOW:time_w_BADO +
               log_BADO:time_w_BADO +
               s(elev_scaled, k = 3) +
               year_scaled +
               NDist_scaled +
               Dist_scaled +
               FCounters_scaled,
             spatial_varying = ~ 0 + year_scaled,
             time = "year",
             spatial = "on",
             # spatial = "off",
             spatiotemporal = "off",
             # spatiotemporal = "iid",
             # spatiotemporal = "rw",
             family = delta_gamma(),
             # silent = FALSE,
             mesh = mesh,
             data = dat)
mf <- run_extra_optimization(mf)
sanity(mf)
mf

# slightly simplified model
mf1 <- sdmTMB(count ~ 1 +
               log_GHOW +
               log_BADO +
               # log_GHOW:log_BADO +
               time_w_BADO +
               log_GHOW:time_w_BADO +
               log_BADO:time_w_BADO +
               s(elev_scaled, k = 3) +
               year_scaled +
               NDist_scaled +
               Dist_scaled +
               FCounters_scaled,
              spatial_varying = ~ 0 + year_scaled,
              time = "year",
              spatial = "on",
              # spatial = "off",
              spatiotemporal = "off",
              # spatiotemporal = "iid",
              # spatiotemporal = "rw",
              family = delta_gamma(),
              # silent = FALSE,
              mesh = mesh,
              data = dat)
sanity(mf1)
mf1 <- run_extra_optimization(mf1)


# slightly simplified model
mf2 <- sdmTMB(count ~ 1 +
               log_GHOW +
               log_BADO +
               # log_GHOW:log_BADO +
               time_w_BADO +
               log_GHOW:time_w_BADO +
               # log_BADO:time_w_BADO +
               s(elev_scaled, k = 3) +
               year_scaled +
               NDist_scaled +
               Dist_scaled +
               FCounters_scaled,
              spatial_varying = ~ 0 + year_scaled,
              time = "year",
              spatial = "on",
              # spatial = "off",
              spatiotemporal = "off",
              # spatiotemporal = "iid",
              # spatiotemporal = "rw",
              family = delta_gamma(),
              # silent = FALSE,
              mesh = mesh,
              data = dat)
mf2 <- run_extra_optimization(mf2)
sanity(mf2)

mf3 <- sdmTMB( count ~ 1 +
                log_GHOW +
                log_BADO +
                time_w_BADO +
                s(elev_scaled, k = 3) +
                year_scaled +
                NDist_scaled +
                Dist_scaled +
                FCounters_scaled,
               spatial_varying = ~ 0 + year_scaled,
               time = "year",
               spatial = "on",
               # spatial = "off",
               spatiotemporal = "off",
               # spatiotemporal = "iid",
               # spatiotemporal = "rw",
               family = delta_gamma(),
               # silent = FALSE,
               mesh = mesh,
               data = dat)

mf3 <- run_extra_optimization(mf3)
sanity(mf3)


# dat$log_total_BADO <- log(dat$total_BADO +1)
#
# # total BADO model
# mf4 <- sdmTMB( count ~ 1 +
#                 log_GHOW +
#                 log_total_BADO +
#                 time_w_BADO +
#                 log_total_BADO:time_w_BADO +
#                 log_GHOW:time_w_BADO +
#                 log_GHOW:log_total_BADO +
#                 s(elev_scaled, k = 3) +
#                 year_scaled +
#                 NDist_scaled +
#                 Dist_scaled +
#                 FCounters_scaled,
#                spatial_varying = ~ 0 + year_scaled,
#                time = "year",
#                spatial = "on",
#                # spatial = "off",
#                spatiotemporal = "off",
#                # spatiotemporal = "iid",
#                # spatiotemporal = "rw",
#                family = delta_gamma(),
#                # silent = FALSE,
#                mesh = mesh,
#                data = dat)
# mf4 <- run_extra_optimization(mf4)
# sanity(mf4)

# dat$log_total_BADO <- log(dat$total_BADO +1)
#
# no svc, iid (non-linear didn't fit)
mf7 <- sdmTMB( count ~ 1 +
                 log_GHOW +
                 log_BADO +
                 time_w_BADO +
                 log_GHOW:log_BADO +
                 # log_GHOW:time_w_BADO +
                 # log_BADO:time_w_BADO +
                s(elev_scaled, k = 3) +
                # year_scaled +
                NDist_scaled +
                Dist_scaled +
                FCounters_scaled,
               # spatial_varying = ~ 0 + time_w_BADO,
               time = "year",
               spatial = "on",
               # spatial = "off",
               # spatiotemporal = "off",
               spatiotemporal = "iid",
               share_range = FALSE,
               # spatiotemporal = "rw",
               family = delta_gamma(),
               # silent = FALSE,
               mesh = mesh,
               data = dat)
mf6 <- run_extra_optimization(mf6)
sanity(mf7)
mf7

# saveRDS(mf6, "data/WESO-delta-no-landcover-twb-svc-fullish.rds")
saveRDS(mf5, "data/WESO-delta-no-landcover-no-svc-fullish.rds")
AIC(mf5, mf6, mf7)

AIC(mf, # full model
    # mf4, # replace BADO with total BADO -- no improvement
    # mf5,
    # mf1, mf2,
    mf3) # simplified

mf3

tidy(mf, "fixed", conf.int = T, model = 1)
tidy(mf1, "fixed", conf.int = T, model = 1)
tidy(mf2, "fixed", conf.int = T, model = 1)
tidy(mf3, "fixed", conf.int = T, model = 1)
tidy(mf3, "fixed", conf.int = T, model = 2)
tidy(mf5, "fixed", conf.int = T, model = 2)

response_desc1 <- "Probability WESO present"
response_desc2 <- "WESO count when present"
col_scheme <- colorRampPalette(viridis::viridis(12))(5)

# visreg2d_delta(mf, "log_BADO", "time_w_BADO", scale = "response", model = 2,
#                col = col_scheme ,
#                  main = response_desc2 ,
#                  xlab = "log(Barred Owl count +1)",
#                  ylab = "Years since first Barred Owl")
#
# visreg2d_delta(mf3, "log_GHOW", "time_w_BADO", scale = "response", model = 2,
#                col = col_scheme ,
#                main = response_desc2 ,
#                xlab = "log(Great Horned Owl count +1)",
#                ylab = "Years since first Barred Owl")


# visreg2d_delta(mf, "log_GHOW", "log_BADO", scale = "response", model = 2,
#                col = col_scheme ,
#                  main = response_desc ,
#                  xlab = "log(Great Horned Owl count +1)",
#                  ylab = "log(Barred Owl count +1)")


visreg_delta(mf3, "elev_scaled", scale = "response", ylab = response_desc1, model = 1)
visreg_delta(mf3, "elev_scaled", scale = "response", ylab = response_desc2, model = 2)

visreg_delta(mf3, "NDist_scaled", scale = "response", ylab = response_desc1, model = 1)
visreg_delta(mf3, "NDist_scaled", scale = "response", ylab = response_desc2, model = 2)

visreg_delta(mf3, "Dist_scaled", scale = "response", ylab = response_desc1, model = 1)
visreg_delta(mf3, "Dist_scaled", scale = "response", ylab = response_desc2, model = 2)

visreg_delta(mf3, "FCounters_scaled", scale = "response", ylab = response_desc1, model = 1)
visreg_delta(mf3, "FCounters_scaled", scale = "response", ylab = response_desc2, model = 2)

visreg_delta(mf3, "log_BADO", scale = "response", ylab = response_desc1, model = 1)
visreg_delta(mf3, "log_BADO", scale = "response", ylab = response_desc2, model = 2)

visreg_delta(mf3, "log_GHOW", scale = "response", ylab = response_desc1, model = 1)
visreg_delta(mf3, "log_GHOW", scale = "response", ylab = response_desc2, model = 2)

visreg_delta(mf5, "time_w_BADO", scale = "response", ylab = response_desc1, model = 1)
visreg_delta(mf3, "time_w_BADO", scale = "response", ylab = response_desc2, model = 2)



response_desc1 <- "Probability present"
response_desc2 <- "Abundance when present"
col_scheme <- colorRampPalette(viridis::viridis(12))(5)
visreg2d_delta(mf5, "log_BADO", "log_GHOW", scale = "response", model = 1,
               col = col_scheme,
               main = response_desc1,
               ylab = "log(Great Horned Owl count +1)",
               xlab = "log(Barred Owl + 1)")

visreg2d_delta(mf5, "log_BADO", "log_GHOW", scale = "response", model = 2,
               col = col_scheme,
               main = response_desc2,
               ylab = "log(Great Horned Owl count +1)",
               xlab = "log(Barred Owl + 1)")



# started with cutoff of 50, but range was estimating near 50, so dropped to 30
tidy(mf3, "ran_pars", conf.int = T, model = 1)
tidy(mf3, "ran_pars", conf.int = T, model = 2)


saveRDS(mf3, "data/WESO-delta-no-landcover-top.rds")


# check residuals
# warning: will take a couple minutes and requires rstan!
# see ?residuals.sdmTMB
set.seed(1)
dat$resid_mcmc <- residuals(
  mf5,
  type = "mle-mcmc",
  mcmc_iter = 201,
  mcmc_warmup = 200
)
qqnorm(dat$resid_mcmc)
qqline(dat$resid_mcmc)

