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

dat <- readRDS("data/WESO_with_landcover.rds")
## for some reason missing from orignal calculation
# dat <- dat %>% mutate(
#   urban_mean = mean(urban, na.rm = TRUE),
#   urban_sd = sd(urban, na.rm = TRUE),
#   urban_scaled = (urban - urban_mean)/urban_sd)
# saveRDS(dat, "data/WESO_with_landcover.rds")
# dat <- dat %>% mutate(
#   log_BADO = log(BADO_count +1),
#   log_GHOW = log(GHOW_count +1),
#   GHOW_mean = mean(GHOW_count, na.rm = TRUE),
#   GHOW_sd = sd(GHOW_count, na.rm = TRUE),
#   GHOW_scaled = (GHOW_count - GHOW_mean)/GHOW_sd)
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


dat <- dat %>% filter(!is.na(urban))
dat$year_f <- as.factor(dat$year)
dat$LocID <- as.factor(dat$LocID)

# dat$Lat_scaled <- scale(dat$Latitude)
# dat <- dat1

mesh <- make_mesh(dat, xy_cols = c("X", "Y"), cutoff = 30)
plot(mesh)

hist(dat$count)

# go straight to full model
mf <- sdmTMB(count ~ 1 +
               log_GHOW +
               log_BADO +
               # log_GHOW:log_BADO +
               time_w_BADO +
               # log_GHOW:time_w_BADO_scaled +
               log_BADO:time_w_BADO +
               s(elev_scaled, k = 3) +
               urban +
               urban:time_w_BADO +
               forest +
               forest:time_w_BADO +
               year_scaled +
               NDist_scaled +
               Dist_scaled +
               FCounters_scaled,
             spatial_varying = ~ 0 + year_scaled,
             time = "year",
             spatial = "on",
             spatiotemporal = "off",
             family = delta_gamma(),
             mesh = mesh,
             data = dat)
mf <- run_extra_optimization(mf)

sanity(mf)
mf

tidy(mf, "fixed", conf.int = T, model = 1)
tidy(mf, "fixed", conf.int = T, model = 2)


response_desc1 <- "Probability WESO present"
response_desc2 <- "WESO count when present"
col_scheme <- colorRampPalette(viridis::viridis(12))(5)

visreg2d_delta(mf, "log_BADO", "time_w_BADO", scale = "response", model = 2,
               col = col_scheme ,
                 main = response_desc2 ,
                 xlab = "log(Barred Owl count +1)",
                 ylab = "Years since first Barred Owl")

# visreg2d_delta(mf, "log_GHOW", "log_BADO", scale = "response", model = 2,
#                col = col_scheme ,
#                  main = response_desc ,
#                  xlab = "log(Great Horned Owl count +1)",
#                  ylab = "log(Barred Owl count +1)")

visreg2d_delta(mf, "forest", "time_w_BADO", scale = "response", model = 2,
               col = col_scheme ,
               main = response_desc2 ,
                 xlab = "Proportion forest",
                 ylab = "Years since first Barred Owl")

visreg2d_delta(mf, "urban", "time_w_BADO", scale = "response", model = 2,
               col = col_scheme ,
                 main = response_desc2,
                 xlab = "Proportion urban",
                 ylab = "Years since first Barred Owl"
)

visreg_delta(mf, "elev_scaled", scale = "response", ylab = response_desc1, model = 1)
visreg_delta(mf, "elev_scaled", scale = "response", ylab = response_desc2, model = 2)

visreg_delta(mf, "urban", scale = "response", ylab = response_desc1, model = 1)
visreg_delta(mf, "urban", scale = "response", ylab = response_desc2, model = 2)

visreg_delta(mf, "forest", scale = "response", ylab = response_desc1, model = 1)
visreg_delta(mf, "forest", scale = "response", ylab = response_desc2, model = 2)

visreg_delta(mf, "NDist_scaled", scale = "response", ylab = response_desc1, model = 1)
visreg_delta(mf, "NDist_scaled", scale = "response", ylab = response_desc2, model = 2)

visreg_delta(mf, "Dist_scaled", scale = "response", ylab = response_desc1, model = 1)
visreg_delta(mf, "Dist_scaled", scale = "response", ylab = response_desc2, model = 2)

visreg_delta(mf, "FCounters_scaled", scale = "response", ylab = response_desc1, model = 1)
visreg_delta(mf, "FCounters_scaled", scale = "response", ylab = response_desc2, model = 2)

visreg_delta(mf, "log_BADO", scale = "response", ylab = response_desc1, model = 1)
visreg_delta(mf, "log_BADO", scale = "response", ylab = response_desc2, model = 2) # included in interaction

visreg_delta(mf, "log_GHOW", scale = "response", ylab = response_desc1, model = 1)
visreg_delta(mf, "log_GHOW", scale = "response", ylab = response_desc2, model = 2)

visreg_delta(mf, "time_w_BADO", scale = "response", ylab = response_desc1, model = 1) # included in interaction
visreg_delta(mf, "time_w_BADO", scale = "response", ylab = response_desc2, model = 2) # included in interaction



# started with cutoff of 50, but range was estimating near 50, so dropped to 30
tidy(mf, "ran_pars", conf.int = T, model = 1)
tidy(mf, "ran_pars", conf.int = T, model = 2)


saveRDS(mf, "data/WESO-delta-svc-time-full.rds")



# check residuals
# warning: will take a couple minutes and requires rstan!
# see ?residuals.sdmTMB
set.seed(1)
dat$resid_mcmc <- residuals(
  mf,
  type = "mle-mcmc",
  mcmc_iter = 201,
  mcmc_warmup = 200
)
qqnorm(dat$resid_mcmc)
qqline(dat$resid_mcmc)

