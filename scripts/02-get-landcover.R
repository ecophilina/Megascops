
### raster land cover data?
library(tidyverse)
library(stars)
library(raster)
# library(rgdal)
library(sf)


# to get for actual relevant sampled years (this is what has already been done)
# dat <- readRDS("data/WESO_with_all_effort_filled_in.rds") %>% filter(year > 1991 & year <= 2015)

# to get for all circles in species data file
dat <- readxl::read_excel("raw/PE-CBC_data 2/PE-CBC_Circle_Species_Report_SQL_updated.xlsx") %>%
  dplyr::select(LocID = Abbrev, Latitude, Longitude) %>% distinct()

# # or all circles ever
# dat <- readxl::read_excel("raw/PE-CBC_data 2/1-121-CBC_Count_History_Report.xlsx") %>%
#   dplyr::select(LocID = Abbrev, Latitude, Longitude) %>% distinct()
# dat <- dat %>% dplyr::filter(Latitude > 20, Longitude < -50)
#
# all_ids_sf <- st_as_sf(dat, coords = c("Longitude", "Latitude"))
# st_crs(all_ids_sf) <- 4326
# ggplot(all_ids_sf) + geom_sf()

# choose spatial extent of analysis
lon_min <- -125
lon_max < -113
lat_min <- 35
lat_max <- 60

dat <- dat %>% dplyr::filter(
  Longitude > lon_min & Longitude < lon_max &
    Latitude > lat_min & Latitude < lat_max)

# crop to the region of interest
dxy <- dat %>% mutate(
  x = Longitude,
  y = Latitude
)

e <- extent(dxy)

# add buffer for plotting and extracting whole count circles
e@xmin <- e@xmin - 2
e@xmax <- e@xmax + 2
e@ymin <- e@ymin - 2
e@ymax <- e@ymax + 2

p <- as(e, 'SpatialPolygons')

# years <- unique(dat$year)
years <-c(1992:2015)
# years <- c(1992, 2012)

newdat <- list()

for (i in seq_along(years)){
yr <- years[i]
# yr <- 2004
lc_yr <- raster(paste0("/Volumes/Herring1B/landcover/ESACCI-LC-L4-LCCS-Map-300m-P1Y-", yr, "-v2.0.7.tif"))

crs(p) <- crs(lc_yr)
lc_crop <- crop(lc_yr, p)


# plot(lc)
## if just getting for actual years sampled
# d <- dxy %>% filter(year %in% c(yr))

# to get for all years
d <- dxy %>% mutate(year = yr)

# note that both data sets are using lat lon still
pts <- d %>%
  dplyr::select(x,y)
pts <- as.data.frame(pts, xy = TRUE)

### MOSAIC and OPEN FOREST
fm <- lc_crop
# define all forest categories as 1 and others as 0
fm[!fm %in% c(100,110,62,72,82)] <- 0
fm[fm %in% c(100,110,62,72,82)] <- 1
# plot(lc)
# 24.14 km = 24140 m
# extract values from cells from circle with 24.14 km = 24140 m radius
mosaic <- raster::extract(fm, pts, buffer = 24140, fun = mean, na.rm = T, exact=T)
mosaic_sum <- raster::extract(fm, pts, buffer = 24140, fun = sum, na.rm = T, exact=T)

### FOREST
lc <- lc_crop
# define all forest categories as 1 and others as 0
lc[!lc %in% c(50,60,61,62,70,71,72,80,81,82,90)] <- 0
lc[lc %in% c(50,60,61,62,70,71,72,80,81,82,90)] <- 1
# plot(lc)
# 24.14 km = 24140 m
# extract values from cells from circle with 24.14 km = 24140 m radius
forest <- raster::extract(lc, pts, buffer = 24140, fun = mean, na.rm = T, exact=T)
forest_sum <- raster::extract(lc, pts, buffer = 24140, fun = sum, na.rm = T, exact=T)

### URBAN
urban <- lc_crop
urban[!urban %in% c(190)] <- 0
urban[urban %in% c(190)] <- 1
# plot(urban)
urban <- raster::extract(urban, pts, buffer = 24140, fun = mean, na.rm = T, exact=T)
# urban_sum <- raster::extract(urban, pts, buffer = 24140, fun = sum, na.rm = T, exact=T)
# NOT SURE WHY, BUT ABOVE ERRORS OUT.


### ALL TREES
trees <- lc_crop
# define all forest categories as 1 and others as 0
trees[!trees %in% c(100,110,50,60,61,62,70,71,72,80,81,82,90)] <- 0
trees[trees %in% c(100,110,50,60,61,62,70,71,72,80,81,82,90)] <- 1
# plot(lc)
# 24.14 km = 24140 m
# extract values from cells from circle with 24.14 km = 24140 m radius
alltrees <- raster::extract(trees, pts, buffer = 24140, fun = mean, na.rm = T, exact=T)
alltrees_sum <- raster::extract(trees, pts, buffer = 24140, fun = sum, na.rm = T, exact=T)


newdat[[i]] <- bind_cols(d,
                         forest = forest, forest_sum = forest_sum,
                         urban = urban, mosaic = mosaic,
                         alltrees = alltrees, alltrees_sum = alltrees_sum
                         )

}

alldat <- do.call(rbind, newdat)

# range(alldat$forest)
# range(alldat$forest_sum)
# range(alldat$urban)


dat <- alldat %>% mutate(
  urban_mean = mean(urban),
  urban_sd = sd(urban),
  urban_scaled = (urban - urban_mean)/urban_sd,
  forest_mean = mean(forest_sum),
  forest_sd = sd(forest_sum),
  forest_scaled = (forest_sum - forest_mean)/forest_sd,
  mosaic_mean = mean(mosaic),
  mosaic_sd = sd(mosaic),
  mosaic_scaled = (mosaic - mosaic_mean)/mosaic_sd
  )
# saveRDS(dat, "data/WESO_with_forest_urban.rds")


dat <- mutate(alldat,
              alltrees_mean = mean(alltrees),
              alltrees_sd = sd(alltrees),
              alltrees_scaled = (alltrees - alltrees_mean)/alltrees_sd
)

# saveRDS(dat, "data/WESO_with_alltrees.rds")

saveRDS(dat, "data/landcover_for_all_circles.rds")

### to avoid rerunning the above, will add the above to updated dataframe
#
# dat <- readRDS("data/WSOW_with_all_effort_filled_in.rds")
# lc_dat <- readRDS("data/WESO_with_alltrees.rds")
# lc_dat2 <- lc_dat %>% dplyr::select(LocID, year, (ncol(lc_dat)-14):ncol(lc_dat))
# dat <- left_join(dat, lc_dat2)
#
# saveRDS(dat, "data/WESO_with_landcover.rds")
