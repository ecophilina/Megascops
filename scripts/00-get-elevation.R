# get elevation

library(marmap)

lon_min <- -125
lon_max <- -113
lat_min <- 35
lat_max <- 60


proj_crs <- 32610

# use Albers projection for X Y coordinates
## same as grid used in https://doi.org/10.1002/ecs2.2707
## https://github.com/tmeeha/inlaSVCBC
# cbc_na_grid <- rgdal::readOGR(dsn="grid", layer="cbc_na_grid")
# Albers <- raster::crs(cbc_na_grid)
# CRS arguments:
# projection to match grid
# Albers <- "+proj=aea +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

# Download topo data and save on disk
topo <- getNOAA.bathy(lon_min-4, lon_max+4,
                      lat_min-4, lat_max+4, res = 1, keep = TRUE) #resolution of the grid, in minutes (default is 4 or > 4 miles)
# saveRDS(topo, "data/topo_res1_marmap.rds")


blues <- c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2", "lightsteelblue1")
greys <- c(grey(0.6), grey(0.93), grey(0.99))

# Plot
plot(topo, image = TRUE, land = TRUE, lwd = 0.1, bpal = list(c(0, max(topo), greys), c(min(topo), 0, blues)))

saveRDS(topo, "data/topo.rds")
rast_topo <- marmap::as.raster(topo)

# change projection to match grid

rast_topo <- raster::projectRaster(rast_topo, crs = proj_crs)

rast_topo[rast_topo < 0] <- NA

saveRDS(rast_topo, "data/rest_topo.rds")
plot(rast_topo)


# rast_topo2 <- marmap::as.raster(topo)
# rast_topo2 <- raster::projectRaster(rast_topo2, crs = Albers)
# rast_land_area <- rast_topo2
# rast_land_area[rast_topo2 <  0] <- 1
# rast_land_area[rast_topo2 >=  0] <- 0

# plot(rast_land_area)
