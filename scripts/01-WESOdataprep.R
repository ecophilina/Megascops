# western screech and barred owls

library(readxl)
library(tidyverse)

library(rgdal)
library(sp)
# library(spdep)
library(sf)

# choose spatial extent of analysis
lon_min <- -125
lon_max < -113
lat_min <- 35
lat_max <- 60

# use Albers projection for X Y coordinates
## same as grid used in https://doi.org/10.1002/ecs2.2707
## https://github.com/tmeeha/inlaSVCBC
# cbc_na_grid <- rgdal::readOGR(dsn="grid", layer="cbc_na_grid")
# Albers <- raster::crs(cbc_na_grid)
# CRS arguments:
Albers <- "+proj=aea +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"


#### --- get data ####

d <- read_excel("raw/PE-CBC_data 2/1-121-CBC_Count_History_Report.xlsx") %>%
  select(LocID = Abbrev, Latitude, Longitude, surveyID = Count_yr, spp_count = TotalSpecies) %>%
  filter(surveyID > 82) %>%
  mutate(year = ifelse(surveyID <= 100, paste0(19, surveyID-1),
                       ifelse(surveyID <= 110, paste0(200, surveyID-101), paste0(20, surveyID-101))),
         year_f = as.factor(year))

a <- read_excel("raw/PE-CBC_data 2/PE-CBC_Circle_Species_Report_SQL_updated.xlsx") %>%
  select(LocID = Abbrev, Species = COM_NAME, Latitude, Longitude, surveyID = Count_yr,
  count = how_many, spp_count2 = TotalSpecies) %>%
  mutate(year = ifelse(surveyID <= 100, paste0(19, surveyID-1),
                       ifelse(surveyID <= 110, paste0(200, surveyID-101), paste0(20, surveyID-101)))) %>%
  filter(Longitude > lon_min & Longitude < lon_max & Latitude > lat_min & Latitude < lat_max)

e <- read_excel("raw/PE-CBC_data 2/PE-CBC_Effort_Report_SQL_updated-1.xlsx") %>%
  select(LocID = Abbrev, surveyID = Count_yr, Nocturnal_hrs, Nocturnal_distance, Field_counters) %>%
  filter(surveyID > 82) %>%
  mutate(year = ifelse(surveyID <= 100, paste0(19, surveyID-1),
                       ifelse(surveyID <= 110, paste0(200, surveyID-101), paste0(20, surveyID-101))))


## these species will need effort data
# before loading this, second description column needs relabeling "units"
e2 <- read_excel("raw/PE-CBC_data 2/PE-CBC_Effort_Report_SQL_updated-2.xlsx") %>%
  filter(Description == "foot") %>%
  # group_by(Abbrev, Count_yr, units) %>% summarise(Distance = sum(Distance), Hours = sum(Hours)) %>%
  mutate(Distance = ifelse(units == "Miles", Distance*1.60934, Distance)) %>%
  select(LocID = Abbrev, surveyID = Count_yr, Distance, Hours) %>%
  filter(surveyID > 82) %>%
  mutate(year = ifelse(surveyID <= 100, paste0(19, surveyID-1),
                       ifelse(surveyID <= 110, paste0(200, surveyID-101), paste0(20, surveyID-101))))


## adding screech-owl sp. didn't change anything, but keeping here in case expanding spatial extent made it useful
w <- filter(a, Species == c("Western Screech-Owl", "screech-owl sp."))

# select locations with at least one record (includes 0 that represent count week presence)
w_loc <- w %>% select(LocID) %>%
  # keep only circles with records in more than 3 years
  group_by(LocID) %>% mutate(n = n()) %>% filter(n > 1)

# get all years with records for these circles
dw <- filter(d, LocID %in% c(unique(w_loc$LocID)))

dw <- left_join(dw, e) %>% left_join(., e2)

# # check and scale effort data
# ggplot(dw, aes(Field_counters, Hours, colour = as.integer(year))) +
#   geom_point(alpha = 0.75) + geom_smooth(method = "lm") + scale_colour_viridis_c(direction = -1)
# # Very correlated!
#
# ggplot(dw, aes(Field_counters, Distance, colour = as.integer(year))) +
#   geom_point(alpha = 0.75) + geom_smooth(method = "lm") + scale_colour_viridis_c(direction = -1)
# # Very correlated!
#
# ggplot(dw, aes(Hours, Nocturnal_hrs, colour = as.integer(year))) +
#   geom_point(alpha = 0.75) + geom_smooth(method = "lm") + scale_colour_viridis_c(direction = -1)
# # somewhat correlated with eachother
#
# ggplot(dw, aes(Nocturnal_hrs, Nocturnal_distance, colour = as.integer(year))) +
#   geom_point(alpha = 0.75) + geom_smooth(method = "lm") + scale_colour_viridis_c(direction = -1)
# # pretty correlated with eachother
#
# ggplot(dw, aes(Field_counters, Nocturnal_hrs, colour = as.integer(year))) +
#   geom_point(alpha = 0.75) + geom_smooth(method = "lm") + scale_colour_viridis_c(direction = -1)
# # somewhat correlated with eachother
#
# ggplot(dw, aes(Field_counters, Nocturnal_distance, colour = as.integer(year))) +
#   geom_point(alpha = 0.75) + geom_smooth(method = "lm") + scale_colour_viridis_c(direction = -1)
# # Not correlated, USE both!
#
# ggplot(dw, aes(Hours, Nocturnal_distance, colour = as.integer(year))) +
#   geom_point(alpha = 0.75) + geom_smooth(method = "lm") + scale_colour_viridis_c(direction = -1)
# # could use both
#
# ggplot(dw, aes(log(Distance+1), Nocturnal_distance, colour = as.integer(year))) +
#   geom_point(alpha = 0.75) + geom_smooth(method = "lm") + scale_colour_viridis_c(direction = -1)
# # could use both
# # choose to use distance over hrs because probably more representative of opportunity to find daytime owls
#
# hist(dw$Nocturnal_hrs)
# hist(log(dw$Nocturnal_hrs+1))
# hist(dw$Nocturnal_distance)
# hist(log(dw$Nocturnal_distance+1))
# hist(dw$Distance)
# hist(log(dw$Distance+1)) # worse if both car and foot, better with foot only
# hist(dw$Hours)
# hist(log(dw$Hours+1)) # worse if both car and foot, better with foot only
# hist(dw$Field_counters)
# hist(log(dw$Field_counters+1))


# dw[dw$spp_count == TRUE & is.na(dw$Field_counters), ]

dw2 <- dw %>% mutate(
  # Adjust outliers and some missing data...
  Field_counters2 = log(ifelse(spp_count == TRUE & is.na(Field_counters), 9 , Field_counters) + 1),
  ## combined foot and car = two outliers adjusted to just above all other values
  # Distance2 = (ifelse(Distance>2100, 2100, Distance)),
  ## just foot still has two outliers, adjusted to just above all other values
  Distance2 = log(ifelse(Distance>1000, 1000, Distance)+1),
  ## combined foot and car = outlier appears to be a typo
  # Hours2 = (ifelse(Hours>600,60, Hours)),
  Hours2 = log(Hours + 1), # when just using time on foot
  Nocturnal_hrs2 = log(ifelse(Nocturnal_hrs>40,40, Nocturnal_hrs) + 1), # 1 outlier adjusted to just above all other values
  Nocturnal_distance2 = log(Nocturnal_distance + 1),
  # Calculate global means on adjusted values
  FCounters_mean = mean(log(Field_counters + 1), na.rm = T), # not using filled in data here
  Dist_mean = mean(Distance2, na.rm = T),
  Hrs_mean = mean(Hours2, na.rm = T),
  NDist_mean = mean(Nocturnal_distance2, na.rm = T),
  NHrs_mean = mean(Nocturnal_hrs2, na.rm = T),
  # Calculate global SDs on adjusted values
  FCounters_sd = sd(log(Field_counters + 1), na.rm = T), # not using filled in data here
  Dist_sd = sd(Distance2, na.rm = T),
  Hrs_sd = sd(Hours2, na.rm = T),
  NDist_sd = sd(Nocturnal_distance, na.rm = T),
  NHrs_sd = sd(Nocturnal_hrs, na.rm = T),
  # Scaled variables
  FCounters_scaled = (Field_counters2 - FCounters_mean)/FCounters_sd,
  Dist_scaled = (Distance2 - Dist_mean)/Dist_sd,
  Hrs_scaled = (Hours2 - Hrs_mean)/Hrs_sd,
  NDist_scaled = (Nocturnal_distance2 - NDist_mean)/NDist_sd,
  NHrs_scaled = (Nocturnal_hrs2 - NHrs_mean)/NHrs_sd,
)

dw3 <- filter(dw2, Field_counters < 11)

# dw4 <- dw2 %>% mutate(
#   # Use means for from low effort counts to fill in effort for counts with TRUE spp_counts but no effort data
#   FCounters_scaled = ifelse(spp_count == TRUE & is.na(FCounters_scaled), mean(dw3$FCounters_scaled,na.rm = T), FCounters_scaled),
#   Dist_scaled = ifelse(spp_count == TRUE & is.na(Dist_scaled), mean(dw3$Dist_scaled,na.rm = T), Dist_scaled),
#   Hrs_scaled = ifelse(spp_count == TRUE & is.na(Hrs_scaled), mean(dw3$Hrs_scaled,na.rm = T), Hrs_scaled),
#   NDist_scaled = ifelse(spp_count == TRUE & is.na(NDist_scaled), mean(dw3$NDist_scaled,na.rm = T), NDist_scaled),
#   NHrs_scaled = ifelse(spp_count == TRUE & is.na(NHrs_scaled), mean(dw3$NHrs_scaled,na.rm = T), NHrs_scaled)
# )

# OR use means for from low effort counts to fill in all missing effort data
dw4 <- dw2 %>% mutate(
  FCounters_scaled = ifelse(is.na(FCounters_scaled), mean(dw3$FCounters_scaled,na.rm = T), FCounters_scaled),
  Dist_scaled = ifelse(is.na(Dist_scaled), mean(dw3$Dist_scaled,na.rm = T), Dist_scaled),
  Hrs_scaled = ifelse(is.na(Hrs_scaled), mean(dw3$Hrs_scaled,na.rm = T), Hrs_scaled),
  NDist_scaled = ifelse(is.na(NDist_scaled), mean(dw3$NDist_scaled,na.rm = T), NDist_scaled),
  NHrs_scaled = ifelse(is.na(NHrs_scaled), mean(dw3$NHrs_scaled,na.rm = T), NHrs_scaled)
)

# merge on owl data
w <- left_join(dw4, w)%>%
  mutate(lat = Latitude, lon = Longitude)
w$count[is.na(w$count)] <- 0


# ADD OTHER SPECIES OF OWLS --------------------------

b <- filter(a, Species == "Barred Owl") %>%
  mutate(Species2 = Species, BADO_count = count) %>% select(-spp_count2, -Species, -count)
b <- left_join(w, b)%>%
  mutate(lat = Latitude, lon = Longitude)
b$BADO_count[is.na(b$BADO_count)] <- 0

ggplot(b, aes(Nocturnal_distance, BADO_count)) + geom_point() + geom_smooth(method = "lm")
ggplot(b, aes(BADO_count, log(count+1))) + geom_point() + geom_smooth(method = "lm")


g <- filter(a, Species == "Great Horned Owl") %>%
  mutate(Species3 = Species, GHOW_count = count) %>% select(-spp_count2, -Species, -count)
b <- left_join(b, g)%>%
  mutate(lat = Latitude, lon = Longitude)
b$GHOW_count[is.na(b$GHOW_count)] <- 0

ggplot(b, aes(Nocturnal_distance, GHOW_count)) + geom_point() + geom_smooth(method = "lm")
ggplot(b, aes(GHOW_count, log(count+1))) + geom_point() + geom_smooth(method = "lm")


# convert to sf object
b <- st_as_sf(b, coords = c("lon", "lat"))
st_crs(b) <- 4326

# plot with lat lon
ggplot(b) +
  # geom_sf(data = world) + # can't recall where this is from
  geom_sf(aes(size = count, colour = count), alpha = 0.1) +
  coord_sf(xlim = c(-125,-100), ylim = c(30,50)) +
  scale_colour_viridis_c(option = "C", direction = 1) +
  labs(fill = "Count") +
  facet_wrap(~year) +
  theme_light()

# get xy in albers to match grid and topo raster
bxy <- b %>% sf::st_transform(crs = Albers) %>%
  sf::st_coordinates() %>%
  as.data.frame()
b <- bind_cols(b, bxy)

# select relevant variables and scale X and Y for mesh
bdat <- b %>%
  # select(X, Y, count, BADO_count, year, LocID, Latitude, Longitude, Nocturnal_distance, Field_counters, Distance, Hours) %>%
  select(-Species, -Species2, -spp_count, -spp_count2) %>%
  mutate(year = as.integer(year),
         year_f = as.factor(year),
         year_median = median(as.integer(unique(b$year))),
         year_sd = sd(as.integer(unique(b$year))),
         year_scaled = (year - year_median)/year_sd,
         BADO_sd = sd(sqrt(BADO_count)),
         BADO_mean = mean(sqrt(BADO_count)),
         BADO_scaled = (sqrt(BADO_count)-1) /BADO_sd, # use BADO = 1 as intercept instead of the mean
         X = X / 100000,
         Y = Y / 100000
  )

# remove geometry
st_geometry(bdat) <- NULL

# calculate first year of barred owls
b_time <- bdat %>% filter(BADO_count > 0) %>% group_by(LocID) %>%
  mutate(first_BADO1 = min(as.integer(year), na.rm = T)) %>%
  ungroup() %>% select(LocID, year, first_BADO1)

bdat <- left_join(bdat, b_time)

bdat$first_BADO1[is.na(bdat$first_BADO1)] <- max(as.integer(bdat$year))

bdat <- bdat %>% group_by(LocID) %>% arrange(year) %>%
  mutate(total_BADO = cumsum(BADO_count),
         first_BADO = min(first_BADO1),
         time_w_BADO = as.integer(year) - first_BADO,
         time_w_BADO = ifelse(time_w_BADO < 0, 0, time_w_BADO)
         ) %>% ungroup() %>% select(-first_BADO1) %>%
  mutate(present = ifelse(count > 0, 1, 0),
         total_BADO_sd = sd(total_BADO, na.rm = T),
         total_BADO_mean = mean(total_BADO, na.rm = T),
         total_BADO_scaled = (total_BADO-total_BADO_mean)/total_BADO_sd,
         time_w_BADO_sd = sd(time_w_BADO, na.rm = T),
         time_w_BADO_mean = mean(time_w_BADO, na.rm = T),
         time_w_BADO_scaled = (time_w_BADO-time_w_BADO_mean)/time_w_BADO_sd
         )

# datNA <- bdat %>% filter(is.na(Field_counters))
# datNA %>% ggplot(.) + geom_point(aes(Longitude, Latitude, colour = year, size = -year), alpha = 0.2)+
#   scale_color_viridis_c(trans = "sqrt")
# bdat %>% ggplot(.) + geom_point(aes(Longitude, Latitude, colour = year, size = -year), alpha = 0.2)+
#   scale_color_viridis_c(trans = "sqrt")
#
# dat <- bdat %>% filter(!is.na(FCounters_scaled))
#
# datNA <- bdat %>% filter(is.na(FCounters_scaled))
# datNA %>% ggplot(.) + geom_point(aes(Longitude, Latitude, colour = year, size = -year), alpha = 0.2)+
#   scale_color_viridis_c(trans = "sqrt")
# dat %>% ggplot(.) + geom_point(aes(Longitude, Latitude, colour = year, size = -year), alpha = 0.2)+
#   scale_color_viridis_c(trans = "sqrt")


# saveRDS(bdat,"data/WESO_with_all_effort_filled_in.rds")


# ADD ELEVATION

# bdat <- readRDS("WESO_with_all_effort_filled_in.rds")


rast_topo <- readRDS("data/rest_topo.rds")

# 24.14 km radius
dxy <- bdat %>% mutate(
  x = X * 100000,
  y = Y * 100000
)

pts <- dxy %>% select(x,y)

pts <- as.data.frame(pts, xy = TRUE)
# grid.df$layer <- NULL
# 24.14 km = 24140 m

# extract values from cells from circle with 24.14 km = 24140 m radius
elev <- raster::extract(rast_topo, pts, buffer = 24140, fun = mean, na.rm = T, exact=T)
# extracted_elev <- cbind(pts, elev)
dat <- bind_cols(dxy, elev = elev)
dat <-  dat %>% mutate(elev_mean = mean(elev), elev_sd = sd(elev), elev_scaled = (elev - elev_mean)/elev_sd)

dat <- dat %>% filter(!is.na(FCounters_scaled))

saveRDS(dat, "data/WESO_with_all_effort_filled_in.rds")


