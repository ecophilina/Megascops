# western screech and barred owls CBC data explore

library(tidyverse)

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


# check data
hist(dat$BADO_count)
hist(dat$total_BADO)
hist(dat$time_w_BADO)
sd(dat$BADO_count)^2
sd(dat$time_w_BADO_scaled)^2
# variance is close to 1 in count data if model extends further east

ggplot(dat) + geom_point(aes(time_w_BADO, BADO_count))
ggplot(dat) + geom_point(aes(time_w_BADO, count))

ggplot(dat, aes(BADO_count, count)) + geom_point()#+ geom_smooth(method = "lm")
ggplot(dat, aes(GHOW_count, count)) + geom_point()#+ geom_smooth(method = "lm")
ggplot(dat, aes(log(BADO_count+1), log(count+1))) + geom_point() + geom_smooth(method = "lm")
ggplot(dat, aes(log(total_BADO+1), log(count+1))) + geom_point() + geom_smooth(method = "lm")


dat %>% ggplot(.) + geom_point(aes(Longitude, Latitude, size = count), alpha = 0.2)
dat %>% ggplot(.) + geom_point(aes(Longitude, Latitude, colour = time_w_BADO), alpha = 0.2)
dat %>% ggplot(.) + geom_point(aes(Longitude, Latitude, colour = total_BADO), alpha = 0.2) +
  scale_color_viridis_c(trans = "sqrt")


dat %>% filter(count > 0) %>%
  ggplot(.) + geom_point(aes(Longitude, Latitude, colour = alltrees, size = count), alpha = 0.2) +
  scale_color_viridis_c(trans = "sqrt")

dat %>% filter(count > 0) %>%
  ggplot(.) + geom_point(aes(Longitude, Latitude, colour = mosaic, size = count), alpha = 0.2) +
  scale_color_viridis_c()

dat %>% filter(count > 0) %>%
  ggplot(.) + geom_point(aes(Longitude, Latitude, colour = forest, size = count), alpha = 0.2) +
  scale_color_viridis_c()

dat %>% filter(count > 0) %>%
  ggplot(.) + geom_point(aes(Longitude, Latitude, colour = urban, size = count), alpha = 0.2) +
  scale_color_viridis_c(trans = "sqrt")


#Explore effort
# check and scale effort data
ggplot(dat, aes(Field_counters, Hours, colour = as.integer(year))) +
  geom_point(alpha = 0.75) + geom_smooth(method = "lm") + scale_colour_viridis_c(direction = -1)
# Very correlated!

ggplot(dat, aes(Field_counters, Distance, colour = as.integer(year))) +
  geom_point(alpha = 0.75) + geom_smooth(method = "lm") + scale_colour_viridis_c(direction = -1)
# Very correlated!

ggplot(dat, aes(Hours, Nocturnal_hrs, colour = as.integer(year))) +
  geom_point(alpha = 0.75) + geom_smooth(method = "lm") + scale_colour_viridis_c(direction = -1)
# somewhat correlated with each other

ggplot(dat, aes(Nocturnal_hrs, Nocturnal_distance, colour = as.integer(year))) +
  geom_point(alpha = 0.75) + geom_smooth(method = "lm") + scale_colour_viridis_c(direction = -1)
# pretty correlated with each other

ggplot(dat, aes(Field_counters, Nocturnal_hrs, colour = as.integer(year))) +
  geom_point(alpha = 0.75) + geom_smooth(method = "lm") + scale_colour_viridis_c(direction = -1)
# somewhat correlated with each other

ggplot(dat, aes(Field_counters, Nocturnal_distance, colour = as.integer(year))) +
  geom_point(alpha = 0.75) + geom_smooth(method = "lm") + scale_colour_viridis_c(direction = -1)
# Not correlated, USE both!

ggplot(dat, aes(Hours, Nocturnal_distance, colour = as.integer(year))) +
  geom_point(alpha = 0.75) + geom_smooth(method = "lm") + scale_colour_viridis_c(direction = -1)
# could use both

ggplot(dat, aes(log(Distance+1), Nocturnal_distance, colour = as.integer(year))) +
  geom_point(alpha = 0.75) + geom_smooth(method = "lm") + scale_colour_viridis_c(direction = -1)
# could use both
# choose to use distance over hrs because probably more representative of opportunity to find daytime owls

hist(dat$NHrs_scaled)
hist(dat$NDist_scaled)
hist(dat$Hrs_scaled)
hist(dat$Dist_scaled)
hist(dat$FCounters_scaled)



ggplot(dat, aes(forest_scaled, urban_scaled, colour = as.integer(year))) +
  geom_point(alpha = 0.75) + geom_smooth(method = "lm") + scale_colour_viridis_c(direction = -1)
# highly correlated

ggplot(dat, aes(alltrees_scaled, urban_scaled, colour = as.integer(year))) +
  geom_point(alpha = 0.75) + geom_smooth(method = "lm") + scale_colour_viridis_c(direction = -1)
# slightly less correlated?

