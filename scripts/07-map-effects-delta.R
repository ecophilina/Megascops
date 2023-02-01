# make maps ###
library(sdmTMB)
library(tidyverse)
library(sf)
library("rnaturalearth")

## --- get coastlines and lakes
# if albers?
# proj_crs <- "+proj=aea +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

proj_crs <- 32610

land <- ne_countries(scale = "medium", returnclass = "sf") %>% sf::st_transform(crs = proj_crs)
coast <- ne_coastline(scale = "medium", returnclass = "sf") %>% sf::st_transform(crs = proj_crs)
lakes <- sf::st_read("raw/ne_10m_lakes", quiet = T)
lakes <- lakes[lakes$scalerank==0,] %>% sf::st_transform(crs = proj_crs)



# model <- "data/WESO-delta-svc-time-full.rds"
model <-"data/WESO-delta-svc-time-top.rds"
plot_year <- 2015
model_name <- "w_landcover_top_"


# model <- "data/WESO-delta-no-landcover-top.rds"
model <- "data/WESO-delta-no-landcover-no-svc-fullish.rds"
plot_year <- 2020
model_name <- "no_landcover_top_"


legend_position <- c(0.78,0.25)
width_singles <- 4.5
height_singles <- 6.5
save_location <- "slides/images/"


m <- readRDS(model)

m

# predict for sampling points
p1 <- predict(m)
p1 <- p1 %>% mutate(
  # X10 = X, Y10 = Y,
  X = x,
  Y = y
)

# ggplot(p1) + geom_point(aes(X,Y, colour = BADO_count))
# ggplot(p1) + geom_point(aes(X,Y, colour = zeta_s_year_scaled))
# ggplot(p1) + geom_point(aes(X,Y, colour = omega_s))
# # ggplot(p1) + geom_point(aes(X,Y, colour = epsilon_st)) + facet_wrap(~year)
# ggplot(p1) + geom_point(aes(X,Y, colour = est)) + facet_wrap(~year)


dat <- readRDS("data/WESO_with_all_effort_filled_in.rds")
dat <- dat %>% mutate(
  # X10 = X, Y10 = Y,
  X = x,
  Y = y
)
p_proj <- dat %>% mutate(x = X, y = Y) %>% st_as_sf(., coords = c("x", "y"), crs = proj_crs)

b <- tidy(m, model = 1)
b2 <- tidy(m, model = 2)

p_mean <- p1 %>% group_by(LocID) %>% summarise(
  X = mean(X), Y = mean(Y),
  prob_presence = plogis(mean(est1)),
  predicted_count = plogis(mean(est1))*exp(mean(est2)),
  zeta_s_year_scaled1 = mean(zeta_s_year_scaled1),
  zeta_s_year_scaled2 = mean(zeta_s_year_scaled2),
  omega_s1 = mean(omega_s1),
  omega_s2 = mean(omega_s2),
  prob_trend = (b[b$term == "year_scaled", 2][[1]] + mean(zeta_s_year_scaled1)),
  trend = plogis(b[b$term == "year_scaled", 2][[1]] + mean(zeta_s_year_scaled1))*exp(b2[b2$term == "year_scaled", 2][[1]] + mean(zeta_s_year_scaled2)) ,
  time_w_BADO = max(time_w_BADO),
  max_BADO = max(BADO_count),
  total_BADO = max(total_BADO)
)

p_all <- p1 %>% mutate(
  prob_presence = plogis((est1)),
  predicted_count = plogis((est1))*exp((est2))
  # max_BADO = max()
)


p_all <- p1 %>% mutate(
  prob_presence = plogis((est1)),
  predicted_count = plogis((est1))*exp((est2)),
  prob_trend = (b[b$term == "year_scaled", 2][[1]] + (zeta_s_year_scaled1)),
  trend = plogis(b[b$term == "year_scaled", 2][[1]] + zeta_s_year_scaled1)*exp(b2[b2$term == "year_scaled", 2][[1]] + zeta_s_year_scaled2)
  # max_BADO = max()
)

# range(p_mean$prob_presence)
# range(p_all$prob_presence)
# range(p_mean$predicted_count)
# range(p_all$predicted_count)
# range(m$data$count)
# ptest <- filter(p_all, year == plot_year)
# range(ptest$predicted_count)
#
# range(m$data$year)

# prob presence

ggplot(data = p_proj) +
  geom_sf(data = land, fill = "white", colour = "white",lwd = 0.35) +
  geom_point(
    # data = p_mean, aes(X,Y, colour = prob_presence), size =4, alpha = 0.75) +
  data = filter(p_all, year == plot_year), aes(X,Y, colour = prob_presence), size = 4, alpha = 0.75) +
  # data = p_mean, aes(X,Y, colour = zeta_s_year_scaled, size = prob_presence), alpha = 0.75) +
  # data = p_all, aes(X,Y, colour = prob_trend, size = prob_presence), alpha = 0.1) +
  geom_sf(data = coast, colour = "gray40", fill = NA, lwd = 0.2) +
  geom_sf(data = lakes, colour = "gray40", fill = NA, lwd = 0.2) +
  coord_sf(xlim = c(min(p_proj$X)-120000, max(p_proj$X))+70000,
           ylim = c(min(p_proj$Y), max(p_proj$Y)))+
  scale_colour_viridis_c(limits = c(0, 1)) +
  guides(size = guide_legend(order = 2),col = guide_colorbar(order = 1))+
  labs(x= "Longitude", y = "Latitude",
       colour = "Probability\npresent") +
       # size = "Mean\nProbability\npresent") +
       # size = "Probability\npresent") +
  ggsidekick::theme_sleek() + theme(
    legend.position = legend_position,
    panel.background = element_rect(fill = "grey86", colour = NA),
    panel.grid.major = element_line(colour = "grey98"),
    axis.title = element_blank())


ggsave(
  paste0(save_location, "WESO_presence_", model_name , plot_year, "_delta.png"),
  width = width_singles, height = height_singles)


# predicted counts

ggplot(data = p_proj) +
  geom_sf(data = land, fill = "white", colour = "white",lwd = 0.35) +
  geom_point(
    # data = p_mean, aes(X,Y, colour = prob_presence), size =4, alpha = 0.75) +
    data = filter(p_all, year == plot_year), aes(X,Y, colour = predicted_count), size =4, alpha = 0.75) +
  # data = p_mean, aes(X,Y, colour = zeta_s_year_scaled, size = prob_presence), alpha = 0.75) +
  # data = p_all, aes(X,Y, colour = prob_trend, size = prob_presence), alpha = 0.1) +
  geom_sf(data = coast, colour = "gray40", fill = NA, lwd = 0.2) +
  geom_sf(data = lakes, colour = "gray40", fill = NA, lwd = 0.2) +
  coord_sf(xlim = c(min(p_proj$X)-120000, max(p_proj$X))+70000,
           ylim = c(min(p_proj$Y), max(p_proj$Y)))+
  scale_colour_viridis_c(limits = c(0, 7), na.value = "yellow") +
  guides(size = guide_legend(order = 2),col = guide_colorbar(order = 1))+
  labs(x= "Longitude", y = "Latitude",
       colour = "Predicted\ncount") +
  # size = "Mean\nProbability\npresent") +
  # size = "Probability\npresent") +
  ggsidekick::theme_sleek() + theme(
    legend.position = legend_position,
    panel.background = element_rect(fill = "grey86", colour = NA),
    panel.grid.major = element_line(colour = "grey98"),
    axis.title = element_blank())


ggsave(
  # "slides/images/WESO_count_W_habitat_2015_delta.png",
  paste0(save_location, "WESO_count_", model_name , plot_year, "_delta.png"),
  width = width_singles, height = height_singles)



# spatially varying presence trend

ggplot(data = p_proj) +
  geom_sf(data = land, fill = "white", colour = "white",lwd = 0.35) +
  geom_point(
    data = filter(p_all, year == plot_year), aes(X,Y, colour = trend, size = predicted_count), alpha = 0.75) +
    # data = p_mean, aes(X,Y, colour = trend, size = predicted_count), alpha = 0.95) +
  # data = p_mean, aes(X,Y, colour = zeta_s_year_scaled, size = prob_presence), alpha = 0.75) +
  # data = p_all, aes(X,Y, colour = prob_trend, size = prob_presence), alpha = 0.1) +
  geom_sf(data = coast, colour = "gray40", fill = NA, lwd = 0.2) +
  geom_sf(data = lakes, colour = "gray40", fill = NA, lwd = 0.2) +
  coord_sf(xlim = c(min(p_proj$X)-120000, max(p_proj$X))+70000,
           ylim = c(min(p_proj$Y), max(p_proj$Y)))+

  scale_size_continuous(limits = c(0,15)) +
  scale_colour_viridis_c(limits = c(0, 1.19)) +
  guides(size = guide_legend(order = 2),col = guide_colorbar(order = 1))+
  # scale_colour_viridis_c(direction = -1, end = 0.6, begin = 0.2, option = "B") +
  # scale_colour_gradient2(high="deepskyblue", mid = "lightgrey", low = "red") +
  # guides(size = "none") +
  labs(x= "Longitude", y = "Latitude",
       colour = "Abundance\ntrend per\ndecade",
       # size = "Mean\nProbability\npresent") +
       size = "Predicted\ncount") +
  ggsidekick::theme_sleek() + theme(
    legend.position = legend_position,
    panel.background = element_rect(fill = "grey86", colour = NA),
    panel.grid.major = element_line(colour = "grey98"),
    axis.title = element_blank())

ggsave(
  paste0(save_location, "WESO_zeta_adund_trend_", model_name, plot_year, "_delta.png"),
       width = width_singles, height = height_singles)


### predictions with time BADO
ggplot(data = p_proj) +
  geom_sf(data = land, fill = "white", colour = "white",lwd = 0.35) +
  geom_point(
    # data = p_mean, aes(X,Y, colour = max_BADO,
    data = filter(p_all, year == plot_year), aes(X,Y, colour = time_w_BADO,
                                            size = predicted_count), alpha = 0.75) +
  geom_sf(data = coast, colour = "gray40", fill = NA, lwd = 0.2) +
  geom_sf(data = lakes, colour = "gray40", fill = NA, lwd = 0.2) +
  coord_sf(xlim = c(min(p_proj$X)-120000, max(p_proj$X))+70000,
           ylim = c(min(p_proj$Y), max(p_proj$Y)))+
  scale_colour_viridis_c(trans = "sqrt", limits = c(0,max(dat$time_w_BADO)), direction = 1, end = 0.8, option = "B") +
  scale_size_continuous(limits = c(0,15)) +
  guides(size = guide_legend(order = 2),col = guide_colorbar(order = 1))+
  labs(x= "Longitude", y = "Latitude",
       colour = "Years with\nBADO",
       # size = "Mean\nprobability\npresent",
       size = "Predicted\ncount") +
  guides(size = guide_legend(order = 2),col = guide_colorbar(order = 1))+
  ggsidekick::theme_sleek() + theme(
    legend.position = legend_position,
    panel.background = element_rect(fill = "grey86", colour = NA),
    panel.grid.major = element_line(colour = "grey98"),
    axis.title = element_blank())

ggsave(paste0(save_location, "WESO_abudance_time_w_BADO_", model_name , plot_year, "_delta.png"),
       width = width_singles, height = height_singles)


### BADO count by predicted WESO
ggplot(data = p_proj) +
  geom_sf(data = land, fill = "white", colour = "white",lwd = 0.35) +
  geom_point(
    # data = p_mean, aes(X,Y, colour = max_BADO,
    data = filter(p_all, year == plot_year), aes(X,Y, colour = BADO_count,
                                            size = predicted_count), alpha = 0.75) +
  geom_sf(data = coast, colour = "gray40", fill = NA, lwd = 0.2) +
  geom_sf(data = lakes, colour = "gray40", fill = NA, lwd = 0.2) +
  coord_sf(xlim = c(min(p_proj$X)-120000, max(p_proj$X))+70000,
           ylim = c(min(p_proj$Y), max(p_proj$Y)))+
  scale_colour_viridis_c(trans = "sqrt", limits = c(0,max(dat$BADO_count)), direction = 1, end = 0.8, option = "B") +

  scale_size_continuous(limits = c(0,15)) +
  # guides(size = "none") +
  guides(size = guide_legend(order = 2),col = guide_colorbar(order = 1))+
  labs(x= "Longitude", y = "Latitude",
       # size = "Mean\nprobability\npresent",
       size = "Predicted\ncount",
       colour = "BADO count") +
  ggsidekick::theme_sleek() + theme(
    legend.position = legend_position,
    panel.background = element_rect(fill = "grey86", colour = NA),
    panel.grid.major = element_line(colour = "grey98"),
    axis.title = element_blank())

ggsave(paste0(save_location, "WESO_abudance_BADO_count_", model_name , plot_year, "_delta.png"),
       width = width_singles, height = height_singles)

#
# # total BADO
# ggplot(data = p_proj) +
#   geom_sf(data = land, fill = "white", colour = "white",lwd = 0.35) +
#   # geom_point(data = p_mean, aes(X,Y, colour = total_BADO,
#   #       size = prob_presence), alpha = 0.5) +
#   geom_point(
#     data = p_mean, aes(X,Y, colour = total_BADO,
#                        # data = filter(p_all, year == 2020), aes(X,Y, colour = total_BADO,
#                        size = predicted_count), alpha = 0.75) +
#   geom_sf(data = coast, colour = "gray40", fill = NA, lwd = 0.2) +
#   geom_sf(data = lakes, colour = "gray40", fill = NA, lwd = 0.2) +
#   coord_sf(xlim = c(min(p_proj$X)-120000, max(p_proj$X))+70000,
#            ylim = c(min(p_proj$Y), max(p_proj$Y)))+
#   scale_colour_viridis_c(trans = "sqrt", direction = 1, end = 0.8, option = "B") +
#   # guides(size = "none") +
#   scale_size_continuous(limits = c(0,15)) +
#   guides(size = guide_legend(order = 2),col = guide_colorbar(order = 1))+
#   labs(x= "Longitude", y = "Latitude",
#        # size = "Mean\nprobability\npresent",
#        size = "Predicted\ncount",
#        colour = "Cumulative\nBADO count") +
#   ggsidekick::theme_sleek() + theme(
#     legend.position = legend_position,
#     panel.background = element_rect(fill = "grey86", colour = NA),
#     panel.grid.major = element_line(colour = "grey98"),
#     axis.title = element_blank())
#
# ggsave(#"slides/images/WESO_cumulative_BADO_mean.png",
#   paste0(save_location, "WESO_abudance_accumulative_BADO_", model_name , plot_year, "_delta.png"),
#        width = width_singles, height = height_singles)

## omega1
ggplot(data = p_proj) +
  geom_sf(data = land, fill = "white", colour = "white",lwd = 0.35) +
  geom_point(
    data = p_mean, aes(X,Y,
                       # data = filter(p_all, year == 2020), aes(X,Y,
                       # size = time_w_BADO,
                       colour = omega_s1),
    size = 4,
    alpha = 0.75) +
  geom_sf(data = coast, colour = "gray40", fill = NA, lwd = 0.2) +
  geom_sf(data = lakes, colour = "gray40", fill = NA, lwd = 0.2) +
  coord_sf(xlim = c(min(p_proj$X)-120000, max(p_proj$X))+70000,
           ylim = c(min(p_proj$Y), max(p_proj$Y)))+
  # scale_colour_viridis_c() +
  scale_colour_gradient2(high="deepskyblue", mid = "lightgrey", low = "red") +
  # scale_size_continuous(trans = "sqrt") +
  # guides(size = "none") +
  labs(x= "Longitude", y = "Latitude",
       # size = "Cumulative\nBADO count"
       # size = "Probability",
       colour = "Spatial\nrandom\nfield for\npresence"
  ) +
  ggsidekick::theme_sleek() + theme(
    legend.position = legend_position,
    panel.background = element_rect(fill = "grey86", colour = NA),
    panel.grid.major = element_line(colour = "grey98"),
    axis.title = element_blank())

ggsave(paste0(save_location, "WESO_omega_presence_", model_name , plot_year, "_delta.png"),
       width = width_singles, height = height_singles)

## omega2
ggplot(data = p_proj) +
  geom_sf(data = land, fill = "white", colour = "white",lwd = 0.35) +
  geom_point(
    data = p_mean, aes(X,Y,
                       # data = filter(p_all, year == 2020), aes(X,Y,
                       # size = time_w_BADO,
                       colour = omega_s2),
    size = 4,
    alpha = 0.75) +
  geom_sf(data = coast, colour = "gray40", fill = NA, lwd = 0.2) +
  geom_sf(data = lakes, colour = "gray40", fill = NA, lwd = 0.2) +
  coord_sf(xlim = c(min(p_proj$X)-120000, max(p_proj$X))+70000,
           ylim = c(min(p_proj$Y), max(p_proj$Y)))+
  # scale_colour_viridis_c() +
  scale_colour_gradient2(high="deepskyblue", mid = "lightgrey", low = "red") +
  # scale_size_continuous(trans = "sqrt") +
  # guides(size = "none") +
  labs(x= "Longitude", y = "Latitude",
       # size = "Cumulative\nBADO count"
       # size = "Probability",
       colour = "Spatial\nrandom\nfield for\nabundance"
  ) +
  ggsidekick::theme_sleek() + theme(
    legend.position = legend_position,
    panel.background = element_rect(fill = "grey86", colour = NA),
    panel.grid.major = element_line(colour = "grey98"),
    axis.title = element_blank())

ggsave(paste0(save_location, "WESO_omega_abundance_", model_name , plot_year, "_delta.png"),
       width = width_singles, height = height_singles)


# # epsilon
ggplot(data = p_proj) +
  # geom_sf(data = land, fill = "white", colour = "white",lwd = 0.35) +
  geom_sf(data = coast, fill = NA, colour = "gray40", lwd = 0.25) +
  geom_point(data = p_all, aes(X,Y, colour = epsilon_st2), alpha = 0.75, size = 1) +
  # geom_sf(data = coast, fill = NA, colour = "gray40", lwd = 0.25) +
  coord_sf(xlim = c(min(p_proj$X)-120000, max(p_proj$X))+70000,
           ylim = c(min(p_proj$Y), max(p_proj$Y)))+

  scale_colour_gradient2(high="deepskyblue", mid = "lightgrey", low = "red") +
  ggsidekick::theme_sleek() + theme(#legend.position = c(0.7,0.09),
                                    axis.text = element_blank(), axis.ticks = element_blank(),
                                    axis.title = element_blank()) +
  facet_wrap(~year, nrow = 4)
# ggsave("slides/images/WESO_presence_epsilon.png", width = 8, height = 8)
ggsave("slides/images/WESO_count_epsilon.png", width = 12, height = 12)


#### maps of covariates ---------------
range(dat$BADO_count)
### time since BADO
ggplot(data = p_proj) +
  geom_sf(data = land, fill = "white", colour = "white",lwd = 0.35) +
  geom_point(
    # data = p_mean, aes(X,Y, colour = max_BADO,
    data = filter(p_all, year == plot_year), aes(X,Y, colour = time_w_BADO,
                                            size = BADO_count), alpha = 0.75) +
  geom_sf(data = coast, colour = "gray40", fill = NA, lwd = 0.2) +
  geom_sf(data = lakes, colour = "gray40", fill = NA, lwd = 0.2) +
  coord_sf(xlim = c(min(p_proj$X)-120000, max(p_proj$X))+70000,
           ylim = c(min(p_proj$Y), max(p_proj$Y)))+
  scale_colour_viridis_c(trans = "sqrt", limits = c(0,max(dat$time_w_BADO)), direction = 1, end = 0.8, option = "B") +
  # guides(size = "none") +
  scale_size_continuous(limits = c(0,max(dat$BADO_count))) +
  labs(x= "Longitude", y = "Latitude",
       colour = "Years with\nBADO",
       # size = "Mean\nprobability\npresent",
       size = "BADO count") +
  guides(size = guide_legend(order = 2),col = guide_colorbar(order = 1))+
  ggsidekick::theme_sleek() + theme(
    legend.position = legend_position,
    panel.background = element_rect(fill = "grey86", colour = NA),
    panel.grid.major = element_line(colour = "grey98"),
    axis.title = element_blank())

ggsave(
  paste0(save_location, "WESO_time_w_BADO_", plot_year, ".png"),
  # "slides/images/WESO_time_w_BADO_2015.png",
       width = width_singles, height = height_singles)



### GHOW distibution
ggplot(data = p_proj) +
  geom_sf(data = land, fill = "white", colour = "white",lwd = 0.35) +
  geom_point(
    # data = p_mean, aes(X,Y, colour = max_BADO,
    data = filter(p_all, year == plot_year), aes(X,Y,
                                                 # size = BADO_count,
                                                 colour = GHOW_count),
    size = 4,
    alpha = 0.75) +
  geom_sf(data = coast, colour = "gray40", fill = NA, lwd = 0.2) +
  geom_sf(data = lakes, colour = "gray40", fill = NA, lwd = 0.2) +
  coord_sf(xlim = c(min(p_proj$X)-120000, max(p_proj$X))+70000,
           ylim = c(min(p_proj$Y), max(p_proj$Y)))+
  scale_colour_viridis_c(trans = "sqrt", limits = c(0, max(dat$GHOW_count)), direction = 1) +
  # guides(size = "none") +
  labs(x= "Longitude", y = "Latitude",
       # size = "Mean\nprobability\npresent",
       # size = "BADO count",
       colour = "GHOW count") +
  guides(size = guide_legend(order = 2),col = guide_colorbar(order = 1))+
  ggsidekick::theme_sleek() + theme(
    legend.position = legend_position,
    panel.background = element_rect(fill = "grey86", colour = NA),
    panel.grid.major = element_line(colour = "grey98"),
    axis.title = element_blank())

ggsave(paste0(save_location, "WESO_GHOW_", plot_year, ".png"),
       # "slides/images/WESO_GHOW_by_BADO_2015.png",
       width = width_singles, height = height_singles)



### forest distibution
ggplot(data = p_proj) +
  geom_sf(data = land, fill = "white", colour = "white",lwd = 0.35) +
  geom_point(
    data = filter(p_all, year == min(p_all$year)), aes(X,Y,
                                            colour = forest),size = 4,  alpha = 0.75) +
  geom_sf(data = coast, colour = "gray40", fill = NA, lwd = 0.2) +
  geom_sf(data = lakes, colour = "gray40", fill = NA, lwd = 0.2) +
  coord_sf(xlim = c(min(p_proj$X)-120000, max(p_proj$X))+70000,
           ylim = c(min(p_proj$Y), max(p_proj$Y)))+
  scale_colour_viridis_c(trans = "sqrt", direction = 1, end = 0.8, option = "B") +
  labs(x= "Longitude", y = "Latitude",
       colour = "forest") +
  guides(size = guide_legend(order = 2),col = guide_colorbar(order = 1))+
  ggsidekick::theme_sleek() + theme(
    legend.position = legend_position,
    panel.background = element_rect(fill = "grey86", colour = NA),
    panel.grid.major = element_line(colour = "grey98"),
    axis.title = element_blank())

ggsave(paste0(save_location, "WESO_forest_", min(p_all$year), ".png"),
       width = width_singles, height = height_singles)


### forest distibution
ggplot(data = p_proj) +
  geom_sf(data = land, fill = "white", colour = "white",lwd = 0.35) +
  geom_point(
    data = filter(p_all, year == max(p_all$year)), aes(X,Y,
                                                 colour = forest),size =4, alpha = 0.75) +
  geom_sf(data = coast, colour = "gray40", fill = NA, lwd = 0.2) +
  geom_sf(data = lakes, colour = "gray40", fill = NA, lwd = 0.2) +
  coord_sf(xlim = c(min(p_proj$X)-120000, max(p_proj$X))+70000,
           ylim = c(min(p_proj$Y), max(p_proj$Y)))+
  scale_colour_viridis_c(trans = "sqrt", direction = 1, end = 0.8, option = "B") +
  labs(x= "Longitude", y = "Latitude",
       colour = "forest") +
  guides(size = guide_legend(order = 2),col = guide_colorbar(order = 1))+
  ggsidekick::theme_sleek() + theme(
    legend.position = legend_position,
    panel.background = element_rect(fill = "grey86", colour = NA),
    panel.grid.major = element_line(colour = "grey98"),
    axis.title = element_blank())

ggsave(paste0(save_location, "WESO_forest_", max(p_all$year), ".png"),
       width = width_singles, height = height_singles)


### urban distibution
ggplot(data = p_proj) +
  geom_sf(data = land, fill = "white", colour = "white",lwd = 0.35) +
  geom_point(
    data = filter(p_all, year == min(p_all$year)),
    aes(X,Y, colour = urban), size = 4, alpha = 0.75) +
  geom_sf(data = coast, colour = "gray40", fill = NA, lwd = 0.2) +
  geom_sf(data = lakes, colour = "gray40", fill = NA, lwd = 0.2) +
  coord_sf(xlim = c(min(p_proj$X)-120000, max(p_proj$X))+70000,
           ylim = c(min(p_proj$Y), max(p_proj$Y)))+
  scale_colour_viridis_c(trans = "sqrt", limits = c(0, 0.49),
                         direction = 1, end = 0.8, option = "B") +
  labs(x= "Longitude", y = "Latitude",
       colour = "urban") +
  guides(size = guide_legend(order = 2),col = guide_colorbar(order = 1))+
  ggsidekick::theme_sleek() + theme(
    legend.position = legend_position,
    panel.background = element_rect(fill = "grey86", colour = NA),
    panel.grid.major = element_line(colour = "grey98"),
    axis.title = element_blank())

ggsave(paste0(save_location, "WESO_urban_", min(p_all$year), ".png"),
       width = width_singles, height = height_singles)


### urban distibution
ggplot(data = p_proj) +
  geom_sf(data = land, fill = "white", colour = "white",lwd = 0.35) +
  geom_point(
    data = filter(p_all, year == max(p_all$year)),
    aes(X,Y, colour = urban), size = 4, alpha = 0.75) +
  geom_sf(data = coast, colour = "gray40", fill = NA, lwd = 0.2) +
  geom_sf(data = lakes, colour = "gray40", fill = NA, lwd = 0.2) +
  coord_sf(xlim = c(min(p_proj$X)-120000, max(p_proj$X))+70000,
           ylim = c(min(p_proj$Y), max(p_proj$Y)))+
  scale_colour_viridis_c(trans = "sqrt", limits = c(0, 0.49),
                         direction = 1, end = 0.8, option = "B") +
  labs(x= "Longitude", y = "Latitude",
       colour = "urban") +
  guides(size = guide_legend(order = 2),col = guide_colorbar(order = 1))+
  ggsidekick::theme_sleek() + theme(
    legend.position = legend_position,
    panel.background = element_rect(fill = "grey86", colour = NA),
    panel.grid.major = element_line(colour = "grey98"),
    axis.title = element_blank())

ggsave(paste0(save_location, "WESO_urban_", max(p_all$year), ".png"),
       width = width_singles, height = height_singles)



## Estimates for all years: THESE ARE SLOWER -----------------

# ests through time
g <- ggplot(data = p_proj) +
  # geom_sf(data = land, fill = "white", colour = "white",lwd = 0.35) +
  geom_point(data = p_all, aes(X,Y, colour = prob_presence), alpha = 0.75, size = 1) +
  geom_sf(data = coast, fill = NA, colour = "gray40", lwd = 0.25) +
  coord_sf(xlim = c(min(p_proj$X)-120000, max(p_proj$X))+70000,
           ylim = c(min(p_proj$Y), max(p_proj$Y)))+
  scale_colour_viridis_c() +
  labs(x= "Longitude", y = "Latitude", colour = "WESO\nPresence") +
  ggsidekick::theme_sleek() + theme(#legend.position = c(0.85,0.09),
    axis.text = element_blank(), axis.ticks = element_blank(),
    axis.title = element_blank()) +
  facet_wrap(~year, nrow = 5)
ggsave(paste0(save_location, "WESO_annual_presence_", model_name , plot_year, "_delta.png"),
       width = 9, height = 9)


# ests through time
g <- ggplot(data = p_proj) +
  # geom_sf(data = land, fill = "white", colour = "white",lwd = 0.35) +
  geom_point(data = p_all, aes(X,Y, colour = predicted_count), alpha = 0.75, size = 1) +
  geom_sf(data = coast, fill = NA, colour = "gray40", lwd = 0.25) +
  coord_sf(xlim = c(min(p_proj$X)-120000, max(p_proj$X))+70000,
           ylim = c(min(p_proj$Y), max(p_proj$Y)))+
  scale_colour_viridis_c() +
  labs(x= "Longitude", y = "Latitude", colour = "WESO\nAbundance") +
  ggsidekick::theme_sleek() + theme(#legend.position = c(0.85,0.09),
    axis.text = element_blank(), axis.ticks = element_blank(),
    axis.title = element_blank()) +
  facet_wrap(~year, nrow = 5)
ggsave(paste0(save_location, "WESO_annual_abund_", model_name , plot_year, "_delta.png"),
       width = 9, height = 9)


