# plot conditional effects
library(sdmTMB)
library(tidyverse)
library(visreg)
library(RColorBrewer)

# model <- "data/WESO-presence-svc-time-no-st-full.rds"
model <- "data/WESO-presence-svc-time-no-st-final.rds"

m <- readRDS(model)

m

# effort variables
visreg::visreg(m, "NDist_scaled", scale = "response")
visreg::visreg(m, "Dist_scaled", scale = "response")
visreg::visreg(m, "FCounters_scaled", scale = "response")




pdf("figs/WESO-BADO-time-by-GHOW-plot.pdf", width = 6, height = 5)
visreg::visreg2d(m, "log_GHOW", "time_w_BADO", scale = "response", col = colorRampPalette(viridis::viridis(12))(5),
                 main = "Probability of detecting WESO",
                 xlab = "log(Great Horned Owl count +1)",
                 ylab = "Years since first Barred Owl")
dev.off()


pdf("figs/WESO-BADO-time-by-BADO-plot.pdf", width = 6, height = 5)
visreg::visreg2d(m, "log_BADO", "time_w_BADO", scale = "response", col = colorRampPalette(viridis::viridis(12))(5),
                 main = "Probability of detecting WESO",
                 xlab = "log(Barred Owl count +1)",
                 ylab = "Years since first Barred Owl")
dev.off()


pdf("figs/WESO-BADO-time-by-urban-plot.pdf", width = 6, height = 5)
visreg::visreg2d(m, "urban", "time_w_BADO", scale = "response", col = colorRampPalette(viridis::viridis(12))(5),
                 main = "Probability of detecting WESO",
                 xlab = "Proportion urban",
                 ylab = "Years since first Barred Owl"
)
dev.off()


pdf("figs/WESO-GHOW-by-BADO-plot.pdf", width = 6, height = 5)
visreg::visreg2d(m, "log_GHOW", "log_BADO", scale = "response", col = colorRampPalette(viridis::viridis(12))(5),
                 main = "Probability of detecting WESO",
                 ylab = "log(Barred Owl count +1)",
                 xlab = "log(Great Horned Owl count +1)")
dev.off()



# conditional effects of time with barred owls ###
ndt <- expand.grid(
  NDist_scaled = 0,
  Dist_scaled = 0,
  FCounters_scaled = 0,
  year_scaled = 0,
  time_w_BADO = seq(min(m$dat$time_w_BADO),
                    max(m$dat$time_w_BADO), length.out = 30),
  time_w_BADO_scaled = seq(min(m$dat$time_w_BADO_scaled),
                           max(m$dat$time_w_BADO_scaled), length.out = 30),
  log_GHOW = 0,
  log_BADO = 0,
  BADO_count = 0,
  BADO_scaled = 0,
  elev_scaled = 0,
  forest = 0,
  forest_scaled = 0,
  urban = 0,
  urban_scaled = 0,
  # total_BADO = seq(min(m$data$total_BADO),
  #     max(m$data$total_BADO), length.out = max(m$data$total_BADO)+1),
  year = m$dat$year_median[1],
  year_f = as.factor(m$dat$year_median[1])
)


pt1 <- predict(m, newdata = ndt, se_fit = TRUE, re_form = NA)

(gt1 <- pt1 %>%
    ggplot(., aes(
      # total_BADO,
      time_w_BADO,
      # time_w_BADO_scaled * m$dat$time_w_BADO_sd[1]+m$dat$time_w_BADO_mean[1],
      plogis(est),
      ymin = plogis(est - 1.96 * est_se),
      ymax = plogis(est + 1.96 * est_se)
    )) +
    geom_ribbon(alpha = 0.1, colour=NA) +
    geom_line() +
    ylim(0,1)+
    ylab("Presence of Western Screech-Owl") +
    xlab("Years since first Barred Owl") +
    scale_colour_viridis_c()+ ggsidekick::theme_sleek()
)

# conditional effect of barred owl count ###
ndb <- expand.grid(
  # NHrs_scaled = 0,
  # Hrs_scaled = 0,
  NDist_scaled = 0,
  Dist_scaled = 0,
  FCounters_scaled = 0,
  year_scaled = 0,
  elev_scaled = 0,
  forest = 0,
  forest_scaled = 0,
  urban = 0,
  urban_scaled = 0,
  time_w_BADO = 0,
  time_w_BADO_scaled = 0,
  log_GHOW = 0,
  log_BADO = seq(0, max(m$dat$log_BADO), length.out = 30),
  # BADO_count = seq(min(m$dat$BADO_count),
  #                  max(m$dat$BADO_count), length.out = 30),
  # BADO_scaled = seq(min(m$dat$BADO_scaled),
  #                   max(m$dat$BADO_scaled), length.out = 30),
  year = m$dat$year_median[1],
  year_f = as.factor(m$dat$year_median[1])
)

pb1 <- predict(m, newdata = ndb, se_fit = TRUE, re_form = NA)

(gb1 <- pb1 %>%
    ggplot(., aes(
      exp(log_BADO)-1,
      # BADO_count,
      # (BADO_scaled*m$dat$BADO_sd[1]+1)^2,
      plogis(est),
      ymin = plogis(est - 1.96 * est_se), ymax = plogis(est + 1.96 * est_se)
    )) +
    geom_ribbon(alpha = 0.1, colour=NA) +
    geom_line() +
    ylim(0,1)+
    scale_colour_viridis_c()+
    xlab("Count of Barred Owls") +
    ggsidekick::theme_sleek() +
    theme(axis.title.y = element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank() )
)

gt1 + gb1 + patchwork::plot_layout()

# ggsave("figs/WESO-presence-main-effects-scaled.png", width = 5, height = 4)
ggsave("figs/WESO-presence-main-effects-BADO.png", width = 5, height = 4)



# conditional effect of GHOW owl count ###
ndg <- expand.grid(
  # NHrs_scaled = 0,
  # Hrs_scaled = 0,
  NDist_scaled = 0,
  Dist_scaled = 0,
  FCounters_scaled = 0,
  year_scaled = 0,
  elev_scaled = 0,
  forest = 0,
  forest_scaled = 0,
  urban = 0,
  urban_scaled = 0,
  time_w_BADO = 0,
  time_w_BADO_scaled = 0,
  log_GHOW = seq(0, max(m$dat$log_GHOW), length.out = 30),
  log_BADO = 0,
  # BADO_count = seq(min(m$dat$BADO_count),
  #                  max(m$dat$BADO_count), length.out = 30),
  # BADO_scaled = seq(min(m$dat$BADO_scaled),
  #                   max(m$dat$BADO_scaled), length.out = 30),
  year = m$dat$year_median[1],
  year_f = as.factor(m$dat$year_median[1])
)

pg1 <- predict(m, newdata = ndg, se_fit = TRUE, re_form = NA)

(gg1 <- pg1 %>%
    ggplot(., aes(
      exp(log_GHOW)-1,
      plogis(est),
      ymin = plogis(est - 1.96 * est_se), ymax = plogis(est + 1.96 * est_se)
    )) +
    geom_ribbon(alpha = 0.1, colour=NA) +
    geom_line() +
    ylim(0,1)+
    scale_colour_viridis_c()+
    xlab("Count of Great Horned Owls") +
    ggsidekick::theme_sleek() +
    ylab("Presence of Western Screech-Owl")
    # theme(axis.title.y = element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank() )
)
gg1x <- gg1 + theme(axis.title.y = element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank() )

gt1 + gb1 + gg1x + patchwork::plot_layout()

ggsave("figs/WESO-presence-main-effects-owls.png", width = 8, height = 3.5)


# conditional effect of year
ndy <- expand.grid(
  # NHrs_scaled = 0,
  # Hrs_scaled = 0,
  NDist_scaled = 0,
  Dist_scaled = 0,
  FCounters_scaled = 0,
  urban = 0,
  elev_scaled = 0,
  forest = 0,
  forest_scaled = 0,
  time_w_BADO = 0,
  time_w_BADO_scaled = 0,
  log_GHOW = 0,
  log_BADO = 0,
  year_scaled = seq(min(m$dat$year_scaled),
                    max(m$dat$year_scaled), length.out = 50),
  time_w_BADO = 0,
  time_w_BADO_scaled = 0,
  BADO_count = 0,
  BADO_scaled = 0,
  year = m$dat$year_median[1],
  year_f = as.factor(m$dat$year_median[1])
)

py1 <- predict(m, newdata = ndy, se_fit = TRUE, re_form = NA)

(gy1 <- py1 %>%
    ggplot(., aes(year_scaled*m$dat$year_sd[1]+m$dat$year_median[1],
                  plogis(est),
                  ymin = plogis(est - 1.96 * est_se),
                  ymax = plogis(est + 1.96 * est_se)
    )) +
    geom_ribbon(alpha = 0.1, colour=NA) +
    geom_line() +
    ylim(0,1)+
    ggsidekick::theme_sleek() +
    # theme(axis.title.y = element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank()) +
    ylab("Presence of Western Screech-Owl") + xlab("Year")
)

#
# # conditional effect of forest ###
# ndf <- expand.grid(
#   NDist_scaled = 0,
#   Dist_scaled = 0,
#   FCounters_scaled = 0,
#   year_scaled = 0,
#   elev_scaled = 0, elev_scaled2 = 0,
#   alltrees_scaled = seq(min(m$dat$alltrees_scaled),
#                         max(m$dat$alltrees_scaled), length.out = 50),
#   # forest_scaled = seq(min(m$dat$forest_scaled),
#   #                     max(m$dat$forest_scaled), length.out = 50),
#   # mosaic_scaled = seq(min(m$dat$mosaic_scaled),
#   #                     max(m$dat$mosaic_scaled), length.out = 50),
#   urban = 0,
#   urban2 = 0,
#   time_w_BADO = 0,
#   time_w_BADO_scaled = 0,
#   BADO_count = 0,
#   BADO_scaled = 0, BADO_scaled2 = 0,
#   year = m$dat$year_median[1],
#   year_f = as.factor(m$dat$year_median[1])
# )
# # ndf$forest_scaled2 <- ndf$forest_scaled^2
# # ndf$mosaic_scaled2 <- ndf$mosaic_scaled^2
# ndf$alltrees_scaled2 <- ndf$alltrees_scaled^2
#
# pf1 <- predict(m1, newdata = ndf, se_fit = TRUE, re_form = NA)
#
# (gf1 <- pf1 %>%
#     ggplot(., aes(
#       -alltrees_scaled*m$dat$alltrees_sd[1]+m$dat$alltrees_mean[1],
#       # forest_scaled*m$dat$forest_sd[1]+m$dat$forest_mean[1],
#       # mosaic_scaled*m$dat$mosaic_sd[1]+m$dat$mosaic_mean[1],
#       plogis(est),
#       ymin = plogis(est - 1.96 * est_se), ymax = plogis(est + 1.96 * est_se)
#     )) +
#     geom_ribbon(alpha = 0.1, colour=NA) +
#     geom_line() +
#     # ylim(0,1)+
#     scale_colour_viridis_c()+
#     xlab("Proportion treed habitats") +
#     ggsidekick::theme_sleek() +
#     theme(axis.title.y = element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank() )
# )
#
#
# conditional effect of urban ###
ndu <- expand.grid(
  NDist_scaled = 0,
  Dist_scaled = 0,
  FCounters_scaled = 0,
  year_scaled = 0,
  elev_scaled = 0,
  forest = 0,
  forest_scaled = 0,

  time_w_BADO = 0,
  time_w_BADO_scaled = 0,
  log_GHOW = 0,
  log_BADO = 0,
  urban = seq(min(m$dat$urban),
              max(m$dat$urban), length.out = 50),
  # urban_scaled = 0,
  # BADO_count = 0,
  # BADO_scaled = 0, BADO_scaled2 = 0,
  year = m$dat$year_median[1],
  year_f = as.factor(m$dat$year_median[1])
)

pu1 <- predict(m, newdata = ndu, se_fit = TRUE, re_form = NA)

(gu1 <- pu1 %>%
    ggplot(., aes(
      urban,
      plogis(est),
      ymin = plogis(est - 1.96 * est_se), ymax = plogis(est + 1.96 * est_se)
    )) +
    geom_ribbon(alpha = 0.1, colour=NA) +
    geom_line() +
    ylim(0,1)+
    scale_colour_viridis_c()+
    xlab("Proportion urban") +
    ggsidekick::theme_sleek() +
    theme(axis.title.y = element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank() )
)


# conditional effect of elevation
nde <- expand.grid(
  # NHrs_scaled = 0,
  # Hrs_scaled = 0,
  NDist_scaled = 0,
  Dist_scaled = 0,
  FCounters_scaled = 0,
  elev_scaled = seq(min(m$dat$elev_scaled),
                    max(m$dat$elev_scaled), length.out = 50),
  year_scaled = 0,
  time_w_BADO = 0,
  time_w_BADO_scaled = 0,
  log_GHOW = 0,
  log_BADO = 0,
  BADO_count = 0,
  BADO_scaled = 0,
  forest = 0,
  forest_scaled = 0,
  urban = 0,
  urban_scaled = 0,
  year = m$dat$year_median[1],
  year_f = as.factor(m$dat$year_median[1])
)

pe1 <- predict(m, newdata = nde, se_fit = TRUE, re_form = NA)

(ge1 <- pe1 %>%
    ggplot(., aes(elev_scaled*m$dat$elev_sd[1]+m$dat$elev_mean[1],
                  plogis(est),
                  ymin = plogis(est - 1.96 * est_se),
                  ymax = plogis(est + 1.96 * est_se)
    )) +
    geom_ribbon(alpha = 0.1, colour=NA) +
    geom_line() +
    ylim(0,1)+
    ggsidekick::theme_sleek() +
    theme(axis.title.y = element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank()) +
    xlab("Mean Elevation")
)


gt1 <- gt1 + theme(axis.title.y = element_blank())
gy1 <- gy1 + theme(axis.title.y = element_blank())

gt1 + gb1 + gg1 + ge1 + gy1 + gu1 + patchwork::plot_layout(nrow = 3)

# ggsave("figs/WESO-presence-main-effects-all-scaled.png", width = 9, height = 9)
ggsave("figs/WESO-presence-main-effects.png", width = 6, height = 7)

