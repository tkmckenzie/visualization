library(ggplot2)
library(mgcv)

setwd("~/git/visualization/cyclic")
rm(list = ls())

temps = read.csv("../data/temps.csv") %>%
  mutate(DATE = as.Date(DATE),
         DAY.OF.YEAR = as.numeric(DATE) - as.numeric(as.Date(strftime(DATE, "%Y-01-01"))) + 1,
         MONTH = as.factor(strftime(DATE, "%b")),
         MONTH = factor(MONTH, levels = month.abb),
         MONTH.num = as.numeric(MONTH),
         YEAR = as.factor(as.numeric(strftime(DATE, "%Y")))) %>%
  rename(Location = NAME) %>%
  na.exclude

abq.temps = temps %>%
  filter(Location == "ALBUQUERQUE INTERNATIONAL AIRPORT, NM US")

ggplot(abq.temps, aes(DAY.OF.YEAR, TAVG)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc", k = 12), se = FALSE) +
  ylim(0, NA) +
  coord_polar() +
  theme_bw()

ggplot(temps, aes(DAY.OF.YEAR, TAVG)) +
  geom_smooth(aes(color = Location), method = "gam", formula = y ~ s(x, bs = "cc", k = 12), se = FALSE) +
  ylim(0, NA) +
  coord_polar() +
  theme_bw() +
  theme(legend.position = "top")
