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
         YEAR = as.factor(as.numeric(strftime(DATE, "%Y"))),
         NAME = recode(NAME,
                       "ALBUQUERQUE INTERNATIONAL AIRPORT, NM US" = "ABQ",
                       "PORTLAND INTERNATIONAL AIRPORT, OR US" = "PDX",
                       "SAN FRANCISCO INTERNATIONAL AIRPORT, CA US" = "SFO",
                       "JFK INTERNATIONAL AIRPORT, NY US" = "JFK")) %>%
  rename(Location = NAME) %>%
  na.exclude

ggplot(temps, aes(DAY.OF.YEAR, TAVG)) +
  # geom_point(aes(color = Location), alpha = 0.1) +
  geom_smooth(aes(color = Location), method = "gam", formula = y ~ s(x, bs = "cc", k = 12), se = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = as.numeric(as.Date(paste0("2020-", 1:12, "-01"))) - as.numeric(as.Date("2020-01-01")) + 1,
                     labels = month.abb) +
  ylim(0, NA) +
  xlab("") +
  coord_polar() +
  theme_bw() +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.direction = "vertical",
        legend.background = element_blank())
