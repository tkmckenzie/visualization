library(dplyr)
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

abq.temps = temps %>%
  filter(Location == "ABQ")

ggplot(abq.temps, aes(DAY.OF.YEAR, TAVG)) +
  geom_point(alpha = 0.1) +
  theme_bw()

# Binning x values
data = abq.temps %>%
  select(x = DAY.OF.YEAR, y = TAVG)

n.bins = 30
x.range = range(data$x)
x.intervals = seq(x.range[1], x.range[2], length.out = n.bins)
x.midpoints = (diff(x.intervals) / 2 + x.intervals[-length(x.intervals)])
data$interval = findInterval(data$x, x.intervals, rightmost.closed = TRUE)
data$x.midpoint = x.midpoints[data$interval]

ggplot(data, aes(x = factor(x.midpoint), y = y)) +
  stat_density(aes(fill = after_stat(density)), geom = "tile", position = "identity") +
  # scale_x_continuous(breaks = as.numeric(as.Date(paste0("2020-", 1:12, "-01"))) - as.numeric(as.Date("2020-01-01")) + 1,
  #                    labels = month.abb) +
  ylim(0, NA) +
  # coord_polar() +
  xlab("") +
  ylab("Temperature (F)") +
  theme_bw() +
  theme(legend.position = "top")
