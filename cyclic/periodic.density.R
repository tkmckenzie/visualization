library(dplyr)
library(ggplot2)
library(mgcv)

setwd("~/git/visualization/cyclic")
rm(list = ls())

# Reading data and cleaning
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
  scale_x_continuous(breaks = as.numeric(as.Date(paste0("2020-", 1:12, "-01"))) - as.numeric(as.Date("2020-01-01")) + 1,
                     labels = month.abb) +
  ylim(0, NA) +
  coord_polar() +
  theme_bw()


# Parameters/data for 2D KDE
num.bins = 50
prop.wrap.range = 0.1 # What proportion of x.range to wrap around to ends for KDE

x = abq.temps$DAY.OF.YEAR
y = abq.temps$TAVG

x.range = range(x)
x.step.size = (x.range[2] - x.range[1]) / num.bins
# x.seq = seq(x.range[1] + x.step.size / 2, x.range[2] - x.step.size / 2, length.out = num.bins)

y.range = range(y)
y.range[1] = 0
y.step.size = (y.range[2] - y.range[1]) / num.bins

num.wrap.steps = ceiling((x.range[2] - x.range[1]) * prop.wrap.range / x.step.size)
wrap.window.size = x.step.size * num.wrap.steps
wrap.obs.lower = x <= x.range[1] + wrap.window.size
wrap.obs.upper = x >= x.range[2] - wrap.window.size

x = c(x.range[1] - (x.range[2] - x[wrap.obs.upper]), x, x.range[2] + (x[wrap.obs.lower] - x.range[1]))
y = c(y[wrap.obs.upper], y, y[wrap.obs.lower])

m = MASS::kde2d(x, y, n = num.bins, lim = c(x.range[1] + x.step.size / 2,
                                            x.range[2] - x.step.size / 2,
                                            y.range[1] + y.step.size / 2,
                                            y.range[2] - y.step.size / 2))
plot.df = data.frame(x = rep(m$x, times = num.bins),
                     y = rep(m$y, each = num.bins),
                     fill = c(m$z))

ggplot(plot.df, aes(x = x, y = y, fill = fill)) +
  geom_tile() +
  scale_x_continuous(limits = x.range,
                     breaks = as.numeric(as.Date(paste0("2020-", 1:12, "-01"))) - as.numeric(as.Date("2020-01-01")) + 1,
                     labels = month.abb) +
  ylim(0, NA) +
  coord_polar() +
  theme_bw() +
  theme(legend.position = "top")
