# https://stats.stackexchange.com/questions/412817/how-to-visualize-an-evolution-of-a-distribution-in-time
library(dplyr)
library(ggplot2)
library(ggridges)

setwd("~/git/visualization/density")
rm(list = ls())

temps = read.csv("../data/temps.csv") %>%
  mutate(DATE = as.Date(DATE),
         # MONTH = as.factor(as.numeric(strftime(DATE, "%m"))),
         MONTH = as.factor(strftime(DATE, "%b")),
         MONTH = factor(MONTH, levels = rev(month.abb)),
         YEAR = as.factor(as.numeric(strftime(DATE, "%Y")))) %>%
  rename(Location = NAME) %>%
  na.exclude

abq.temps = temps %>%
  filter(Location == "ALBUQUERQUE INTERNATIONAL AIRPORT, NM US")

# Ridgeline of single series/location aggregated by month
ggplot(abq.temps, aes(TAVG, MONTH)) +
  geom_density_ridges() +
  xlab("Average Temperature (F)") +
  ylab("Month") +
  ggtitle("Density of Average Temperatures in ABQ By Month") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Ridgeline of single series/location aggregated by month, radial
# Needs some adjustment to y coords of density to correct for distortion, but pretty cool nonetheless
ggplot(abq.temps, aes(TAVG, MONTH)) +
  geom_density_ridges() +
  coord_polar(theta = "y") +
  xlab("Average Temperature (F)") +
  ylab("Month") +
  ggtitle("Density of Average Temperatures in ABQ By Month") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Ridgeline of single series/location aggregated by month with years as different colors
ggplot(abq.temps, aes(TAVG, MONTH)) +
  geom_density_ridges(aes(fill = YEAR), alpha = 0.25) +
  scale_fill_brewer(palette = "YlOrRd") +
  xlab("Average Temperature (F)") +
  ylab("Month") +
  ggtitle("Density of Average Temperatures in ABQ By Month Across Years") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Ridgeline of multiple series/locations aggregated by month as different colors on same plot
ggplot(temps, aes(TAVG, MONTH)) +
  geom_density_ridges(aes(fill = Location), alpha = 0.5) +
  scale_fill_brewer(palette = "Accent") +
  xlab("Average Temperature (F)") +
  ylab("Month") +
  ggtitle("Density of Average Temperatures At Four Airports By Month") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

# Ridgeline of multiple series/locations aggregated by month as different facets
ggplot(temps, aes(TAVG, MONTH)) +
  geom_density_ridges(aes(fill = Location), alpha = 0.5) +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(~ Location, scales = "fixed") +
  xlab("Average Temperature (F)") +
  ylab("Month") +
  ggtitle("Density of Average Temperatures At Four Airports By Month") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
  guides(fill = "none")

# Ridgeline of multiple series/locations aggregated by month as different facets, gradient fill
# Gradient fill corresponds to temperature and makes it easier to compare across facets
ggplot(temps, aes(TAVG, MONTH, fill = after_stat(x))) +
  geom_density_ridges_gradient() +
  scale_fill_viridis_c(option = "C", name = "Temperature (F)") +
  facet_wrap(~ Location, scales = "fixed") +
  xlab("Average Temperature (F)") +
  ylab("Month") +
  ggtitle("Density of Average Temperatures At Four Airports By Month") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "right")
