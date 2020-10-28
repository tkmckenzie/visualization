library(dplyr)
library(ggfan)
library(ggplot2)
library(gridExtra)

setwd("~/git/m2020/UncertaintyBoundsSurvey/R")
rm(list = ls())

width = 6
height = 6
scale = 1

num.sims = 1000
num.plot.points = 1000
x.seq = seq(-3, 3, length.out = num.plot.points)

mean.mu = 0
sd.mu = 0.5

shape.sigma = 100
rate.sigma = 100

set.seed(0)

mu = rnorm(num.sims, mean.mu, sd.mu)
sigma = rgamma(num.sims, shape.sigma, rate.sigma)

quantiles = sapply(1:num.sims, function(i) pnorm(x.seq, mu[i], sigma[i]))

# Multiple lines
df = data.frame(X = rep(x.seq, times = 100),
                CDF = c(quantiles[,sample(num.sims, 100),drop=FALSE]),
                iteration = rep(1:100, each = num.plot.points))
p.lines = ggplot(df, aes(X, CDF)) +
  geom_line(aes(group = iteration), alpha = 0.25) +
  ggtitle("Sample of Many CDFs") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Ridgeline plot
num.ridges = 20
x.ridges.index = round(seq(1, num.plot.points, length.out = num.ridges))
x.ridges.seq = round(x.seq[x.ridges.index], 1)

df = data.frame(X = factor(rep(x.ridges.seq, times = num.sims)),
                CDF = c(quantiles[x.ridges.index,,drop=FALSE]),
                iteration = rep(1:num.sims, each = num.ridges))
p.ridgeline = ggplot(df, aes(CDF, X)) +
  geom_density_ridges(scale = 5) +
  xlim(0, 1) +
  coord_flip() +
  ggtitle("CDF Ridgeline") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1))

# Internal interval plot
quantile.level = 0.90
df = data.frame(X = x.seq,
                CDF = apply(quantiles, 1, mean),
                ymin = apply(quantiles, 1, quantile, probs = (1 - quantile.level) / 2),
                ymax = apply(quantiles, 1, quantile, probs = 1 - (1 - quantile.level) / 2))
p.interval = ggplot(df, aes(X, CDF)) +
  geom_line() +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.25) +
  ggtitle("CDF 90% Internal Interval") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Fan plot
num.quantile.points = 99
round.digits = 3 # Something weird here with geom_fan accepting high-precision floats, rounding for a quick fix
quantile.seq = seq(0, 1, length.out = num.quantile.points + 2)[-c(1, num.quantile.points + 2)]
quantile.seq = unique(round(sort(c(quantile.seq, c(0.05, 0.25, 0.5, 0.75, 0.95))), round.digits))
num.quantile.points = length(quantile.seq)

quantile.df = data.frame(X = rep(x.seq, times = num.quantile.points),
                         Quantile = rep(quantile.seq, each = num.plot.points),
                         CDF = c(c(t(apply(quantiles, 1, quantile, probs = quantile.seq))))) %>%
  arrange(X, Quantile)
p.interval.gradient = ggplot(quantile.df, aes(X, CDF, quantile = Quantile)) +
  geom_fan(intervals = quantile.seq) +
  scale_fill_distiller(palette = "YlOrRd", limits = c(0, 1)) +
  ggtitle("CDF Interval Gradient") +
  theme_bw() +
  theme(legend.position = c(1, 0), legend.justification = c(1, 0),
        legend.background = element_blank(),
        plot.title = element_text(hjust = 0.5))

# Putting all into a final plot
p.final = arrangeGrob(p.lines, p.ridgeline, p.interval, p.interval.gradient, ncol = 2)
ggsave("images/credible_quantitative_intervals.png", p.final, width = width, height = height, scale = scale)
