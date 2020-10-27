library(dplyr)
library(ggplot2)

rm(list = ls())

# Making moving avg. stat
StatMA = ggproto("StatMA", Stat,
                 required_aes = c("x", "y"),
                 setup_params = function(data, params){
                   if (!is.null(params$window.size)){
                     return(params)
                   }
                   
                   x.split = split(data$x, data$group)
                   max.x.diff = max(vapply(x.split, function(v) max(sort(diff(v))), numeric(1)))
                   fitted.window.size = max(vapply(x.split, function(v) diff(range(v)) / 5, numeric(1)))
                   recommended.window.size = 2 * max(max.x.diff, fitted.window.size)
                   
                   message(paste0("Setting window size to ", signif(recommended.window.size), "."))
                   
                   params$window.size = recommended.window.size
                   return(params)
                 },
                 compute_group = function(data, scales, n = 100, window.size = Inf){
                   x.range = range(data$x, na.rm = TRUE)
                   output.df = data.frame(x = seq(x.range[1], x.range[2], length.out = n))
                   
                   output.df$y = vapply(output.df$x, function(x.i){
                     subset.df = subset(data, abs(x - x.i) <= window.size / 2)
                     
                     weights = dnorm(subset.df$x - x.i)
                     weights = weights / sum(weights)
                     
                     return(subset.df$y %*% weights)
                   }, numeric(1))
                   
                   return(output.df)
                 })
stat_ma = function(mapping = NULL, data = NULL, geom = "line",
                   position = "identity", na.rm = FALSE, show.legend = NA,
                   inherit.aes = TRUE, n = 100, window.size = NULL,
                   ...){
  layer(stat = StatMA, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(n = n, window.size = window.size, na.rm = na.rm, ...))
}


# Data
set.seed(0)
num.points = 100

alpha = runif(1, -5, 5)
beta = runif(1, -5, 5)

df = data.frame(x = runif(num.points, -10, 10)) %>%
  mutate(y = alpha + beta * x + rnorm(num.points, sd = 5)) %>%
  arrange(x)

# Plotting
ggplot(df, aes(x, y)) +
  geom_point() +
  stat_ma() +
  theme_bw()

# Debugging
data = df
n = 100
window.size = 5
