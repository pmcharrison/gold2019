library(tidyverse)
theme_set(theme_classic())

outliers <- function(x, threshold = 2) {
  abs(scale(x)) >= threshold
}

dat_4a <- read_csv("input/fig-4a.csv",
                   col_types = cols(), 
                   col_names = c("x", "y")) %>% 
  mutate(x_outlier = outliers(x))

dat_4b <- read_csv("input/fig-4b.csv", 
                   col_types = cols(), 
                   col_names = c("x", "y")) %>% 
  mutate(x_outlier = outliers(x))

dat_4a %>% 
  ggplot(aes(x, y, colour = x_outlier)) +
  geom_point() +
  scale_colour_manual("Outlier", values = c('black', 'red')) +
  theme(aspect.ratio = 1, legend.position = "none")

with(dat_4a, cor.test(x, y))
with(dat_4a %>% filter(!x_outlier), cor.test(x, y))

dat_4b %>% 
  ggplot(aes(x, y, colour = x_outlier)) +
  geom_point() +
  scale_colour_manual("Outlier", values = c('black', 'red')) +
  theme(aspect.ratio = 1, legend.position = "none")

with(dat_4b, cor.test(x, y))
with(dat_4b %>% filter(!x_outlier), cor.test(x, y))
