library(bayestestR)
library(ggplot2)
simulate_simpson(
  n = 100,
  r = 0.5,
  groups = 3,
  difference = 1,
  group_prefix = "Grupo_"
)

data <- simulate_simpson(n = 100, groups = 3, r = 0.5, group_prefix = "Grupo_")


ggplot(data, aes(x = V1, y = V2)) +
  geom_point(aes(color = Group)) +
  geom_smooth(aes(color = Group), method = "lm") +
  geom_smooth(method = "lm")
  
  



