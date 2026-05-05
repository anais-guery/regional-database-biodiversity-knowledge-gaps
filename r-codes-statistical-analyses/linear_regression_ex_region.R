library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)

#Prepare the datasets
setwd("")
data <- st_read("")
data <- st_drop_geometry(data)

#Linear regression
rl <- lm(inverseign~  moyhotspot +maxnatural+ moypressag + moypressfr+ moypressur,
         data = data)
summary(rl)

#Plot the results
data_long <- pivot_longer(data,
                          cols = c(moyhotspot, maxnatural, moypressag, moypressfr, moypressur),
                          names_to = "variable",
                          values_to = "value")


ggplot(data_long, aes(x = value, y = lacunesesp)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~ variable, scales = "free_x") +
  labs(x = "Predictor Value", y = "Lacunes") +
  theme_minimal()