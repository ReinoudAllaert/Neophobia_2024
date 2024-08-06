library(ggplot2)
library(dplyr)
library(ggpubr)


# Create individual violin plots
p1 <- ggplot(data, aes(x = Object, y = sqrt(Latency_to_enter), fill = Context)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, position = position_dodge(width = 0.9), outlier.size = 0.5) +
  theme_minimal() +
  labs(title = "Log-transformed latency to enter", x = "Object", y = "Log Latency to Enter (s)")

p2 <- ggplot(data, aes(x = Object, y = sqrt(Latency_to_Eat + 0.001), fill = Context)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, position = position_dodge(width = 0.9), outlier.size = 0.5) +
  theme_minimal() +
  labs(title = "Log-transformed latency to eat", x = "Object", y = "Log Latency to Eat (s)")

p3 <- ggplot(data, aes(x = Object, y = sqrt(Zoi_duration + 0.001), fill = Context)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, position = position_dodge(width = 0.9), outlier.size = 0.5) +
  theme_minimal() +
  labs(title = " Log-transformed ZOI Duration", x = "Object", y = "Log ZOI Duration (s)")

# Arrange the plots using ggarrange
ggarrange(p1, p2, p3, ncol = 1, nrow = 3, common.legend = TRUE, legend = "bottom")


