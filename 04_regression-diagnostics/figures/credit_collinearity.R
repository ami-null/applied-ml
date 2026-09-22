library(tidyverse)
library(patchwork)
library(ISLR2)                      # Credit data

theme_set(
    theme_minimal(base_size = 9) +
        theme(
            plot.title = element_text(size = 8, face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
)

p1 <- ggplot(Credit, aes(Limit, Age)) +
  geom_point(size = 0.5, alpha = 0.5, colour = "grey30") +
  labs(title = sprintf("age vs limit (r = %.2f)", cor(Credit$Limit, Credit$Age)))
p2 <- ggplot(Credit, aes(Limit, Rating)) +
  geom_point(size = 0.5, alpha = 0.5, colour = "grey30") +
  labs(title = sprintf("rating vs limit (r = %.3f)", cor(Credit$Limit, Credit$Rating)))

p <- p1 | p2

p

ggsave("reg_credit_collinearity.pdf", p, width = 6.0, height = 2.75, bg = "transparent")
