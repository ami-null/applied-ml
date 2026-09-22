library(tidyverse)
theme_set(
    theme_minimal(base_size = 9) +
        theme(
            plot.title = element_text(size = 8, face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
    )

adv_url <- "https://raw.githubusercontent.com/JWarmenhoven/ISLR-python/master/Notebooks/Data/Advertising.csv"
ad <- read_csv(adv_url, show_col_types = FALSE) |> select(-1)

fit <- lm(Sales ~ TV, data = ad)
grid <- tibble(TV = seq(0, 300, length.out = 100))
conf_band <- bind_cols(grid, as_tibble(predict(fit, grid, interval = "confidence")))
pred_band <- bind_cols(grid, as_tibble(predict(fit, grid, interval = "prediction")))

p <- ggplot(ad, aes(TV, Sales)) +
    geom_ribbon(data = pred_band, aes(TV, ymin = lwr, ymax = upr), inherit.aes = FALSE,
                              fill = "steelblue", alpha = 0.15) +
    geom_ribbon(data = conf_band, aes(TV, ymin = lwr, ymax = upr), inherit.aes = FALSE,
                              fill = "steelblue", alpha = 0.45) +
    geom_point(size = 0.5, colour = "grey30") +
    geom_line(data = conf_band, aes(TV, fit), inherit.aes = FALSE, colour = "red", linewidth = 0.6) +
    labs(title = "95% CI and PI bands",
         x = "TV budget (thousands of dollars)", y = "sales (thousands of units)")

p

ggsave("reg_ci_pi.pdf", p, width = 3.4, height = 2.75, bg = "transparent")
