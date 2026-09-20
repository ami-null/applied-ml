library(tidyverse)
library(patchwork)
theme_set(theme_minimal(base_size = 9) +
              theme(plot.title = element_text(size = 9, face = "bold")))

adv_url <- "https://raw.githubusercontent.com/JWarmenhoven/ISLR-python/master/Notebooks/Data/Advertising.csv"
ad <- read_csv(adv_url, show_col_types = FALSE) |> select(-1)

fit <- lm(Sales ~ TV, data = ad)
ad <- ad |> mutate(fitted = fitted(fit))

p_line <- ggplot(ad, aes(TV, Sales)) +
      geom_segment(aes(xend = TV, yend = fitted), colour = "grey65", linewidth = 0.2) +
      geom_point(size = 0.6, colour = "grey30") +
      geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], colour = "red", linewidth = 0.6) +
      labs(title = "residuals: vertical distances to the line",
                    x = "TV budget (thousands of dollars)", y = "sales (thousands of units)")

# RSS as a function of the slope; the intercept is set to its best value for each slope
rss_of_slope <- function(b1) sum((ad$Sales - (mean(ad$Sales) - b1 * mean(ad$TV)) - b1 * ad$TV)^2)
profile <- tibble(b1 = seq(0.02, 0.075, length.out = 200)) |>
      mutate(rss = map_dbl(b1, rss_of_slope))
best <- tibble(b1 = unname(coef(fit)[2]), rss = sum(resid(fit)^2))

p_rss <- ggplot(profile, aes(b1, rss)) +
      geom_line() +
      geom_point(data = best, colour = "red", size = 1.6) +
      labs(title = "RSS as a function of the slope", x = "slope", y = "RSS")

p <- p_line | p_rss

p

ggsave("reg_slr_ls.pdf", p, width = 6.0, height = 2.75)
