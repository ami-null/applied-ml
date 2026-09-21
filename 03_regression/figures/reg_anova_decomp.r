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
ad <- read_csv(adv_url, show_col_types = FALSE) |> select(-1)   # drop the row-index column

# total deviation = explained part + residual, for five markets
fit <- lm(Sales ~ TV, data = ad)
ybar <- mean(ad$Sales)
pts <- ad |>
      mutate(fitted = fitted(fit)) |>
      arrange(TV) |>
      slice(round(seq(15, 185, length.out = 5)))

parts <- c("total: y - mean(y)", "explained: fitted - mean(y)", "residual: y - fitted")
seg <- bind_rows(
      transmute(pts, x = TV - 4, y0 = ybar,   y1 = Sales,  part = parts[1]),
      transmute(pts, x = TV,     y0 = ybar,   y1 = fitted, part = parts[2]),
      transmute(pts, x = TV + 4, y0 = fitted, y1 = Sales,  part = parts[3])
    ) |> mutate(part = factor(part, levels = parts))

p <- ggplot(ad, aes(TV, Sales)) +
      geom_point(size = 0.5, colour = "grey70") +
      geom_hline(yintercept = ybar, linetype = "dashed") +
      geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], colour = "red", linewidth = 0.5) +
      geom_segment(data = seg, aes(x = x, xend = x, y = y0, yend = y1, colour = part),
                                    linewidth = 0.8, inherit.aes = FALSE) +
      geom_point(data = pts, size = 1.2) +
      scale_colour_manual(values = c("grey30", "steelblue", "darkorange")) +
      labs(x = "TV budget (thousands of dollars)", y = "sales (thousands of units)", colour = NULL) +
      theme(legend.position = "bottom")

p

ggsave("reg_anova_decomp.pdf", p, width = 6.0, height = 2.75, bg = "transparent")
