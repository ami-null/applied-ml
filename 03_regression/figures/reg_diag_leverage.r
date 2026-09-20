library(tidyverse)
library(patchwork)

theme_set(
    theme_minimal(base_size = 9) +
        theme(
            plot.title = element_text(size = 8, face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
)


set.seed(6)
n <- 50
d <- tibble(x = runif(n, -2, 2)) |> mutate(y = 1 + 2 * x + rnorm(n, sd = 0.7))
hl <- tibble(x = 5, y = 2)                              # extreme x, far below the line
d2 <- bind_rows(d, hl)
fit_all <- lm(y ~ x, data = d2)
fit_wo <- lm(y ~ x, data = d)

p_data <- ggplot(d2, aes(x, y)) +
      geom_point(size = 0.6, colour = "grey30") +
      geom_point(data = hl, colour = "red", size = 1.6) +
      geom_abline(intercept = coef(fit_all)[1], slope = coef(fit_all)[2], colour = "red", linewidth = 0.5) +
      geom_abline(intercept = coef(fit_wo)[1], slope = coef(fit_wo)[2], colour = "blue",
                                  linetype = "dashed", linewidth = 0.5) +
      labs(title = "fit with (red) and without (blue) the point")

dg <- tibble(lev = hatvalues(fit_all), std = rstandard(fit_all), extreme = seq_len(n + 1) == n + 1)
p_lev <- ggplot(dg, aes(lev, std, colour = extreme)) +
      geom_point(size = 0.6) +
      geom_vline(xintercept = 2 / (n + 1), linetype = "dashed") +      # average leverage (p + 1) / n
      geom_hline(yintercept = c(-3, 3), linetype = "dashed") +
      scale_colour_manual(values = c("grey30", "red"), guide = "none") +
      labs(title = "leverage vs studentized residual", x = "leverage", y = "studentized residual")

p <- p_data | p_lev
p

ggsave("reg_diag_leverage.pdf", p, width = 6.0, height = 2.75, bg = "transparent")
