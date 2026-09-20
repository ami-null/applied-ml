library(tidyverse)
library(patchwork)
library(ISLR2)                      # Auto data

theme_set(
    theme_minimal(base_size = 9) +
        theme(
            plot.title = element_text(size = 8, face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
)


grid <- tibble(horsepower = seq(min(Auto$horsepower), max(Auto$horsepower), length.out = 200))

curves <- map_dfr(c(1, 2, 5), function(d) {
      m <- lm(mpg ~ poly(horsepower, d), data = Auto)
      mutate(grid, fit = predict(m, grid), degree = factor(d))
    })

p_fit <- ggplot(Auto, aes(horsepower, mpg)) +
      geom_point(size = 0.5, alpha = 0.5, colour = "grey40") +
      geom_line(data = curves, aes(y = fit, colour = degree), linewidth = 0.6) +
      scale_colour_manual(values = c("1" = "orange", "2" = "red", "5" = "blue")) +
      labs(title = "fits of degree 1, 2 and 5", x = "horsepower", y = "mpg") +
      theme(legend.position = "inside", legend.position.inside = c(0.85, 0.8))

lin <- lm(mpg ~ horsepower, data = Auto)
quad <- lm(mpg ~ poly(horsepower, 2), data = Auto)
res <- bind_rows(tibble(fitted = fitted(lin), resid = resid(lin), model = "linear"),
                                    tibble(fitted = fitted(quad), resid = resid(quad), model = "quadratic"))
p_res <- ggplot(res, aes(fitted, resid)) +
      geom_point(size = 0.5, alpha = 0.5, colour = "grey40") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "red", linewidth = 0.5) +
      facet_wrap(~ model) +
      labs(title = "residuals vs fitted values", x = "fitted values", y = "residuals")

p <- p_fit + p_res + plot_layout(widths = c(1, 1.6))

p

ggsave("reg_auto_poly.pdf", p, width = 6.0, height = 2.75, bg = "transparent")
