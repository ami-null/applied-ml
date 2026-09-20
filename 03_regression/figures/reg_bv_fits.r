library(tidyverse)

theme_set(
    theme_minimal(base_size = 9) +
        theme(
            plot.title = element_text(size = 8, face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
)

set.seed(1)
f <- function(x) 3 * exp(-3 * x) * sin(2.5 * pi * x)     # true regression function
n <- 20; sigma <- 0.5
x_train <- seq(0, 1, length.out = n)                      # fixed feature values
x_grid <- tibble(x = seq(0, 1, length.out = 200))

fit_once <- function(degree, rep) {                       # one training set, one fitted curve
      train <- tibble(x = x_train, y = f(x_train) + rnorm(n, sd = sigma))
      fit <- lm(y ~ poly(x, degree), data = train)
      mutate(x_grid, fit = predict(fit, x_grid), degree = degree, rep = rep)
    }
fits <- expand_grid(degree = c(1, 4, 9), rep = 1:30) |>
      pmap_dfr(fit_once) |>
      mutate(panel = factor(paste("degree", degree), levels = paste("degree", c(1, 4, 9))))

p <- ggplot(fits, aes(x, fit, group = rep)) +
      geom_line(colour = "steelblue", alpha = 0.35, linewidth = 0.3) +
      geom_function(fun = f, inherit.aes = FALSE, colour = "black", linewidth = 0.7) +
      facet_wrap(~ panel) +
      coord_cartesian(ylim = c(-3, 3)) +
      labs(x = "x", y = "fitted value")

p

ggsave("reg_bv_fits.pdf", p, width = 6.0, height = 2.75, bg = "transparent")
