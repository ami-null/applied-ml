library(tidyverse)

theme_set(
    theme_minimal(base_size = 9) +
        theme(
            plot.title = element_text(size = 8, face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
)

set.seed(2)
f <- function(x) 3 * exp(-3 * x) * sin(2.5 * pi * x)     # true regression function
n <- 20; sigma <- 0.5
train_df <- tibble(x = seq(0, 1, length.out = n)) |> mutate(y = f(x) + rnorm(n, sd = sigma))
test_df <- tibble(x = runif(1000)) |> mutate(y = f(x) + rnorm(1000, sd = sigma))

err <- map_dfr(1:10, function(d) {
      fit <- lm(y ~ poly(x, d), data = train_df)
      tibble(degree = d, training = mean(resid(fit)^2),
                        test = mean((test_df$y - predict(fit, test_df))^2))
    }) |> pivot_longer(c(training, test), names_to = "set", values_to = "mse")

p <- ggplot(err, aes(degree, mse, colour = set)) +
      geom_line(linewidth = 0.6) +
      geom_point(size = 1) +
      geom_hline(yintercept = sigma^2, linetype = "dashed") +
      annotate("text", x = 10, y = sigma^2, label = "irreducible error", vjust = -0.6, hjust = 1, size = 2.5) +
      scale_x_continuous(breaks = 1:10) +
      labs(title = "polynomial regression, simulated data", x = "polynomial degree", y = "MSE", colour = NULL)

p

ggsave("reg_train_test.pdf", p, width = 3.4, height = 2.75, bg = "transparent")
