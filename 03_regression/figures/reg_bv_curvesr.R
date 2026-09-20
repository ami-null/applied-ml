library(tidyverse)

theme_set(
    theme_minimal(base_size = 9) +
        theme(
            plot.title = element_text(size = 8, face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
)

f <- function(x) 3 * exp(-3 * x) * sin(2.5 * pi * x)     # same setup as the previous figure
n <- 20; sigma <- 0.5; reps <- 500
x_train <- seq(0, 1, length.out = n)
x_grid <- seq(0, 1, length.out = 200)

bias_variance <- function(degree) {
      preds <- sapply(seq_len(reps), function(r) {
            set.seed(r)                                           # same training sets for every degree
            y <- f(x_train) + rnorm(n, sd = sigma)
            predict(lm(y ~ poly(x_train, degree)), data.frame(x_train = x_grid))
          })                                                      # 200 x reps matrix of fitted values
      tibble(degree = degree,
                        bias2 = mean((rowMeans(preds) - f(x_grid))^2),
                        variance = mean(apply(preds, 1, var)))
    }

res <- map_dfr(1:10, bias_variance) |>
      mutate(irreducible = sigma^2, test_mse = bias2 + variance + irreducible) |>
      pivot_longer(c(test_mse, bias2, variance, irreducible), names_to = "term", values_to = "value") |>
      mutate(term = factor(term, levels = c("test_mse", "bias2", "variance", "irreducible"),
                                                    labels = c("expected test MSE", "bias squared", "variance", "irreducible error")))

p <- ggplot(res, aes(degree, value, colour = term, linetype = term)) +
      geom_line(linewidth = 0.6) +
      scale_x_continuous(breaks = 1:10) +
      guides(colour = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2)) +
      labs(title = "expected errors at test points", x = "polynomial degree (flexibility)", y = NULL,
                    colour = NULL, linetype = NULL) +
      theme(legend.position = "bottom")

p

ggsave("reg_bv_curves.pdf", p, width = 3.4, height = 2.75, bg = "transparent")
