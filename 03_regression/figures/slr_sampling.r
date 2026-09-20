library(tidyverse)
library(patchwork)
theme_set(theme_minimal(base_size = 9) +
              theme(plot.title = element_text(size = 9, face = "bold")))

set.seed(1)
beta0 <- 2; beta1 <- 3; sigma <- 3; n <- 100          # the "true" model y = 2 + 3x + error
sim <- function(id) {
      x <- runif(n, -2, 2)
      tibble(id = id, x = x, y = beta0 + beta1 * x + rnorm(n, sd = sigma))
    }

p_one <- ggplot(sim(1), aes(x, y)) +
      geom_point(size = 0.5, colour = "grey40") +
      geom_abline(intercept = beta0, slope = beta1, colour = "red", linewidth = 0.6) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "blue", linewidth = 0.6) +
      labs(title = "one sample: true line (red), fitted line (blue)")

p_many <- ggplot(map_dfr(1:10, sim), aes(x, y, group = id)) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "lightblue", linewidth = 0.4) +
      geom_abline(intercept = beta0, slope = beta1, colour = "red", linewidth = 0.7) +
      coord_cartesian(ylim = c(-12, 16)) +
      labs(title = "ten samples: ten different fitted lines")

p <- p_one | p_many
p

ggsave("reg_slr_sampling.pdf", p, width = 6.0, height = 2.75)
