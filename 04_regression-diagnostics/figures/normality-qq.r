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

# left panel: simulated normal errors (reference shape)
set.seed(7)
n_sim <- 200
x_sim <- runif(n_sim, -2, 2)
y_sim <- 1 + 2 * x_sim + rnorm(n_sim)
sim <- tibble(std = rstandard(lm(y_sim ~ x_sim)))

# right panel: Auto, mpg model used on the variance and outlier slides
fit_auto <- lm(mpg ~ horsepower + weight + year, data = Auto)
auto <- tibble(std = rstandard(fit_auto))

qq_panel <- function(df, title) {
    ggplot(df, aes(sample = std)) +
        stat_qq(size = 0.4, alpha = 0.6) +
        stat_qq_line(colour = "red", linewidth = 0.4) +
        labs(title = title, x = "theoretical quantiles", y = "studentized residuals")
}

p_qq <- qq_panel(sim, "simulated normal errors (reference)") | qq_panel(auto, "Auto: mpg model")

p_qq

ggsave("reg_diag_normal_qq.pdf", p_qq, width = 6.0, height = 2.75, bg = "transparent")
