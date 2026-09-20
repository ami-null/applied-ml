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

fit_add <- lm(Sales ~ TV + Radio, data = ad)
fit_int <- lm(Sales ~ TV * Radio, data = ad)
res <- bind_rows(tibble(fitted = fitted(fit_add), resid = resid(fit_add), model = "additive: TV + radio"),
                                    tibble(fitted = fitted(fit_int), resid = resid(fit_int), model = "with TV x radio interaction"))

p <- ggplot(res, aes(fitted, resid)) +
      geom_point(size = 0.5, alpha = 0.6, colour = "grey30") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "red", linewidth = 0.5) +
      facet_wrap(~ model) +
      labs(x = "fitted values", y = "residuals")

p

ggsave("reg_diag_nonlinear.pdf", p, width = 6.0, height = 2.75, bg = "transparent")
