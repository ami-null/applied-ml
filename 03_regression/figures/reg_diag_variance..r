library(tidyverse)
library(ISLR2)                      # Auto data

theme_set(
    theme_minimal(base_size = 9) +
        theme(
            plot.title = element_text(size = 8, face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
)

fit_raw <- lm(mpg ~ poly(horsepower, 2), data = Auto)
fit_log <- lm(log(mpg) ~ poly(horsepower, 2), data = Auto)
res <- bind_rows(tibble(fitted = fitted(fit_raw), resid = resid(fit_raw), response = "response: mpg"),
                                    tibble(fitted = fitted(fit_log), resid = resid(fit_log), response = "response: log(mpg)"))

p <- ggplot(res, aes(fitted, resid)) +
      geom_point(size = 0.5, alpha = 0.5, colour = "grey30") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      facet_wrap(~ response, scales = "free") +
      labs(x = "fitted values", y = "residuals")

p

ggsave("reg_diag_variance.pdf", p, width = 6.0, height = 2.75, bg = "transparent")
