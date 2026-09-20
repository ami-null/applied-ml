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


fit <- lm(mpg ~ poly(horsepower, 2), data = Auto)
dg <- tibble(fitted = fitted(fit), resid = resid(fit),
                            std = rstandard(fit), lev = hatvalues(fit))
pts <- geom_point(size = 0.4, alpha = 0.6, colour = "grey30")
smooth <- geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                                              colour = "red", linewidth = 0.4)

p1 <- ggplot(dg, aes(fitted, resid)) + pts + smooth +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = "residuals vs fitted", x = "fitted values", y = "residuals")
p2 <- ggplot(dg, aes(sample = std)) +
      stat_qq(size = 0.4, alpha = 0.6) + stat_qq_line(colour = "red", linewidth = 0.4) +
      labs(title = "normal QQ plot", x = "theoretical quantiles", y = "standardized residuals")
p3 <- ggplot(dg, aes(fitted, sqrt(abs(std)))) + pts + smooth +
      labs(title = "scale-location", x = "fitted values", y = "sqrt |standardized residual|")
p4 <- ggplot(dg, aes(lev, std)) + pts +
      geom_hline(yintercept = c(-3, 0, 3), linetype = "dashed") +
      labs(title = "residuals vs leverage", x = "leverage", y = "standardized residuals")

p <- (p1 | p2) / (p3 | p4)
p

ggsave("reg_diag_four.pdf", p, width = 4.4, height = 2.75, bg = "transparent")
