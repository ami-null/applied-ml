library(tidyverse)
library(patchwork)
library(ISLR2)                      # Auto data


fit <- lm(mpg ~ horsepower + weight + year, data = Auto)

# ---- Figure: reg_diag_four.pdf -- four standard diagnostic plots ----------
theme_set(
    theme_minimal(base_size = 9) +
        theme(
            plot.title = element_text(size = 8, face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
)


dg <- tibble(fitted = fitted(fit), resid = resid(fit),
             std = rstandard(fit), lev = hatvalues(fit))
pts <- geom_point(size = 0.4, alpha = 0.6, colour = "grey30")

p1 <- ggplot(dg, aes(fitted, resid)) + pts +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "residuals vs fitted", x = "fitted values", y = "residuals")
p2 <- ggplot(dg, aes(sample = std)) +
  stat_qq(size = 0.4, alpha = 0.6) + stat_qq_line(colour = "red", linewidth = 0.4) +
  labs(title = "normal QQ plot", x = "theoretical quantiles", y = "standardized residuals")
p3 <- ggplot(dg, aes(fitted, sqrt(abs(std)))) + pts +
  labs(title = "scale-location", x = "fitted values", y = "sqrt |standardized residual|")
p4 <- ggplot(dg, aes(lev, std)) + pts +
  geom_hline(yintercept = c(-3, 0, 3), linetype = "dashed") +
  labs(title = "residuals vs leverage", x = "leverage", y = "standardized residuals")

p_four <- (p1 | p2) / (p3 | p4)

p_four

ggsave("reg_diag_four.pdf", p_four, width = 4.4, height = 2.75, bg = "transparent")



# ---- Figure: reg_diag_variance.pdf -- mpg vs log(mpg) as the response -----
theme_set(
    theme_minimal(base_size = 9) +
        theme(
            plot.title = element_text(size = 8, face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
)


fit_raw <- fit
fit_log <- lm(log(mpg) ~ horsepower + weight + year, data = Auto)
res <- bind_rows(
  tibble(fitted = fitted(fit_raw), resid = resid(fit_raw), response = "response: mpg"),
  tibble(fitted = fitted(fit_log), resid = resid(fit_log), response = "response: log(mpg)"))

p_variance <- ggplot(res, aes(fitted, resid)) +
  geom_point(size = 0.5, alpha = 0.5, colour = "grey30") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ response, scales = "free") +
  labs(x = "fitted values", y = "residuals")

p_variance

ggsave("reg_diag_variance.pdf", p_variance, width = 6.0, height = 2.75, bg = "transparent")
