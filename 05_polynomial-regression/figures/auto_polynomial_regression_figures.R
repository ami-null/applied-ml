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

# ---- reg_auto_scatter.pdf: straight-line fit and its residuals ----
lin <- lm(mpg ~ horsepower, data = Auto)

p_fit <- ggplot(Auto, aes(horsepower, mpg)) +
  geom_point(size = 0.5, alpha = 0.5, colour = "grey40") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "orange", linewidth = 0.6) +
  labs(title = "straight-line fit", x = "horsepower", y = "mpg")
p_res <- ggplot(tibble(fitted = fitted(lin), resid = resid(lin)), aes(fitted, resid)) +
  geom_point(size = 0.5, alpha = 0.5, colour = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "residuals vs fitted values", x = "fitted values", y = "residuals")

p <- p_fit | p_res
p
ggsave("reg_auto_scatter.pdf", p, width = 6.0, height = 2.75, bg = "transparent")


# ---- reg_auto_poly.pdf: fits of degree 1, 2, 5 + residuals ----
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
  facet_wrap(~ model) +
  labs(title = "residuals vs fitted values", x = "fitted values", y = "residuals")

p <- p_fit + p_res + plot_layout(widths = c(1, 1.6))
p
ggsave("reg_auto_poly.pdf", p, width = 6.0, height = 2.75, bg = "transparent")

# ---- reg_poly_extrap.pdf: fits extended beyond the data ----
grid <- tibble(horsepower = seq(40, 300, length.out = 300))
curves <- map_dfr(c(1, 2, 5), function(d) {
  m <- lm(mpg ~ poly(horsepower, d), data = Auto)
  mutate(grid, fit = predict(m, grid), degree = factor(d))
})

p <- ggplot(Auto, aes(horsepower, mpg)) +
  annotate("rect", xmin = max(Auto$horsepower), xmax = 300, ymin = -Inf, ymax = Inf, alpha = 0.08) +
  geom_point(size = 0.5, alpha = 0.5, colour = "grey40") +
  geom_line(data = curves, aes(y = fit, colour = degree), linewidth = 0.6) +
  geom_vline(xintercept = max(Auto$horsepower), linetype = "dashed") +
  scale_colour_manual(values = c("1" = "orange", "2" = "red", "5" = "blue")) +
  coord_cartesian(xlim = c(40, 300), ylim = c(0, 60)) +
  labs(title = "shaded: beyond the data", x = "horsepower", y = "mpg", colour = "degree")
p
ggsave("reg_poly_extrap.pdf", p, width = 6.0, height = 2.75, bg = "transparent")
