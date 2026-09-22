library(tidyverse)

adv_url <- "https://raw.githubusercontent.com/JWarmenhoven/ISLR-python/master/Notebooks/Data/Advertising.csv"
ad <- read_csv(adv_url, show_col_types = FALSE) |> select(-1)   # drop the row-index column

# ---- Figure: reg_diag_nonlinear.pdf -- additive vs interaction model ------
theme_set(
    theme_minimal(base_size = 9) +
        theme(
            plot.title = element_text(size = 8, face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
)

fit_add <- lm(Sales ~ TV + Radio, data = ad)
fit_int <- lm(Sales ~ TV * Radio, data = ad)
res_nl <- bind_rows(tibble(fitted = fitted(fit_add), resid = resid(fit_add), model = "additive: TV + radio"),
                     tibble(fitted = fitted(fit_int), resid = resid(fit_int), model = "with TV x radio interaction"))

p_nonlinear <- ggplot(res_nl, aes(fitted, resid)) +
  geom_point(size = 0.5, alpha = 0.6, colour = "grey30") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ model) +
  labs(x = "fitted values", y = "residuals")

p_nonlinear

ggsave("reg_diag_nonlinear.pdf", p_nonlinear, width = 6.0, height = 2.75, bg = "transparent")




# ---- Figure: reg_junk_r2.pdf -- R^2 / adjusted R^2 with pure-noise features ----
set.seed(1)
n_noise <- 100
noise <- matrix(rnorm(nrow(ad) * n_noise), nrow(ad), n_noise,
                dimnames = list(NULL, paste0("noise", 1:n_noise)))
ad_noise <- bind_cols(ad, as_tibble(noise))

res_r2 <- map_dfr(seq(0, n_noise, by = 5), function(k) {
  f <- reformulate(c("TV", "Radio", if (k > 0) paste0("noise", 1:k)), response = "Sales")
  s <- summary(lm(f, data = ad_noise))
  tibble(noise_features = k, r2 = s$r.squared, adj_r2 = s$adj.r.squared)
}) |> pivot_longer(c(r2, adj_r2), names_to = "measure", values_to = "value")

p_junk_r2 <- ggplot(res_r2, aes(noise_features, value, colour = measure)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1) +
  scale_colour_manual(values = c(r2 = "steelblue", adj_r2 = "darkorange"),
                      labels = c(r2 = "R-squared", adj_r2 = "adjusted R-squared")) +
  labs(x = "number of pure-noise features added to TV + radio", y = NULL, colour = NULL) +
  theme(legend.position = "bottom")

p_junk_r2

ggsave("reg_junk_r2.pdf", p_junk_r2, width = 6.0, height = 2.75, bg = "transparent")
