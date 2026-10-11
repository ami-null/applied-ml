# auto.R
# Auto data (ISLR2): mpg ~ polynomial(horsepower, d), d = 1..10.
# Produces: auto_fits.pdf, auto_validation_splits.pdf,
#           auto_cv_curves.pdf

library(ISLR2)
library(ggplot2)
library(dplyr)
library(tidyr)

data(Auto)
n <- nrow(Auto)
degrees <- 1:10

# ---- Figure 1: scatter plot with polynomial fits ----------------------------
hp_grid  <- data.frame(horsepower = seq(min(Auto$horsepower), max(Auto$horsepower), length.out = 200))
deg_show <- c(1, 2, 5)
fits <- bind_rows(lapply(deg_show, function(d) {
  m <- lm(mpg ~ poly(horsepower, d), data = Auto)
  data.frame(horsepower = hp_grid$horsepower,
             mpg = predict(m, hp_grid),
             degree = factor(d))
}))

p1 <- ggplot() +
  geom_point(data = Auto, aes(horsepower, mpg), colour = "grey50", alpha = 0.6, size = 1.2) +
  geom_line(data = fits, aes(horsepower, mpg, colour = degree), linewidth = 0.8) +
  scale_colour_manual(values = c("1" = "#0072B2", "2" = "#D55E00", "5" = "#009E73"),
                      name = "Degree") +
  labs(x = "Horsepower", y = "Miles per gallon") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

p1

ggsave("auto_fits.pdf", p1, width = 5, height = 3.6)

# ---- Figure 2: validation set approach, one split and ten splits ------------
val_mse <- function(seed) {
  set.seed(seed)
  train <- sample(n, n %/% 2)
  sapply(degrees, function(d) {
    fit <- lm(mpg ~ poly(horsepower, d), data = Auto, subset = train)
    mean((Auto$mpg - predict(fit, Auto))[-train]^2)
  })
}

val <- bind_rows(lapply(1:10, function(s) data.frame(split = s, degree = degrees, mse = val_mse(s))))

val_plot <- bind_rows(
  val |> filter(split == 1) |> mutate(panel = "One random split"),
  val |> mutate(panel = "Ten random splits")
) |>
  mutate(panel = factor(panel, levels = c("One random split", "Ten random splits")))

p2 <- ggplot(val_plot, aes(degree, mse, group = split, colour = factor(split))) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.4) +
  facet_wrap(~ panel) +
  scale_x_continuous(breaks = degrees) +
  labs(x = "Polynomial degree", y = "Validation MSE") +
  guides(colour = "none") +
  theme_bw(base_size = 11)

p2

ggsave("auto_validation_splits.pdf", p2, width = 8.5, height = 3.2)

# ---- Cross-validation helpers -----------------------------------------------
# LOOCV via the hat-value shortcut for least squares
loocv <- function(d) {
  fit <- lm(mpg ~ poly(horsepower, d), data = Auto)
  mean((residuals(fit) / (1 - hatvalues(fit)))^2)
}

# per-fold MSEs of k-fold CV
cv_folds <- function(d, k = 10, seed = 1) {
  set.seed(seed)
  fold <- sample(rep(seq_len(k), length.out = n))
  sapply(seq_len(k), function(j) {
    fit <- lm(mpg ~ poly(horsepower, d), data = Auto[fold != j, ])
    mean((Auto$mpg[fold == j] - predict(fit, Auto[fold == j, ]))^2)
  })
}

# ---- Figure 3: LOOCV and ten-fold CV curves ---------------------------------
loo <- data.frame(degree = degrees, cv = sapply(degrees, loocv), seed = 0,
                  panel = "LOOCV")
kf  <- bind_rows(lapply(1:9, function(s) {
  data.frame(degree = degrees, cv = sapply(degrees, function(d) mean(cv_folds(d, 10, s))),
             seed = s, panel = "10-fold CV (nine different partitions)")
}))

cv_plot <- bind_rows(loo, kf) |>
  mutate(panel = factor(panel, levels = c("LOOCV", "10-fold CV (nine different partitions)")))

p3 <- ggplot(cv_plot, aes(degree, cv, group = seed, colour = factor(seed))) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.4) +
  facet_wrap(~ panel) +
  scale_x_continuous(breaks = degrees) +
  labs(x = "Polynomial degree", y = "CV estimate of test MSE") +
  guides(colour = "none") +
  theme_bw(base_size = 11)

p3

ggsave("auto_cv_curves.pdf", p3, width = 8.5, height = 3.2)
