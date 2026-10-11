# sim_biasvar.R
# Simulated data: y = sin(2 pi x) + eps, polynomial regression of degree 1..10.
# Produces: sim_fits.pdf, sim_train_test_mse.pdf, sim_biasvar_decomp.pdf

library(ggplot2)
library(dplyr)
library(tidyr)


f <- function(x) sin(2 * pi * x)
sigma <- 0.3
n <- 30
degrees <- 1:10

# training x always contains the endpoints 0 and 1, so no extrapolation on [0, 1]
sim_data <- function(n) {
  x <- c(0, 1, runif(n - 2))
  data.frame(x = x, y = f(x) + rnorm(n, sd = sigma))
}

set.seed(1)
train <- sim_data(n)

# ---- Figure 1: fits of degree 1, 3, 10 -------------------------------------
x_grid   <- seq(0, 1, length.out = 200)
deg_show <- c(1, 3, 10)
lab_show <- paste("Degree", deg_show)

fits <- bind_rows(lapply(deg_show, function(d) {
  m <- lm(y ~ poly(x, d), data = train)
  data.frame(x = x_grid,
             yhat = predict(m, data.frame(x = x_grid)),
             model = factor(paste("Degree", d), levels = lab_show))
}))

truth <- data.frame(x = x_grid, y = f(x_grid))

p1 <- ggplot() +
  geom_point(data = train, aes(x, y), colour = "grey40", size = 1.2) +
  geom_line(data = truth, aes(x, y, colour = "True f"), linewidth = 0.7) +
  geom_line(data = fits, aes(x, yhat, colour = "Fitted"), linewidth = 0.8) +
  facet_wrap(~ model) +
  scale_colour_manual(values = c("True f" = "black", "Fitted" = "#D55E00"), name = NULL) +
  labs(x = "x", y = "y") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

p1

ggsave("sim_fits.pdf", p1, width = 9, height = 3.2)

# ---- Figure 2: training vs test MSE (one training set, large test set) ------
test <- sim_data(5000)

mse <- bind_rows(lapply(degrees, function(d) {
  m <- lm(y ~ poly(x, d), data = train)
  data.frame(degree = d,
             Training = mean(residuals(m)^2),
             Test = mean((test$y - predict(m, test))^2))
})) |>
  pivot_longer(c(Training, Test), names_to = "set", values_to = "mse")

p2 <- ggplot(mse, aes(degree, mse, colour = set)) +
  geom_hline(yintercept = sigma^2, linetype = "dashed", colour = "grey30") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = degrees) +
  scale_colour_manual(values = c(Training = "#0072B2", Test = "#D55E00"), name = NULL) +
  labs(x = "Polynomial degree (flexibility)", y = "MSE") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

p2

ggsave("sim_train_test_mse.pdf", p2, width = 5, height = 3.4)

# ---- Figure 3: bias-variance decomposition from repeated training sets ------
R      <- 500
x_test <- seq(0, 1, length.out = 100)
preds  <- array(NA_real_, dim = c(R, length(x_test), length(degrees)))

set.seed(2)
for (r in seq_len(R)) {
  tr <- sim_data(n)
  for (d in degrees) {
    m <- lm(y ~ poly(x, d), data = tr)
    preds[r, , d] <- predict(m, data.frame(x = x_test))
  }
}

bv <- bind_rows(lapply(degrees, function(d) {
  P <- preds[, , d]
  data.frame(degree = d,
             bias2 = mean((colMeans(P) - f(x_test))^2),
             variance = mean(apply(P, 2, var)))
})) |>
  mutate(noise = sigma^2, test_mse = bias2 + variance + noise)

comp_levels <- c("Expected test MSE", "Squared bias", "Variance", "Irreducible error")
bv_long <- bv |>
  pivot_longer(c(test_mse, bias2, variance, noise), names_to = "component", values_to = "value") |>
  mutate(component = factor(component,
                            levels = c("test_mse", "bias2", "variance", "noise"),
                            labels = comp_levels))

best <- bv$degree[which.min(bv$test_mse)]

p3 <- ggplot(bv_long, aes(degree, value, colour = component, linetype = component)) +
  geom_vline(xintercept = best, colour = "grey60", linetype = "dotted") +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = degrees) +
  scale_colour_manual(values = c("Expected test MSE" = "black", "Squared bias" = "#0072B2",
                                 "Variance" = "#D55E00", "Irreducible error" = "grey40"),
                      name = NULL) +
  scale_linetype_manual(values = c("Expected test MSE" = "solid", "Squared bias" = "solid",
                                   "Variance" = "solid", "Irreducible error" = "dashed"),
                        name = NULL) +
  coord_cartesian(ylim = c(0, max(bv$test_mse[bv$degree <= 8]) * 1.1)) +
  labs(x = "Polynomial degree (flexibility)", y = NULL) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2))

p3

ggsave("sim_biasvar_decomp.pdf", p3, width = 5, height = 3.6)
