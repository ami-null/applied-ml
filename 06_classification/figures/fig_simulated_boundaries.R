library(ggplot2)

# ── Figure: linear and nonlinear true boundaries (set.seed(1)) ───────────────
# cls_boundaries_linear_nonlinear.pdf

set.seed(1)
n <- 150

sim <- function(f) {
    d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
    d$y <- factor(rbinom(n, 1, plogis(f(d$x1, d$x2))))
    d
}

lin <- sim(function(x1, x2) 3 * (x1 + x2));            lin$panel <- "Linear boundary"
non <- sim(function(x1, x2) 4 * (x1^2 + x2^2 - 1.2)); non$panel <- "Nonlinear boundary"
d <- rbind(lin, non)

t_seq  <- seq(0, 2 * pi, length.out = 200)
circle <- data.frame(x1 = sqrt(1.2) * cos(t_seq), x2 = sqrt(1.2) * sin(t_seq),
                     panel = "Nonlinear boundary")
line   <- data.frame(x1 = c(-3, 3), x2 = c(3, -3), panel = "Linear boundary")

p <- ggplot(d, aes(x1, x2)) +
    geom_point(aes(colour = y), size = 0.9) +
    geom_path(data = circle) +
    geom_path(data = line) +
    facet_wrap(~ panel) +
    scale_colour_manual(values = c("0" = "#0072B2", "1" = "#D55E00")) +
    coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3)) +
    theme_minimal(base_size = 9) +
    theme(legend.position = "none")

p

ggsave("cls_boundaries_linear_nonlinear.pdf", p,
       width = 8.6, height = 3.4, units = "cm")

# ── Figure: fitted logistic boundary and probability contours (set.seed(2)) ──
# cls_logistic_boundary.pdf

set.seed(2)
n    <- 200
d    <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
d$y  <- rbinom(n, 1, plogis(0.5 + 2 * d$x1 - 1.5 * d$x2))
fit  <- glm(y ~ x1 + x2, data = d, family = binomial)
grid <- expand.grid(x1 = seq(-3, 3, length.out = 200),
                    x2 = seq(-3, 3, length.out = 200))
grid$p <- predict(fit, newdata = grid, type = "response")
d$y    <- factor(d$y)

p <- ggplot() +
    geom_contour(data = grid, aes(x1, x2, z = p),
                 breaks = c(0.1, 0.3, 0.7, 0.9), colour = "grey60", linewidth = 0.3) +
    geom_contour(data = grid, aes(x1, x2, z = p),
                 breaks = 0.5, colour = "black", linewidth = 0.8) +
    geom_point(data = d, aes(x1, x2, colour = y), size = 1) +
    scale_colour_manual(values = c("0" = "#0072B2", "1" = "#D55E00")) +
    coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3)) +
    theme_minimal(base_size = 9) +
    theme(legend.position = "none")

p

ggsave("cls_logistic_boundary.pdf", p,
       width = 4.8, height = 4.8, units = "cm")
