library(ggplot2)
library(class)

# ── Shared data (set.seed(3)) ─────────────────────────────────────────────────

set.seed(3)
n <- 200
d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
d$y <- factor(rbinom(n, 1, plogis(2.5 * (d$x1 + 0.5 * d$x2^2 - 0.5))))   # curved true boundary

# ── Figure: k-NN boundaries for k = 1, 15, 100 ───────────────────────────────
# cls_knn_k.pdf

grid_150 <- expand.grid(x1 = seq(-3, 3, length.out = 150),
                        x2 = seq(-3, 3, length.out = 150))

knn_prob <- function(k) {
    cl <- knn(train = d[, c("x1", "x2")], test = grid_150,
              cl = d$y, k = k, prob = TRUE)
    ifelse(cl == "1", attr(cl, "prob"), 1 - attr(cl, "prob"))   # P(y = 1 | x)
}

ks   <- c(1, 15, 100)
pred <- do.call(rbind, lapply(ks, function(k)
    data.frame(grid_150, p1 = knn_prob(k), k = paste0("k = ", k))))
pred$k <- factor(pred$k, levels = paste0("k = ", ks))

p <- ggplot() +
    geom_raster(data = pred, aes(x1, x2, fill = p1 > 0.5), alpha = 0.25) +
    geom_contour(data = pred, aes(x1, x2, z = p1),
                 breaks = 0.5, colour = "black", linewidth = 0.1) +
    geom_point(data = d, aes(x1, x2, colour = y), size = 0.1, alpha = 0.5) +
    facet_wrap(~ k) +
    scale_fill_manual(values = c("TRUE" = "#D55E00", "FALSE" = "#0072B2"), guide = "none") +
    scale_colour_manual(values = c("0" = "#0072B2", "1" = "#D55E00"), guide = "none") +
    coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3), expand = FALSE) +
    theme_minimal(base_size = 8)

p

ggsave("cls_knn_k.pdf", p, width = 10, height = 4.25, units = "cm")

# ── Figure: logistic boundary vs k-NN (k = 15) on the same data ──────────────
# cls_logistic_vs_knn.pdf

grid_200 <- expand.grid(x1 = seq(-3, 3, length.out = 200),
                        x2 = seq(-3, 3, length.out = 200))

fit <- glm(y ~ x1 + x2, data = d, family = binomial)
grid_200$p_logit <- predict(fit, newdata = grid_200, type = "response")

cl <- knn(train = d[, c("x1", "x2")], test = grid_200[, c("x1", "x2")],
          cl = d$y, k = 15, prob = TRUE)
grid_200$p_knn <- ifelse(cl == "1", attr(cl, "prob"), 1 - attr(cl, "prob"))

p <- ggplot() +
    geom_point(data = d, aes(x1, x2, colour = y), size = 0.5, alpha = 0.75) +
    geom_contour(data = grid_200, aes(x1, x2, z = p_logit, linetype = "Logistic"),
                 breaks = 0.5, colour = "black", linewidth = 0.3) +
    geom_contour(data = grid_200, aes(x1, x2, z = p_knn, linetype = "k-NN (k = 15)"),
                 breaks = 0.5, colour = "black", linewidth = 0.3) +
    scale_linetype_manual(values = c("Logistic" = "solid", "k-NN (k = 15)" = "dashed")) +
    scale_colour_manual(values = c("0" = "#0072B2", "1" = "#D55E00"), guide = "none") +
    labs(linetype = NULL) +
    coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3)) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")

p

ggsave("cls_logistic_vs_knn.pdf", p, width = 8, height = 7, units = "cm")
