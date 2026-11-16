# simulated_2d.R: simulated 2-D data: the first component, variance is not relevance (frames 2.2, 2.11)
# Produces: fig_pc1_line.pdf, fig_pca_variance_not_relevance.pdf, numbers_simulated_2d.txt
# Run from this folder (helpers.R is one level up)
source("../helpers.R")

numbers_start("simulated_2d")


# ---- Frame 2.2: first principal component as a line ---------------------
numbers_section("First principal component: simulated 2-D data")
set.seed(12)
n <- 40
t <- rnorm(n, sd = 2)
S <- data.frame(x1 = t + rnorm(n, sd = 0.6),
                x2 = 0.6 * t + rnorm(n, sd = 0.6))
S <- as.data.frame(scale(S, scale = FALSE))
pcs <- orient_pca(prcomp(S))
v1 <- pcs$rotation[, 1]
proj <- as.matrix(S) %*% v1
foot <- data.frame(f1 = proj * v1[1], f2 = proj * v1[2])
dist2 <- mean(rowSums((as.matrix(S) - as.matrix(foot))^2))
var_pc1 <- mean(proj^2)
report("Variance along PC1 and PC2 (divisor n)",
       c(PC1 = var_pc1, PC2 = mean((as.matrix(S) %*% pcs$rotation[, 2])^2)), 2)
report("Mean squared distance to the PC1 line", dist2, 3)
report("Variance along PC1 + mean squared distance", var_pc1 + dist2, 3)
report("Total variance (divisor n)", sum(apply(S, 2, function(z) mean(z^2))), 3)
ex <- range(c(S$x1, S$x2))
p <- ggplot() +
    geom_segment(aes(x = S$x1, y = S$x2, xend = foot$f1, yend = foot$f2),
                 colour = okabe[8], linewidth = 0.3) +
    geom_abline(slope = v1[2] / v1[1], intercept = 0, colour = okabe[2], linewidth = 1) +
    geom_point(aes(S$x1, S$x2), colour = okabe[1], size = 1.5) +
    geom_point(aes(foot$f1, foot$f2), colour = okabe[2], size = 1, alpha = 0.8) +
    coord_equal(xlim = ex, ylim = ex) +
    labs(x = expression(x[1]), y = expression(x[2]))
ggsave("fig_pc1_line.pdf", p, width = 3.4, height = 3.2)


# ---- Frame 2.11: variance is not relevance ------------------------------
numbers_section("Variance is not relevance: simulated two classes")
set.seed(21)
n <- 200
cls <- rep(c("A", "B"), each = n / 2)
# large variance along x1 (no class information); classes differ along x2
x1 <- rnorm(n, sd = 4)
x2 <- rnorm(n, sd = 0.7) + ifelse(cls == "A", -1.2, 1.2)
D <- data.frame(x1, x2, cls)
pcd <- orient_pca(prcomp(D[, 1:2]))
sc <- data.frame(PC1 = pcd$x[, 1], PC2 = pcd$x[, 2], cls = cls)
report("Share of variance on PC1 (%)",
       pcd$sdev[1]^2 / sum(pcd$sdev^2) * 100, 1)
report("PC1 loadings", pcd$rotation[, 1], 3)
# accuracy of the best single threshold (classes are balanced)
acc <- function(s) {
    ths <- sort(s)
    best <- sapply(ths, function(th) mean((s > th) == (cls == "B")))
    max(best, 1 - best)
}
report("Accuracy of a threshold on PC1", acc(sc$PC1), 3)
report("Accuracy of a threshold on PC2", acc(sc$PC2), 3)
p <- ggplot(sc, aes(PC1, PC2, colour = cls)) +
    geom_point(size = 1.5, alpha = 0.8) +
    scale_colour_manual(values = okabe[1:2]) +
    coord_equal() +
    labs(colour = "Class")
ggsave("fig_pca_variance_not_relevance.pdf", p, width = 4.6, height = 2.6)
