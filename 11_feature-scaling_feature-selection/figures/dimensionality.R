# dimensionality.R: Part 2 figures and numbers (curse of dimensionality)
# Produces: fig_p_vs_n.pdf, fig_knn_noise_features.pdf,
#           numbers_dimensionality.txt
source("helpers.R")
library(class)

numbers_start("dimensionality")


# ---- Sparsity: the same 20 points, one more feature at a time -----------
# The slide draws these 20 points (TikZ) on a line and in a square, with each
# feature cut into 4 equal bins. Coordinates are on the 0-4 scale of the
# drawing; the same x values appear in both panels.
numbers_section("Sparsity: 20 points, 4 bins per feature")
pts <- data.frame(
    x = c(0.3, 0.7, 0.9, 1.2, 1.5, 1.8, 1.4, 0.5, 2.1, 2.4,
          2.6, 2.9, 2.2, 3.1, 3.4, 3.7, 3.3, 3.8, 2.7, 0.2),
    y = c(2.6, 0.4, 3.5, 1.1, 2.4, 0.6, 2.2, 0.4, 2.8, 0.3,
          1.6, 2.6, 1.2, 0.8, 2.4, 3.3, 2.9, 0.2, 2.1, 0.9))
n_pts <- nrow(pts)
report("Points per bin with one feature", table(floor(pts$x)), 0)
cells2 <- table(factor(floor(pts$x), 0:3), factor(floor(pts$y), 0:3))
report("Two features: cells, mean points per cell, empty cells",
       c(cells = 16, mean = n_pts / 16, empty = sum(cells2 == 0)), 2)
report("Three features: cells, mean points per cell, empty cells at least",
       c(cells = 64, mean = n_pts / 64, empty = 64 - n_pts), 2)
report("Cells 4^p for p = 1, 2, 3, 10", setNames(4^c(1, 2, 3, 10),
                                                  paste0("p", c(1, 2, 3, 10))), 0)
report("Points needed for 5 per cell, 5 x 4^p, p = 1, 2, 3, 10",
       setNames(5 * 4^c(1, 2, 3, 10), paste0("p", c(1, 2, 3, 10))), 0)


# ---- Neighbourhoods stop being local --------------------------------------
# Data uniform in [0, 1]^p. A sub-cube that captures a fraction r of the data
# has volume r, so its edge length (the share of each feature's range it
# spans) is r^(1/p).
numbers_section("Edge length of a sub-cube capturing 10% of the data")
show_p <- c(1, 2, 3, 10, 100)
report("Edge length at p = 1, 2, 3, 10, 100",
       setNames(0.1^(1 / show_p), paste0("p", show_p)), 2)


# ---- Almost every point is near an edge -----------------------------------
# A value is "extreme" if it lies in the outer 5% at either end of the
# feature's range (10% of a uniform feature). A point is interior only if it
# is interior in every feature, which happens with probability 0.9^p.
numbers_section("Share of uniform points with no extreme value (0.9^p)")
show_p <- c(1, 2, 3, 10, 50, 100)
report("0.9^p at p = 1, 2, 3, 10, 50, 100",
       setNames(0.9^show_p, paste0("p", show_p)), 3)
set.seed(4)
X <- matrix(runif(1e5 * 10), ncol = 10)
report("Simulation, p = 10 (100,000 points)",
       mean(apply(X > 0.05 & X < 0.95, 1, all)), 3)


# ---- Distance concentration -----------------------------------------------
# n = 500 points uniform in [0, 1]^p; all pairwise Euclidean distances.
numbers_section("Distance concentration (n = 500 uniform points)")
set.seed(6)
n <- 500
for (pp in c(2, 10, 100, 1000)) {
    X <- matrix(runif(n * pp), n)
    d <- as.numeric(dist(X))
    report(sprintf("p = %d: min, mean, max pairwise distance", pp),
           c(min = min(d), mean = mean(d), max = max(d)), 3)
    report(sprintf("p = %d: max / min", pp), max(d) / min(d), 2)
}


# ---- p approaching n: OLS training vs test error ------------------------
# n = 100 training points, 1000 test points; y depends on the first 5 of 99
# standard-normal features with coefficients 1 and noise SD 1, so the
# irreducible test MSE is 1. Fit OLS (with intercept) on the first p
# features. Median over 50 replications.
numbers_section("OLS as p approaches n (n = 100, 5 relevant features, noise variance 1)")
set.seed(7)
n <- 100
n_test <- 1000
p_max <- n - 1
ps <- c(1, 2, 3, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 98, 99)
reps <- 50
mse <- array(NA, c(reps, length(ps), 2))
for (r in seq_len(reps)) {
    Xtr <- matrix(rnorm(n * p_max), n)
    Xte <- matrix(rnorm(n_test * p_max), n_test)
    ytr <- rowSums(Xtr[, 1:5]) + rnorm(n)
    yte <- rowSums(Xte[, 1:5]) + rnorm(n_test)
    for (j in seq_along(ps)) {
        A <- cbind(1, Xtr[, 1:ps[j], drop = FALSE])
        b <- qr.solve(A, ytr)
        mse[r, j, 1] <- mean((ytr - A %*% b)^2)
        mse[r, j, 2] <- mean((yte - cbind(1, Xte[, 1:ps[j], drop = FALSE]) %*% b)^2)
    }
}
med <- apply(mse, c(2, 3), median)
show <- ps %in% c(5, 20, 50, 90, 98, 99)
report("p shown", ps[show], 0)
report("Median training MSE", med[show, 1], 3)
report("Median test MSE", med[show, 2], 2)
# at p = 99 the fit interpolates (training MSE is 0 up to rounding), which
# cannot be drawn on a log axis, so the training curve stops at p = 98
pn <- rbind(data.frame(p = ps, mse = med[, 1], set = "Training")[ps < n - 1, ],
            data.frame(p = ps, mse = med[, 2], set = "Test"))
pn$set <- factor(pn$set, levels = c("Training", "Test"))
p <- ggplot(pn, aes(p, mse, colour = set)) +
    geom_hline(yintercept = 1, linetype = "dotted", colour = "grey50") +
    geom_vline(xintercept = n - 1, linetype = "dashed", colour = "grey50") +
    geom_line(linewidth = 1) +
    geom_point(size = 1.8) +
    annotate("text", x = n - 3, y = 100, hjust = 1, size = 3.2,
             colour = "grey40", label = "p + 1 = n") +
    scale_y_log10(breaks = 10^(-3:3), labels = c("0.001", "0.01", "0.1", "1",
                                                 "10", "100", "1000")) +
    scale_colour_manual(values = okabe[c(1, 2)]) +
    labs(x = "Number of features p", y = "Median MSE",
         colour = NULL)
ggsave("fig_p_vs_n.pdf", p, width = 4.3, height = 3.5)


# ---- Irrelevant features hurt k-NN ---------------------------------------
# Two classes, equal size; the first two features are N(mu_y, I) with
# mu_0 = (0, 0) and mu_1 = (1.5, 1.5); m extra N(0, 1) noise features carry
# no information. All features have SD 1, so scaling is not the issue.
# k-NN with k = 15, 200 training and 1000 test points, 30 replications.
numbers_section("k-NN with irrelevant features (k = 15)")
set.seed(8)
delta <- c(1.5, 1.5)
# equal priors, identity covariance: the Bayes rule picks the nearer class
# mean, and its accuracy is Phi(Delta / 2) with Delta the distance between
# the means
bayes <- pnorm(sqrt(sum(delta^2)) / 2)
report("Class means: class 0 at (0, 0), class 1 at", delta, 1)
report("Distance between the class means, Delta", sqrt(sum(delta^2)), 2)
report("Delta / 2", sqrt(sum(delta^2)) / 2, 2)
report("Bayes accuracy", bayes, 3)
ms <- c(0, 1, 2, 5, 10, 20, 50, 100, 200)
sim <- function(n, m) {
    y <- rep(0:1, length.out = n)
    X <- cbind(matrix(rnorm(n * 2), n) + outer(y, delta),
               matrix(rnorm(n * m), n))
    list(X = X, y = factor(y))
}
accs <- sapply(ms, function(m) {
    mean(replicate(30, {
        tr <- sim(200, m)
        te <- sim(1000, m)
        mean(knn(tr$X, te$X, tr$y, k = 15) == te$y)
    }))
})
report("Noise features m", ms, 0)
report("Mean test accuracy", accs, 3)
p <- ggplot(data.frame(m = ms, acc = accs), aes(m, acc)) +
    geom_hline(yintercept = bayes, linetype = "dashed", colour = okabe[3]) +
    geom_hline(yintercept = 0.5, linetype = "dotted", colour = "grey50") +
    geom_line(colour = okabe[1], linewidth = 1) +
    geom_point(colour = okabe[1], size = 2.2) +
    annotate("text", x = 200, y = bayes + 0.02, hjust = 1, size = 3.2,
             colour = okabe[3], label = "Bayes accuracy") +
    annotate("text", x = 200, y = 0.52, hjust = 1, size = 3.2,
             colour = "grey40", label = "guessing") +
    scale_x_continuous(trans = "log1p", breaks = ms) +
    scale_y_continuous(limits = c(0.45, 0.9)) +
    labs(x = "Irrelevant features added",
         y = "Mean test accuracy")
ggsave("fig_knn_noise_features.pdf", p, width = 4.3, height = 3.5)
