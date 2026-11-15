# dimensionality.R: Part 2 figures and numbers (curse of dimensionality)
# Produces: fig_hypercube_edge.pdf, fig_nn_distance.pdf,
#           fig_distance_concentration.pdf, fig_ball_cube.pdf,
#           fig_p_vs_n.pdf, fig_knn_noise_features.pdf,
#           numbers_dimensionality.txt
source("helpers.R")
library(patchwork)
library(class)

numbers_start("dimensionality")
p_breaks <- c(1, 2, 5, 10, 20, 50, 100)


# ---- Volume and sparsity: edge length of a sub-cube ----------------------
# Data uniform in the unit hypercube [0, 1]^p. A sub-cube that captures a
# fraction r of the data has volume r, so its edge length is r^(1/p).
numbers_section("Edge length of a sub-cube capturing a fraction r of the data")
ps <- 1:100
fracs <- c(0.01, 0.1, 0.3)
show_p <- c(1, 2, 3, 10, 100)
for (r in fracs) {
    report(sprintf("r = %.2f, edge length at p = 1, 2, 3, 10, 100", r),
           setNames(r^(1 / show_p), paste0("p", show_p)), 2)
}
edge <- expand.grid(p = ps, r = fracs)
edge$e <- edge$r^(1 / edge$p)
edge$r <- factor(paste0(edge$r * 100, "% of the data"),
                 levels = paste0(fracs * 100, "% of the data"))
p <- ggplot(edge, aes(p, e, colour = r)) +
    geom_line(linewidth = 1) +
    scale_x_log10(breaks = p_breaks) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_colour_manual(values = okabe[c(2, 1, 3)]) +
    labs(x = "Dimension p", y = "Edge length of the sub-cube", colour = NULL)
ggsave("fig_hypercube_edge.pdf", p, width = 6.5, height = 4.5)


# ---- Neighbourhoods are not local ----------------------------------------
# N points uniform in the unit ball in R^p. Median distance from the centre
# to the nearest point: d(p, N) = (1 - (1/2)^(1/N))^(1/p)
# (Hastie, Tibshirani and Friedman, ESL, Section 2.5). Checked by simulation.
numbers_section("Median distance from the centre to the nearest of N = 500 points (unit ball)")
N <- 500
d_med <- function(p, N) (1 - 0.5^(1 / N))^(1 / p)
show_p <- c(1, 2, 3, 10, 20, 50, 100)
report("Formula, p = 1, 2, 3, 10, 20, 50, 100",
       setNames(d_med(show_p, N), paste0("p", show_p)), 3)

set.seed(5)
runif_ball <- function(n, p) {
    Z <- matrix(rnorm(n * p), n)
    Z / sqrt(rowSums(Z^2)) * runif(n)^(1 / p)
}
mc_p <- c(2, 10, 50)
mc <- sapply(mc_p, function(p) {
    median(replicate(2000, min(sqrt(rowSums(runif_ball(N, p)^2)))))
})
report("Simulation (2000 replications), p = 2, 10, 50",
       setNames(mc, paste0("p", mc_p)), 3)

curve_df <- data.frame(p = 1:100, d = d_med(1:100, N))
p <- ggplot(curve_df, aes(p, d)) +
    geom_hline(yintercept = 0.5, linetype = "dotted", colour = "grey50") +
    geom_line(linewidth = 1, colour = okabe[1]) +
    geom_point(data = data.frame(p = mc_p, d = mc), colour = okabe[2],
               size = 2.5) +
    annotate("text", x = 1, y = 0.53, hjust = 0, size = 4, colour = "grey40",
             label = "halfway to the boundary") +
    scale_x_log10(breaks = p_breaks) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "Dimension p", y = "Median distance to the nearest point")
ggsave("fig_nn_distance.pdf", p, width = 6.5, height = 4.5)


# ---- Distance concentration ----------------------------------------------
# n = 500 points uniform in [0, 1]^p; all pairwise Euclidean distances,
# divided by their mean so the panels share an axis.
numbers_section("Distance concentration (n = 500 uniform points)")
set.seed(6)
n <- 500
ps <- c(2, 10, 100, 1000)
dc <- list()
for (pp in ps) {
    X <- matrix(runif(n * pp), n)
    d <- as.numeric(dist(X))
    report(sprintf("p = %d: min, mean, max pairwise distance", pp),
           c(min = min(d), mean = mean(d), max = max(d)), 3)
    report(sprintf("p = %d: max / min", pp), max(d) / min(d), 2)
    report(sprintf("p = %d: SD / mean", pp), sd(d) / mean(d), 3)
    dc[[length(dc) + 1]] <- data.frame(p = pp, rel = d / mean(d))
}
dc <- do.call(rbind, dc)
dc$p <- factor(paste("p =", dc$p), levels = paste("p =", ps))
p <- ggplot(dc, aes(rel)) +
    geom_histogram(aes(y = after_stat(density)), bins = 60, fill = okabe[1],
                   colour = "white", linewidth = 0.1) +
    facet_wrap(~p, nrow = 1, scales = "free_y") +
    scale_x_continuous(breaks = c(0, 1, 2)) +
    coord_cartesian(xlim = c(0, 2.6)) +
    labs(x = "Pairwise distance / mean pairwise distance", y = NULL) +
    theme(axis.text.y = element_blank())
ggsave("fig_distance_concentration.pdf", p, width = 11, height = 3.6)


# ---- Where the volume is: ball in a cube, and the outer shell -----------
numbers_section("Volume of the unit ball and its bounding cube")
vol_ball <- function(p) pi^(p / 2) / gamma(p / 2 + 1)
pv <- 1:20
report("Volume of the unit ball, p = 1, 2, 3, 5, 10, 20",
       setNames(vol_ball(c(1, 2, 3, 5, 10, 20)), paste0("p", c(1, 2, 3, 5, 10, 20))), 3)
report("Dimension with the largest unit-ball volume", pv[which.max(vol_ball(pv))], 0)
ratio_p <- c(2, 3, 5, 10, 20)
report("Ball volume / cube [-1, 1]^p volume, p = 2, 3, 5, 10, 20",
       setNames(vol_ball(ratio_p) / 2^ratio_p, paste0("p", ratio_p)), 3,
       sci = TRUE)
shell_p <- c(2, 10, 20, 50, 100)
report("Fraction of the ball's volume within 0.1 of the surface, p = 2, 10, 20, 50, 100",
       setNames(1 - 0.9^shell_p, paste0("p", shell_p)), 3)

p1 <- ggplot(data.frame(p = pv, ratio = vol_ball(pv) / 2^pv), aes(p, ratio)) +
    geom_line(colour = okabe[1], linewidth = 1) +
    geom_point(colour = okabe[1], size = 1.8) +
    scale_y_log10(labels = scales::label_log()) +
    labs(title = "Ball volume / cube volume", x = "Dimension p", y = NULL)
p2 <- ggplot(data.frame(p = 1:100, f = 1 - 0.9^(1:100)), aes(p, f)) +
    geom_line(colour = okabe[2], linewidth = 1) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = "Share of the ball within 0.1 of its surface",
         x = "Dimension p", y = NULL)
ggsave("fig_ball_cube.pdf", p1 + p2, width = 11, height = 4.2)


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
    annotate("text", x = n - 3, y = 100, hjust = 1, size = 4,
             colour = "grey40", label = "p + 1 = n") +
    scale_y_log10(breaks = 10^(-3:3), labels = c("0.001", "0.01", "0.1", "1",
                                                 "10", "100", "1000")) +
    scale_colour_manual(values = okabe[c(1, 2)]) +
    labs(x = "Number of features p", y = "MSE (median of 50 runs)",
         colour = NULL)
ggsave("fig_p_vs_n.pdf", p, width = 7, height = 4.5)


# ---- Irrelevant features hurt k-NN ---------------------------------------
# Two classes, equal size; the first two features are N(mu_y, I) with
# mu_0 = (0, 0) and mu_1 = (1.5, 1.5); m extra N(0, 1) noise features carry
# no information. All features have SD 1, so scaling is not the issue.
# k-NN with k = 15, 200 training and 1000 test points, 30 replications.
numbers_section("k-NN with irrelevant features (k = 15)")
set.seed(8)
delta <- c(1.5, 1.5)
bayes <- pnorm(sqrt(sum(delta^2)) / 2)
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
    annotate("text", x = 200, y = bayes + 0.02, hjust = 1, size = 4,
             colour = okabe[3], label = "Bayes accuracy") +
    annotate("text", x = 200, y = 0.52, hjust = 1, size = 4,
             colour = "grey40", label = "guessing") +
    scale_x_continuous(trans = "log1p", breaks = ms) +
    scale_y_continuous(limits = c(0.45, 0.9)) +
    labs(x = "Number of irrelevant features added",
         y = "Test accuracy (mean of 30 runs)")
ggsave("fig_knn_noise_features.pdf", p, width = 7, height = 4.5)
