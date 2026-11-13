# other_methods.R: K-medoids, DBSCAN and Gaussian mixture figures (simulated data)
# Produces: fig_kmedoids.pdf, fig_dbscan.pdf, fig_gmm.pdf
source("helpers.R")
library(cluster)
library(mclust)
library(patchwork)

blob <- function(n, mu, sd) {
    data.frame(x1 = rnorm(n, mu[1], sd), x2 = rnorm(n, mu[2], sd))
}

# ---- K-medoids vs K-means: two groups plus three far outliers -----------
set.seed(7)
out_dat <- rbind(
    blob(40, c(0, 0), 0.6),
    blob(40, c(3, 0), 0.6),
    data.frame(x1 = c(11.5, 12, 12.3), x2 = c(7.6, 8.2, 7.8))   # outliers
)
km <- kmeans(out_dat, centers = 2, nstart = 25)
pm <- pam(out_dat, k = 2)
pts <- rbind(
    data.frame(out_dat, cluster = factor(km$cluster), panel = "K-means (K = 2)"),
    data.frame(out_dat, cluster = factor(pm$clustering), panel = "K-medoids (K = 2)")
)
ctr <- rbind(
    data.frame(km$centers, panel = "K-means (K = 2)"),
    data.frame(pm$medoids, panel = "K-medoids (K = 2)")
)
p <- ggplot(pts, aes(x1, x2)) +
    geom_point(aes(colour = cluster), size = 1.8) +
    geom_point(data = ctr, shape = 4, size = 5, stroke = 2) +
    scale_colour_manual(values = okabe[1:2]) +
    facet_wrap(~ panel, nrow = 1) +
    labs(x = expression(x[1]), y = expression(x[2])) +
    theme(legend.position = "none")
p
ggsave("fig_kmedoids.pdf", p, width = 7.5, height = 2.6)

# ---- DBSCAN (written out here, no extra package needed) ------------------
# A point is a core point if at least min_pts points (itself included) lie
# within eps of it. Clusters are grown from core points; points reached
# from no core point are noise (cluster 0).
dbscan_simple <- function(X, eps, min_pts) {
    D <- as.matrix(dist(X))
    n <- nrow(D)
    nb <- lapply(seq_len(n), function(i) which(D[i, ] <= eps))
    core <- lengths(nb) >= min_pts
    cl <- integer(n)
    k <- 0L
    for (i in which(core)) {
        if (cl[i] != 0L) next
        k <- k + 1L
        cl[i] <- k
        queue <- i
        while (length(queue) > 0) {
            j <- queue[1]
            queue <- queue[-1]
            if (!core[j]) next                    # border points do not expand
            for (m in nb[[j]]) {
                if (cl[m] == 0L) {
                    cl[m] <- k
                    queue <- c(queue, m)
                }
            }
        }
    }
    list(cluster = cl, core = core)
}

# same half-moons as fig_kmeans_limits.pdf (sim_clusters.R), plus noise
set.seed(3)
th <- runif(150, 0, pi)
moons <- rbind(
    data.frame(x1 = cos(th), x2 = sin(th)),
    data.frame(x1 = 1 - cos(th), x2 = 0.5 - sin(th))
)
moons$x1 <- moons$x1 + rnorm(nrow(moons), sd = 0.08)
moons$x2 <- moons$x2 + rnorm(nrow(moons), sd = 0.08)
unequal <- rbind(                                  # used for the GMM figure
    blob(300, c(0, 0), 1.2),
    blob(40, c(5, 0), 0.3),
    blob(40, c(0, 5), 0.3)
)
set.seed(30)
noisy <- rbind(moons, data.frame(x1 = runif(25, -1.3, 2.3),
                                 x2 = runif(25, -0.9, 1.4)))
db <- dbscan_simple(noisy, eps = 0.2, min_pts = 5)
cat("DBSCAN clusters:", max(db$cluster),
    " noise points:", sum(db$cluster == 0), "\n")
set.seed(3)
km_m <- kmeans(noisy, centers = 2, nstart = 25)
lab_db <- ifelse(db$cluster == 0, "noise", paste("cluster", db$cluster))
pts <- rbind(
    data.frame(noisy, group = paste("cluster", km_m$cluster),
               panel = "K-means (K = 2)"),
    data.frame(noisy, group = lab_db,
               panel = "DBSCAN (eps = 0.2, minPts = 5)")
)
pts$panel <- factor(pts$panel, levels = unique(pts$panel))
p <- ggplot(pts, aes(x1, x2, colour = group, shape = group == "noise")) +
    geom_point(size = 1.6, stroke = 1) +
    scale_colour_manual(values = c("cluster 1" = okabe[1], "cluster 2" = okabe[2],
                                   "cluster 3" = okabe[3], "noise" = "grey50")) +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 4)) +
    facet_wrap(~ panel, nrow = 1) +
    labs(x = expression(x[1]), y = expression(x[2])) +
    theme(legend.position = "none")
p
ggsave("fig_dbscan.pdf", p, width = 10, height = 3.6)

# ---- Gaussian mixture vs K-means: unequal sizes and spreads ---------------
set.seed(3)
km_u <- kmeans(unequal, centers = 3, nstart = 25)
gm <- Mclust(unequal, G = 3, verbose = FALSE)
cat("GMM covariance model:", gm$modelName, "\n")
cat("GMM points with max posterior < 0.9:", sum(apply(gm$z, 1, max) < 0.9), "\n")
pts <- rbind(
    data.frame(unequal, cluster = factor(km_u$cluster), certainty = 1,
               panel = "K-means (K = 3)"),
    data.frame(unequal, cluster = factor(gm$classification),
               certainty = apply(gm$z, 1, max),
               panel = "Gaussian mixture (K = 3)")
)
pts$panel <- factor(pts$panel, levels = unique(pts$panel))
p <- ggplot(pts, aes(x1, x2, colour = cluster, alpha = certainty)) +
    geom_point(size = 1.4) +
    scale_colour_manual(values = okabe[1:3]) +
    scale_alpha_continuous(range = c(0.15, 1), limits = c(1 / 3, 1)) +
    facet_wrap(~ panel, nrow = 1) +
    labs(x = expression(x[1]), y = expression(x[2])) +
    theme(legend.position = "none")
p
ggsave("fig_gmm.pdf", p, width = 10, height = 3.6)
