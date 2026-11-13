# sim_clusters.R: figures based on simulated data
# Produces: fig_kmeans_iter.pdf, fig_kmeans_starts.pdf, fig_kmeans_limits.pdf,
#           fig_dendro_reading.pdf, fig_linkage_compare.pdf, fig_divisive.pdf,
#           fig_corr_distance.pdf, fig_elbow.pdf, fig_silhouette.pdf
source("helpers.R")
library(cluster)
library(patchwork)

blob <- function(n, mu, sd) {
    data.frame(x1 = rnorm(n, mu[1], sd), x2 = rnorm(n, mu[2], sd))
}

# ---- K-means iterations (Lloyd's algorithm, K = 3) ----------------------
set.seed(1)
sim3 <- rbind(
    blob(50, c(0, 0), 0.9),
    blob(50, c(4, 3), 0.9),
    blob(50, c(-0.5, 4.5), 0.9)
)
X3 <- as.matrix(sim3)
K <- 3

centroids <- function(X, a, K) {
    t(sapply(1:K, function(k) colMeans(X[a == k, , drop = FALSE])))
}

a <- sample(K, nrow(X3), replace = TRUE)          # random initial assignment
states <- list(list(a = a, m = centroids(X3, a, K)))
for (it in 1:50) {
    m <- states[[length(states)]]$m
    d <- as.matrix(dist(rbind(m, X3)))[-(1:K), 1:K]
    a_new <- max.col(-d)                          # nearest centroid
    states[[length(states) + 1]] <- list(a = a_new, m = centroids(X3, a_new, K))
    if (all(a_new == a)) break
    a <- a_new
}
idx <- unique(c(1, 2, 3, length(states)))
idx <- idx[idx <= length(states)]
panel_name <- function(i) {
    if (i == 1) "Random initial assignment"
    else if (i == length(states)) "Final (converged)"
    else paste("After iteration", i - 1)
}
pts <- do.call(rbind, lapply(idx, function(i) {
    data.frame(sim3, cluster = factor(states[[i]]$a), panel = panel_name(i))
}))
cen <- do.call(rbind, lapply(idx, function(i) {
    data.frame(x1 = states[[i]]$m[, 1], x2 = states[[i]]$m[, 2],
               cluster = factor(1:K), panel = panel_name(i))
}))
lv <- unique(pts$panel)
pts$panel <- factor(pts$panel, levels = lv)
cen$panel <- factor(cen$panel, levels = lv)

p <- ggplot() +
    geom_point(data = pts, aes(x1, x2, colour = cluster), size = 1.6) +
    geom_point(data = cen, aes(x1, x2, colour = cluster),
               shape = 4, size = 5, stroke = 2) +
    scale_colour_manual(values = okabe[1:3]) +
    facet_wrap(~ panel, nrow = 2) +
    labs(x = expression(x[1]), y = expression(x[2])) +
    theme(legend.position = "none")
p
ggsave("fig_kmeans_iter.pdf", p, width = 9, height = 5.5)

# ---- Local optima: single random starts (K = 4) -------------------------
set.seed(2)
sim4 <- rbind(
    blob(60, c(0, 0), 0.8),
    blob(60, c(4, 0), 0.8),
    blob(60, c(0, 4), 0.8),
    blob(60, c(4, 4), 0.8)
)
runs <- lapply(1:200, function(s) {
    set.seed(s)
    kmeans(sim4, centers = 4, nstart = 1)
})
wss <- sapply(runs, function(r) r$tot.withinss)
cand <- which(!duplicated(round(wss, 3)))         # distinct solutions
cand <- cand[order(wss[cand])]
pick <- unique(c(cand[1], cand[ceiling(length(cand) / 2)], cand[length(cand)]))
pts <- do.call(rbind, lapply(seq_along(pick), function(j) {
    r <- runs[[pick[j]]]
    data.frame(sim4, cluster = factor(r$cluster),
               panel = sprintf("Start %d: total WSS = %.0f", j, r$tot.withinss))
}))
pts$panel <- factor(pts$panel, levels = unique(pts$panel))
p <- ggplot(pts, aes(x1, x2, colour = cluster)) +
    geom_point(size = 1.4) +
    scale_colour_manual(values = okabe[1:4]) +
    facet_wrap(~ panel, nrow = 1) +
    labs(x = expression(x[1]), y = expression(x[2])) +
    theme(legend.position = "none")
p
ggsave("fig_kmeans_starts.pdf", p, width = 12, height = 4.2)

# ---- Limitations: non-convex clusters and unequal sizes/spreads ---------
set.seed(3)
th <- runif(150, 0, pi)
moons <- rbind(
    data.frame(x1 = cos(th), x2 = sin(th)),
    data.frame(x1 = 1 - cos(th), x2 = 0.5 - sin(th))
)
moons$x1 <- moons$x1 + rnorm(nrow(moons), sd = 0.08)
moons$x2 <- moons$x2 + rnorm(nrow(moons), sd = 0.08)
unequal <- rbind(
    blob(300, c(0, 0), 1.2),
    blob(40, c(5, 0), 0.3),
    blob(40, c(0, 5), 0.3)
)
km_m <- kmeans(moons, centers = 2, nstart = 25)
km_u <- kmeans(unequal, centers = 3, nstart = 25)
pts <- rbind(
    data.frame(moons, cluster = factor(km_m$cluster),
               panel = "Non-convex clusters (K = 2)"),
    data.frame(unequal, cluster = factor(km_u$cluster),
               panel = "Unequal sizes and spreads (K = 3)")
)
p <- ggplot(pts, aes(x1, x2, colour = cluster)) +
    geom_point(size = 1.4) +
    scale_colour_manual(values = okabe[1:3]) +
    facet_wrap(~ panel, nrow = 1, scales = "free") +
    labs(x = expression(x[1]), y = expression(x[2])) +
    theme(legend.position = "none")
p
ggsave("fig_kmeans_limits.pdf", p, width = 10, height = 4)

# ---- Reading a dendrogram: 9 observations -------------------------------
set.seed(4)
pts9 <- rbind(
    blob(3, c(0, 0), 0.35),
    blob(3, c(3, 0.3), 0.35),
    blob(3, c(1.2, 2.8), 0.35)
)
rownames(pts9) <- 1:9
hc9 <- hclust(dist(pts9), method = "complete")
p_sc <- ggplot(cbind(pts9, id = 1:9), aes(x1, x2, label = id)) +
    geom_point(colour = "grey70", size = 6) +
    geom_text(size = 5) +
    labs(x = expression(x[1]), y = expression(x[2]))
p_dd <- gg_dendro(hc9, label_size = 6)
p_dd
ggsave("fig_dendro_reading.pdf", p_sc + p_dd + plot_layout(widths = c(1, 1.2)),
       width = 10, height = 4.2)

# ---- Effect of linkage --------------------------------------------------
set.seed(5)
sim45 <- rbind(
    blob(15, c(0, 0), 1),
    blob(15, c(3.5, 2), 1),
    blob(15, c(-1, 4), 1)
)
d45 <- dist(sim45)
plots <- lapply(c("complete", "average", "single"), function(m) {
    gg_dendro(hclust(d45, method = m), label = FALSE) +
        labs(title = paste(tools::toTitleCase(m), "linkage"))
})
plots
ggsave("fig_linkage_compare.pdf", wrap_plots(plots, nrow = 1),
       width = 12, height = 3.8)

# ---- Divisive (DIANA) vs agglomerative (complete), same 45 points -------
hc_div <- as.hclust(diana(d45))
hc_agg <- hclust(d45, method = "complete")
cl_div <- cutree(hc_div, k = 3)
cl_agg <- cutree(hc_agg, k = 3)
print(table(divisive = cl_div, agglomerative = cl_agg))

cut_at <- function(hc, k) mean(sort(hc$height, decreasing = TRUE)[c(k - 1, k)])
p_sc <- ggplot(data.frame(sim45, cluster = factor(cl_div)),
               aes(x1, x2, colour = cluster)) +
    geom_point(size = 2.2) +
    scale_colour_manual(values = okabe[1:3]) +
    labs(x = expression(x[1]), y = expression(x[2]),
         title = "Divisive clusters, K = 3") +
    theme(legend.position = "none")
p_div <- gg_dendro(hc_div, label = FALSE, cut_height = cut_at(hc_div, 3)) +
    labs(title = "Divisive (DIANA)", y = "Height (cluster diameter)")
p_agg <- gg_dendro(hc_agg, label = FALSE, cut_height = cut_at(hc_agg, 3)) +
    labs(title = "Agglomerative (complete linkage)")
p <- p_sc + p_div + p_agg
p
ggsave("fig_divisive.pdf", p, width = 12, height = 3.8)

# ---- Correlation-based vs Euclidean distance ----------------------------
set.seed(6)
j <- 1:20
x1 <- 3 + 2 * sin(j / 3)
x2 <- 3 + rnorm(20, sd = 0.3)
x3 <- x1 + 5 + rnorm(20, sd = 0.15)
obs <- data.frame(
    j = rep(j, 3),
    value = c(x1, x2, x3),
    observation = factor(rep(1:3, each = 20))
)
M <- rbind(x1, x2, x3)
de <- as.matrix(dist(M))
dc <- 1 - cor(t(M))
cap <- sprintf(
    "Euclidean: d(1,2) = %.1f, d(1,3) = %.1f.  Correlation distance: d(1,2) = %.2f, d(1,3) = %.2f",
    de[1, 2], de[1, 3], dc[1, 2], dc[1, 3]
)
p <- ggplot(obs, aes(j, value, colour = observation)) +
    geom_line(linewidth = 1) +
    scale_colour_manual(values = okabe[1:3]) +
    labs(x = "Variable", y = "Value", colour = "Observation", caption = cap) +
    theme(plot.caption = element_text(size = 9, hjust = 0))
p
ggsave("fig_corr_distance.pdf", p, width = 6, height = 4.2)

# ---- Choosing K: elbow and silhouette (sim4) ----------------------------
Ks <- 1:8
set.seed(10)
wss_k <- sapply(Ks, function(k) kmeans(sim4, centers = k, nstart = 25)$tot.withinss)
p <- ggplot(data.frame(K = Ks, WSS = wss_k), aes(K, WSS)) +
    geom_line() +
    geom_point(size = 3) +
    scale_x_continuous(breaks = Ks) +
    labs(x = "Number of clusters K", y = "Within-cluster sum of squares")
p
ggsave("fig_elbow.pdf", p, width = 6, height = 4.2)

D <- dist(sim4)
Ks2 <- 2:8
avg_sil <- sapply(Ks2, function(k) {
    set.seed(10)
    cl <- kmeans(sim4, centers = k, nstart = 25)$cluster
    mean(silhouette(cl, D)[, "sil_width"])
})
p1 <- ggplot(data.frame(K = Ks2, sil = avg_sil), aes(K, sil)) +
    geom_line() +
    geom_point(size = 3) +
    scale_x_continuous(breaks = Ks2) +
    labs(x = "Number of clusters K", y = "Average silhouette width")

set.seed(10)
km4 <- kmeans(sim4, centers = 4, nstart = 25)
sil <- silhouette(km4$cluster, D)
sil_df <- data.frame(cluster = factor(sil[, "cluster"]), width = sil[, "sil_width"])
sil_df <- sil_df[order(sil_df$cluster, -sil_df$width), ]
sil_df$id <- seq_len(nrow(sil_df))
p2 <- ggplot(sil_df, aes(id, width, fill = cluster)) +
    geom_col(width = 1) +
    geom_hline(yintercept = mean(sil_df$width), linetype = "dashed") +
    scale_fill_manual(values = okabe[1:4]) +
    labs(x = "Observations (sorted within cluster)", y = "Silhouette width s(i)") +
    theme(axis.text.x = element_blank(), legend.position = "none")
p1 + p2
ggsave("fig_silhouette.pdf", p1 + p2, width = 11, height = 4.2)
