# usarrests.R: USArrests: PCA via the SVD, biplot, scaling, scree (frames 2.4-2.9)
# Produces: fig_usarrests_biplot.pdf, fig_pca_scaling.pdf, fig_scree_usarrests.pdf, numbers_usarrests.txt
# Run from this folder (helpers.R is one level up)
source("../helpers.R")
library(patchwork)

numbers_start("usarrests")


# ---- Frames 2.4-2.9: PCA on USArrests -----------------------------------
numbers_section("USArrests: PCA via the SVD")
X <- as.matrix(USArrests)
n <- nrow(X)
Zs <- scale(X)

# SVD of the centered, scaled data matrix and the PCA vocabulary
sv <- svd(Zs)
pc <- orient_pca(prcomp(USArrests, scale. = TRUE))
report("Singular values", sv$d, 3)
report("sigma_j^2 / (n - 1)", sv$d^2 / (n - 1), 3)
report("prcomp variances (sdev^2)", pc$sdev^2, 3)
report("Max difference, |loadings from svd| vs |prcomp|",
       max(abs(abs(sv$v) - abs(pc$rotation))), 3, sci = TRUE)
report("Max difference, |U Sigma| vs |scores from prcomp|",
       max(abs(abs(sv$u %*% diag(sv$d)) - abs(pc$x))), 3, sci = TRUE)
cs <- cor(pc$x)
report("Largest correlation between two different scores",
       max(abs(cs[upper.tri(cs)])), 3, sci = TRUE)

for (j in 1:4) report(paste0("Loadings PC", j), pc$rotation[, j], 3)

pve <- pc$sdev^2 / sum(pc$sdev^2)
report("PVE (%)", pve * 100, 1)
report("Cumulative PVE (%)", cumsum(pve) * 100, 1)
report("Components with variance above 1 (Kaiser rule)", sum(pc$sdev^2 > 1), 0)

# Eckart-Young check: squared error of the rank-k approximation
for (k in 1:3) {
    Xk <- sv$u[, 1:k, drop = FALSE] %*% diag(sv$d[1:k], k) %*%
        t(sv$v[, 1:k, drop = FALSE])
    report(paste0("Squared error of the rank-", k, " approximation"),
           sum((Zs - Xk)^2), 3)
    report(paste0("Sum of the dropped sigma^2, k = ", k),
           sum(sv$d[-(1:k)]^2), 3)
}

# Biplot
sc <- as.data.frame(pc$x[, 1:2])
sc$state <- rownames(sc)
ld <- as.data.frame(pc$rotation[, 1:2])
ld$variable <- rownames(ld)
mult <- 0.85 * max(abs(sc[, 1:2])) / max(abs(ld[, 1:2]))
p <- ggplot() +
    geom_text(data = sc, aes(PC1, PC2, label = state),
              colour = okabe[8], size = 2.2, check_overlap = TRUE) +
    geom_segment(data = ld,
                 aes(x = 0, y = 0, xend = PC1 * mult, yend = PC2 * mult),
                 arrow = arrow(length = unit(0.15, "cm")), colour = okabe[2]) +
    geom_text(data = ld,
              aes(PC1 * mult * 1.12, PC2 * mult * 1.12, label = variable),
              colour = okabe[2], fontface = "bold", size = 3.2) +
    labs(x = sprintf("PC1 (%.0f%% of variance)", pve[1] * 100),
         y = sprintf("PC2 (%.0f%% of variance)", pve[2] * 100))
ggsave("fig_usarrests_biplot.pdf", p, width = 5.2, height = 3.9)

# Scaling: PC1 loadings, raw vs standardized
numbers_section("USArrests: PCA without standardizing")
pr <- orient_pca(prcomp(USArrests, scale. = FALSE))
pve_raw <- pr$sdev^2 / sum(pr$sdev^2)
report("Raw: PVE (%)", pve_raw * 100, 1)
report("Raw: PC1 loadings", pr$rotation[, 1], 3)
report("Standardized: PC1 loadings", pc$rotation[, 1], 3)
df <- rbind(
    data.frame(variable = rownames(pr$rotation), loading = pr$rotation[, 1],
               data = "Raw"),
    data.frame(variable = rownames(pc$rotation), loading = pc$rotation[, 1],
               data = "Standardized")
)
df$variable <- factor(df$variable, levels = colnames(X))
df$data <- factor(df$data, levels = c("Raw", "Standardized"))
p <- ggplot(df, aes(variable, loading, fill = data)) +
    geom_col(position = position_dodge(0.8), width = 0.75) +
    scale_fill_manual(values = okabe[1:2]) +
    labs(x = NULL, y = "PC1 loading", fill = NULL)
ggsave("fig_pca_scaling.pdf", p, width = 3.4, height = 2.8)

# Scree plot
df <- data.frame(pc = 1:4, pve = pve * 100, cum = cumsum(pve) * 100)
p1 <- ggplot(df, aes(pc, pve)) +
    geom_line(colour = okabe[1]) + geom_point(colour = okabe[1], size = 2) +
    scale_x_continuous(breaks = 1:4) + ylim(0, 100) +
    labs(x = "Principal component", y = "Variance explained (%)")
p2 <- ggplot(df, aes(pc, cum)) +
    geom_line(colour = okabe[2]) + geom_point(colour = okabe[2], size = 2) +
    scale_x_continuous(breaks = 1:4) + ylim(0, 100) +
    labs(x = "Principal component", y = "Cumulative (%)")
ggsave("fig_scree_usarrests.pdf", p1 + p2, width = 5.6, height = 2.6)
