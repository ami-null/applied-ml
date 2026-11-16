# nci60.R: NCI60 gene expression: PCA, visualization, clustering in PC space (frames 3.5, 3.6)
# Produces: fig_nci60_pca.pdf, fig_nci60_cluster_pca.pdf, numbers_nci60.txt
# Run from this folder (helpers.R is one level up)
source("../helpers.R")
library(patchwork)
library(ISLR2)

numbers_start("nci60")


X <- NCI60$data
type <- NCI60$labs
numbers_section("NCI60 data")
report("Observations and genes", dim(X), 0)
report("Cancer types", length(unique(type)), 0)

# ---- PCA (genes standardized) --------------------------------------------
numbers_section("PCA on the standardized genes")
pc <- orient_pca(prcomp(X, scale. = TRUE))
pve <- pc$sdev^2 / sum(pc$sdev^2)
report("PVE of components 1-5 (%)", pve[1:5] * 100, 1)
report("Cumulative PVE, first 5 components (%)", sum(pve[1:5]) * 100, 1)
report("Cumulative PVE, first 10 components (%)", sum(pve[1:10]) * 100, 1)
report("Components needed for 90% of the variance", which(cumsum(pve) >= 0.9)[1], 0)
report("Components with nonzero variance (at most n - 1)", sum(pc$sdev > 1e-8), 0)

sc <- data.frame(PC1 = pc$x[, 1], PC2 = pc$x[, 2], PC3 = pc$x[, 3], type = type)
# 14 types: colour by type, and use shape to help with the colour limit
types <- sort(unique(type))
pal <- setNames(rep(okabe[1:7], 2), types)
shp <- setNames(rep(c(16, 17), each = 7), types)
p1 <- ggplot(sc, aes(PC1, PC2, colour = type, shape = type)) +
    geom_point(size = 2) +
    scale_colour_manual(values = pal) +
    scale_shape_manual(values = shp) +
    labs(x = sprintf("PC1 (%.1f%%)", pve[1] * 100),
         y = sprintf("PC2 (%.1f%%)", pve[2] * 100), colour = NULL, shape = NULL) +
    theme(legend.position = "right", legend.text = element_text(size = 8),
          legend.key.size = unit(0.35, "cm"))
ggsave("fig_nci60_pca.pdf", p1, width = 6, height = 3.4)

# How well do two raw genes separate the types? (compare with PC1 and PC2)
numbers_section("Two raw genes vs two principal components")
sep <- function(a, b) {
    # between-type share of total variance in the plane (a, b)
    M <- cbind(a, b)
    tot <- sum(apply(M, 2, var)) * (nrow(M) - 1)
    within <- sum(sapply(split(seq_len(nrow(M)), type), function(i)
        sum(apply(M[i, , drop = FALSE], 2, function(z) sum((z - mean(z))^2)))))
    1 - within / tot
}
set.seed(60)
gene_pairs <- replicate(1000, sample(ncol(X), 2))
sep_genes <- apply(gene_pairs, 2, function(g) sep(scale(X[, g[1]]), scale(X[, g[2]])))
report("Between-type share of variance in PC1-PC2 plane", sep(pc$x[, 1], pc$x[, 2]), 3)
report("Between-type share, median of 1,000 random gene pairs", median(sep_genes), 3)
report("Between-type share, best of 1,000 random gene pairs", max(sep_genes), 3)

# ---- Clustering in PC space ----------------------------------------------
numbers_section("Hierarchical clustering: all genes vs first PCs")
K <- 4
purity <- function(cl) {
    # share of observations whose cluster's majority cancer type is their own
    sum(apply(table(cl, type), 1, max)) / length(type)
}
# adjusted Rand index of a clustering against the cancer types
ari <- function(cl) {
    tab <- table(cl, type)
    c2 <- function(x) x * (x - 1) / 2
    sij <- sum(c2(tab)); a <- sum(c2(rowSums(tab))); b <- sum(c2(colSums(tab)))
    e <- a * b / c2(length(type))
    (sij - e) / ((a + b) / 2 - e)
}
Zg <- scale(X)
t0 <- system.time(hc_all <- hclust(dist(Zg), method = "complete"))
cl_all <- cutree(hc_all, K)
npc <- 5
t1 <- system.time(hc_pc <- hclust(dist(pc$x[, 1:npc]), method = "complete"))
cl_pc <- cutree(hc_pc, K)
report("Clusters K", K, 0)
report("Principal components used", npc, 0)
report("Majority-type purity, all genes", purity(cl_all), 3)
report("Majority-type purity, first PCs", purity(cl_pc), 3)
report("Cluster sizes, all genes", as.integer(table(cl_all)), 0)
report("Cluster sizes, first PCs", as.integer(table(cl_pc)), 0)
report("Share of observation pairs treated alike by both clusterings",
       {
           same <- outer(cl_all, cl_all, "==") == outer(cl_pc, cl_pc, "==")
           mean(same[upper.tri(same)])
       }, 3)
report("Adjusted Rand index vs cancer type, all genes", ari(cl_all), 3)
report("Adjusted Rand index vs cancer type, first PCs", ari(cl_pc), 3)
for (k in c(2, 5, 10, 20, 63)) {
    h <- hclust(dist(pc$x[, 1:k]), method = "complete")
    report(paste0("Adjusted Rand index, ", k, " PCs, K = ", K), ari(cutree(h, K)), 3)
}
report("Adjusted Rand index, all genes, K = 14", ari(cutree(hc_all, 14)), 3)
report("Adjusted Rand index, first 5 PCs, K = 14", ari(cutree(hc_pc, 14)), 3)
report("Adjusted Rand index, first 10 PCs, K = 14",
       ari(cutree(hclust(dist(pc$x[, 1:10]), method = "complete"), 14)), 3)

sc$cluster_pc <- factor(cl_pc)
sc$cluster_all <- factor(cl_all)
pa <- ggplot(sc, aes(PC1, PC2, colour = cluster_all)) +
    geom_point(size = 2) +
    scale_colour_manual(values = okabe[1:K]) +
    labs(title = "Clusters from all 6,830 genes", colour = "Cluster",
         x = "PC1", y = "PC2")
pb <- ggplot(sc, aes(PC1, PC2, colour = cluster_pc)) +
    geom_point(size = 2) +
    scale_colour_manual(values = okabe[1:K]) +
    labs(title = paste0("Clusters from the first ", npc, " PCs"), colour = "Cluster",
         x = "PC1", y = "PC2")
ggsave("fig_nci60_cluster_pca.pdf", pa + pb + plot_layout(guides = "collect") &
           theme(legend.position = "bottom"), width = 6.4, height = 3)
