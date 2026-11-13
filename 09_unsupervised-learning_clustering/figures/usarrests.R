# usarrests.R: USArrests figures (base R dataset)
# Produces: fig_usarrests_variance.pdf, fig_usarrests_scale_dendro.pdf,
#           fig_usarrests_profile.pdf, fig_usarrests_pairs.pdf,
#           fig_usarrests_stability.pdf
source("helpers.R")
library(patchwork)
library(GGally)

X <- USArrests
Z <- scale(X)

# ---- Share of total variance: raw vs standardized -----------------------
share <- function(M) apply(M, 2, var) / sum(apply(M, 2, var)) * 100
df <- rbind(
    data.frame(variable = names(X), share = unname(share(X)), data = "Raw"),
    data.frame(variable = names(X), share = unname(share(Z)), data = "Standardized")
)
df$variable <- factor(df$variable, levels = names(X))
df$data <- factor(df$data, levels = c("Raw", "Standardized"))
p <- ggplot(df, aes(variable, share, fill = data)) +
    geom_col(position = position_dodge(0.8), width = 0.75) +
    scale_fill_manual(values = okabe[1:2]) +
    labs(x = NULL, y = "Share of total variance (%)", fill = NULL)
p
ggsave("fig_usarrests_variance.pdf", p, width = 6, height = 4.5)

# ---- Dendrograms: raw vs standardized -----------------------------------
hc_raw <- hclust(dist(X), method = "complete")
hc_std <- hclust(dist(Z), method = "complete")
# wide and short, to span the full slide width; small labels so they don't overlap
p_raw <- gg_dendro(hc_raw, label_size = 2.5, label_space = 0.35) +
    labs(title = "Raw variables")
p_std <- gg_dendro(hc_std, label_size = 2.5, label_space = 0.35) +
    labs(title = "Standardized variables", y = NULL)
# no padding beside the outermost leaves and almost no outer margin, so the
# trees use the whole width of the slide
p <- (p_raw + p_std) &
    scale_x_continuous(expand = expansion(add = 0.7)) &
    theme(plot.margin = margin(2, 4, 0, 1))
p
ggsave("fig_usarrests_scale_dendro.pdf", p, width = 13, height = 3.5)

# ---- K-means with K = 3 on standardized data ----------------------------
set.seed(1)
km <- kmeans(Z, centers = 3, nstart = 50)
ord <- order(km$centers[, "Assault"])             # label clusters by Assault level
cl_num <- match(km$cluster, ord)
sizes <- table(cl_num)
cl <- factor(cl_num, labels = paste0("Cluster ", 1:3, " (n = ", sizes, ")"))

# ---- Cluster profiles ---------------------------------------------------
prof <- data.frame(
    cluster = rep(levels(cl), times = ncol(Z)),
    variable = factor(rep(colnames(Z), each = nlevels(cl)), levels = colnames(Z)),
    mean_z = as.vector(sapply(colnames(Z), function(v) tapply(Z[, v], cl, mean)))
)
p <- ggplot(prof, aes(variable, mean_z, fill = cluster)) +
    geom_col(position = position_dodge(0.8), width = 0.75) +
    geom_hline(yintercept = 0) +
    scale_fill_manual(values = okabe[1:3]) +
    labs(x = NULL, y = "Cluster mean (standardized units)", fill = NULL) +
    guides(fill = guide_legend(nrow = 3))
p
ggsave("fig_usarrests_profile.pdf", p, width = 6, height = 4.8)

# ---- Pairs plot ---------------------------------------------------------
pairs_df <- data.frame(X, cluster = cl)
p <- ggpairs(
    pairs_df, columns = 1:4, mapping = aes(colour = cluster),
    upper = list(continuous = "points"),
    lower = list(continuous = "points"),
    diag = list(continuous = wrap("densityDiag", alpha = 0.4))
) +
    scale_colour_manual(values = okabe[1:3]) +
    scale_fill_manual(values = okabe[1:3])
p
ggsave("fig_usarrests_pairs.pdf", p, width = 8, height = 5.5)

# ---- Stability: bootstrap Jaccard similarity of each K-means cluster ----
# For each bootstrap sample, re-run K-means and record, for every original
# cluster, its best Jaccard similarity with a bootstrap cluster (computed on
# the states drawn in that sample). Values near 1 mean the cluster reappears.
kmeans_ordered <- function(M, K, nstart) {        # clusters numbered by Assault
    fit <- kmeans(M, centers = K, nstart = nstart)
    match(fit$cluster, order(fit$centers[, "Assault"]))
}
jaccard <- function(a, b) length(intersect(a, b)) / length(union(a, b))
B <- 200
stab <- do.call(rbind, lapply(2:4, function(K) {
    set.seed(1)
    base <- kmeans_ordered(Z, K, nstart = 50)
    J <- t(sapply(seq_len(B), function(b) {
        idx <- sample(nrow(Z), replace = TRUE)
        bc <- kmeans_ordered(Z[idx, ], K, nstart = 10)
        u <- unique(idx)
        bc_u <- bc[match(u, idx)]                 # one label per drawn state
        sapply(seq_len(K), function(k) {
            orig <- intersect(which(base == k), u)
            max(sapply(seq_len(K), function(m) jaccard(orig, u[bc_u == m])))
        })
    }))
    data.frame(K = paste("K =", K), cluster = factor(rep(seq_len(K), each = B)),
               jaccard = as.vector(J))
}))
print(aggregate(jaccard ~ K + cluster, data = stab, FUN = mean))
p <- ggplot(stab, aes(cluster, jaccard, fill = cluster)) +
    geom_boxplot(outlier.size = 0.8, width = 0.6) +
    geom_hline(yintercept = c(0.5, 0.85), linetype = "dashed", colour = "grey40") +
    scale_fill_manual(values = okabe[1:4]) +
    facet_wrap(~ K, nrow = 1, scales = "free_x") +
    labs(x = "Cluster (numbered by Assault level)",
         y = "Bootstrap Jaccard similarity") +
    theme(legend.position = "none")
p
ggsave("fig_usarrests_stability.pdf", p, width = 9, height = 3.8)
