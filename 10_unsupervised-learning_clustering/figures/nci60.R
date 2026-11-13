# nci60.R: NCI60 gene expression (ISLR2 package: 64 cell lines, 6830 genes)
# Produces: fig_nci_dendro_distance.pdf, fig_nci_ari.pdf
source("helpers.R")
library(ISLR2)
library(mclust)
library(patchwork)

labs_nci <- as.character(NCI60$labs)
Xn <- scale(NCI60$data)

# ---- Dissimilarities between cell lines ---------------------------------
d_euc <- dist(Xn)
d_cor <- as.dist(1 - cor(t(Xn)))

# ---- Dendrograms: Euclidean vs correlation distance (average linkage) ---
types <- sort(unique(labs_nci))
cols <- setNames(scales::hue_pal()(length(types)), types)

hc_euc <- hclust(d_euc, method = "average")
hc_cor <- hclust(d_cor, method = "average")
hc_euc$labels <- labs_nci
hc_cor$labels <- labs_nci

# wide and short, to span the full slide width; small labels so they don't overlap
p_euc <- gg_dendro(hc_euc, label_groups = labs_nci, label_size = 2.1,
                   label_space = 0.3) +
    scale_colour_manual(values = cols) +
    labs(title = "Euclidean distance")
p_cor <- gg_dendro(hc_cor, label_groups = labs_nci, label_size = 2.1,
                   label_space = 0.3) +
    scale_colour_manual(values = cols) +
    labs(title = "Correlation-based distance", y = NULL)
# no padding beside the outermost leaves and almost no outer margin, so the
# trees use the whole width of the slide
p <- (p_euc + p_cor) &
    scale_x_continuous(expand = expansion(add = 0.7)) &
    theme(plot.margin = margin(2, 4, 0, 1))
p
ggsave("fig_nci_dendro_distance.pdf", p, width = 13, height = 3.5)

# ---- Agreement with cancer type (adjusted Rand index) -------------------
hcs <- list(
    "Complete (Euclid.)" = hclust(d_euc, method = "complete"),
    "Average (Euclid.)" = hclust(d_euc, method = "average"),
    "Single (Euclid.)" = hclust(d_euc, method = "single"),
    "Ward (Euclid.)" = hclust(d_euc, method = "ward.D2"),
    "Average (corr.)" = hclust(d_cor, method = "average")
)
Ks <- c(4, 14)
res <- do.call(rbind, lapply(Ks, function(K) {
    set.seed(1)
    km <- kmeans(Xn, centers = K, nstart = 25)$cluster
    ari <- c(
        sapply(hcs, function(h) adjustedRandIndex(cutree(h, K), labs_nci)),
        "K-means" = adjustedRandIndex(km, labs_nci)
    )
    data.frame(method = names(ari), ari = unname(ari), K = paste("K =", K))
}))
res$method <- factor(res$method, levels = rev(c(names(hcs), "K-means")))
res$K <- factor(res$K, levels = paste("K =", Ks))

p <- ggplot(res, aes(method, ari, fill = K)) +
    geom_col(position = position_dodge(0.8), width = 0.75) +
    coord_flip() +
    scale_fill_manual(values = okabe[1:2]) +
    labs(x = NULL, y = "Adjusted Rand index vs. cancer type", fill = NULL)
p
ggsave("fig_nci_ari.pdf", p, width = 8, height = 4.5)
