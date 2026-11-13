# usarrests.R: USArrests figures (base R dataset)
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
p <- gg_dendro(hc_raw, label_size = 2.4) + labs(title = "Raw variables") +
    gg_dendro(hc_std, label_size = 2.4) + labs(title = "Standardized variables")
p
ggsave("fig_usarrests_scale_dendro.pdf", p, width = 13, height = 5.5)

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
