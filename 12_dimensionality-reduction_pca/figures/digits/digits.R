# digits.R: UCI optdigits (8x8 handwritten digits; frames 2.9, 2.10, 3.1, 3.4, 3.7, 3.9)
# The data are downloaded once into data/ (commit the files so reruns need no network)
# Produces: fig_digits_tsne.pdf, fig_digits_scree.pdf, fig_digits_pca.pdf, fig_digits_reconstruction.pdf, fig_digits_denoise.pdf, fig_digits_knn_pca.pdf, numbers_digits.txt
# Run from this folder (helpers.R is one level up)
source("../helpers.R")
library(patchwork)
library(readr)
library(Rtsne)

numbers_start("digits")


# ---- Data ---------------------------------------------------------------
dir.create("data", showWarnings = FALSE)
f <- "data/optdigits.tra"
if (!file.exists(f)) {
    download.file(
        "https://archive.ics.uci.edu/ml/machine-learning-databases/optdigits/optdigits.tra",
        f, mode = "wb")
}
raw <- read_csv(f, col_names = FALSE, show_col_types = FALSE)
digit <- raw[[65]]
X <- as.matrix(raw[, 1:64])
numbers_section("Digits data")
report("Images and pixels", dim(X), 0)
report("Pixel value range", range(X), 0)
report("Constant pixels (always the same value)",
       sum(apply(X, 2, sd) == 0), 0)

# Draw 8x8 images from rows of a matrix: one row of the plot per label
digit_df <- function(M, row_label, col_label) {
    n <- nrow(M)
    data.frame(
        value = as.vector(t(M)),
        x = rep(rep(1:8, times = 8), n),
        y = rep(rep(8:1, each = 8), n),
        row = rep(row_label, each = 64),
        col = rep(col_label, each = 64)
    )
}
plot_digits <- function(d) {
    ggplot(d, aes(x, y, fill = value)) +
        geom_raster() +
        scale_fill_gradient(low = "white", high = "black", limits = c(0, 16),
                            oob = scales::squish, guide = "none") +
        coord_equal() +
        facet_grid(row ~ col, switch = "y") +
        theme_void(base_size = 11) +
        theme(strip.text.y.left = element_text(angle = 0, hjust = 1),
              strip.text.x = element_text(margin = margin(b = 3)),
              panel.spacing = unit(0.15, "lines"))
}


# ---- Frame 2.10: reconstruction -----------------------------------------
numbers_section("PCA on the digits (pixels centered, not scaled)")
# the pixels share one scale (0 to 16), so we center but do not scale
pc <- prcomp(X, center = TRUE, scale. = FALSE)
pve <- pc$sdev^2 / sum(pc$sdev^2)
report("PVE of components 1-3 (%)", pve[1:3] * 100, 1)
ks <- c(2, 10, 30, 64)
for (k in ks) report(paste0("Cumulative PVE, k = ", k, " (%)"), sum(pve[1:k]) * 100, 1)
report("Components needed for 90% of the variance", which(cumsum(pve) >= 0.9)[1], 0)
report("Components with nonzero variance", sum(pc$sdev > 1e-8), 0)

# Scree plot for the digits (first 30 of 64 components)
df <- data.frame(pc = 1:30, pve = pve[1:30] * 100, cum = cumsum(pve)[1:30] * 100)
p1 <- ggplot(df, aes(pc, pve)) +
    geom_line(colour = okabe[1]) + geom_point(colour = okabe[1], size = 1.6) +
    labs(x = "Principal component", y = "Variance explained (%)")
p2 <- ggplot(df, aes(pc, cum)) +
    geom_line(colour = okabe[2]) + geom_point(colour = okabe[2], size = 1.6) +
    geom_hline(yintercept = 90, linetype = "dashed", colour = okabe[8]) +
    labs(x = "Principal component", y = "Cumulative (%)")
ggsave("fig_digits_scree.pdf", p1 + p2, width = 5.6, height = 2.6)

# Scores on PC1 and PC2, coloured by the digit (labels not used by PCA)
pcd <- orient_pca(pc)
sc <- data.frame(PC1 = pcd$x[, 1], PC2 = pcd$x[, 2], digit = factor(digit))
pal10 <- c(okabe[1:8], "#000000", "#7B3294")
p <- ggplot(sc, aes(PC1, PC2, colour = digit)) +
    geom_text(aes(label = digit), size = 2.2) +
    scale_colour_manual(values = pal10, guide = "none") +
    labs(x = sprintf("PC1 (%.1f%%)", pve[1] * 100),
         y = sprintf("PC2 (%.1f%%)", pve[2] * 100))
ggsave("fig_digits_pca.pdf", p, width = 4.6, height = 3.6)
# share of variance in the PC1-PC2 plane that lies between digits
between <- function(M) {
    tot <- sum(apply(M, 2, function(z) sum((z - mean(z))^2)))
    within <- sum(sapply(split(seq_len(nrow(M)), digit), function(i)
        sum(apply(M[i, , drop = FALSE], 2, function(z) sum((z - mean(z))^2)))))
    1 - within / tot
}
report("Between-digit share of variance in the PC1-PC2 plane",
       between(pcd$x[, 1:2]), 3)
set.seed(2)
pairs <- replicate(1000, sample(64, 2))
report("Between-digit share, median of 1,000 random pixel pairs",
       median(apply(pairs, 2, function(g) between(X[, g]))), 3)
for (d in 0:9) report(paste0("Mean PC1 and PC2 for digit ", d),
                      colMeans(pcd$x[digit == d, 1:2]), 1)

recon <- function(pc, X, k) {
    Z <- sweep(X, 2, pc$center)
    R <- Z %*% pc$rotation[, 1:k, drop = FALSE] %*% t(pc$rotation[, 1:k, drop = FALSE])
    sweep(R, 2, pc$center, "+")
}
for (k in ks) {
    report(paste0("Mean squared reconstruction error per pixel, k = ", k),
           mean((X - recon(pc, X, k))^2), 3)
}
report("Mean squared reconstruction error from sum of dropped variances, k = 10",
       sum(pc$sdev[-(1:10)]^2) * (nrow(X) - 1) / nrow(X) / ncol(X), 3)

# one example for each of three digits
set.seed(5)
show <- sapply(c(3, 8, 4), function(d) sample(which(digit == d), 1))
rows <- list()
for (i in seq_along(show)) {
    orig <- X[show[i], , drop = FALSE]
    mats <- rbind(orig, do.call(rbind, lapply(ks, function(k) recon(pc, orig, k))))
    rows[[i]] <- digit_df(mats,
                          row_label = rep(paste("digit", digit[show[i]]), 5),
                          col_label = rep(c("original", paste0("k = ", ks)), 1))
}
d <- do.call(rbind, rows)
d$col <- factor(d$col, levels = c("original", paste0("k = ", ks)))
d$row <- factor(d$row, levels = unique(d$row))
ggsave("fig_digits_reconstruction.pdf", plot_digits(d), width = 5, height = 3)


# ---- Frame 3.7: denoising ------------------------------------------------
numbers_section("Denoising: PCA fitted on noisy digits")
set.seed(7)
sigma <- 4
noisy <- X + matrix(rnorm(length(X), sd = sigma), nrow(X))
idx <- sample(nrow(X), 1000)           # held-out images, the rest train PCA
Xtr <- noisy[-idx, ]
Xte_clean <- X[idx, ]
Xte_noisy <- noisy[idx, ]
pcn <- prcomp(Xtr, center = TRUE, scale. = FALSE)
report("Noise SD (pixel values range 0 to 16)", sigma, 1)
report("MSE of the noisy test images against the clean ones",
       mean((Xte_noisy - Xte_clean)^2), 2)
kd <- c(5, 10, 20, 40)
for (k in kd) {
    report(paste0("MSE against clean after keeping k = ", k, " components"),
           mean((recon(pcn, Xte_noisy, k) - Xte_clean)^2), 2)
}
mse_k <- sapply(1:64, function(k) mean((recon(pcn, Xte_noisy, k) - Xte_clean)^2))
report("Best k (smallest error against the clean images)", which.min(mse_k), 0)
report("Smallest MSE against the clean images", min(mse_k), 2)

pick <- sapply(c(0, 3, 7, 9), function(dg) which(digit[idx] == dg)[1])
cols <- c("clean", "noisy", paste0("k = ", c(10, 20)))
mats <- NULL; rl <- NULL; cl <- NULL
for (i in pick) {
    m <- rbind(Xte_clean[i, ], Xte_noisy[i, ],
               recon(pcn, Xte_noisy[i, , drop = FALSE], 10),
               recon(pcn, Xte_noisy[i, , drop = FALSE], 20))
    mats <- rbind(mats, m)
    rl <- c(rl, rep(paste("digit", digit[idx][i]), 4))
    cl <- c(cl, cols)
}
d <- digit_df(mats, rl, cl)
d$col <- factor(d$col, levels = cols)
d$row <- factor(d$row, levels = unique(d$row))
ggsave("fig_digits_denoise.pdf", plot_digits(d), width = 4.6, height = 4)


# ---- Frame 3.1: PCA scores as inputs to a model (k-NN on digits) --------
numbers_section("k-NN on the first k principal components (digits)")
library(class)
ft <- "data/optdigits.tes"
if (!file.exists(ft)) {
    download.file(
        "https://archive.ics.uci.edu/ml/machine-learning-databases/optdigits/optdigits.tes",
        ft, mode = "wb")
}
rawt <- read_csv(ft, col_names = FALSE, show_col_types = FALSE)
Xt <- as.matrix(rawt[, 1:64])
yt <- factor(rawt[[65]])
ytr <- factor(digit)
report("Training and test images", c(nrow(X), nrow(Xt)), 0)
# PCA is fitted on the training images only, then applied to the test images
scores_tr <- pc$x
scores_te <- sweep(Xt, 2, pc$center) %*% pc$rotation
acc_knn <- function(k) {
    mean(knn(scores_tr[, 1:k, drop = FALSE], scores_te[, 1:k, drop = FALSE],
             ytr, k = 5) == yt)
}
set.seed(8)                      # knn() breaks ties at random
kk <- c(1:10, 15, 20, 25, 30, 40, 50, 62)
acc <- sapply(kk, acc_knn)
acc_raw <- mean(knn(X, Xt, ytr, k = 5) == yt)
report("Test accuracy, k-NN (5 neighbours) on all 64 pixels", acc_raw, 3)
for (k in c(2, 5, 10, 20, 30)) {
    report(paste0("Test accuracy on the first ", k, " components"), acc[kk == k], 3)
}
report("Best number of components", kk[which.max(acc)], 0)
report("Best test accuracy", max(acc), 3)
df <- data.frame(k = kk, acc = acc)
p <- ggplot(df, aes(k, acc)) +
    geom_hline(yintercept = acc_raw, linetype = "dashed", colour = okabe[8]) +
    annotate("text", x = 62, y = acc_raw - 0.03, label = "all 64 pixels",
             hjust = 1, colour = okabe[8], size = 3.2) +
    geom_line(colour = okabe[1]) + geom_point(colour = okabe[1], size = 1.8) +
    labs(x = "Number of principal components used", y = "Test accuracy (5-NN)")
ggsave("fig_digits_knn_pca.pdf", p, width = 4.4, height = 2.8)


# ---- Frame 3.9: nonlinear alternative on real data (t-SNE on the digits) ---
numbers_section("t-SNE vs PCA on the digits (all 3,823 training images)")
set.seed(9)
ts <- Rtsne(X, perplexity = 30, check_duplicates = FALSE, verbose = FALSE)$Y
pc2 <- pcd$x[, 1:2]
# leave-one-out 5-NN accuracy in the two-dimensional pictures
set.seed(10)
acc_pca2 <- mean(knn.cv(pc2, ytr, k = 5) == ytr)
acc_tsne2 <- mean(knn.cv(ts, ytr, k = 5) == ytr)
report("Leave-one-out 5-NN accuracy in the PCA plane (PC1, PC2)", acc_pca2, 3)
report("Leave-one-out 5-NN accuracy in the t-SNE plane", acc_tsne2, 3)
report("t-SNE perplexity", 30, 0)

emb <- rbind(
    data.frame(a = pc2[, 1], b = pc2[, 2], digit = factor(digit), method = "PCA"),
    data.frame(a = ts[, 1], b = ts[, 2], digit = factor(digit), method = "t-SNE")
)
emb$method <- factor(emb$method, levels = c("PCA", "t-SNE"))
p <- ggplot(emb, aes(a, b, colour = digit)) +
    geom_text(aes(label = digit), size = 1.8) +
    scale_colour_manual(values = pal10, guide = "none") +
    facet_wrap(~method, scales = "free") +
    labs(x = NULL, y = NULL) +
    theme(axis.text = element_blank(), panel.grid = element_blank())
ggsave("fig_digits_tsne.pdf", p, width = 6.4, height = 3.2)
