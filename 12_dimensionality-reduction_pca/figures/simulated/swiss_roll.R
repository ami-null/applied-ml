# swiss_roll.R: simulated Swiss roll: where linear PCA fails (frame 3.8)
# Produces: fig_swiss_roll.pdf, numbers_swiss_roll.txt
# Run from this folder (helpers.R is one level up)
source("../helpers.R")
library(patchwork)
library(Rtsne)

numbers_start("swiss_roll")


# ---- Frame 3.8: beyond linear PCA (Swiss roll) --------------------------
numbers_section("Swiss roll")
set.seed(3)
n <- 1500
t <- 1.5 * pi * (1 + 2 * runif(n))      # position along the roll
h <- 20 * runif(n)                      # height
S <- cbind(t * cos(t), h, t * sin(t))
pcs <- prcomp(S)
report("Share of variance, PC1 and PC2 (%)", pcs$sdev[1:2]^2 / sum(pcs$sdev^2) * 100, 1)
report("Share of variance on PC3 (%)", pcs$sdev[3]^2 / sum(pcs$sdev^2) * 100, 1)

# how well is neighbourhood structure along the sheet preserved?
# (nearest neighbours in the embedding vs distance along the sheet: t and h)
sheet <- cbind(t = t * 3, h = h)         # roughly arc-length scaled
knn_overlap <- function(E, S3, k = 10, m = 300) {
    set.seed(1)
    i <- sample(nrow(E), m)
    mean(sapply(i, function(r) {
        a <- order(colSums((t(E) - E[r, ])^2))[2:(k + 1)]
        b <- order(colSums((t(sheet) - sheet[r, ])^2))[2:(k + 1)]
        length(intersect(a, b)) / k
    }))
}
set.seed(4)
ts <- Rtsne(S, perplexity = 30, verbose = FALSE, check_duplicates = FALSE)$Y
report("Share of the 10 nearest neighbours on the sheet that PCA (2 PCs) keeps",
       knn_overlap(pcs$x[, 1:2], sheet), 2)
report("Share of the 10 nearest neighbours on the sheet that t-SNE keeps",
       knn_overlap(ts, sheet), 2)

# the sheet before rolling: position along the roll and height
d3 <- data.frame(x = t, y = h, t = t)
pa <- ggplot(d3, aes(x, y, colour = t)) +
    geom_point(size = 0.6) +
    scale_colour_viridis_c(guide = "none") +
    labs(title = "The sheet before rolling", x = NULL, y = NULL) +
    theme(axis.text = element_blank(), panel.grid = element_blank())
pb <- ggplot(data.frame(a = pcs$x[, 1], b = pcs$x[, 2], t = t), aes(a, b, colour = t)) +
    geom_point(size = 0.6) +
    scale_colour_viridis_c(guide = "none") +
    coord_equal() + labs(title = "PCA (2 components)", x = NULL, y = NULL) +
    theme(axis.text = element_blank(), panel.grid = element_blank())
pc_ <- ggplot(data.frame(a = ts[, 1], b = ts[, 2], t = t), aes(a, b, colour = t)) +
    geom_point(size = 0.6) +
    scale_colour_viridis_c(guide = "none") +
    coord_equal() + labs(title = "t-SNE", x = NULL, y = NULL) +
    theme(axis.text = element_blank(), panel.grid = element_blank())
ggsave("fig_swiss_roll.pdf", (pa + pb + pc_) & theme(plot.title = element_text(size = 9)), width = 7, height = 2.6)
