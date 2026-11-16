# credit.R: Credit data: two correlated features and one long axis (frame 0.2)
# Produces: fig_two_features_axis.pdf, numbers_credit.txt
# Run from this folder (helpers.R is one level up)
source("../helpers.R")
library(ISLR2)

numbers_start("credit")


# ---- Frame 0.2: correlated features, one long axis (Credit) -------------
numbers_section("Correlated features: Credit Limit and Rating")
Z2 <- scale(Credit[, c("Limit", "Rating")])
report("Correlation of Limit and Rating", cor(Credit$Limit, Credit$Rating), 3)
pc2 <- orient_pca(prcomp(Z2))
report("Share of variance on PC1 (%)",
       pc2$sdev[1]^2 / sum(pc2$sdev^2) * 100, 1)
report("PC1 loadings (Limit, Rating)", pc2$rotation[, 1], 3)

d2 <- as.data.frame(Z2)
slope <- pc2$rotation[2, 1] / pc2$rotation[1, 1]
p <- ggplot(d2, aes(Limit, Rating)) +
    geom_point(colour = okabe[1], alpha = 0.5, size = 1.4) +
    geom_abline(slope = slope, intercept = 0, colour = okabe[2], linewidth = 1) +
    coord_equal() +
    labs(x = "Limit (standardized)", y = "Rating (standardized)")
ggsave("fig_two_features_axis.pdf", p, width = 3.4, height = 3.2)
