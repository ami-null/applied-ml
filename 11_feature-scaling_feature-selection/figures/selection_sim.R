# selection_sim.R: Part 3 figures and numbers on simulated data
# Produces: fig_filter_scores.pdf, fig_xor.pdf, fig_rfe.pdf,
#           fig_importance.pdf, fig_selection_leakage.pdf,
#           numbers_selection_sim.txt
source("helpers.R")
library(patchwork)
library(class)
library(randomForest)

numbers_start("selection_sim")

# ---- Shared simulation for the filter and RFE frames ---------------------
# n = 200, p = 30 standard-normal features, binary response from a logistic
# model on x1..x5 with coefficients 1.5, 1.2, 1.0, 0.8, 0.6.
# x6 is a noisy copy of x1 (redundant: informative alone, adds nothing to x1).
# x7..x30 are pure noise.
numbers_section("Shared simulation (filter scores and RFE)")
p_sim <- 30
beta <- c(1.5, 1.2, 1.0, 0.8, 0.6)
make_data <- function(n) {
    X <- matrix(rnorm(n * p_sim), n)
    X[, 6] <- X[, 1] + rnorm(n, sd = 0.3)
    colnames(X) <- paste0("x", 1:p_sim)
    y <- rbinom(n, 1, plogis(drop(X[, 1:5] %*% beta)))
    list(X = X, y = y)
}
ftype <- factor(c(rep("Informative", 5), "Redundant (copy of x1)",
                  rep("Noise", p_sim - 6)),
                levels = c("Informative", "Redundant (copy of x1)", "Noise"))
set.seed(10)
dat <- make_data(200)
report("n and p", c(n = 200, p = p_sim), 0)
report("Share of y = 1", mean(dat$y), 3)
report("Correlation of x6 with x1", cor(dat$X[, 1], dat$X[, 6]), 3)


# ---- Filter: univariate scores --------------------------------------------
# F statistic of a one-way ANOVA of each feature on the class (= t^2), and
# mutual information between the feature (cut into 10 quantile bins) and y.
numbers_section("Filter: univariate F statistic and mutual information")
f_test <- t(apply(dat$X, 2, function(v) {
    a <- anova(lm(v ~ factor(dat$y)))
    c(F = a$`F value`[1], p = a$`Pr(>F)`[1])
}))
mutual_info <- function(v, y, bins = 10) {
    b <- cut(v, unique(quantile(v, seq(0, 1, length.out = bins + 1))),
             include.lowest = TRUE)
    pj <- table(b, y) / length(y)
    pb <- rowSums(pj)
    py <- colSums(pj)
    sum(ifelse(pj > 0, pj * log(pj / outer(pb, py)), 0))
}
mi <- apply(dat$X, 2, mutual_info, y = dat$y)

top_f <- order(-f_test[, "F"])[1:8]
report("Top 8 features by F", paste(colnames(dat$X)[top_f], collapse = ", "))
report("Top 8 features by mutual information",
       paste(colnames(dat$X)[order(-mi)[1:8]], collapse = ", "))
report("F statistic of x1..x6", f_test[1:6, "F"], 1)
noise <- ftype == "Noise"
report("Noise features with p < 0.05 (out of 24)",
       sum(f_test[noise, "p"] < 0.05), 0)
report("Expected number by chance, 24 x 0.05", 24 * 0.05, 1)
report("Expected number by chance with 5,000 noise features, 5000 x 0.05",
       5000 * 0.05, 0)
report("Bonferroni threshold 0.05 / 30", 0.05 / 30, 5)
report("Features passing Bonferroni, by type",
       table(ftype[f_test[, "p"] < 0.05 / 30]), 0)
report("Mutual information, max over noise features", max(mi[noise]), 3)
report("Mutual information of x1..x6", mi[1:6], 3)

score_df <- rbind(
    data.frame(feature = colnames(dat$X), type = ftype, score = f_test[, "F"],
               panel = "ANOVA F statistic"),
    data.frame(feature = colnames(dat$X), type = ftype, score = mi,
               panel = "Mutual information (nats)"))
score_plot <- function(d) {
    d$feature <- factor(d$feature, levels = d$feature[order(-d$score)])
    ggplot(d, aes(feature, score, fill = type)) +
        geom_col(width = 0.8) +
        scale_fill_manual(values = okabe[c(1, 5, 8)], drop = FALSE) +
        labs(title = d$panel[1], x = NULL, y = NULL, fill = NULL) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10),
              panel.grid.major.x = element_blank())
}
p <- (score_plot(score_df[score_df$panel == "ANOVA F statistic", ]) +
          geom_hline(yintercept = qf(1 - 0.05 / 30, 1, 198),
                     linetype = "dashed", colour = okabe[2]) |
          score_plot(score_df[score_df$panel != "ANOVA F statistic", ])) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
ggsave("fig_filter_scores.pdf", p, width = 8.8, height = 2.4)
report("F cut-off drawn on the plot (Bonferroni, df = 1, 198)",
       qf(1 - 0.05 / 30, 1, 198), 2)


# ---- Filter pitfall: XOR -------------------------------------------------
# y = 1 when x1 and x2 have the same sign. Each feature alone carries no
# information about y; together they determine it.
numbers_section("XOR: useless alone, useful together")
set.seed(11)
xor_data <- function(n) {
    X <- matrix(runif(2 * n, -1, 1), n, dimnames = list(NULL, c("x1", "x2")))
    list(X = X, y = factor(as.integer(X[, 1] * X[, 2] > 0)))
}
tr <- xor_data(400)
te <- xor_data(2000)
pv <- sapply(1:2, function(j) t.test(tr$X[tr$y == 1, j],
                                     tr$X[tr$y == 0, j])$p.value)
report("Univariate t-test p-values for x1 and x2", setNames(pv, c("x1", "x2")), 3)
acc1 <- mean(knn(tr$X[, 1, drop = FALSE], te$X[, 1, drop = FALSE], tr$y,
                 k = 15) == te$y)
acc2 <- mean(knn(tr$X, te$X, tr$y, k = 15) == te$y)
report("k-NN (k = 15) test accuracy with x1 only", acc1, 3)
report("k-NN (k = 15) test accuracy with x1 and x2", acc2, 3)

xd <- data.frame(tr$X, y = factor(ifelse(tr$y == 1, "Class 1", "Class 0")))
p_sc <- ggplot(xd, aes(x1, x2, colour = y)) +
    geom_point(size = 1.6, alpha = 0.8) +
    scale_colour_manual(values = okabe[1:2]) +
    scale_x_continuous(breaks = c(-1, 0, 1)) +
    scale_y_continuous(breaks = c(-1, 0, 1)) +
    coord_equal() +
    labs(x = expression(x[1]), y = expression(x[2]), colour = NULL)
dens <- function(var, lab) {
    ggplot(xd, aes(.data[[var]], colour = y)) +
        geom_density(linewidth = 1) +
        scale_colour_manual(values = okabe[1:2], guide = "none") +
        scale_y_continuous(limits = c(0, 0.8)) +
        scale_x_continuous(breaks = c(-1, 0, 1)) +
        labs(x = lab, y = "Density", colour = NULL)
}
# widths chosen so the square scatter fills its cell; legend on the right and
# no outer margins, so the figure has no empty border on the slide
p <- (p_sc | dens("x1", expression(x[1])) | dens("x2", expression(x[2]))) +
    plot_layout(guides = "collect", widths = c(1, 1.3, 1.3)) &
    theme(legend.position = "right",
          legend.box.spacing = unit(2, "pt"),
          legend.margin = ggplot2::margin(0, 0, 0, 0),
          plot.margin = ggplot2::margin(1, 3, 1, 1))
ggsave("fig_xor.pdf", p, width = 8.2, height = 2.2)


# ---- Wrapper: recursive feature elimination, inside 10-fold CV ----------
# Logistic regression on standardized features; at each step drop the
# feature with the smallest |coefficient|. RFE is rerun inside each training
# fold, so the held-out fold never influences which features are kept.
numbers_section("Recursive feature elimination (logistic regression, 10-fold CV)")
rfe_order <- function(X, y) {
    keep <- colnames(X)
    dropped <- character(0)
    while (length(keep) > 1) {
        fit <- suppressWarnings(glm(y ~ ., family = binomial,
                                    data = data.frame(X[, keep, drop = FALSE], y = y)))
        b <- abs(coef(fit)[-1])
        worst <- keep[which.min(b)]
        dropped <- c(dropped, worst)
        keep <- setdiff(keep, worst)
    }
    rev(c(dropped, keep))   # most important first
}
set.seed(12)
folds <- sample(rep(1:10, length.out = nrow(dat$X)))
cv_acc <- matrix(NA, 10, p_sim)
for (f in 1:10) {
    trn <- folds != f
    sc <- scale_train_test(dat$X[trn, ], dat$X[!trn, ])
    ord <- rfe_order(sc$train, dat$y[trn])
    for (k in 1:p_sim) {
        vars <- ord[1:k]
        fit <- suppressWarnings(glm(y ~ ., family = binomial,
                                    data = data.frame(sc$train[, vars, drop = FALSE],
                                                      y = dat$y[trn])))
        pr <- predict(fit, data.frame(sc$test[, vars, drop = FALSE]),
                      type = "response")
        cv_acc[f, k] <- mean((pr > 0.5) == dat$y[!trn])
    }
}
acc_k <- colMeans(cv_acc)
best_k <- which.max(acc_k)
full_order <- rfe_order(scale(dat$X), dat$y)
report("CV accuracy with all 30 features", acc_k[p_sim], 3)
report("Number of features with the highest CV accuracy", best_k, 0)
report("Highest CV accuracy", acc_k[best_k], 3)
report("CV accuracy with 5 features", acc_k[5], 3)
report("RFE on all data: first 8 features in importance order",
       paste(full_order[1:8], collapse = ", "))

rfe_df <- data.frame(k = 1:p_sim, acc = acc_k,
                     se = apply(cv_acc, 2, sd) / sqrt(10))
p <- ggplot(rfe_df, aes(k, acc)) +
    geom_ribbon(aes(ymin = acc - se, ymax = acc + se), fill = okabe[1],
                alpha = 0.15) +
    geom_line(colour = okabe[1], linewidth = 1) +
    geom_point(colour = okabe[1], size = 1.8) +
    geom_vline(xintercept = 5, linetype = "dotted", colour = "grey40") +
    annotate("point", x = best_k, y = acc_k[best_k], shape = 1, size = 6,
             stroke = 1.2, colour = okabe[2]) +
    annotate("text", x = 5.4, y = min(acc_k - rfe_df$se), hjust = 0,
             size = 3.2, colour = "grey40", label = "5 informative features") +
    scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 30)) +
    labs(x = "Number of features kept", y = "CV accuracy (± 1 SE)")
ggsave("fig_rfe.pdf", p, width = 3.7, height = 3.2)


# ---- Embedded: impurity vs permutation importance -----------------------
# y depends on a binary feature and a continuous feature. Two noise
# features: one continuous (many distinct split points), one binary.
# Random forest, 500 trees; mean over 20 independent data sets of n = 500.
# Impurity = mean decrease in Gini; permutation = mean decrease in OOB
# accuracy (unscaled).
numbers_section("Random forest importance: impurity vs permutation")
set.seed(13)
imp <- replicate(20, {
    n <- 500
    d <- data.frame(
        `Informative, binary` = rbinom(n, 1, 0.5),
        `Informative, continuous` = rnorm(n),
        `Noise, continuous` = rnorm(n),
        `Noise, binary` = rbinom(n, 1, 0.5),
        check.names = FALSE)
    y <- factor(rbinom(n, 1, plogis(1.2 * (2 * d[[1]] - 1) + 0.8 * d[[2]])))
    rf <- randomForest(d, y, ntree = 500, importance = TRUE)
    cbind(impurity = importance(rf, type = 2)[, 1],
          permutation = importance(rf, type = 1, scale = FALSE)[, 1])
})
imp_mean <- apply(imp, c(1, 2), mean)
report("Impurity importance (mean decrease in Gini)", imp_mean[, "impurity"], 2)
report("Permutation importance (mean decrease in OOB accuracy)",
       imp_mean[, "permutation"], 4)
report("Rank of 'Noise, continuous' by impurity (1 = most important)",
       rank(-imp_mean[, "impurity"])["Noise, continuous"], 0)

imp_df <- rbind(
    data.frame(feature = rownames(imp_mean), measure = "Impurity (Gini)",
               value = imp_mean[, "impurity"] / max(imp_mean[, "impurity"])),
    data.frame(feature = rownames(imp_mean), measure = "Permutation (OOB accuracy)",
               value = imp_mean[, "permutation"] / max(imp_mean[, "permutation"])))
imp_df$feature <- factor(imp_df$feature, levels = rev(rownames(imp_mean)))
imp_df$noise <- grepl("Noise", imp_df$feature)
p <- ggplot(imp_df, aes(value, feature, fill = noise)) +
    geom_col(width = 0.7) +
    facet_wrap(~measure, ncol = 1) +
    scale_fill_manual(values = c(okabe[1], okabe[8]), guide = "none") +
    labs(x = "Relative importance", y = NULL)
ggsave("fig_importance.pdf", p, width = 4.2, height = 3.6)


# ---- Selection leakage ---------------------------------------------------
# n = 50, p = 5000 pure-noise features, y = 25 zeros and 25 ones assigned at
# random: no classifier can beat 50% on new data. Keep the 20 features with
# the largest |t| and estimate the accuracy of 5-NN by 5-fold CV, either
# selecting once on all 50 observations (wrong) or reselecting inside each
# training fold (right). 200 replications.
numbers_section("Selection leakage (n = 50, p = 5000, pure noise)")
tstat <- function(X, y) {
    a <- X[y == 1, , drop = FALSE]
    b <- X[y == 0, , drop = FALSE]
    na <- nrow(a)
    nb <- nrow(b)
    sp <- sqrt(((na - 1) * apply(a, 2, var) + (nb - 1) * apply(b, 2, var)) /
                   (na + nb - 2))
    (colMeans(a) - colMeans(b)) / (sp * sqrt(1 / na + 1 / nb))
}
top_k <- function(X, y, k = 20) order(-abs(tstat(X, y)))[1:k]
set.seed(14)
n <- 50
p_noise <- 5000
leak <- t(replicate(200, {
    X <- matrix(rnorm(n * p_noise), n)
    y <- sample(rep(0:1, n / 2))
    folds <- sample(rep(1:5, length.out = n))
    sel_all <- top_k(X, y)
    wrong <- right <- numeric(5)
    for (f in 1:5) {
        trn <- folds != f
        pred <- knn(X[trn, sel_all], X[!trn, sel_all], y[trn], k = 5)
        wrong[f] <- mean(pred == y[!trn])
        sel_f <- top_k(X[trn, ], y[trn])
        pred <- knn(X[trn, sel_f], X[!trn, sel_f], y[trn], k = 5)
        right[f] <- mean(pred == y[!trn])
    }
    c(wrong = mean(wrong), right = mean(right))
}))
report("Mean CV accuracy, selection outside CV (wrong)",
       mean(leak[, "wrong"]), 3)
report("Mean CV accuracy, selection inside CV (right)",
       mean(leak[, "right"]), 3)
report("SD of the CV accuracy (wrong, right)",
       c(wrong = sd(leak[, "wrong"]), right = sd(leak[, "right"])), 3)
report("Share of replications with CV accuracy >= 0.8 (wrong, right)",
       c(wrong = mean(leak[, "wrong"] >= 0.8),
         right = mean(leak[, "right"] >= 0.8)), 3)

lk <- rbind(
    data.frame(acc = leak[, "wrong"], how = "Select on all data, then CV"),
    data.frame(acc = leak[, "right"], how = "Select inside each CV fold"))
lk$how <- factor(lk$how, levels = c("Select on all data, then CV",
                                    "Select inside each CV fold"))
p <- ggplot(lk, aes(acc, fill = how)) +
    geom_histogram(binwidth = 0.02, colour = "white", linewidth = 0.1,
                   position = "identity", alpha = 0.75) +
    geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey30") +
    annotate("text", x = 0.49, y = Inf, vjust = 1.5, hjust = 1, size = 3.2,
             colour = "grey30", label = "true accuracy 0.5") +
    scale_fill_manual(values = okabe[c(2, 1)]) +
    guides(fill = guide_legend(ncol = 1)) +
    coord_cartesian(xlim = c(0.1, 1)) +
    labs(x = "CV accuracy (200 replications)", y = "Count",
         fill = NULL)
ggsave("fig_selection_leakage.pdf", p, width = 4.2, height = 3.4)
