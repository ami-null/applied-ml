# selection_credit.R: Part 3 figures and numbers on the Credit data (ISLR2)
# Response Balance; 10 predictors, 11 columns after dummy coding
# (Own, Student, Married, Region with 3 levels).
# Produces: fig_filter_corr.pdf, fig_subset_selection.pdf,
#           fig_lasso_path.pdf, numbers_selection_credit.txt
source("helpers.R")
library(patchwork)
library(ISLR2)
library(leaps)
library(glmnet)

numbers_start("selection_credit")
x <- model.matrix(Balance ~ ., Credit)[, -1]
y <- Credit$Balance
report("Observations and predictor columns", c(n = nrow(x), p = ncol(x)), 0)


# ---- Filter: correlation between predictors ------------------------------
numbers_section("Correlation filter")
num <- Credit[, c("Income", "Limit", "Rating", "Cards", "Age", "Education")]
C <- cor(num)
pairs <- which(upper.tri(C), arr.ind = TRUE)
pairs <- data.frame(a = rownames(C)[pairs[, 1]], b = colnames(C)[pairs[, 2]],
                    r = C[pairs])
pairs <- pairs[order(-abs(pairs$r)), ]
big <- pairs[abs(pairs$r) > 0.5, ]
report("Pairs with |r| > 0.5",
       setNames(big$r, paste(big$a, big$b, sep = "-")), 3)
report("Pairs with |r| > 0.9", nrow(pairs[abs(pairs$r) > 0.9, ]), 0)
report("Correlation with Balance (Limit, Rating)",
       c(Limit = cor(Credit$Limit, y), Rating = cor(Credit$Rating, y)), 3)

cd <- as.data.frame(as.table(C))
names(cd) <- c("a", "b", "r")
cd$a <- factor(cd$a, levels = colnames(C))
cd$b <- factor(cd$b, levels = rev(colnames(C)))
cd$flag <- abs(cd$r) > 0.9 & cd$a != cd$b
p <- ggplot(cd, aes(a, b, fill = r)) +
    geom_tile(colour = "white") +
    geom_tile(data = cd[cd$flag, ], colour = "black", linewidth = 1,
              fill = NA) +
    geom_text(aes(label = ifelse(abs(r) > 0.99 & a != b, sprintf("%.3f", r),
                                 sprintf("%.2f", r))), size = 3.2) +
    scale_fill_gradient2(low = okabe[1], mid = "white", high = okabe[2],
                         limits = c(-1, 1)) +
    coord_equal() +
    labs(x = NULL, y = NULL, fill = "r") +
    theme(panel.grid = element_blank(), legend.position = "right",
          axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("fig_filter_corr.pdf", p, width = 4.0, height = 3.6)


# ---- Wrappers: best subset, forward and backward selection --------------
numbers_section("Best subset, forward and backward selection (BIC)")
report("Number of subsets 2^p for p = 10, 11, 20, 30, 40",
       setNames(2^c(10, 11, 20, 30, 40), paste0("p", c(10, 11, 20, 30, 40))), 0)
pc <- ncol(x)
report("Models fitted by forward selection, 1 + p(p + 1)/2 (p = 11)",
       1 + pc * (pc + 1) / 2, 0)

methods <- c(`Best subset` = "exhaustive", Forward = "forward",
             Backward = "backward")
sel <- list()
bic <- list()
for (m in names(methods)) {
    fit <- regsubsets(Balance ~ ., Credit, nvmax = pc, method = methods[[m]])
    s <- summary(fit)
    sel[[m]] <- s$which[, -1]
    bic[[m]] <- data.frame(method = m, size = seq_along(s$bic), bic = s$bic)
    best <- which.min(s$bic)
    report(paste0(m, ": size with the lowest BIC"), best, 0)
    report(paste0(m, ": variables in that model"),
           paste(colnames(s$which)[-1][s$which[best, -1]], collapse = " + "))
}
for (k in 1:4) {
    for (m in names(methods)) {
        report(sprintf("%s, size %d", m, k),
               paste(colnames(sel[[m]])[sel[[m]][k, ]], collapse = " + "))
    }
}
differ <- which(apply(sel[["Best subset"]] != sel[["Forward"]], 1, any))
report("Sizes at which forward selection differs from best subset",
       if (length(differ)) unname(differ) else "none", 0)
differ <- which(apply(sel[["Best subset"]] != sel[["Backward"]], 1, any))
report("Sizes at which backward selection differs from best subset",
       if (length(differ)) unname(differ) else "none", 0)

bic <- do.call(rbind, bic)
bic$method <- factor(bic$method, levels = names(methods))
mins <- do.call(rbind, lapply(split(bic, bic$method),
                              function(d) d[which.min(d$bic), ]))
p <- ggplot(bic, aes(size, bic, colour = method, shape = method,
                     linetype = method)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    geom_point(data = mins, size = 6, shape = 1, stroke = 1.2,
               show.legend = FALSE) +
    scale_x_continuous(breaks = 1:pc) +
    scale_colour_manual(values = okabe[1:3]) +
    labs(x = "Number of predictors", y = "BIC", colour = NULL, shape = NULL,
         linetype = NULL)
ggsave("fig_subset_selection.pdf", p, width = 3.7, height = 3.2)


# ---- Embedded: lasso (and ridge) paths, lambda by 10-fold CV ------------
# glmnet standardizes the predictors internally; the paths are shown as
# standardized coefficients (coefficient x SD of the predictor) so they are
# comparable across predictors.
numbers_section("Lasso and ridge on Credit")
fit_l <- glmnet(x, y, alpha = 1)
fit_r <- glmnet(x, y, alpha = 0)
set.seed(9)
cv <- cv.glmnet(x, y, alpha = 1, nfolds = 10)
sds <- apply(x, 2, sd)
nz <- function(l) {
    b <- coef(fit_l, s = l)[-1, 1]
    names(b)[b != 0]
}
report("lambda.min and lambda.1se",
       c(min = cv$lambda.min, `1se` = cv$lambda.1se), 3)
report("Non-zero coefficients at lambda.min", length(nz(cv$lambda.min)), 0)
report("Variables at lambda.min", paste(nz(cv$lambda.min), collapse = " + "))
report("Non-zero coefficients at lambda.1se", length(nz(cv$lambda.1se)), 0)
report("Variables at lambda.1se", paste(nz(cv$lambda.1se), collapse = " + "))
report("CV MSE at lambda.min and lambda.1se",
       c(min = cv$cvm[cv$lambda == cv$lambda.min],
         `1se` = cv$cvm[cv$lambda == cv$lambda.1se]), 0)
# order in which variables enter the lasso path
first_in <- apply(as.matrix(fit_l$beta) != 0, 1,
                  function(v) if (any(v)) max(fit_l$lambda[v]) else NA)
report("Order in which variables enter the lasso path",
       paste(names(sort(first_in, decreasing = TRUE)), collapse = ", "))
b_ridge <- coef(fit_r, s = cv$lambda.1se)[-1, 1]
report("Ridge at the same lambda (lambda.1se of the lasso): exact zeros",
       sum(b_ridge == 0), 0)

highlight <- c("Income", "Limit", "Rating", "StudentYes")
path_df <- function(fit) {
    B <- as.matrix(fit$beta) * sds
    data.frame(var = rep(rownames(B), times = ncol(B)),
               lambda = rep(fit$lambda, each = nrow(B)),
               coef = as.numeric(B))
}
path_plot <- function(df, title, ylab = NULL) {
    df$group <- ifelse(df$var %in% highlight, df$var, "Other")
    df$group <- factor(df$group, levels = c(highlight, "Other"))
    ggplot(df, aes(lambda, coef, group = var, colour = group)) +
        geom_hline(yintercept = 0, colour = "grey60") +
        geom_line(linewidth = 0.9) +
        scale_x_log10(labels = scales::label_log()) +
        scale_colour_manual(values = c(okabe[1:4], "grey70"),
                            labels = c("Income", "Limit", "Rating",
                                       "Student", "Other")) +
        labs(title = title, x = expression(lambda), y = ylab, colour = NULL)
}
cv_df <- data.frame(lambda = cv$lambda, m = cv$cvm, lo = cv$cvlo, hi = cv$cvup)
p_cv <- ggplot(cv_df, aes(lambda, m)) +
    geom_errorbar(aes(ymin = lo, ymax = hi), colour = "grey70", width = 0) +
    geom_point(colour = "black", size = 1.2) +
    geom_vline(xintercept = c(cv$lambda.min, cv$lambda.1se),
               linetype = "dashed", colour = "grey40") +
    scale_x_log10(labels = scales::label_log()) +
    scale_y_log10(labels = scales::label_comma()) +
    labs(title = "Lasso: 10-fold CV", x = expression(lambda),
         y = "CV MSE")
# the legend is collected inside the two path panels only, so it sits
# between them and the CV panel (which has no colour mapping)
paths <- (path_plot(path_df(fit_l), "Lasso path", "Std. coefficient") +
              geom_vline(xintercept = cv$lambda.1se, linetype = "dashed",
                         colour = "grey40") |
              path_plot(path_df(fit_r), "Ridge path")) +
    plot_layout(guides = "collect") &
    theme(legend.position = "right")
p <- wrap_plots(paths, p_cv, widths = c(2.4, 1))
ggsave("fig_lasso_path.pdf", p, width = 8.8, height = 2.2)
