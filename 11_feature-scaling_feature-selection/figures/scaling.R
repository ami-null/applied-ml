# scaling.R: Part 1 figures and numbers (feature scaling)
# Produces: fig_usarrests_variance.pdf, fig_unit_norm.pdf,
#           fig_scaling_contours.pdf, fig_scaling_models.pdf,
#           fig_ridge_units.pdf, fig_scalers_outlier.pdf,
#           fig_skew_transforms.pdf, numbers_scaling.txt
source("helpers.R")
library(patchwork)
library(ISLR2)
library(class)
library(e1071)
library(rpart)
library(randomForest)

numbers_start("scaling")


# ---- Motivation: units drive the distance (USArrests, lecture 10) -------
numbers_section("Motivation: USArrests")
X <- USArrests
Z <- scale(X)
share <- function(M) apply(M, 2, var) / sum(apply(M, 2, var)) * 100
report("SD of each variable", apply(X, 2, sd), 1)
report("Share of total variance, raw (%)", share(X), 1)
report("Share of total variance, standardized (%)", share(Z), 1)

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
ggsave("fig_usarrests_variance.pdf", p, width = 6, height = 4.5)


# ---- Numeric example of the three scalers (table on the slide) ----------
numbers_section("Numeric example of the three scalers")
x <- c(2, 4, 6, 8, 30)
report("x", x, 0)
report("Mean and SD", c(mean = mean(x), sd = sd(x)), 2)
report("Min and max", c(min = min(x), max = max(x)), 0)
report("Median and IQR (R default quantile type 7)",
       c(median = median(x), IQR = IQR(x)), 2)
report("Standardized (z-score)", (x - mean(x)) / sd(x), 2)
report("Min-max", (x - min(x)) / (max(x) - min(x)), 2)
report("Robust", (x - median(x)) / IQR(x), 2)


# ---- Row-wise normalization to unit norm --------------------------------
numbers_section("Row-wise normalization to unit length")
set.seed(1)
P <- data.frame(x1 = runif(25, 0.2, 3), x2 = runif(25, 0.2, 3))
nrm <- sqrt(P$x1^2 + P$x2^2)
U <- P / nrm
report("Euclidean norms before (min, max)", range(nrm), 2)
report("Euclidean norms after (min, max)", range(sqrt(U$x1^2 + U$x2^2)), 2)

arc <- data.frame(t = seq(0, pi / 2, length.out = 200))
pts <- rbind(
    data.frame(P, type = "Original row"),
    data.frame(U, type = "Normalized row")
)
pts$type <- factor(pts$type, levels = c("Original row", "Normalized row"))
p <- ggplot() +
    geom_path(data = arc, aes(cos(t), sin(t)),
              linetype = "dashed", colour = "grey50") +
    geom_segment(data = data.frame(P, u1 = U$x1, u2 = U$x2),
                 aes(x = x1, y = x2, xend = u1, yend = u2),
                 colour = "grey75", linewidth = 0.4) +
    geom_point(data = pts, aes(x1, x2, colour = type), size = 2.2) +
    scale_colour_manual(values = okabe[c(8, 1)]) +
    coord_equal(xlim = c(0, 3.1), ylim = c(0, 3.1)) +
    labs(x = expression(x[1]), y = expression(x[2]), colour = NULL)
ggsave("fig_unit_norm.pdf", p, width = 5, height = 5.3)


# ---- Gradient descent on unscaled vs standardized features --------------
# Least squares, loss L(b) = ||y - Xb||^2 / (2n), centered data, no intercept.
# Fixed step size 2 / (lambda_max + lambda_min), the best fixed step for a
# quadratic, so the comparison is fair to both versions.
numbers_section("Gradient descent: unscaled vs standardized")
set.seed(2)
n <- 200
Xg <- cbind(x1 = rnorm(n, 0, 1), x2 = rnorm(n, 0, 5))
yg <- drop(Xg %*% c(2, 0.6)) + rnorm(n)
Xg <- scale(Xg, scale = FALSE)
yg <- yg - mean(yg)

gd <- function(X, y, tol = 1e-6, max_iter = 10000) {
    H <- crossprod(X) / nrow(X)
    g <- drop(crossprod(X, y)) / nrow(X)
    bstar <- solve(H, g)
    ev <- eigen(H, symmetric = TRUE)$values
    alpha <- 2 / (max(ev) + min(ev))
    b <- c(0, 0)
    path <- matrix(b, nrow = 1)
    for (it in seq_len(max_iter)) {
        b <- b - alpha * drop(H %*% b - g)
        path <- rbind(path, b)
        if (sqrt(sum((b - bstar)^2)) / sqrt(sum(bstar^2)) < tol) break
    }
    list(path = path, iters = it, bstar = bstar, H = H,
         kappa = max(ev) / min(ev))
}
Xs <- scale(Xg)
g_raw <- gd(Xg, yg)
g_std <- gd(Xs, yg)
report("SD of x1 and x2", apply(Xg, 2, sd), 2)
report("Condition number of the Hessian, raw", g_raw$kappa, 1)
report("Condition number of the Hessian, standardized", g_std$kappa, 2)
report("Iterations to relative error 1e-6, raw", g_raw$iters, 0)
report("Iterations to relative error 1e-6, standardized", g_std$iters, 0)

contour_panel <- function(g, title, xlim, ylim) {
    grid <- expand.grid(b1 = seq(xlim[1], xlim[2], length.out = 300),
                        b2 = seq(ylim[1], ylim[2], length.out = 300))
    D <- cbind(grid$b1 - g$bstar[1], grid$b2 - g$bstar[2])
    grid$loss <- 0.5 * rowSums((D %*% g$H) * D)
    l0 <- 0.5 * drop(t(g$bstar) %*% g$H %*% g$bstar)  # excess loss at the start
    path <- data.frame(b1 = g$path[, 1], b2 = g$path[, 2])
    ggplot() +
        geom_contour(data = grid, aes(b1, b2, z = loss),
                     breaks = l0 * c(0.005, 0.02, 0.06, 0.15, 0.3, 0.6, 1, 1.6),
                     colour = "grey60", linewidth = 0.4) +
        geom_path(data = path, aes(b1, b2), colour = okabe[2], linewidth = 0.5) +
        geom_point(data = path, aes(b1, b2), colour = okabe[2], size = 0.9) +
        annotate("point", x = g$bstar[1], y = g$bstar[2],
                 shape = 4, size = 3, stroke = 1.2) +
        coord_equal(xlim = xlim, ylim = ylim, expand = FALSE) +
        labs(title = title, x = expression(b[1]), y = expression(b[2]))
}
p <- contour_panel(g_raw, paste0("Raw: ", g_raw$iters, " iterations"),
                   c(-0.6, 3.2), c(-0.6, 1.8)) +
    contour_panel(g_std, paste0("Standardized: ", g_std$iters, " iterations"),
                  c(-0.6, 3.6), c(-0.6, 3.6))
ggsave("fig_scaling_contours.pdf", p, width = 10, height = 4.6)


# ---- Which models need scaling? Balanced Default subsample --------------
# All defaults plus an equal-sized random sample of non-defaults, so that
# accuracy is not dominated by the 97% majority class. 50 random 70/30
# splits; balance and income are standardized with the training mean and
# SD, the student dummy is left as 0/1.
numbers_section("Which models need scaling: balanced Default subsample")
set.seed(3)
yes <- which(Default$default == "Yes")
no <- sample(which(Default$default == "No"), length(yes))
d <- Default[c(yes, no), ]
Xd <- data.frame(balance = d$balance, income = d$income,
                 student = as.numeric(d$student == "Yes"))
yd <- d$default
report("Full Default data: share of customers who do not default (%)",
       100 * mean(Default$default == "No"), 1)
report("Subsample size and number of defaults",
       c(n = nrow(d), defaults = sum(yd == "Yes")), 0)
report("SD of balance and income in the subsample",
       sapply(Xd[, c("balance", "income")], sd), 0)

fit_predict <- function(tr, te, ytr, seed) {
    trd <- data.frame(tr, y = ytr)
    out <- list()
    out[["k-NN (k = 15)"]] <- knn(tr, te, ytr, k = 15)
    out[["SVM (RBF kernel)"]] <- predict(
        svm(as.matrix(tr), ytr, kernel = "radial", scale = FALSE),
        as.matrix(te))
    lg <- glm(y ~ ., data = trd, family = binomial)
    out[["Logistic regression"]] <- factor(
        ifelse(predict(lg, te, type = "response") > 0.5, "Yes", "No"),
        levels = levels(ytr))
    out[["Classification tree"]] <- predict(
        rpart(y ~ ., data = trd, method = "class"), te, type = "class")
    set.seed(seed)
    out[["Random forest"]] <- predict(randomForest(tr, ytr, ntree = 300), te)
    out
}

n_splits <- 50
res <- list()
same <- list()
for (s in seq_len(n_splits)) {
    set.seed(100 + s)
    tr <- sample(nrow(Xd), round(0.7 * nrow(Xd)))
    raw_tr <- Xd[tr, ]
    raw_te <- Xd[-tr, ]
    sc <- scale_train_test(as.matrix(raw_tr[, 1:2]), as.matrix(raw_te[, 1:2]))
    std_tr <- data.frame(sc$train, student = raw_tr$student)
    std_te <- data.frame(sc$test, student = raw_te$student)
    p_raw <- fit_predict(raw_tr, raw_te, yd[tr], seed = s)
    p_std <- fit_predict(std_tr, std_te, yd[tr], seed = s)
    for (m in names(p_raw)) {
        res[[length(res) + 1]] <- data.frame(
            model = m, split = s,
            Raw = mean(p_raw[[m]] == yd[-tr]),
            Standardized = mean(p_std[[m]] == yd[-tr]))
        same[[length(same) + 1]] <- data.frame(
            model = m, identical = all(p_raw[[m]] == p_std[[m]]))
    }
}
res <- do.call(rbind, res)
same <- do.call(rbind, same)

acc <- rbind(
    data.frame(model = res$model, data = "Raw", acc = res$Raw),
    data.frame(model = res$model, data = "Standardized", acc = res$Standardized)
)
summ <- aggregate(acc ~ model + data, data = acc,
                  FUN = function(v) c(mean = mean(v), sd = sd(v)))
summ <- data.frame(model = summ$model, data = summ$data,
                   mean = summ$acc[, "mean"], sd = summ$acc[, "sd"])
for (m in unique(summ$model)) {
    r <- summ[summ$model == m, ]
    report(paste0(m, ": mean test accuracy (raw, standardized)"),
           setNames(r$mean, r$data), 3)
}
ident <- aggregate(identical ~ model, data = same, FUN = mean)
for (i in seq_len(nrow(ident))) {
    report(paste0(ident$model[i],
                  ": share of splits with identical predictions"),
           ident$identical[i], 2)
}

ord <- c("k-NN (k = 15)", "SVM (RBF kernel)", "Logistic regression",
         "Classification tree", "Random forest")
summ$model <- factor(summ$model, levels = rev(ord))
summ$data <- factor(summ$data, levels = c("Raw", "Standardized"))
p <- ggplot(summ, aes(mean, model, colour = data)) +
    geom_vline(xintercept = 0.5, linetype = "dotted", colour = "grey50") +
    geom_errorbar(aes(xmin = mean - sd, xmax = mean + sd), width = 0.25,
                  orientation = "y", position = position_dodge(0.5)) +
    geom_point(size = 2.8, position = position_dodge(0.5)) +
    scale_colour_manual(values = okabe[1:2]) +
    labs(x = "Test accuracy (mean ± 1 SD over 50 splits)", y = NULL,
         colour = NULL)
ggsave("fig_scaling_models.pdf", p, width = 7.5, height = 4.5)


# ---- The ridge penalty depends on the units: Credit ---------------------
# Ridge regression of Balance on Income plus three other predictors (those
# three standardized). Only the unit of Income changes. The other three are
# nearly uncorrelated with Income, so the Income coefficient shrinks because
# of its own penalty (with Limit or Rating in the model, Income's coefficient
# would also move as their coefficients shrink). Closed form on centered
# data, loss ||y - Xb||^2 / (2n) + (lambda / 2) ||b||^2.
numbers_section("Ridge penalty depends on units: Credit")
y_cr <- Credit$Balance - mean(Credit$Balance)
others <- scale(model.matrix(~ Age + Cards + Education, Credit)[, -1])
report("Correlation of Income with Age, Cards, Education",
       cor(Credit$Income, others)[1, ], 2)
units <- list(
    "Dollars" = Credit$Income * 1000,
    "$1000s (as recorded)" = Credit$Income,
    "Standardized" = as.numeric(scale(Credit$Income))
)
lambdas <- 10^seq(-3, 11, length.out = 400)
ridge_path <- list()
for (u in names(units)) {
    Xr <- scale(cbind(Income = units[[u]], others), scale = FALSE)
    n_cr <- nrow(Xr)
    b_ols <- solve(crossprod(Xr), crossprod(Xr, y_cr))[1]
    ratio <- sapply(lambdas, function(l) {
        solve(crossprod(Xr) + n_cr * l * diag(ncol(Xr)),
              crossprod(Xr, y_cr))[1] / b_ols
    })
    ridge_path[[u]] <- data.frame(unit = u, lambda = lambdas, ratio = ratio)
    report(paste0("Variance of Income, ", u), var(units[[u]]), 1)
    report(paste0("OLS coefficient of Income, ", u), b_ols, 4)
    half <- approx(ratio, log10(lambdas), xout = 0.5)$y
    report(paste0("lambda at which the Income coefficient halves, ", u),
           10^half, 3)
}
ridge_path <- do.call(rbind, ridge_path)
ridge_path$unit <- factor(ridge_path$unit, levels = names(units))
p <- ggplot(ridge_path, aes(lambda, ratio, colour = unit)) +
    geom_line(linewidth = 1) +
    scale_x_log10(labels = scales::label_log()) +
    scale_colour_manual(values = okabe[c(2, 1, 3)]) +
    labs(x = expression(lambda),
         y = "Income coefficient / OLS", colour = "Income in")
ggsave("fig_ridge_units.pdf", p, width = 7.5, height = 4.8)


# ---- One outlier under the three scalers ---------------------------------
# 50 regular values plus one outlier. Each scaler is fitted twice, without
# and with the outlier; we plot where the 50 regular values end up.
numbers_section("One outlier under the three scalers")
set.seed(4)
reg <- rnorm(50, 50, 10)
outlier <- 250
all_v <- c(reg, outlier)
scalers <- list(
    "Standardized (z-score)" = function(v, ref) (v - mean(ref)) / sd(ref),
    "Min-max" = function(v, ref) (v - min(ref)) / (max(ref) - min(ref)),
    "Robust (median / IQR)" = function(v, ref) (v - median(ref)) / IQR(ref)
)
so <- list()
lab <- list()
for (s in names(scalers)) {
    f <- scalers[[s]]
    without <- f(reg, reg)
    with <- f(reg, all_v)
    so[[s]] <- rbind(
        data.frame(scaler = s, fit = "Fitted without outlier", value = without),
        data.frame(scaler = s, fit = "Fitted with outlier", value = with))
    lab[[s]] <- data.frame(scaler = s, out = f(outlier, all_v))
    report(paste0(s, ": range of the 50 regular values, fitted without outlier"),
           range(without), 2)
    report(paste0(s, ": range of the 50 regular values, fitted with outlier"),
           range(with), 2)
    report(paste0(s, ": scaled value of the outlier"), f(outlier, all_v), 2)
}
so <- do.call(rbind, so)
lab <- do.call(rbind, lab)
so$scaler <- factor(so$scaler, levels = names(scalers))
lab$scaler <- factor(lab$scaler, levels = names(scalers))
so$fit <- factor(so$fit, levels = c("Fitted with outlier",
                                    "Fitted without outlier"))
lab$xpos <- tapply(so$value, so$scaler, max)[as.character(lab$scaler)]
p <- ggplot(so, aes(value, fit, colour = fit)) +
    geom_point(position = position_jitter(height = 0.12, seed = 1),
               size = 1.6, alpha = 0.8) +
    geom_text(data = lab,
              aes(x = xpos, y = 1.45,
                  label = sprintf("outlier at %.1f", out)),
              inherit.aes = FALSE, hjust = 1, size = 4, colour = okabe[2]) +
    facet_wrap(~scaler, ncol = 1, scales = "free_x") +
    scale_colour_manual(values = okabe[c(2, 1)], guide = "none") +
    labs(x = "Scaled value of the 50 regular observations", y = NULL)
ggsave("fig_scalers_outlier.pdf", p, width = 8, height = 6)


# ---- Skewed features: scaling does not change the shape -----------------
numbers_section("Skewed feature: diamonds price")
price <- ggplot2::diamonds$price
bc <- MASS::boxcox(lm(price ~ 1), lambda = seq(-1, 1, by = 0.01),
                   plotit = FALSE)
lam <- bc$x[which.max(bc$y)]
price_bc <- if (abs(lam) < 1e-8) log(price) else (price^lam - 1) / lam
report("Number of diamonds", length(price), 0)
report("Skewness of price", skewness(price), 2)
report("Skewness of standardized price", skewness(as.numeric(scale(price))), 2)
report("Skewness of log(price)", skewness(log(price)), 2)
report("Box-Cox lambda (maximum likelihood, grid step 0.01)", lam, 2)
report("Skewness after Box-Cox", skewness(price_bc), 2)

hist_panel <- function(v, title, xlab,
                       subtitle = sprintf("skewness %.2f", skewness(v))) {
    ggplot(data.frame(v = v), aes(v)) +
        geom_histogram(bins = 50, fill = okabe[1], colour = "white",
                       linewidth = 0.1) +
        labs(title = title, subtitle = subtitle, x = xlab, y = "Count")
}
p <- hist_panel(price, "Raw", "Price (USD)") +
    hist_panel(log(price), "Log", "log(price)") +
    hist_panel(price_bc, "Box-Cox", "Transformed price",
               subtitle = bquote(lambda == .(round(lam, 2)) * ", skewness" ~
                                     .(sprintf("%.2f", skewness(price_bc)))))
ggsave("fig_skew_transforms.pdf", p, width = 12, height = 4)
