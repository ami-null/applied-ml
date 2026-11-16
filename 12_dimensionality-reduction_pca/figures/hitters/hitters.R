# hitters.R: Hitters: principal components regression, as in ISLP (frames 3.2, 3.3)
# Produces: fig_pcr_cv.pdf, fig_pcr_vs_others.pdf, numbers_hitters.txt
# Run from this folder (helpers.R is one level up)
source("../helpers.R")
library(ISLR2)
library(pls)

numbers_start("hitters")


H <- na.omit(Hitters)
numbers_section("Hitters data")
report("Observations after dropping missing Salary", nrow(H), 0)
report("Predictors (dummies included)", ncol(model.matrix(Salary ~ ., H)) - 1, 0)

# Collinearity among the predictors (callback to lecture 4)
Xh <- model.matrix(Salary ~ ., H)[, -1]
cm <- cor(Xh)
diag(cm) <- 0
report("Pairs of predictors with |r| > 0.9", sum(abs(cm[upper.tri(cm)]) > 0.9), 0)

numbers_section("PCR with 10-fold cross-validation (standardized predictors)")
set.seed(2)
fit <- pcr(Salary ~ ., data = H, scale = TRUE, validation = "CV", segments = 10)
cv <- RMSEP(fit, estimate = "CV")
rmse <- as.numeric(cv$val[1, 1, ])        # entry 1 is the intercept-only model
ncomp <- 0:(length(rmse) - 1)
best <- ncomp[which.min(rmse)]
report("CV RMSE, intercept only", rmse[1], 1)
report("CV RMSE, 1 component", rmse[2], 1)
report("CV RMSE, best number of components", min(rmse), 1)
report("Best number of components", best, 0)
report("CV RMSE, all components (equals OLS)", rmse[length(rmse)], 1)
report("CV RMSE for 4 to 8 components", setNames(rmse[5:9], 4:8), 1)

# share of predictor variance and of Salary variance explained
ev <- explvar(fit)
report("Predictor variance explained by components 1-3 (%)", ev[1:3], 1)
report("Cumulative predictor variance, first 5 components (%)", sum(ev[1:5]), 1)
report("Cumulative predictor variance, first 10 components (%)", sum(ev[1:10]), 1)

df <- data.frame(ncomp = ncomp, rmse = rmse)
p <- ggplot(df, aes(ncomp, rmse)) +
    geom_line(colour = okabe[1]) +
    geom_point(colour = okabe[1], size = 1.6) +
    geom_point(data = df[df$ncomp == best, ], colour = okabe[2], size = 3.2) +
    labs(x = "Number of principal components",
         y = "CV RMSE ($1000s)")
ggsave("fig_pcr_cv.pdf", p, width = 4.4, height = 2.8)


# ---- Frame 3.3: PCR against PLS, ridge and lasso -------------------------
numbers_section("Cross-validated RMSE of PCR, PLS, ridge and lasso (Hitters)")
library(glmnet)
y <- H$Salary
set.seed(2)
folds <- sample(rep(1:10, length.out = nrow(H)))   # same folds for every method

# PLS: like PCR, but the components are chosen using Salary
set.seed(2)
fit_pls <- plsr(Salary ~ ., data = H, scale = TRUE, validation = "CV", segments = 10)
rm_pls <- as.numeric(RMSEP(fit_pls, estimate = "CV")$val[1, 1, ])
report("PLS: best number of components", which.min(rm_pls) - 1, 0)
report("PLS: CV RMSE at the best number", min(rm_pls), 1)
report("PLS: CV RMSE with 1 component", rm_pls[2], 1)

cvg <- function(alpha) {
    set.seed(2)
    cv.glmnet(Xh, y, alpha = alpha, foldid = folds, standardize = TRUE)
}
cv_ridge <- cvg(0); cv_lasso <- cvg(1)
rm_ridge <- sqrt(min(cv_ridge$cvm)); rm_lasso <- sqrt(min(cv_lasso$cvm))
report("Ridge: CV RMSE at the best lambda", rm_ridge, 1)
report("Lasso: CV RMSE at the best lambda", rm_lasso, 1)
nz <- sum(coef(cv_lasso, s = "lambda.min")[-1] != 0)
report("Lasso: predictors kept at the best lambda (of 19)", nz, 0)
report("PCR: CV RMSE at the best number of components", min(rmse), 1)
report("PCR: CV RMSE with 6 components", rmse[7], 1)

res <- data.frame(
    method = c("PCR (18 comp.)", "PCR (6 comp.)", "PLS", "Ridge", "Lasso"),
    rmse = c(min(rmse), rmse[7], min(rm_pls), rm_ridge, rm_lasso)
)
res$method <- factor(res$method, levels = rev(res$method))
p <- ggplot(res, aes(rmse, method)) +
    geom_segment(aes(x = 300, xend = rmse, yend = method), colour = okabe[8]) +
    geom_point(colour = okabe[1], size = 3) +
    geom_text(aes(label = round(rmse, 1)), vjust = -1, size = 3.2) +
    xlim(300, 355) +
    labs(x = "10-fold CV RMSE ($1000s)", y = NULL)
ggsave("fig_pcr_vs_others.pdf", p, width = 4.4, height = 2.4)
