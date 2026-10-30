library(ggplot2)
library(rpart)
library(randomForest)
library(gbm)

# Heart.csv from https://www.statlearning.com/resources-first-edition
# the first column is a row index, so removing that column
heart <- read.csv("https://www.statlearning.com/s/Heart.csv", stringsAsFactors = TRUE)[, -1]
heart <- na.omit(heart)
p <- ncol(heart) - 1  # number of predictors (13)


# ---------------------------------------------------------------------------
# 1. Training and CV misclassification error against tree size
#    rpart reports errors relative to the root node, so we rescale by the
#    root-node error rate
# ---------------------------------------------------------------------------
set.seed(1)
full <- rpart(AHD ~ ., data = heart, method = "class",
              parms = list(split = "gini"),
              control = rpart.control(cp = 0, minsplit = 5, xval = 10))

err_root <- min(table(heart$AHD)) / nrow(heart)
cpt <- as.data.frame(full$cptable)
cpt$size <- cpt$nsplit + 1
cpt <- subset(cpt, size <= 15)

err <- rbind(
  data.frame(size = cpt$size, error = cpt[["rel error"]] * err_root, type = "Training"),
  data.frame(size = cpt$size, error = cpt$xerror * err_root, type = "Cross-validation")
)

p_cv <- ggplot(err, aes(x = size, y = error, colour = type)) +
  geom_line() +
  geom_point() +
  labs(x = "Tree size (number of leaves)", y = "Misclassification error", colour = NULL) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("heart_cv.pdf", p_cv, width = 4.5, height = 3.6)


# ---------------------------------------------------------------------------
# 2. OOB error: bagging (m = p) versus random forests (m = p/2, sqrt(p))
# ---------------------------------------------------------------------------
set.seed(1)
ntree <- 500
mtrys <- c(p, floor(p / 2), floor(sqrt(p)))

oob <- do.call(rbind, lapply(mtrys, function(m) {
  rf <- randomForest(AHD ~ ., data = heart, mtry = m, ntree = ntree)
  data.frame(trees = 1:ntree, error = rf$err.rate[, "OOB"], m = paste0("m = ", m))
}))
oob$m <- factor(oob$m, levels = unique(oob$m))

p_oob <- ggplot(oob, aes(x = trees, y = error, colour = m)) +
  geom_line() +
  labs(x = "Number of trees", y = "OOB error", colour = NULL) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("heart_rf_oob.pdf", p_oob, width = 4.5, height = 3.6)

# ---------------------------------------------------------------------------
# 3. Variable importance (mean decrease in Gini index)
# ---------------------------------------------------------------------------
set.seed(1)
rf <- randomForest(AHD ~ ., data = heart, mtry = floor(sqrt(p)), ntree = 500,
                   importance = TRUE)
imp <- data.frame(variable = rownames(importance(rf)),
                  importance = importance(rf, type = 2)[, 1])

p_imp <- ggplot(imp, aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "grey40") +
  coord_flip() +
  labs(x = NULL, y = "Mean decrease in Gini index") +
  theme_bw(base_size = 11)

ggsave("heart_importance.pdf", p_imp, width = 4.5, height = 3.6)


# ---------------------------------------------------------------------------
# 4. Boosting: test error against number of trees for d = 1, 2 (lambda = 0.01),
#    with the test error of a random forest as a reference line
# ---------------------------------------------------------------------------
set.seed(1)
train <- sample(nrow(heart), nrow(heart) / 2)
y_test <- heart$AHD[-train]

hg <- heart
hg$y <- as.numeric(hg$AHD == "Yes")
hg$AHD <- NULL
y_test01 <- hg$y[-train]

B <- 2000
boost_err <- do.call(rbind, lapply(c(1, 2), function(d) {
  fit <- gbm(y ~ ., data = hg[train, ], distribution = "bernoulli",
             n.trees = B, interaction.depth = d, shrinkage = 0.01)
  pr <- predict(fit, newdata = hg[-train, ], n.trees = 1:B, type = "response")
  data.frame(trees = 1:B,
             error = colMeans(sweep(pr > 0.5, 1, y_test01, "!=")),
             depth = paste0("d = ", d))
}))

rf_train <- randomForest(AHD ~ ., data = heart[train, ], mtry = floor(sqrt(p)), ntree = 500)
rf_err <- mean(predict(rf_train, newdata = heart[-train, ]) != y_test)

p_boost <- ggplot(boost_err, aes(x = trees, y = error, colour = depth)) +
  geom_line() +
  geom_hline(yintercept = rf_err, linetype = "dashed") +
  annotate("text", x = B, y = rf_err, label = "random forest", vjust = -0.6,
           hjust = 1, size = 3) +
  labs(x = "Number of trees", y = "Test error", colour = NULL) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("heart_boost.pdf", p_boost, width = 4.5, height = 3.6)


# ---------------------------------------------------------------------------
# 5. Tuning m for a random forest: OOB error against the number of predictors
#    tried at each split (red point = smallest OOB error, dashed line = floor(sqrt(p)))
# ---------------------------------------------------------------------------
set.seed(1)
oob_m <- data.frame(
    m = 1:p,
    error = sapply(1:p, function(m) {
        rf <- randomForest(AHD ~ ., data = heart, mtry = m, ntree = 500)
        rf$err.rate[500, "OOB"]
    })
)
best <- oob_m[which.min(oob_m$error), ]

p_mtry <- ggplot(oob_m, aes(x = m, y = error)) +
    geom_line() +
    geom_point() +
    geom_point(data = best, colour = "red", size = 2.5) +
    geom_vline(xintercept = floor(sqrt(p)), linetype = "dashed") +
    scale_x_continuous(breaks = 1:p) +
    labs(x = "Number of predictors per split (m)", y = "OOB error") +
    theme_bw(base_size = 11)

ggsave("heart_rf_mtry.pdf", p_mtry, width = 4.5, height = 3.6)
