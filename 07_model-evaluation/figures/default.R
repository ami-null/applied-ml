# default.R
# Default data (ISLR2): logistic regression of default on student, balance, income.
# Produces: default_threshold_metrics.pdf, default_roc_pr.pdf
# Also prints the confusion-matrix counts and metrics used in the slides (tau = 0.5, 0.2, 0.1).

library(ISLR2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)


data(Default)
fit   <- glm(default ~ student + balance + income, data = Default, family = binomial)
p_hat <- predict(fit, type = "response")
y     <- as.integer(Default$default == "Yes")

metrics_at <- function(tau) {
  pred <- as.integer(p_hat > tau)
  tp <- sum(pred == 1 & y == 1); fp <- sum(pred == 1 & y == 0)
  fn <- sum(pred == 0 & y == 1); tn <- sum(pred == 0 & y == 0)
  precision <- if (tp + fp == 0) NA_real_ else tp / (tp + fp)
  recall    <- tp / (tp + fn)
  data.frame(tau = tau, tp = tp, fp = fp, fn = fn, tn = tn,
             accuracy    = (tp + tn) / length(y),
             precision   = precision,
             recall      = recall,
             specificity = tn / (tn + fp),
             f1          = if (is.na(precision)) NA_real_ else 2 * precision * recall / (precision + recall))
}

# numbers for the slides
print(round(bind_rows(lapply(c(0.5, 0.2, 0.1), metrics_at)), 3))

# ---- Figure 1: metrics as a function of the threshold -----------------------
taus <- seq(0.01, 0.99, by = 0.01)
thr <- bind_rows(lapply(taus, metrics_at)) |>
  select(tau, accuracy, precision, recall, f1) |>
  pivot_longer(-tau, names_to = "metric", values_to = "value") |>
  mutate(metric = factor(metric,
                         levels = c("accuracy", "precision", "recall", "f1"),
                         labels = c("Accuracy", "Precision", "Recall", "F1")))

p1 <- ggplot(thr, aes(tau, value, colour = metric)) +
  geom_vline(xintercept = c(0.5, 0.2, 0.1), linetype = "dotted", colour = "grey60") +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  scale_colour_manual(values = c(Accuracy = "grey30", Precision = "#0072B2",
                                 Recall = "#D55E00", F1 = "#009E73"), name = NULL) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = expression("Threshold " * tau), y = "Metric value") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

p1

ggsave("default_threshold_metrics.pdf", p1, width = 6.5, height = 3.4)

# ---- Figure 2: ROC and precision-recall curves ------------------------------
ord  <- order(p_hat, decreasing = TRUE)
tp   <- cumsum(y[ord])
fp   <- cumsum(1 - y[ord])
tpr  <- tp / sum(y)
fpr  <- fp / sum(1 - y)
prec <- tp / (tp + fp)

roc <- data.frame(fpr = c(0, fpr), tpr = c(0, tpr))
auc <- sum(diff(roc$fpr) * (head(roc$tpr, -1) + tail(roc$tpr, -1)) / 2)

pr  <- data.frame(recall = tpr, precision = prec)

ops <- bind_rows(lapply(c(0.5, 0.2, 0.1), metrics_at)) |>
  mutate(fpr = 1 - specificity, label = paste0("tau == ", tau))

p_roc <- ggplot(roc, aes(fpr, tpr)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.8, colour = "#0072B2") +
  geom_point(data = ops, aes(fpr, recall), colour = "#D55E00", size = 2) +
  geom_text(data = ops, aes(fpr, recall, label = label), parse = TRUE,
            hjust = -0.2, vjust = 1.4, size = 3, colour = "#D55E00") +
  annotate("text", x = 0.6, y = 0.2, label = sprintf("AUC = %.3f", auc)) +
  labs(x = "False positive rate", y = "True positive rate (recall)", title = "ROC curve") +
  theme_bw(base_size = 11)

p_pr <- ggplot(pr, aes(recall, precision)) +
  geom_hline(yintercept = mean(y), linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.8, colour = "#0072B2", na.rm = TRUE) +
  geom_point(data = ops, aes(recall, precision), colour = "#D55E00", size = 2) +
  geom_text(data = ops, aes(recall, precision, label = label), parse = TRUE,
            hjust = -0.2, vjust = -0.6, size = 3, colour = "#D55E00") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Recall", y = "Precision", title = "Precision-recall curve") +
  theme_bw(base_size = 11)

p2 <- p_roc + p_pr
p2

ggsave("default_roc_pr.pdf", p2, width = 8.5, height = 3.4)
