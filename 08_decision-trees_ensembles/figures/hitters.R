library(ISLR2)
library(ggplot2)
library(rpart)

hit <- na.omit(Hitters)
hit$logSalary <- log(hit$Salary)

# ---------------------------------------------------------------------------
# 1. Partition of the (Years, Hits) space by the three-leaf tree
#    Cutpoints Years < 4.5 and Hits < 117.5 are those of ISLP Figure 8.1
# ---------------------------------------------------------------------------
hit$region <- with(hit, ifelse(Years < 4.5, "R1", ifelse(Hits < 117.5, "R2", "R3")))
print(tapply(hit$logSalary, hit$region, mean))  # leaf means shown in the tree

p_part <- ggplot(hit, aes(x = Years, y = Hits)) +
  geom_point(aes(colour = logSalary), size = 1.3) +
  geom_vline(xintercept = 4.5, linetype = "dashed") +
  annotate("segment", x = 4.5, xend = max(hit$Years) + 1,
           y = 117.5, yend = 117.5, linetype = "dashed") +
  annotate("text", x = 2.2, y = 225, label = "R[1]", parse = TRUE, size = 4) +
  annotate("text", x = 14, y = 60, label = "R[2]", parse = TRUE, size = 4) +
  annotate("text", x = 14, y = 200, label = "R[3]", parse = TRUE, size = 4) +
  scale_colour_viridis_c(name = "log(Salary)") +
  theme_bw(base_size = 11)

ggsave("hitters_partition.pdf", p_part, width = 4.5, height = 3.6)


# ---------------------------------------------------------------------------
# 2. Training and CV error against tree size (all predictors)
#    rpart reports errors relative to the root node, so we rescale by the
#    root-node MSE
# ---------------------------------------------------------------------------
set.seed(1)
full <- rpart(logSalary ~ . - Salary, data = hit, method = "anova",
              control = rpart.control(cp = 0, minsplit = 5, xval = 10))

mse_root <- sum((hit$logSalary - mean(hit$logSalary))^2) / nrow(hit)
cpt <- as.data.frame(full$cptable)
cpt$size <- cpt$nsplit + 1
cpt <- subset(cpt, size <= 12)

err <- rbind(
  data.frame(size = cpt$size, mse = cpt[["rel error"]] * mse_root, type = "Training"),
  data.frame(size = cpt$size, mse = cpt$xerror * mse_root, type = "Cross-validation")
)

p_cv <- ggplot(err, aes(x = size, y = mse, colour = type)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  labs(x = "Tree size (number of leaves)", y = "Mean squared error", colour = NULL) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("hitters_cv.pdf", p_cv, width = 4.5, height = 3.6)
