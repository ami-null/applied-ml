library(ggplot2)
library(rpart)


# ---------------------------------------------------------------------------
# 1. Impurity measures for K = 2 classes as a function of p = proportion in class 1
# ---------------------------------------------------------------------------
p <- seq(0.001, 0.999, length.out = 500)
imp <- data.frame(
  p = rep(p, 3),
  value = c(1 - pmax(p, 1 - p),                  # classification error
            2 * p * (1 - p),                     # Gini index
            -(p * log(p) + (1 - p) * log(1 - p))),  # entropy (natural log)
  measure = rep(c("Classification error", "Gini index", "Entropy"), each = length(p))
)

p_imp <- ggplot(imp, aes(x = p, y = value, colour = measure)) +
  geom_line() +
  labs(x = "p", y = "Impurity", colour = NULL) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("impurity.pdf", p_imp, width = 4.5, height = 3.6)

# ---------------------------------------------------------------------------
# 2. Decision regions: logistic regression versus a decision tree, on a
#    linear and on a non-linear (axis-aligned) true boundary
# ---------------------------------------------------------------------------
set.seed(1)
n <- 300

simulate <- function(scenario) {
  x1 <- runif(n, -1, 1)
  x2 <- runif(n, -1, 1)
  if (scenario == "Linear boundary") {
    cls <- x1 + x2 > 0
  } else {
    cls <- (x1 > 0) == (x2 > 0)  # checkerboard of boxes
  }
  flip <- runif(n) < 0.05        # 5% label noise
  data.frame(x1 = x1, x2 = x2, y = factor(ifelse(xor(cls, flip), "A", "B")),
             scenario = scenario)
}

grid <- expand.grid(x1 = seq(-1, 1, length.out = 150), x2 = seq(-1, 1, length.out = 150))

fit_regions <- function(dat) {
  lg <- glm(y ~ x1 + x2, data = dat, family = binomial)
  tr <- rpart(y ~ x1 + x2, data = dat, method = "class")
  g1 <- grid
  g1$class <- ifelse(predict(lg, newdata = grid, type = "response") > 0.5, "B", "A")
  g1$method <- "Logistic regression"
  g2 <- grid
  g2$class <- as.character(predict(tr, newdata = grid, type = "class"))
  g2$method <- "Decision tree"
  out <- rbind(g1, g2)
  out$scenario <- dat$scenario[1]
  out
}

scenarios <- c("Linear boundary", "Non-linear boundary")
dats <- lapply(scenarios, simulate)
regions <- do.call(rbind, lapply(dats, fit_regions))
pts <- do.call(rbind, dats)
pts <- merge(pts, data.frame(method = c("Logistic regression", "Decision tree")))

p_reg <- ggplot() +
  geom_raster(data = regions, aes(x = x1, y = x2, fill = class), alpha = 0.3) +
  geom_point(data = pts, aes(x = x1, y = x2, colour = y), size = 0.6) +
  facet_grid(method ~ scenario) +
  scale_fill_manual(values = c(A = "#D55E00", B = "#0072B2")) +
  scale_colour_manual(values = c(A = "#D55E00", B = "#0072B2")) +
  labs(x = expression(x[1]), y = expression(x[2])) +
  theme_bw(base_size = 9) +
  theme(legend.position = "none")

ggsave("tree_vs_linear.pdf", p_reg, width = 4.5, height = 4)
