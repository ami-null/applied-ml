library(ggplot2)
library(ISLR2)

data(Default)
Default$y <- as.integer(Default$default == "Yes")

# ── Model fits (numbers used in the slides) ───────────────────────────────────

m_balance  <- glm(default ~ balance,         data = Default, family = binomial)
m_student  <- glm(default ~ student,         data = Default, family = binomial)
m_full     <- glm(default ~ balance + income + student,
                  data = Default, family = binomial)

cat("=== balance only ===\n");   print(summary(m_balance)$coefficients)
cat("\nOdds ratio per $100 balance:",
    exp(100 * coef(m_balance)["balance"]), "\n")
cat("95% CI (OR per $100):",
    exp(100 * confint.default(m_balance)["balance", ]), "\n")
cat("P(default | balance = 1000):",
    predict(m_balance, newdata = data.frame(balance = 1000), type = "response"), "\n")
cat("P(default | balance = 2000):",
    predict(m_balance, newdata = data.frame(balance = 2000), type = "response"), "\n")

cat("\n=== student only ===\n"); print(summary(m_student)$coefficients)
cat("\n=== balance + income + student ===\n")
print(summary(m_full)$coefficients)

cat("\nMean balance by student status:\n")
print(tapply(Default$balance, Default$student, mean))

# ── Figure: linear fit vs logistic fit ───────────────────────────────────────
# cls_linear_vs_logistic.pdf

p <- ggplot(Default, aes(x = balance, y = y)) +
    geom_hline(yintercept = c(0, 1), colour = "grey70", linewidth = 0.3) +
    geom_point(alpha = 0.1, size = 0.5) +
    geom_smooth(aes(colour = "Linear",   linetype = "Linear"),
                method = "lm",  se = FALSE, linewidth = 0.8) +
    geom_smooth(aes(colour = "Logistic", linetype = "Logistic"),
                method = "glm", method.args = list(family = binomial),
                se = FALSE, linewidth = 0.8) +
    scale_colour_manual(values = c(Linear = "#D55E00", Logistic = "#0072B2")) +
    scale_linetype_manual(values = c(Linear = "dashed", Logistic = "solid")) +
    labs(x = "Balance", y = "Default (0/1)", colour = NULL, linetype = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")

p

ggsave("cls_linear_vs_logistic.pdf", p, width = 8, height = 6, units = "cm")
