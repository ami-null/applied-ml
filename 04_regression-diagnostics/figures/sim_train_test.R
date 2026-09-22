library(tidyverse)

theme_set(
    theme_minimal(base_size = 9) +
        theme(
            plot.title = element_text(size = 8, face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
)


# training vs test MSE as features are added: 45 candidate features, only 5 matter
set.seed(2)
n_train <- 50; n_test <- 2000; n_feat <- 45; sigma <- 1
beta <- c(2, -1.5, 1, 1, -1, rep(0, n_feat - 5))          # only the first five features matter

sim_data <- function(n) {
  X <- matrix(rnorm(n * n_feat), n, n_feat, dimnames = list(NULL, paste0("x", 1:n_feat)))
  as_tibble(X) |> mutate(y = drop(X %*% beta) + rnorm(n, sd = sigma))
}
train_df <- sim_data(n_train)
test_df <- sim_data(n_test)

err <- map_dfr(1:n_feat, function(k) {
  fit <- lm(reformulate(paste0("x", 1:k), response = "y"), data = train_df)
  tibble(features = k, training = mean(resid(fit)^2),
         test = mean((test_df$y - predict(fit, test_df))^2))
}) |> pivot_longer(c(training, test), names_to = "set", values_to = "mse")


p <- ggplot(err, aes(features, mse, colour = set)) +
  geom_line(linewidth = 0.6) +
  geom_hline(yintercept = sigma^2, linetype = "dashed") +
  annotate("text", x = 45, y = sigma^2, label = "irreducible error", vjust = -0.6, hjust = 1, size = 2.5) +
  coord_cartesian(ylim = c(0, 10)) +
  labs(title = "nested linear models, simulated data", x = "number of features used",
       y = "MSE", colour = NULL) +
  theme(plot.title = element_text(size = 8, face = "bold"))

p

ggsave("reg_train_test.pdf", p, width = 3.4, height = 2.75, bg = "transparent")
