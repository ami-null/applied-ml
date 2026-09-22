library(tidyverse)

theme_set(
    theme_minimal(base_size = 9) +
        theme(
            plot.title = element_text(size = 8, face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
)

set.seed(3)
n <- 100
ar1 <- function(rho) {                      # autocorrelated errors: e_t = rho * e_(t-1) + u_t
  e <- numeric(n); u <- rnorm(n); e[1] <- u[1]
  for (k in 2:n) e[k] <- rho * e[k - 1] + u[k]
  e
}
res <- map_dfr(c(0, 0.5, 0.9), function(rho) {
  d <- tibble(t = 1:n, y = 1 + 0.05 * (1:n) + ar1(rho))
  tibble(t = d$t, resid = resid(lm(y ~ t, data = d)), panel = paste("rho =", rho))
})

p <- ggplot(res, aes(t, resid)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 0.3) +
  geom_point(size = 0.4) +
  facet_wrap(~ panel) +
  labs(x = "observation order", y = "residual")

p

ggsave("reg_diag_autocorr.pdf", p, width = 6.0, height = 2.75, bg = "transparent")
