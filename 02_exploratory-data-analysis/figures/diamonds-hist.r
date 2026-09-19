# ---- R code: figures/eda_diamonds_hist_density.pdf ----
library(ggplot2)
library(dplyr)
library(patchwork)
theme_set(theme_minimal(base_size = 9) +
                            theme(plot.title = element_text(size = 9, face = "bold")))
# dir.create("figures", showWarnings = FALSE)

k_labels <- scales::label_number(scale = 1e-3, suffix = "k")

hist_bw <- function(w) {
      ggplot(diamonds, aes(price)) +
            geom_histogram(binwidth = w, boundary = 0, closed = "left", fill = "grey35") +
            scale_x_continuous(labels = k_labels) +
            labs(title = paste("binwidth =", w), x = "price (USD)", y = "count") +
        theme(
            panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background  = element_rect(fill = "transparent", colour = NA)
        )
}

# p_dens <- ggplot(diamonds, aes(price)) +
#       geom_density(aes(colour = "0.2"), adjust = 0.2) +
#       geom_density(aes(colour = "1"), adjust = 1) +
#       geom_density(aes(colour = "3"), adjust = 3) +
#       scale_x_continuous(labels = k_labels) +
#       labs(title = "density estimates", x = "price (USD)", y = "density", colour = "adjust") +
#       theme(legend.position = "inside", legend.position.inside = c(0.8, 0.7),
#                       legend.key.size = unit(0.3, "cm"))

p <- hist_bw(1000) | hist_bw(100) # | p_dens
p
ggsave("eda_diamonds_hist_density.pdf", p, width = 6.0, height = 1.5)
