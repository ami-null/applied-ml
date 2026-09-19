library(dplyr)
library(ggplot2)
library(patchwork)
theme_set(theme_minimal(base_size = 9) +
                            theme(plot.title = element_text(size = 9, face = "bold")))
# dir.create("figures", showWarnings = FALSE)

k_labels <- scales::label_number(scale = 1e-3, suffix = "k")

p_box <- ggplot(diamonds, aes(x = price, y = "")) +
      geom_boxplot(outlier.size = 0.4, outlier.alpha = 0.3) +
      scale_x_continuous(labels = k_labels) +
      labs(
          # title = "boxplot",
          x = "price (USD)",
          y = NULL
        )

# p_violin <- ggplot(diamonds, aes(x = price, y = "")) +
#       geom_violin(fill = "grey85") +
#       geom_boxplot(width = 0.12, outlier.shape = NA) +
#       scale_x_continuous(labels = k_labels) +
#       labs(title = "violin + boxplot", x = "price (USD)", y = NULL)
#
# p_ecdf <- ggplot(diamonds, aes(price)) +
#       stat_ecdf(geom = "step") +
#       geom_hline(yintercept = c(0.25, 0.5, 0.75), linetype = "dotted", colour = "grey50") +
#       scale_x_continuous(labels = k_labels) +
#       labs(title = "ECDF", x = "price (USD)", y = "cumulative proportion")

p <- p_box #| p_violin | p_ecdf
p
ggsave("eda_diamonds_box_violin_ecdf.pdf", p, width = 6.0, height = 1.5)
