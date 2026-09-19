# ---- R code: figures/eda_diamonds_cut_carat.pdf ----
library(ggplot2)
library(patchwork)
library(dplyr)
theme_set(theme_minimal(base_size = 9) +
                            theme(plot.title = element_text(size = 9, face = "bold")))
set.seed(1)
# dir.create("figures", showWarnings = FALSE)

p_box <- ggplot(diamonds, aes(cut, carat)) +
      geom_boxplot(outlier.size = 0.4, outlier.alpha = 0.3) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      labs(title = "Carat by cut", x = NULL, y = "carat") + theme(
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background  = element_rect(fill = "transparent", colour = NA)
      )

p_scatter <- ggplot(diamonds, aes(carat, price, colour = cut)) +
      geom_point(data = slice_sample(diamonds, n = 5000), alpha = 0.25, size = 0.4) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
      scale_x_log10() +
      scale_y_log10(labels = scales::label_comma()) +
      scale_colour_viridis_d() +
      guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1.5))) +
      labs(title = "Price vs. carat, by cut", x = "carat (log scale)",
                    y = "price, USD (log scale)") +
    theme(
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA)
    )

p <- (p_box | p_scatter) + plot_layout(widths = c(1, 1.6))
p
ggsave("eda_diamonds_cut_carat.pdf", p, width = 6.0, height = 1.7)
