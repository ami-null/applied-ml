library(ggplot2)
data(mtcars)

theme_set(theme_minimal(base_size = 9) +
              theme(plot.title = element_text(size = 9, face = "bold")))

ggplot(mtcars, aes(wt)) + geom_boxplot() +
    theme(
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA),
        axis.text.y = element_blank()
    ) +
    labs(x = NULL)

ggsave("eda_mtcars_wt_boxplot.pdf", width = 6.0, height = 1.6)
