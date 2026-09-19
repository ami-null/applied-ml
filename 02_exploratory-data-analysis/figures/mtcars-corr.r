library(dplyr)
library(tidyr)
library(ggplot2)

theme_set(theme_minimal(base_size = 8))

data(mtcars)
mtcars <- mtcars |> select(mpg, wt, hp, disp)

cm  <- cor(mtcars)
ord <- colnames(cm)[hclust(as.dist(1 - abs(cm)))$order]   # cluster ordering

cdf <- as_tibble(cm, rownames = "row") |>
      pivot_longer(-row, names_to = "col", values_to = "r") |>
      mutate(row = factor(row, levels = rev(ord)),
                        col = factor(col, levels = ord))

p <- ggplot(cdf, aes(col, row, fill = r)) +
      geom_tile(colour = "white") +
      geom_text(aes(label = sprintf("%.2f", r)), size = 1.9) +
              scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC",
                       limits = c(-1, 1)) +
      coord_fixed() +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme(
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA)
    )
p

ggsave("eda_mtcars_corr.pdf", p, width = 3.6, height = 3.4)
