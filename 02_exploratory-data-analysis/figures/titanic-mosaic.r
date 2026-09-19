library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(patchwork)
theme_set(theme_minimal(base_size = 9) +
                            theme(plot.title = element_text(size = 9, face = "bold")))
# dir.create("figures", showWarnings = FALSE)

titanic <- read_csv(
      "https://raw.githubusercontent.com/mwaskom/seaborn-data/master/titanic.csv",
      show_col_types = FALSE) |>
      transmute(outcome = if_else(survived == 1, "survived", "died"),
                              class = paste("class", pclass),
                              sex = sex)

# mosaic <- function(df, var) {
#       tab <- df |>
#             transmute(level = .data[[var]], outcome) |>
#             count(level, outcome) |>
#             group_by(level) |>
#             mutate(n_level = sum(n), share = n / n_level) |>
#             arrange(level, outcome) |>
#             mutate(ymax = cumsum(share), ymin = ymax - share) |>
#             ungroup()
#       widths <- tab |>
#             distinct(level, n_level) |>
#             mutate(w = n_level / sum(n_level), xmax = cumsum(w), xmin = xmax - w)
#       tab <- left_join(tab, widths, by = c("level", "n_level"))
#       ggplot(tab, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = outcome)) +
#             geom_rect(colour = "white") +
#             geom_text(aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2,
#                                               label = scales::percent(share, accuracy = 1)),
#                                       size = 2.4, colour = "white") +
#             scale_x_continuous(breaks = (widths$xmin + widths$xmax) / 2,
#                                                         labels = widths$level, expand = c(0, 0)) +
#             scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
#             scale_fill_manual(values = c(died = "grey55", survived = "steelblue")) +
#             labs(title = paste("outcome by", var), x = NULL, y = NULL)
#     }
#
# p <- (mosaic(titanic, "sex") | mosaic(titanic, "class")) +
#       plot_layout(guides = "collect")
# p
ggplot(titanic) +
    geom_bar(aes(y = class, fill = outcome), alpha = 0.8) +
    theme(
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA)
    )

ggsave("eda_titanic_stacked-barplot.pdf", width = 6.0, height = 1.7)
