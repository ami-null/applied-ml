library(tidyverse)

theme_set(theme_minimal(base_size = 9) +
                            theme(plot.title = element_text(size = 9, face = "bold")))


adv_url <- "https://raw.githubusercontent.com/JWarmenhoven/ISLR-python/master/Notebooks/Data/Advertising.csv"
ad <- read_csv(adv_url, show_col_types = FALSE) |> select(-1)   # drop the row-index column

ad_long <- ad |>
      pivot_longer(c(TV, Radio, Newspaper), names_to = "medium", values_to = "budget") |>
      mutate(medium = factor(medium, levels = c("TV", "Radio", "Newspaper")))

p <- ggplot(ad_long, aes(budget, Sales)) +
      geom_point(size = 0.5, alpha = 0.6, colour = "grey30") +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "red", linewidth = 0.6) +
      facet_wrap(~ medium, scales = "free_x") +
      labs(x = "budget (thousands of dollars)", y = "sales (thousands of units)")

p

ggsave("reg_advertising_scatter.pdf", p, width = 6.0, height = 2.75)
