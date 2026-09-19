library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
theme_set(theme_minimal(base_size = 9) +
                            theme(plot.title = element_text(size = 9, face = "bold")))
# dir.create("figures", showWarnings = FALSE)

titanic <- read_csv(
      "https://raw.githubusercontent.com/mwaskom/seaborn-data/master/titanic.csv",
      show_col_types = FALSE)

freq <- titanic |>
      transmute(outcome = if_else(survived == 1, "survived", "died"),
                              class = paste("class", pclass),
                              sex = sex) |>
      pivot_longer(everything(), names_to = "variable", values_to = "level") |>
      count(variable, level) |>
      group_by(variable) |>
      mutate(share = n / sum(n)) |>
      ungroup()

p <- ggplot(freq, aes(level, n)) +
      geom_col(fill = "grey35") +
      geom_text(aes(label = scales::percent(share, accuracy = 0.1)), vjust = -0.4, size = 2.6) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      facet_wrap(~ variable, scales = "free_x") +
      labs(x = NULL, y = "Number of passengers") +
    theme(
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA)
    )
p
ggsave("eda_titanic_bars.pdf", p, width = 6.0, height = 1.5)
