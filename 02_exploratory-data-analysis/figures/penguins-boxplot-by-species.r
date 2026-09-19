library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(patchwork)
library(ggridges)
theme_set(theme_minimal(base_size = 9) +
                            theme(plot.title = element_text(size = 9, face = "bold")))

peng <- read_csv(
      "https://raw.githubusercontent.com/mwaskom/seaborn-data/master/penguins.csv",
      show_col_types = FALSE) |>
      drop_na(body_mass_g)

p_box <- ggplot(peng, aes(body_mass_g, species)) +
      geom_boxplot(outlier.shape = NA) +
      # geom_jitter(width = 0.15, size = 0.5, alpha = 0.5) +
      labs(x = "Body mass (g)", y = "Species") +
      theme(
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA)
    )

p_box

ggsave("eda_penguins_mass_species.pdf", width = 6.0, height = 1.6)
