library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(patchwork)

theme_set(theme_minimal(base_size = 9) +
                            theme(plot.title = element_text(size = 9, face = "bold")))
# dir.create("figures", showWarnings = FALSE)

vars <- c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")
peng <- read_csv(
      "https://raw.githubusercontent.com/mwaskom/seaborn-data/master/penguins.csv",
      show_col_types = FALSE) |>
      drop_na(all_of(vars), sex)

p_facet <- ggplot(peng, aes(flipper_length_mm, body_mass_g, colour = species, shape = sex)) +
      geom_point(size = 0.8, alpha = 0.8) +
      facet_wrap(~ island) +
      labs(title = "colour = species, shape = sex, facets = island",
                    x = "flipper length (mm)", y = "body mass (g)")

# pc <- peng |>
#       mutate(across(all_of(vars), \(v) as.numeric(scale(v)))) |>
#       mutate(id = row_number()) |>
#       pivot_longer(all_of(vars), names_to = "variable", values_to = "z") |>
#       mutate(variable = factor(variable, levels = vars,
#                                labels = c("bill length", "bill depth", "flipper", "mass")))

# p_par <- ggplot(pc, aes(variable, z, group = id, colour = species)) +
#       geom_line(alpha = 0.2, linewidth = 0.3) +
#       labs(title = "parallel coordinates (standardised)", x = NULL, y = "z-score")

# p <- (p_facet | p_par) + plot_layout(guides = "collect", widths = c(1.4, 1))

p_facet

ggsave("eda_penguins_encodings.pdf", width = 6.0, height = 1.7)
