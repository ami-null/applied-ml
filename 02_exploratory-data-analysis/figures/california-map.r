library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(patchwork)
library(maps)

theme_set(theme_minimal(base_size = 9) +
              theme(plot.title = element_text(size = 9, face = "bold")))

housing <- read_csv(
    "https://raw.githubusercontent.com/ageron/handson-ml/master/datasets/housing/housing.csv",
    show_col_types = FALSE)

# ca <- map_data("state", region = "california")
#
# map_base <- ggplot(housing, aes(longitude, latitude)) +
#     geom_polygon(data = ca, aes(long, lat, group = group),
#                  fill = "grey95", colour = "grey40", linewidth = 0.3,
#                  inherit.aes = FALSE) +
#     coord_quickmap() +
#     labs(x = NULL, y = NULL) +
#     theme(axis.text = element_blank(), panel.grid = element_blank(),
#           legend.key.size = unit(0.25, "cm"), legend.text = element_text(size = 6))

ca <- map_data("state", region = "california")
counties <- map_data("county", region = "california")

map_base <- ggplot(housing, aes(longitude, latitude)) +
    geom_polygon(data = ca, aes(long, lat, group = group),
                 fill = "grey95", colour = NA, inherit.aes = FALSE) +
    geom_polygon(data = counties, aes(long, lat, group = group),
                 fill = NA, colour = "grey75", linewidth = 0.1, inherit.aes = FALSE) +
    geom_polygon(data = ca, aes(long, lat, group = group),
                 fill = NA, colour = "grey40", linewidth = 0.3, inherit.aes = FALSE) +
    coord_quickmap() +
    labs(x = NULL, y = NULL) +
    theme(axis.text = element_blank(), panel.grid = element_blank(),
          legend.key.size = unit(0.25, "cm"), legend.text = element_text(size = 6)) +
    theme(
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA)
    )

p_val <- map_base +
    geom_point(aes(colour = median_house_value), size = 0.3, alpha = 0.6) +
    scale_colour_viridis_c(labels = scales::label_number(scale = 1e-3, suffix = "k")) +
    labs(title = "Median house value (USD)", colour = NULL)

p_inc <- map_base +
    geom_point(aes(colour = median_income), size = 0.3, alpha = 0.6) +
    scale_colour_viridis_c() +
    labs(title = "Median income", colour = NULL)

p_prox <- map_base +
    geom_point(aes(colour = ocean_proximity), size = 0.3, alpha = 0.6) +
    guides(colour = guide_legend(override.aes = list(size = 1.5, alpha = 1))) +
    labs(title = "Ocean proximity", colour = NULL)

p <- p_val | p_inc | p_prox
p

ggsave("eda_california_maps.pdf", p, width = 6.0, height = 2.5)
system("pdfcrop eda_california_maps.pdf eda_california_maps.pdf")
