library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(patchwork)

theme_set(theme_minimal(base_size = 9) +
              theme(plot.title = element_text(size = 9, face = "bold")))

ap <- tibble(
        year = floor(as.numeric(time(AirPassengers)) + 1e-6),
        month = as.integer(cycle(AirPassengers)),
        passengers = as.numeric(AirPassengers)
    ) |>
    mutate(
        date = as.Date(sprintf("%d-%02d-01", year, month)),
        year = factor(year)
    )

p_raw <- ggplot(ap, aes(date, passengers)) +
  geom_line() +
  labs(title = "monthly passengers", x = NULL, y = "thousands") +
    theme(
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA)
    )


p_season <- ggplot(ap, aes(month, passengers, group = year, colour = year)) +
  geom_line(linewidth = 0.4) +
  scale_x_continuous(breaks = c(1, 4, 7, 10)) +
  # scale_colour_viridis_c() +
  labs(title = "seasonal plot: one line per year", x = "month", y = "thousands",
       colour = NULL) +
  theme(legend.key.height = unit(0.5, "cm"), legend.key.width = unit(0.2, "cm")) +
    theme(
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA)
    )

p <- p_raw | p_season
p
ggsave("eda_airpassengers.pdf", p, width = 6.0, height = 2.5)
