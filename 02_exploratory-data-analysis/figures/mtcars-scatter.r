library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(patchwork)
theme_set(theme_minimal(base_size = 9) +
                            theme(plot.title = element_text(size = 9, face = "bold")))

data(mtcars)

ggplot(mtcars) + geom_point(aes(wt, mpg)) + labs(x = "Weight (in 1000 lbs)", y = "MPG") +
    theme(
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA)
    )

ggsave("eda_mtcars-scatter.pdf", width = 6.0, height = 1.6)   # about 3 MB
