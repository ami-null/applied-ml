library(tidyverse)
theme_set(
    theme_minimal(base_size = 9) +
        theme(
            plot.title = element_text(size = 8, face = "bold"),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
    )

Credit <- read.csv("https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/ISLR/Credit.csv")

fit_add <- lm(Balance ~ Income + Student, data = Credit)
fit_int <- lm(Balance ~ Income * Student, data = Credit)
grid <- expand_grid(Income = seq(min(Credit$Income), max(Credit$Income), length.out = 100),
                                          Student = c("No", "Yes"))
fit_lines <- bind_rows(mutate(grid, fit = predict(fit_add, grid), model = "no interaction"),
                                                mutate(grid, fit = predict(fit_int, grid), model = "with interaction"))

p <- ggplot(Credit, aes(Income, Balance, colour = Student)) +
      geom_point(size = 0.5, alpha = 0.4) +
      geom_line(data = fit_lines, aes(y = fit), linewidth = 0.7) +
      facet_wrap(~ model) +
      scale_colour_manual(values = c(No = "steelblue", Yes = "darkorange")) +
      labs(x = "income (thousands of dollars)", y = "balance (dollars)")

p

ggsave("reg_credit_interaction.pdf", p, width = 6.0, height = 2.75, bg = "transparent")
