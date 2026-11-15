# helpers.R: shared theme, palette and number reporting for lecture 11
# sourced by the other scripts; run every script from this folder
library(ggplot2)

# Okabe-Ito colour-blind-safe palette (same as lecture 10)
okabe <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7",
           "#E69F00", "#56B4E9", "#F0E442", "#999999")

theme_set(
    theme_minimal(base_size = 13) +
        theme(
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 12, face = "bold"),
            legend.position = "bottom"
        )
)

# ---- Numbers quoted on the slides --------------------------------------
# Every number that appears on a slide is passed through report(), which
# prints it and appends it to numbers_<script>.txt. The text files are the
# record that backs the slides: rerun the script and the file is rebuilt.
numbers_start <- function(script) {
    f <- paste0("numbers_", script, ".txt")
    cat("# Numbers quoted on the lecture 11 slides\n",
        "# produced by ", script, ".R; do not edit by hand\n",
        file = f, sep = "")
    assign(".numbers_file", f, envir = globalenv())
    invisible(f)
}

numbers_section <- function(title) {
    line <- paste0("\n## ", title, "\n")
    cat(line)
    cat(line, file = get(".numbers_file", envir = globalenv()), append = TRUE)
}

# value can be a number, a (named) vector or a string; numbers are rounded
# to `digits` decimal places, or to `digits` significant digits in
# scientific notation when sci = TRUE (for very small or large values)
report <- function(label, value, digits = 3, sci = FALSE) {
    if (is.numeric(value) && sci) {
        txt <- formatC(value, format = "e", digits = digits - 1)
    } else if (is.numeric(value)) {
        txt <- format(round(value, digits), big.mark = ",",
                      scientific = FALSE, trim = TRUE)
    } else {
        txt <- as.character(value)
    }
    if (!is.null(names(value))) txt <- paste0(names(value), " = ", txt)
    line <- paste0(label, ": ", paste(txt, collapse = ", "), "\n")
    cat(line)
    cat(line, file = get(".numbers_file", envir = globalenv()), append = TRUE)
}

# Sample skewness g1, as defined in lecture 2
skewness <- function(x) {
    z <- (x - mean(x)) / sd(x)
    mean(z^3)
}

# Standardize test data with the training mean and SD (no leakage)
scale_train_test <- function(train, test) {
    mu <- colMeans(train)
    s <- apply(train, 2, sd)
    list(
        train = sweep(sweep(train, 2, mu), 2, s, "/"),
        test  = sweep(sweep(test, 2, mu), 2, s, "/")
    )
}
