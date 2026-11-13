# helpers.R: shared theme, palette and dendrogram helper
# sourced by other scripts
library(ggplot2)
library(ggdendro)

# Okabe-Ito colour-blind-safe palette
okabe <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7",
           "#E69F00", "#56B4E9", "#F0E442", "#999999")

theme_set(
    theme_minimal(base_size = 16) +
        theme(
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 15, face = "bold"),
            legend.position = "bottom"
        )
)

# Dendrogram of an hclust object as a ggplot.
#   label:        draw leaf labels (hc$labels)
#   label_groups: optional vector (in original observation order) used to
#                 colour the leaf labels; add scale_colour_manual() afterwards
#   cut_height:   optional height at which to draw a dashed horizontal line
#   label_space:  space below the tree for the labels, as a fraction of the
#                 height range (increase it for short panels or long labels)
gg_dendro <- function(hc, label = TRUE, label_size = 3,
                      label_groups = NULL, cut_height = NULL,
                      label_space = 0.3) {
    dd <- dendro_data(hc, type = "rectangle")
    p <- ggplot() +
        geom_segment(
            data = dd$segments,
            aes(x = x, y = y, xend = xend, yend = yend),
            linewidth = 0.5
        )

    if (label) {
        lab <- dd$labels
        if (is.null(label_groups)) {
            p <- p + geom_text(
                data = lab, aes(x = x, y = y, label = label),
                angle = 90, hjust = 1, size = label_size
            )
        } else {
            lab$grp <- factor(label_groups[hc$order])
            p <- p + geom_text(
                data = lab, aes(x = x, y = y, label = label, colour = grp),
                angle = 90, hjust = 1, size = label_size, show.legend = FALSE
            )
        }
        p <- p + scale_y_continuous(
            expand = expansion(mult = c(label_space, 0.05)),
            breaks = function(lim) {                # no ticks under the tree
                b <- scales::breaks_extended()(lim)
                b[b >= 0]
            }
        )
    }
    if (!is.null(cut_height)) {
        p <- p + geom_hline(yintercept = cut_height, linetype = "dashed",
                            colour = okabe[2])
    }
    p +
        labs(y = "Height") +
        theme(
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            panel.grid.major.x = element_blank()
        )
}
