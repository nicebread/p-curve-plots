library(ggplot2)
library(ggimage)
library(pwr)
source("p-curve-comp.R")


#' Plot the expected p-value distribution under the null hypothesis (H0)
#'
#' This function generates a ggplot showing a uniform density distribution
#' of p-values, which is expected when there is no true effect (H0).
#'
#' @param stage Integer (1, 2, or 3) indicating the plot depth:
#'   1: Base H0 line and dotted borders.
#'   2: Adds the light red background area.
#'   3: Adds the 5% significance region, text labels, and dart images.
#'      Note: Dart images are searched relative to the path of the Quarto
#'      file calling this function.
#'
#' @return A ggplot object representing the H0 distribution.
#' @export
#'
#' @examples
#' plot_h0(stage = 1)
#' plot_h0(stage = 3)
plot_h0 <- function(stage = 1) {
    p <- ggplot()

    # Stage 2 & 3: Light red background area
    if (stage >= 2) {
        p <- p + annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "#FCD9D9")
    }

    # Stage 3: Dark red significance box and text labels
    if (stage >= 3) {
        p <- p +
            annotate("rect", xmin = 0, xmax = 0.05, ymin = 0, ymax = 1, fill = "firebrick") +
            annotate("text",
                x = 0.025, y = 0.5, label = "5%", color = "white",
                fontface = "bold", angle = 90, size = 6
            )
    }

    # Stage 1: Base lines (always present)
    p <- p +
        geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1), linetype = "dotted") +
        geom_segment(aes(x = 1, xend = 1, y = 0, yend = 1), linetype = "dotted") +
        geom_segment(aes(x = 0, xend = 1, y = 0, yend = 0), linetype = "dotted") +
        geom_segment(aes(x = 0, xend = 1, y = 1, yend = 1), color = "red", linewidth = 1.2) +
        scale_x_continuous(
            limits = c(0, 1), breaks = seq(0, 1, by = 0.2),
            labels = function(x) sprintf("%.1f", x)
        ) +
        scale_y_continuous(limits = c(0, 5)) +
        labs(x = "p value", y = "Density") +
        guides(x = guide_axis(cap = "both"), y = guide_axis(cap = "both")) +
        theme_classic(base_size = 14)

    # Stage 3: Dart symbols (On top of Stage 1 lines)
    if (stage >= 3) {
        dart_data <- data.frame(
            x = c(0.20, 0.29, 0.50, 0.58, 0.74, 0.89),
            y = c(1.05, 1.05, 1.05, 1.05, 1.05, 1.05),
            image = "dart.png"
        )

        p <- p +
            ggimage::geom_image(
                data = dart_data,
                mapping = aes(x = x, y = y, image = image),
                inherit.aes = FALSE,
                asp = 1,
                size = 0.2
            )
    }

    return(p)
}

print(plot_h0(stage = 3))


#' Plot the expected p-value distribution under the alternative hypothesis (H1)
#'
#' This function visualizes the right-skewed p-value distribution (p-curve)
#' expected when a true effect exists, given a specific statistical power.
#'
#' @param power Numeric. The true statistical power (1 - beta). Default is 0.10.
#' @param p.max Numeric. The maximum p-value to plot on the x-axis. Default is 0.9999.
#' @param ymax Numeric. The maximum y-axis value (density). Default is 5.
#' @param sig.region Logical. If TRUE, visually highlights the significant region (p <= 0.05).
#' @param callout Logical. If TRUE, displays a blue callout box pointing to the plot and showing the chosen power value. Default is FALSE.
#' @param label Character. Optional custom text for the blue box. Use \n for line breaks.
#'
#' @return A ggplot object representing the p-curve.
#' @export
#'
#' @examples
#' pcurve_plot(.80, ymax = 30, sig.region = TRUE, callout = TRUE)
pcurve_plot <- function(power = .10, p.max = .9999, ymax = 5, sig.region = FALSE, callout = FALSE, label = NULL) {
    # Generate data for the curve
    df <- data.frame(p = seq(.001, p.max, length.out = 1000))
    df$density <- dpvalue(df$p, power = power)

    # Base plot with area and H0 reference line
    p_base <- ggplot(df, aes(x = p, y = density)) +
        geom_area(
            fill = "#DFFABE", color = "black", linetype = "dotted",
            linewidth = 0.5, outline.type = "full"
        ) +
        geom_line(color = "green", linewidth = 1.2) +
        annotate("segment",
            x = 0.01, xend = p.max, y = 1, yend = 1,
            color = "red", linetype = "dashed", linewidth = 0.8
        ) +
        scale_x_continuous(
            limits = c(0, round(p.max, 1)), breaks = seq(0, 1, by = 0.2),
            labels = function(x) sprintf("%.1f", x)
        ) +
        scale_y_continuous(expand = c(0, 0)) +
        coord_cartesian(ylim = c(0, ymax)) +
        labs(x = "p value", y = "Density") +
        guides(x = guide_axis(cap = "both"), y = guide_axis(cap = "both")) +
        theme_classic(base_size = 14)

    # Add significance region
    if (sig.region) {
        df_sig <- df[df$p <= 0.05, ]

        p_base <- p_base +
            geom_area(
                data = df_sig, fill = "#93F759", color = "black",
                linetype = "dotted", linewidth = 0.5, outline.type = "full"
            ) +
            # Vertical percentage text inside the green box
            annotate("text",
                x = 0.025, y = 0,
                label = paste0(round(power * 100), "%"),
                color = "black", fontface = "bold", angle = 90, hjust = -0.1, size = 6
            )
    }

    # Add blue callout bubble
    if (callout) {
        # Determine final label text
        # If label is NULL, use default percentage, otherwise use provided label
        final_label <- if (is.null(label)) paste(round(power * 100), "% power") else label

        p_base <- p_base +
            # Blue Callout: Pointer/Line
            annotate("segment",
                x = 0.4, y = ymax * 0.7,
                xend = 0.05, yend = dpvalue(0.05, power = power),
                color = "royalblue", linewidth = 2.5
            ) +
            # Blue Callout: Label Box
            geom_label(
                data = data.frame(x = 0.4, y = ymax * 0.7, label = final_label),
                mapping = aes(x = x, y = y, label = label),
                fill = "royalblue", color = "white",
                fontface = "bold",
                size = 8,
                hjust = 0,
                vjust = 0,
                label.padding = unit(0.8, "lines"),
                linewidth = 0,
                inherit.aes = FALSE
            )
    }

    return(p_base)
}

print(pcurve_plot(.10, ymax = 8))
print(pcurve_plot(.10, ymax = 8, sig.region = TRUE))
print(pcurve_plot(.35, ymax = 12, sig.region = TRUE))
print(pcurve_plot(.80, ymax = 30, sig.region = TRUE))
print(pcurve_plot(.95, ymax = 30, sig.region = TRUE))
print(pcurve_plot(.80, ymax = 30, sig.region = TRUE, callout = TRUE))


#' Special P-Curve Plot 1: Highlight the p-value region 0.03 to 0.05
#'
#' This function creates a p-curve plot highlighting a specific interval
#' of "just significant" p-values (0.03 to 0.05), which can be useful for
#' demonstrating concepts like p-hacking or publication bias.
#'
#' @param power Numeric. The true statistical power. Default is 0.65.
#' @param p.max Numeric. The maximum p-value to plot. Default is 0.9999.
#' @param ymax Numeric. The maximum density on the y-axis. Default is 22.
#'
#' @return A ggplot object with the highlighted region.
#' @export
#'
#' @examples
#' plot_special_1()
plot_special_1 <- function(power = .65, p.max = .9999, ymax = 22) {
    df <- data.frame(p = seq(.001, p.max, length.out = 1000))
    df$density <- dpvalue(df$p, power = power)
    df <- df[df$density <= ymax, ]

    df_sig <- df[df$p >= 0.03 & df$p <= 0.05, ]

    p <- ggplot(df, aes(x = p, y = density)) +
        geom_area(fill = "#DFFABE") +
        geom_area(data = df_sig, fill = "#93F759", color = "black", linetype = "dotted", linewidth = 0.5, outline.type = "full") +
        geom_line(color = "green", linewidth = 1.2) +
        scale_x_continuous(limits = c(0, round(p.max, 1)), breaks = seq(0, 1, by = 0.2), labels = function(x) sprintf("%.1f", x)) +
        scale_y_continuous(limits = c(0, ymax), expand = c(0, 0)) +
        labs(x = "p value", y = "Density") +
        guides(x = guide_axis(cap = "both"), y = guide_axis(cap = "both")) +
        theme_classic(base_size = 14)

    return(p)
}

print(plot_special_1())


#' Special P-Curve Plot 2: Highlight specific p-value bins
#'
#' This function creates a detailed view of the p-curve for small p-values
#' (e.g., up to 0.20), specifically highlighting the regions 0 to 0.025 and
#' 0.025 to 0.05 with distinct colors. Optional example elements (darts,
#' callouts, k-values) can be activated and are true for a power of 60%.
#' Optional publication bias annotations can also be activated.
#'
#' @param power Numeric. The true statistical power. Default is 0.60.
#' @param p.max Numeric. The maximum p-value to plot. Default is 0.2.
#' @param ymax Numeric. The maximum density on the y-axis. Default is 50.
#' @param callout Logical. If TRUE, adds dart images, k-labels, and callout bubbles with dynamic percentages of how many of all p-values fall into the respective bins.
#' @param darts_bin1 Integer. Number of darts in the first bin (p < 0.025). Default is 5.
#' @param darts_bin2 Integer. Number of darts in the second bin (0.025 <= p <= 0.05). Default is 13.
#' @param publication_bias Logical. If TRUE, adds vertical red line at 0.05 and arrows indicating published vs. file-drawer bins.
#'
#' @return A ggplot object showing binned significance regions.
#' @export
#'
#' @examples
#' plot_special_2()
#' plot_special_2(callout = TRUE)
plot_special_2 <- function(power = .60, p.max = 0.2, ymax = 50, callout = FALSE, darts_bin1 = 5, darts_bin2 = 13, publication_bias = FALSE) {
    df <- data.frame(p = seq(.001, p.max, length.out = 1000))
    df$density <- dpvalue(df$p, power = power)

    # Handle y-axis overflow by interpolating the exact intersection point at ymax
    if (any(df$density > ymax)) {
      idx <- which.max(df$density <= ymax)
      p_exact <- df$p[idx - 1] + (ymax - df$density[idx - 1]) * (df$p[idx] - df$p[idx - 1]) / (df$density[idx] - df$density[idx - 1])
      df <- df[df$density <= ymax, ]
      df <- rbind(data.frame(p = p_exact, density = ymax), df)
    }

    df_sig1 <- df[df$p >= 0.025 & df$p <= 0.05, ]
    df_sig2 <- df[df$p <= 0.025, ]

    p <- ggplot(df, aes(x = p, y = density)) +
        geom_area(fill = "#FC8C6C") +
        geom_area(data = df_sig1, fill = "#FAC12A") +
        geom_area(data = df_sig2, fill = "#83F41F") +
        geom_line(color = "grey20", linewidth = 0.8) +
        scale_x_continuous(limits = c(0, round(p.max, 1)), breaks = seq(0, p.max, by = 0.05), labels = function(x) sprintf("%.2f", x)) +
        scale_y_continuous() +
        labs(x = "p value", y = "Density") +
        guides(x = guide_axis(cap = "both"), y = guide_axis(cap = "both")) +
        theme_classic(base_size = 14) +
        coord_cartesian(ylim = c(0, ymax), clip = "off") +
        theme(plot.margin = margin(t = 10, r = 10, b = 40, l = 10))

    # Conditional inclusion of callout elements
    if (callout) {
        # Dynamically calculate percentages for the regions
        pct2 <- round(integrate(function(p) dpvalue(p, power = power), 0.025, 0.05)$value * 100)
        pct1 <- round(power * 100) - pct2

        label1 <- paste0(pct1, "% of all p-values should be <.025")
        label2 <- paste0(pct2, "% of all p-values should be between .025 and .05")

        # 1. Dart images: randomly generated in the green (bin1) and yellow (bin2) areas
        x_darts <- numeric(0)
        y_darts <- numeric(0)

        if (darts_bin1 > 0) {
            x1 <- seq(0.014, 0.024, length.out = darts_bin1) + runif(darts_bin1, -0.0005, 0.0005)
            y_min1 <- rep(ymax * 0.07, darts_bin1)
            y_max1 <- pmax(y_min1 + 1, dpvalue(x1, power = power) * 0.85)
            y1 <- runif(darts_bin1, min = y_min1, max = y_max1)
            x_darts <- c(x_darts, x1)
            y_darts <- c(y_darts, y1)
        }

        if (darts_bin2 > 0) {
            x2 <- seq(0.038, 0.049, length.out = darts_bin2) + runif(darts_bin2, -0.0005, 0.0005)
            y_min2 <- rep(ymax * 0.07, darts_bin2)
            y_max2 <- pmax(y_min2 + 1, dpvalue(x2, power = power) * 0.85)
            y2 <- runif(darts_bin2, min = y_min2, max = y_max2)
            x_darts <- c(x_darts, x2)
            y_darts <- c(y_darts, y2)
        }

        if (length(x_darts) > 0) {
            dart_df <- data.frame(x = x_darts, y = y_darts, image = "dart.png")
            p <- p +
                ggimage::geom_image(data = dart_df, mapping = aes(x = x, y = y, image = image), inherit.aes = FALSE, size = 0.08)
        }

        # 2. Text labels below the areas
        p <- p +
            annotate("text", x = 0.0125, y = 0, label = paste0("k=", darts_bin1), size = 5, vjust = 4) +
            annotate("text", x = 0.0375, y = 0, label = paste0("k=", darts_bin2), size = 5, vjust = 4)

        # 3. Callout bubbles and pointers
        p <- p +
            # Pointer to the green area
            annotate("segment",
                x = 0.04, y = ymax * 0.9,
                xend = 0.01, yend = dpvalue(0.01, power = power) * 0.8,
                color = "royalblue", linewidth = 2.5
            ) +
            # Label for the green area
            geom_label(
                data = data.frame(x = 0.04, y = ymax * 0.9, label = label1),
                mapping = aes(x = x, y = y, label = label),
                fill = "royalblue", color = "white",
                fontface = "bold", size = 6, hjust = 0, vjust = 0,
                label.padding = unit(0.8, "lines"), linewidth = 0,
                inherit.aes = FALSE
            ) +

            # Pointer to the yellow area
            annotate("segment",
                x = 0.06, y = ymax * 0.7,
                xend = 0.035, yend = dpvalue(0.035, power = power) * 0.8,
                color = "royalblue", linewidth = 2.5
            ) +
            # Label for the yellow area
            geom_label(
                data = data.frame(x = 0.06, y = ymax * 0.7, label = label2),
                mapping = aes(x = x, y = y, label = label),
                fill = "royalblue", color = "white",
                fontface = "bold", size = 6, hjust = 0, vjust = 0,
                label.padding = unit(0.8, "lines"), linewidth = 0,
                inherit.aes = FALSE
            )
    }

    # Conditional inclusion of publication bias annotations
    if (publication_bias) {
        p <- p +
            # Thick vertical red line at p = 0.05
            annotate("segment",
                x = 0.05, xend = 0.05, y = ymax * 0.02, yend = ymax,
                color = "#cc2200", linewidth = 2.5
            ) +

            # Curved arrow left (published)
            annotate("curve",
                x = 0.035, y = ymax * 0.05, xend = 0.015, yend = -ymax * 0.15,
                curvature = 0.2, arrow = arrow(length = unit(0.4, "cm"), type = "closed"),
                color = "grey60", linewidth = 2
            ) +

            # Text 'published'
            annotate("text",
                x = 0.015, y = -ymax * 0.22, label = "published",
                size = 7, hjust = 0.5
            ) +

            # Curved arrow right (file-drawer) - Symmetrical to the left arrow
            annotate("curve",
                x = 0.065, y = ymax * 0.05, xend = 0.085, yend = -ymax * 0.15,
                curvature = -0.2, arrow = arrow(length = unit(0.4, "cm"), type = "closed"),
                color = "grey60", linewidth = 2
            ) +

            # Text 'file-drawer' - Symmetrical to the left label
            annotate("text",
                x = 0.085, y = -ymax * 0.22, label = "file-drawer",
                size = 7, hjust = 0.5
            )
    }

    return(p)
}

print(plot_special_2())
print(plot_special_2(callout = TRUE))
print(plot_special_2(publication_bias = TRUE))

# =====================================================================
# Validation: Replication of rpsychologist.com
# =====================================================================
# This test verifies our mathematical implementation of the p-value density
# under H1 (dpvalue) against the interactive visualization by
# Kristoffer Magnusson (https://rpsychologist.com/d3/pdist/).
#
# Scenario:
# - Independent t-test, d = 0.5, n = 20 per group
# - Resulting Power = 35.26 %
#
# Verification 1 (Visual):
# The curve should cross the red p=0.05 line at exactly y ≈ 3.1.
#
# Verification 2 (Mathematical):
# The integral of the density function between 0.10 and 0.20 must
# equal the "p-values in selection" from the app: 14.46%.

p_validation <- pcurve_plot(power = 0.3526, p.max = 0.9999, ymax = 25) +
    geom_vline(xintercept = 0.05, color = "red", linetype = "dashed", linewidth = 1) +
    annotate("rect", xmin = 0.10, xmax = 0.20, ymin = 0, ymax = Inf, fill = "red", alpha = 0.2) +
    scale_x_continuous(breaks = seq(0, 1, by = 0.1), labels = function(x) sprintf("%.1f", x)) +
    scale_y_continuous(breaks = seq(0, 25, by = 0.5)) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 3)) +
    ggtitle("Validation: Power = 35.26% (rpsychologist.com)")

print(p_validation)

# Output mathematical verification (should be ~0.1446)
print(integrate(dpvalue, lower = 0.10, upper = 0.20, power = 0.3526))
