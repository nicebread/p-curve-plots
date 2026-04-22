library(ggplot2)
library(pwr)
source("p-curve-comp.R")

#' Plot the expected p-value distribution under the null hypothesis (H0)
#'
#' This function generates a ggplot showing a uniform density distribution
#' of p-values, which is expected when there is no true effect (H0). It highlights
#' the standard significance region (alpha = 5%) in red.
#'
#' @return A ggplot object representing the H0 distribution.
#' @export
#'
#' @examples
#' p_h0 <- plot_h0()
#' print(p_h0)
plot_h0 <- function() {
    ggplot() +
        # Background for the whole 0 to 1 region
        annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "#FCD9D9") +
        # Significance region (alpha = 5%)
        annotate("rect", xmin = 0, xmax = 0.05, ymin = 0, ymax = 1, fill = "firebrick") +
        # Dotted borders like in the original Base R plot
        geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1), linetype = "dotted") +
        geom_segment(aes(x = 1, xend = 1, y = 0, yend = 1), linetype = "dotted") +
        geom_segment(aes(x = 0, xend = 1, y = 0, yend = 0), linetype = "dotted") +
        # H0 density line (y = 1)
        geom_segment(aes(x = 0, xend = 1, y = 1, yend = 1), color = "red", linewidth = 1.2) +
        # Text annotation for 5%
        annotate("text", x = 0.025, y = 0.5, label = "5%", color = "white", fontface = "bold", angle = 90, size = 6) +
        scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), labels = function(x) sprintf("%.1f", x)) +
        scale_y_continuous(limits = c(0, 5)) +
        labs(x = "p value", y = "Density") +
        guides(x = guide_axis(cap = "both"), y = guide_axis(cap = "both")) +
        theme_classic(base_size = 14)
}

print(plot_h0())


#' Plot the expected p-value distribution under the alternative hypothesis (H1)
#'
#' This function visualizes the right-skewed p-value distribution (p-curve)
#' expected when a true effect exists, given a specific statistical power.
#'
#' @param power Numeric. The true statistical power (1 - beta). Default is 0.10.
#' @param p.max Numeric. The maximum p-value to plot on the x-axis. Default is 0.9999.
#' @param ymax Numeric. The maximum y-axis value (density). Default is 5.
#' @param sig.region Logical. If TRUE, visually highlights the significant region (p <= 0.05).
#'
#' @return A ggplot object representing the p-curve.
#' @export
#'
#' @examples
#' pcurve_plot(.80, ymax = 30, sig.region = TRUE)
pcurve_plot <- function(power = .10, p.max = .9999, ymax = 5, sig.region = FALSE) {
    df <- data.frame(p = seq(.001, p.max, length.out = 1000))
    df$density <- dpvalue(df$p, power = power)

    p_base <- ggplot(df, aes(x = p, y = density)) +
        geom_area(fill = "#DFFABE", color = "black", linetype = "dotted", linewidth = 0.5, outline.type = "full") +
        geom_line(color = "green", linewidth = 1.2) +
        annotate("segment", x = 0.01, xend = p.max, y = 1, yend = 1, color = "red", linetype = "dashed", linewidth = 0.8) +
        scale_x_continuous(limits = c(0, round(p.max, 1)), breaks = seq(0, 1, by = 0.2), labels = function(x) sprintf("%.1f", x)) +
        scale_y_continuous(limits = c(0, ymax)) +
        labs(x = "p value", y = "Density") +
        guides(x = guide_axis(cap = "both"), y = guide_axis(cap = "both")) +
        theme_classic(base_size = 14)

    if (sig.region) {
        df_sig <- df[df$p <= 0.05, ]
        p_base <- p_base +
            geom_area(data = df_sig, fill = "#93F759", color = "black", linetype = "dotted", linewidth = 0.5, outline.type = "full") +
            # Annotation text for power.
            annotate("text",
                x = 0.025, y = 0,
                label = paste0(round(power * 100), "%"),
                color = "black", fontface = "bold", angle = 90, hjust = -0.1, size = 6
            )
    }

    return(p_base)
}

print(pcurve_plot(.10, ymax = 8))
print(pcurve_plot(.10, ymax = 8, sig.region = TRUE))
print(pcurve_plot(.35, ymax = 12, sig.region = TRUE))
print(pcurve_plot(.80, ymax = 30, sig.region = TRUE))
print(pcurve_plot(.95, ymax = 30, sig.region = TRUE))


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

    df_sig <- df[df$p >= 0.03 & df$p <= 0.05, ]

    p <- ggplot(df, aes(x = p, y = density)) +
        geom_area(fill = "#DFFABE") +
        geom_area(data = df_sig, fill = "#93F759", color = "black", linetype = "dotted", linewidth = 0.5, outline.type = "full") +
        geom_line(color = "green", linewidth = 1.2) +
        scale_x_continuous(limits = c(0, round(p.max, 1)), breaks = seq(0, 1, by = 0.2), labels = function(x) sprintf("%.1f", x)) +
        scale_y_continuous(limits = c(0, ymax)) +
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
#' 0.025 to 0.05 with distinct colors.
#'
#' @param power Numeric. The true statistical power. Default is 0.60.
#' @param p.max Numeric. The maximum p-value to plot. Default is 0.2.
#' @param ymax Numeric. The maximum density on the y-axis. Default is 50.
#'
#' @return A ggplot object showing binned significance regions.
#' @export
#'
#' @examples
#' plot_special_2()
plot_special_2 <- function(power = .60, p.max = 0.2, ymax = 50) {
    df <- data.frame(p = seq(.001, p.max, length.out = 1000))
    df$density <- dpvalue(df$p, power = power)

    df_sig1 <- df[df$p >= 0.025 & df$p <= 0.05, ]
    df_sig2 <- df[df$p <= 0.025, ]

    p <- ggplot(df, aes(x = p, y = density)) +
        geom_area(fill = "#FC8C6C") +
        geom_area(data = df_sig1, fill = "#FAC12A") +
        geom_area(data = df_sig2, fill = "#83F41F") +
        geom_line(color = "grey20", linewidth = 0.8) +
        scale_x_continuous(limits = c(0, round(p.max, 1)), breaks = seq(0, p.max, by = 0.05), labels = function(x) sprintf("%.2f", x)) +
        scale_y_continuous(limits = c(0, ymax)) +
        labs(x = "p value", y = "Density") +
        guides(x = guide_axis(cap = "both"), y = guide_axis(cap = "both")) +
        theme_classic(base_size = 14)
    return(p)
}

print(plot_special_2())


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
