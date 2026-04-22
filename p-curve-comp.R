# ============================================================
# Density of p-values given statistical power
# ============================================================
# Theory:
#   For a z-test with non-centrality delta, the p-value
#   distribution under H1 depends on delta, which is derived
#   from the desired power and significance level.
#
#   One-sided:  p = 1 - Phi(Z),  Z ~ N(delta, 1)
#   Two-sided:  p = 2*(1 - Phi(|Z|)), |Z| ~ folded-N(delta, 1)
# ============================================================

#' Density of p-values under a given power
#'
#' @param p        Numeric vector of p-values in (0, 1).
#' @param power    Desired statistical power (1 - beta), in (0, 1).
#' @param sig.level Significance level alpha (default 0.05).
#' @param alternative "two.sided" (default) or "one.sided".
#' @param log      Logical; if TRUE, returns log-density.
#'
#' @return Numeric vector of density values (or log-densities).
#'
#' @examples
#' dpvalue(0.05, power = 0.80)
#' dpvalue(seq(0.001, 0.999, length.out = 500), power = 0.80)
dpvalue <- function(p,
                    power,
                    sig.level = 0.05,
                    alternative = c("two.sided", "one.sided"),
                    log = FALSE) {
  alternative <- match.arg(alternative)

  # --- Input validation ---
  stopifnot(
    is.numeric(p), all(p > 0 & p < 1),
    is.numeric(power), length(power) == 1, power > 0 & power < 1,
    is.numeric(sig.level), length(sig.level) == 1, sig.level > 0 & sig.level < 1
  )

  # --- Recover non-centrality parameter delta from power ---
  delta <- .solve_delta(power, sig.level, alternative)

  # --- Compute density ---
  if (alternative == "one.sided") {
    # p = 1 - Phi(Z)  =>  Z = qnorm(1 - p)
    # f(p) = phi(Z - delta) / phi(Z)
    z <- qnorm(1 - p)
    ld <- dnorm(z - delta, log = TRUE) - dnorm(z, log = TRUE)
  } else {
    # p = 2*(1 - Phi(|Z|))  =>  |Z| = qnorm(1 - p/2)
    # f(p) = [phi(q - delta) + phi(q + delta)] / (2 * phi(q))
    q <- qnorm(1 - p / 2)
    ld <- log(dnorm(q - delta) + dnorm(q + delta)) -
      log(2) - dnorm(q, log = TRUE)
  }

  if (log) ld else exp(ld)
}


#' Solve for non-centrality parameter (delta) given statistical power
#'
#' This internal helper function calculates the non-centrality parameter
#' required to achieve a specific statistical power for a given significance level.
#'
#' @param power Numeric. Desired statistical power (1 - beta), in (0, 1).
#' @param sig.level Numeric. Significance level alpha.
#' @param alternative Character. "two.sided" or "one.sided".
#'
#' @return Numeric. The calculated non-centrality parameter (delta).
#' @keywords internal
.solve_delta <- function(power, sig.level, alternative) {
  z_a <- qnorm(1 - sig.level)
  z_a2 <- qnorm(1 - sig.level / 2)

  if (alternative == "one.sided") {
    z_a + qnorm(power) # closed form
  } else {
    # Power = P(Z > z_{a/2} - d) + P(Z < -z_{a/2} - d)
    # is symmetric in d and → 1 at BOTH ±Inf.
    # The unique root for power > alpha lives on (0, +Inf).
    power_fn <- function(d) {
      pnorm(z_a2 - d, lower.tail = FALSE) +
        pnorm(-z_a2 - d) - power
    }

    # At d=0: power_fn = alpha - power < 0  (since power > alpha)
    # At d=upper: power_fn → 1 - power > 0
    upper <- z_a2 + qnorm(power) + 10 # generous upper bound

    uniroot(power_fn, interval = c(0, upper), tol = 1e-10)$root
  }
}


#' Plot p-value density curves for multiple power levels
#'
#' This helper function generates a ggplot visualizing how the expected p-value
#' distribution changes under the alternative hypothesis (H1) for different
#' levels of statistical power.
#'
#' @param powers Numeric vector. A list of statistical power levels to plot. Default is c(0.5, 0.8, 0.9, 0.95).
#' @param sig.level Numeric. The significance level alpha used for the tests. Default is 0.05.
#' @param alternative Character. "two.sided" (default) or "one.sided".
#' @param n Integer. The number of points to calculate along the x-axis for smooth curves. Default is 500.
#'
#' @return A ggplot object with the overlaid density curves.
#' @export
#'
#' @examples
#' plot_dpvalue(powers = c(.10, .35, .80))
plot_dpvalue <- function(powers = c(0.5, 0.8, 0.9, 0.95),
                         sig.level = 0.05,
                         alternative = "two.sided",
                         n = 500) {
  library(ggplot2)

  p <- seq(0.001, 0.999, length.out = n)

  df_list <- lapply(powers, function(pw) {
    data.frame(
      p = p,
      density = dpvalue(p, power = pw, sig.level = sig.level, alternative = alternative),
      power = factor(pw, levels = powers, labels = paste0("power = ", powers))
    )
  })
  df <- do.call(rbind, df_list)

  pal <- hcl.colors(length(powers), palette = "Zissou 1")

  p_plot <- ggplot2::ggplot(df, ggplot2::aes(x = p, y = density, color = power)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "grey60", linewidth = 0.8) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::scale_color_manual(values = pal, name = NULL) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), labels = function(x) sprintf("%.1f", x)) +
    ggplot2::scale_y_continuous(limits = c(0, 10)) +
    ggplot2::labs(
      x = "p-value",
      y = "Density",
      title = sprintf("P-value density under H\u2081\n(%s test, \u03b1 = %.2f)", alternative, sig.level)
    ) +
    ggplot2::theme_classic(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      legend.position = c(0.8, 0.8), # topright equivalent
      legend.background = ggplot2::element_blank()
    )

  print(p_plot)
  invisible(p_plot)
}


# ============================================================
# Quick demo
# ============================================================
# Single value
# dpvalue(0.05, power = 0.80)

# # Verify: integrates to 1
# integrate(dpvalue, lower = 1e-6, upper = 1 - 1e-6,
#           power = 0.80, sig.level = 0.05)

# # Verify: P(p < alpha | power) == power
# integrate(dpvalue, lower = 1e-6, upper = 0.05,
#           power = 0.80, sig.level = 0.05)

# # Plot
# plot_dpvalue(powers = c(.10, .35, .80))
