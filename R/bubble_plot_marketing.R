#' Plot Promotion Ranking Results
#'
#' @description
#' Visualizes the ranking of promotional offers using a bubble chart.
#' The X-axis represents Market Impact, the Y-axis represents Customer Risk.
#' Bubble size reflects Offer Priority – better-ranked offers are shown as larger bubbles.
#'
#' @param x An object of class `promo_rank_res` returned by the promotion ranking function.
#' @param ... Additional arguments.
#' @return A ggplot object.
#'
#' @import ggplot2
#' @import ggrepel
#' @export
plot.promo_rank_res <- function(x, ...) {

  df <- x$ranking_results

  # Labels for offers
  df$Offer_Label <- paste("Offer", df$Offer_ID)

  # Highlight best offers
  df$Offer_Group <- ifelse(df$Offer_Rank <= 3, "Best Offers", "Other Offers")

  # Bubble size logic: higher priority = larger bubble
  max_rank <- max(df$Offer_Rank)
  df$Priority_Size <- (max_rank + 1) - df$Offer_Rank

  # Mean reference lines
  impact_mean <- mean(df$Market_Impact, na.rm = TRUE)
  risk_mean   <- mean(df$Customer_Risk, na.rm = TRUE)

  p <- ggplot(df, aes(x = Market_Impact, y = Customer_Risk)) +

    # Reference lines
    geom_vline(xintercept = impact_mean, linetype = "dashed", color = "grey60") +
    geom_hline(yintercept = risk_mean,   linetype = "dashed", color = "grey60") +

    # Bubble points
    geom_point(
      aes(size = Priority_Size, fill = Offer_Group),
      shape = 21,
      color = "black",
      stroke = 0.8,
      alpha = 0.85
    ) +

    # Offer labels
    geom_text_repel(
      aes(label = Offer_Label),
      size = 3.5,
      box.padding = 0.5,
      point.padding = 0.3,
      max.overlaps = 20
    ) +

    scale_fill_manual(
      values = c(
        "Best Offers"  = "#2E7D32",
        "Other Offers" = "#E0E0E0"
      )
    ) +
    scale_size_continuous(
      range = c(4, 12),
      name = "Offer Priority"
    ) +

    labs(
      title = "Evaluation of Promotional Offers",
      subtitle = "Market Impact vs Customer Risk\nLarger bubbles indicate higher-priority offers",
      x = "Market Impact (Defuzzified Score)",
      y = "Customer Risk (Defuzzified Score)",
      fill = "Offer Category"
    ) +

    theme_minimal() +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, color = "grey30")
    )

  return(p)
}

# Fix for R CMD check global variable warnings
utils::globalVariables(
  c(
    "Market_Impact",
    "Customer_Risk",
    "Priority_Size",
    "Offer_Group",
    "Offer_Label",
    "Offer_ID",
    "Offer_Rank"
  )
)
