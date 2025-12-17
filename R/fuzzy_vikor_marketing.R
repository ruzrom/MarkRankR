#' Fuzzy VIKOR ranking for promotional offers in marketing
#'
#' @description Implements Fuzzy VIKOR method. Returns an object for plotting.
#'
#' @inheritParams fuzzy_topsis
#' @param promo_matrix The decision matrix with m alternatives and n criteria, size (m x 3n)
#' @param criteria_types Character vector indicating the type of benefit/cost ("max" or "min" values)
#' @param v Numerical value in scope (0-1), weight of the group utility strategy.
#' @param criteria_weights Numerical vector of fuzzy weights with length = 3n
#'
#' @return An object of class `promo_rank_res` with S, R, Q, and ranking.
#' @export
fuzzy_vikor_marketing <- function(promo_matrix, criteria_types, v = 0.5, criteria_weights) {

  # 1. Validate input data

  if (!is.matrix(promo_matrix))
    stop("'promo_matrix' must be a matrix.")

  n_cols <- ncol(promo_matrix)
  n_criteria <- n_cols/3

  if (n_criteria != round(n_criteria))
    stop("'promo_matrix' must have 3 columns per criterion.")

  # 2. Expand types vector to fuzzy form

  mark_criteria <- character(n_cols)
  k <- 1
  for (j in seq(1, n_cols, 3)) {
    mark_criteria[j:(j+2)] <- criteria_types[k]
    k <- k + 1
  }

  # 3. Finding ideal solutions (positives and negatives)

  ideal_pos <- ifelse(mark_criteria == "max", apply(promo_matrix, 2, max), apply(promo_matrix, 2, min))
  ideal_neg <- ifelse(mark_criteria == "min", apply(promo_matrix, 2, max), apply(promo_matrix, 2, min))

  # 4. Linear normalization (VIKOR distance)

  distance_matrix <- matrix(0, nrow = nrow(promo_matrix), ncol = n_cols)

  for (i in seq(1, n_cols, 3)) {
    if (mark_criteria[i] == "max") {
      denom <- ideal_pos[i+2] - ideal_neg[i]
      if(denom == 0) denom <- 1e-9
      distance_matrix[, i]   <- (ideal_pos[i]   - promo_matrix[, i+2]) / denom
      distance_matrix[, i+1] <- (ideal_pos[i+1] - promo_matrix[, i+1]) / denom
      distance_matrix[, i+2] <- (ideal_pos[i+2] - promo_matrix[, i])   / denom
    } else {
      denom <- ideal_neg[i+2] - ideal_pos[i]
      if(denom == 0) denom <- 1e-9
      distance_matrix[, i]   <- (promo_matrix[, i]   - ideal_pos[i+2]) / denom
      distance_matrix[, i+1] <- (promo_matrix[, i+1] - ideal_pos[i+1]) / denom
      distance_matrix[, i+2] <- (promo_matrix[, i+2] - ideal_pos[i])   / denom
    }
  }

  # 5. Apply equal weights

  W_diag <- diag(criteria_weights)
  weighted_distance <- distance_matrix %*% W_diag

  # 6. Compute S and R values

  S_fuzzy <- matrix(0, nrow(promo_matrix), 3)
  R_fuzzy <- matrix(0, nrow(promo_matrix), 3)

  S_fuzzy[,1] <- apply(weighted_distance[, seq(1, n_cols, 3), drop=FALSE], 1, sum)
  S_fuzzy[,2] <- apply(weighted_distance[, seq(2, n_cols, 3), drop=FALSE], 1, sum)
  S_fuzzy[,3] <- apply(weighted_distance[, seq(3, n_cols, 3), drop=FALSE], 1, sum)

  R_fuzzy[,1] <- apply(weighted_distance[, seq(1, n_cols, 3), drop=FALSE], 1, max)
  R_fuzzy[,2] <- apply(weighted_distance[, seq(2, n_cols, 3), drop=FALSE], 1, max)
  R_fuzzy[,3] <- apply(weighted_distance[, seq(3, n_cols, 3), drop=FALSE], 1, max)


  # 7. Fuzzy Q index
  # Q_i = v * (S_i - S*) / (S- - S*) + (1-v) * (R_i - R*) / (R- - R*)

  s_star <- min(S_fuzzy[,1])
  s_minus <- max(S_fuzzy[,3])
  r_star <- min(R_fuzzy[,1])
  r_minus <- max(R_fuzzy[,3])

  denom_s <- ifelse(s_minus - s_star == 0, 1, s_minus - s_star)
  denom_r <- ifelse(r_minus - r_star == 0, 1, r_minus - r_star)

  Q_fuzzy <- v * (S_fuzzy - s_star) / denom_s +
    (1 - v) * (R_fuzzy - r_star) / denom_r

  # 8. Defuzzification

  def_S <- (S_fuzzy[,1] + 2*S_fuzzy[,2] + S_fuzzy[,3]) / 4
  def_R <- (R_fuzzy[,1] + 2*R_fuzzy[,2] + R_fuzzy[,3]) / 4
  def_Q <- (Q_fuzzy[,1] + 2*Q_fuzzy[,2] + Q_fuzzy[,3]) / 4

  res_dataframe <- data.frame(
    Offer_ID = 1:nrow(promo_matrix),
    Market_Impact = def_S,
    Customer_Risk = def_R,
    Fuzzy_Q = def_Q,
    Offer_Rank = rank(def_Q, ties.method = "first")
  )

  output <- list(
    ranking_results = res_dataframe,
    details = list(S_fuzzy = S_fuzzy, R_fuzzy = R_fuzzy, Q_fuzzy = Q_fuzzy),
    params = list(v = v)
  )

  class(output) <- "promo_rank_res"
  return(output)
}
