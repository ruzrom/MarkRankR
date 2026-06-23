#' Rozmyta Metoda TOPSIS
#'
#' @description Rozmyta metoda TOPSIS służy do wyznaczania rankingu alternatyw na podstawie
#' ich odległości od rozwiązania idealnego oraz anty-idealnego.
#'
#' @param macierz_decyzyjna Rozmyta macierz decyzyjna (wynik funkcji przygotuj_dane_mcda).
#' @param typy_kryteriow Wektor tekstowy określający charakter kryteriów ("min" lub "max").
#' @param wagi Opcjonalny wektor wag. Jeśli brak, zostaną pobrane z atrybutów lub BWM.
#' @param bwm_kryteria (Opcjonalnie) Nazwy kryteriów dla BWM.
#' @param bwm_najlepsze (Opcjonalnie) Wektor Best-to-Others.
#' @param bwm_najgorsze (Opcjonalnie) Wektor Others-to-Worst.
#' @return Obiekt klasy `rozmyty_topsis_promo_wynik`.
#' @export
rozmyty_topsis_promo <- function(macierz_decyzyjna, typy_kryteriow, wagi = NULL,
                                 bwm_kryteria, bwm_najlepsze, bwm_najgorsze) {

  if (!is.matrix(macierz_decyzyjna)) stop("'macierz_decyzyjna' musi być macierzą.")

  # 1. Przygotowanie wag
  finalne_wagi <- .pobierz_finalne_wagi(macierz_decyzyjna, wagi, bwm_kryteria, bwm_najlepsze, bwm_najgorsze)

  # 2. Rozszerzenie typów kryteriów
  n_kolumn <- ncol(macierz_decyzyjna)
  if (n_kolumn %% 3 != 0) {
    stop("Liczba kolumn w 'macierz_decyzyjna' musi być wielokrotnością 3.")
  }

  n_kryt <- n_kolumn / 3
  if (length(typy_kryteriow) != n_kryt) {
    stop("Długość 'typy_kryteriow' musi odpowiadać liczbie kryteriów.")
  }

  if (any(!typy_kryteriow %in% c("min", "max"))) {
    stop("Elementy 'typy_kryteriow' muszą mieć wartość 'min' albo 'max'.")
  }

  typy_rozmyte <- character(n_kolumn)
  k <- 1
  for (j in seq(1, n_kolumn, 3)) {
    typy_rozmyte[j:(j+2)] <- typy_kryteriow[k]
    k <- k + 1
  }

  # 3. Normalizacja
  macierz_d <- matrix(nrow = nrow(macierz_decyzyjna), ncol = n_kolumn)
  denoms <- sqrt(apply(macierz_decyzyjna^2, 2, sum))

  for (i in seq(1, n_kolumn, 3)) {
    macierz_d[, i]   <- macierz_decyzyjna[, i]   / denoms[i + 2]
    macierz_d[, i+1] <- macierz_decyzyjna[, i+1] / denoms[i + 1]
    macierz_d[, i+2] <- macierz_decyzyjna[, i+2] / denoms[i]
  }

  # 4. Uwzględnienie wag kryteriów
  macierz_wazona_d <- macierz_d * rep(finalne_wagi, each = nrow(macierz_d))

  # 5. Rozwiązania Idealne
  idea_poz <- ifelse(typy_rozmyte == "max", apply(macierz_wazona_d, 2, max), apply(macierz_wazona_d, 2, min))
  idea_neg <- ifelse(typy_rozmyte == "min", apply(macierz_wazona_d, 2, max), apply(macierz_wazona_d, 2, min))

  # 6. Obliczenie odległości euklidesowych w przestrzeni rozmytej
  roznice_poz <- (macierz_wazona_d - matrix(idea_poz, nrow=nrow(macierz_decyzyjna), ncol=n_kolumn, byrow=TRUE))^2
  roznice_neg <- (macierz_wazona_d - matrix(idea_neg, nrow=nrow(macierz_decyzyjna), ncol=n_kolumn, byrow=TRUE))^2

  D_poz_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)
  D_neg_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)

  D_poz_rozmyte[,1] <- sqrt(apply(roznice_poz[, seq(1, n_kolumn, 3), drop=FALSE], 1, sum))
  D_poz_rozmyte[,2] <- sqrt(apply(roznice_poz[, seq(2, n_kolumn, 3), drop=FALSE], 1, sum))
  D_poz_rozmyte[,3] <- sqrt(apply(roznice_poz[, seq(3, n_kolumn, 3), drop=FALSE], 1, sum))

  D_neg_rozmyte[,1] <- sqrt(apply(roznice_neg[, seq(1, n_kolumn, 3), drop=FALSE], 1, sum))
  D_neg_rozmyte[,2] <- sqrt(apply(roznice_neg[, seq(2, n_kolumn, 3), drop=FALSE], 1, sum))
  D_neg_rozmyte[,3] <- sqrt(apply(roznice_neg[, seq(3, n_kolumn, 3), drop=FALSE], 1, sum))

  # 7. Współczynnik bliskości / Closeness Coefficient (R)
  denom <- D_neg_rozmyte + D_poz_rozmyte
  R_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)
  R_rozmyte[,1] <- D_neg_rozmyte[,1] / denom[,3]
  R_rozmyte[,2] <- D_neg_rozmyte[,2] / denom[,2]
  R_rozmyte[,3] <- D_neg_rozmyte[,3] / denom[,1]

  def_C <- (R_rozmyte[,1] + 4*R_rozmyte[,2] + R_rozmyte[,3]) / 6

  # 8. Defuzyfikacja i przygotowanie danych wynikowych
  def_D_poz <- rowMeans(D_poz_rozmyte)
  def_D_neg <- rowMeans(D_neg_rozmyte)

  ramka_wynikow <- data.frame(
    Alternatywa = rownames(macierz_decyzyjna),
    D_plus = def_D_poz,
    D_minus = def_D_neg,
    Score = def_C,
    Ranking = rank(-def_C, ties.method = "first")
  )

  wynik <- list(
    wyniki = ramka_wynikow,
    detale = list(
      D_poz_rozmyte = D_poz_rozmyte,
      D_neg_rozmyte = D_neg_rozmyte,
      R_rozmyte = R_rozmyte
    ),
    metoda = "TOPSIS"
  )
  class(wynik) <- "rozmyty_topsis_promo_wynik"
  return(wynik)
}
