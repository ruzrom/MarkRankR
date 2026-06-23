#' Rozmyta Metoda VIKOR
#'
#' @description Metoda kompromisowa VIKOR. Oblicza wskaźniki S (użyteczność grupy),
#' R (indywidualny żal) oraz Q (indeks kompromisu).
#'
#' @param macierz_decyzyjna Macierz o wymiarach \eqn{m \times 3n}.
#' @param typy_kryteriow Wektor znakowy ("max" dla zysku, "min" dla kosztu).
#' @param wagi (Opcjonalnie) Wektor wag.
#' @param bwm_kryteria (Opcjonalnie) Nazwy kryteriów dla BWM.
#' @param bwm_najlepsze (Opcjonalnie) Wektor Best-to-Others.
#' @param bwm_najgorsze (Opcjonalnie) Wektor Others-to-Worst.
#' @param v Waga strategii "większości kryteriów" (domyślnie 0.5).
#' @return Obiekt klasy `rozmyty_vikor_promo_wynik`.
#' @export
rozmyty_vikor_promo <- function(macierz_decyzyjna, typy_kryteriow, v = 0.5, wagi = NULL,
                                bwm_kryteria, bwm_najlepsze, bwm_najgorsze) {

  if (!is.matrix(macierz_decyzyjna)) stop("'macierz_decyzyjna' musi być macierzą.")

  finalne_wagi <- .pobierz_finalne_wagi(macierz_decyzyjna, wagi, bwm_kryteria, bwm_najlepsze, bwm_najgorsze)

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

  # Rozszerzenie typów
  typy_rozmyte <- character(n_kolumn)
  k <- 1

  for (j in seq(1, n_kolumn, 3)) {
    typy_rozmyte[j:(j+2)] <- typy_kryteriow[k]
    k <- k + 1
  }

  # 1. Rozwiązania Idealne
  idea_poz <- ifelse(typy_rozmyte == "max", apply(macierz_decyzyjna, 2, max), apply(macierz_decyzyjna, 2, min))
  idea_neg <- ifelse(typy_rozmyte == "min", apply(macierz_decyzyjna, 2, max), apply(macierz_decyzyjna, 2, min))

  # 2. Normalizacja liniowa (specyficzna dla VIKOR) i ważenie
  macierz_d <- matrix(0, nrow = nrow(macierz_decyzyjna), ncol = n_kolumn)

  for (i in seq(1, n_kolumn, 3)) {
    if (typy_rozmyte[i] == "max") {
      mianownik <- idea_poz[i+2] - idea_neg[i]

      if(mianownik == 0) mianownik <- 1e-9

      # Wzór dla Benefit: (f* - f_ij) / (f* - f-)
      macierz_d[, i] <- (idea_poz[i] - macierz_decyzyjna[, i+2]) / mianownik
      macierz_d[, i+1] <- (idea_poz[i+1] - macierz_decyzyjna[, i+1]) / mianownik
      macierz_d[, i+2] <- (idea_poz[i+2] - macierz_decyzyjna[, i]) / mianownik
    } else {
      mianownik <- idea_neg[i+2] - idea_poz[i]
      if(mianownik == 0) mianownik <- 1e-9

      # Wzór dla Cost: (f_ij - f*) / (f- - f*)
      macierz_d[, i] <- (macierz_decyzyjna[, i] - idea_poz[i+2]) / mianownik
      macierz_d[, i+1] <- (macierz_decyzyjna[, i+1] - idea_poz[i+1]) / mianownik
      macierz_d[, i+2] <- (macierz_decyzyjna[, i+2] - idea_poz[i]) / mianownik
    }
  }

  # Mnożenie przez wagi
  macierz_wazona_d <- macierz_d * rep(finalne_wagi, each = nrow(macierz_d))

  # 3. Wartości S (suma) i R (max)
  S_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)
  R_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)

  S_rozmyte[,1] <- apply(macierz_wazona_d[, seq(1, n_kolumn, 3), drop=FALSE], 1, sum)
  S_rozmyte[,2] <- apply(macierz_wazona_d[, seq(2, n_kolumn, 3), drop=FALSE], 1, sum)
  S_rozmyte[,3] <- apply(macierz_wazona_d[, seq(3, n_kolumn, 3), drop=FALSE], 1, sum)

  R_rozmyte[,1] <- apply(macierz_wazona_d[, seq(1, n_kolumn, 3), drop=FALSE], 1, max)
  R_rozmyte[,2] <- apply(macierz_wazona_d[, seq(2, n_kolumn, 3), drop=FALSE], 1, max)
  R_rozmyte[,3] <- apply(macierz_wazona_d[, seq(3, n_kolumn, 3), drop=FALSE], 1, max)

  # 4. Indeks Q
  s_star <- min(S_rozmyte[,1])
  s_minus <- max(S_rozmyte[,3])

  r_star <- min(R_rozmyte[,1])
  r_minus <- max(R_rozmyte[,3])

  mianownik_s <- s_minus - s_star
  mianownik_r <- r_minus - r_star

  if (mianownik_s == 0) mianownik_s <- 1
  if (mianownik_r == 0) mianownik_r <- 1

  Q_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)
  czlon1 <- (S_rozmyte - s_star) / mianownik_s
  czlon2 <- (R_rozmyte - r_star) / mianownik_r
  Q_rozmyte <- v * czlon1 + (1 - v) * czlon2

  # Defuzzyfikacja
  def_S <- (S_rozmyte[,1] + 2*S_rozmyte[,2] + S_rozmyte[,3]) / 4
  def_R <- (R_rozmyte[,1] + 2*R_rozmyte[,2] + R_rozmyte[,3]) / 4
  def_Q <- (Q_rozmyte[,1] + 2*Q_rozmyte[,2] + Q_rozmyte[,3]) / 4

  ramka_wynikow <- data.frame(
    Alternatywa = rownames(macierz_decyzyjna),
    Def_S = def_S,
    Def_R = def_R,
    Def_Q = def_Q,
    Ranking = rank(def_Q, ties.method = "first")
  )

  wynik <- list(
    wyniki = ramka_wynikow,
    detale = list(
      S_rozmyte = S_rozmyte,
      R_rozmyte = R_rozmyte,
      Q_rozmyte = Q_rozmyte),
    parametry = list(v = v),
    metoda = "VIKOR"
  )

  class(wynik) <- "rozmyty_vikor_promo_wynik"
  return(wynik)
}
