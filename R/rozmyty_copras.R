#' @title Wewnętrzna defuzyfikacja TFN dla COPRAS
#' @description Zamienia macierz złożoną z trójek (l, m, u) na macierz ostrą
#' przez średnią arytmetyczną \eqn{(l + m + u) / 3}, zgodnie z procedurą
#' opisaną w dołączonej literaturze Fuzzy COPRAS.
#' @keywords internal
.copras_defuzyfikuj_macierz <- function(macierz_decyzyjna) {
  n_kolumn <- ncol(macierz_decyzyjna)

  if (n_kolumn %% 3 != 0) {
    stop("Liczba kolumn w 'macierz_decyzyjna' musi być wielokrotnością 3.")
  }

  n_kryt <- n_kolumn / 3
  macierz_ostra <- matrix(0, nrow = nrow(macierz_decyzyjna), ncol = n_kryt)

  for (j in seq_len(n_kryt)) {
    idx <- ((j - 1) * 3 + 1):(j * 3)
    macierz_ostra[, j] <- rowMeans(macierz_decyzyjna[, idx, drop = FALSE])
  }

  rownames(macierz_ostra) <- rownames(macierz_decyzyjna)

  nazwy_kryteriow <- attr(macierz_decyzyjna, "nazwy_kryteriow")
  if (!is.null(nazwy_kryteriow) && length(nazwy_kryteriow) == n_kryt) {
    colnames(macierz_ostra) <- nazwy_kryteriow
  }

  macierz_ostra
}


#' @title Wewnętrzne przygotowanie wag dla COPRAS
#' @description Defuzyfikuje wagi do postaci ostrej i normalizuje je do sumy 1.
#' @keywords internal
.copras_przygotuj_wagi <- function(macierz_decyzyjna, wagi, bwm_kryteria, bwm_najlepsze, bwm_najgorsze) {
  finalne_wagi <- .pobierz_finalne_wagi(macierz_decyzyjna, wagi, bwm_kryteria, bwm_najlepsze, bwm_najgorsze)
  n_kryt <- ncol(macierz_decyzyjna) / 3

  if (length(finalne_wagi) == ncol(macierz_decyzyjna)) {
    wagi_ostre <- vapply(seq_len(n_kryt), function(j) {
      idx <- ((j - 1) * 3 + 1):(j * 3)
      mean(finalne_wagi[idx])
    }, numeric(1))
  } else if (length(finalne_wagi) == n_kryt) {
    wagi_ostre <- finalne_wagi
  } else {
    stop("Nie udało się dopasować długości wag do liczby kryteriów.")
  }

  if (any(wagi_ostre < 0)) {
    stop("Wagi kryteriów nie mogą być ujemne.")
  }

  suma_wag <- sum(wagi_ostre)
  if (suma_wag <= 0) {
    stop("Suma wag kryteriów musi być dodatnia.")
  }

  wagi_ostre / suma_wag
}


#' Rozmyta Metoda COPRAS (Complex Proportional Assessment)
#'
#' @description Funkcja realizuje algorytm Fuzzy COPRAS do oceny wielokryterialnej.
#' Zgodnie z procedurą opisaną w dołączonej literaturze, macierz rozmyta jest
#' najpierw defuzyfikowana do postaci ostrej, a następnie poddawana klasycznym
#' krokom COPRAS: normalizacji przez sumy kolumn, ważeniu oraz wyznaczeniu
#' wskaźników \eqn{P_i}, \eqn{R_i}, \eqn{Q_i} i \eqn{U_i}.
#'
#' @param macierz_decyzyjna Rozmyta macierz decyzyjna (wynik funkcji przygotuj_dane_mcda).
#' @param typy_kryteriow Wektor tekstowy określający charakter kryteriów ("min" lub "max").
#' @param wagi Opcjonalny wektor wag. Jeśli brak, zostaną pobrane z atrybutów lub BWM.
#' @param bwm_kryteria (Opcjonalnie) Nazwy kryteriów dla BWM.
#' @param bwm_najlepsze (Opcjonalnie) Wektor Best-to-Others.
#' @param bwm_najgorsze (Opcjonalnie) Wektor Others-to-Worst.
#' @return Obiekt klasy `rozmyty_copras_promo_wynik`.
#' @export
rozmyty_copras_promo <- function(macierz_decyzyjna, typy_kryteriow, wagi = NULL,
                                 bwm_kryteria, bwm_najlepsze, bwm_najgorsze) {

  if (!is.matrix(macierz_decyzyjna)) {
    stop("'macierz_decyzyjna' musi być macierzą.")
  }

  n_kolumn <- ncol(macierz_decyzyjna)
  if (n_kolumn %% 3 != 0) {
    stop("Liczba kolumn w 'macierz_decyzyjna' musi być wielokrotnością 3.")
  }

  n_alt <- nrow(macierz_decyzyjna)
  n_kryt <- n_kolumn / 3

  if (length(typy_kryteriow) != n_kryt) {
    stop("Długość 'typy_kryteriow' musi odpowiadać liczbie kryteriów.")
  }

  if (any(!typy_kryteriow %in% c("min", "max"))) {
    stop("Elementy 'typy_kryteriow' muszą mieć wartość 'min' albo 'max'.")
  }

  macierz_ostra <- .copras_defuzyfikuj_macierz(macierz_decyzyjna)
  wagi_ostre <- .copras_przygotuj_wagi(macierz_decyzyjna, wagi, bwm_kryteria, bwm_najlepsze, bwm_najgorsze)

  sumy_kolumn <- colSums(macierz_ostra)
  if (any(sumy_kolumn <= 0)) {
    stop("Po defuzyfikacji każda kolumna musi mieć dodatnią sumę, aby wykonać normalizację COPRAS.")
  }

  macierz_znormalizowana <- sweep(macierz_ostra, 2, sumy_kolumn, "/")
  macierz_wazona <- sweep(macierz_znormalizowana, 2, wagi_ostre, "*")

  maska_korzysci <- typy_kryteriow == "max"
  maska_kosztow <- typy_kryteriow == "min"

  P_i <- if (any(maska_korzysci)) {
    rowSums(macierz_wazona[, maska_korzysci, drop = FALSE])
  } else {
    rep(0, n_alt)
  }

  R_i <- if (any(maska_kosztow)) {
    rowSums(macierz_wazona[, maska_kosztow, drop = FALSE])
  } else {
    rep(0, n_alt)
  }

  eps <- 1e-9
  if (!any(maska_kosztow) || all(R_i <= eps)) {
    Q_i <- P_i
  } else {
    R_i_bezpieczne <- pmax(R_i, eps)
    stala_proporcjonalna <- sum(R_i_bezpieczne) / sum(1 / R_i_bezpieczne)
    Q_i <- P_i + stala_proporcjonalna / R_i_bezpieczne
  }

  Q_max <- max(Q_i)
  U_i <- if (Q_max <= 0) rep(0, n_alt) else (Q_i / Q_max) * 100

  S_plus <- cbind(P_i, P_i, P_i)
  S_minus <- cbind(R_i, R_i, R_i)
  Q_rozmyte <- cbind(Q_i, Q_i, Q_i)

  ramka_wynikow <- data.frame(
    Alternatywa = rownames(macierz_decyzyjna),
    Piorytety_Qi = round(Q_i, 4),
    Uzytecznosc_Ui = round(U_i, 2),
    Ranking = rank(-Q_i, ties.method = "first")
  )

  wynik <- list(
    wyniki = ramka_wynikow,
    detale = list(
      macierz_ostra = macierz_ostra,
      macierz_znormalizowana = macierz_znormalizowana,
      macierz_wazona = macierz_wazona,
      S_plus = S_plus,
      S_minus = S_minus,
      Q_rozmyte = Q_rozmyte,
      P_i = P_i,
      R_i = R_i,
      Q_i = Q_i,
      wagi_ostre = wagi_ostre
    ),
    metoda = "COPRAS"
  )

  class(wynik) <- "rozmyty_copras_promo_wynik"
  wynik
}
