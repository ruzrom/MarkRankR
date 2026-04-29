# ------ Funkcje pomocnicze do wag ------

#' Obliczanie wag metodą Entropii Shannona
#'
#' @description Wyznacza obiektywne wagi kryteriów na podstawie danych,
#' mierząc stopień rozproszenia wartości. Im większa zmienność, tym wyższa waga.
#'
#' @param macierz_decyzyjna Rozmyta macierz (wynik funkcji `przygotuj_dane_mcda`).
#' @return Wektor numeryczny wag sumujący się do 1.
#' @export
oblicz_wagi_entropii <- function(macierz_decyzyjna) {

  # Od-rozmycie macierzy do obliczen entropii (srednia z l, m, u)
  n_kolumn <- ncol(macierz_decyzyjna)
  macierz_ostra <- matrix(0, nrow = nrow(macierz_decyzyjna), ncol = n_kolumn/3)

  k <- 1
  for(j in seq(1, n_kolumn, 3)) {

    # Proste odrozmycie: (l + 4m + u) / 6 lub zwykła średnia arytmetyczna
    macierz_ostra[, k] <- (macierz_decyzyjna[, j] + 4*macierz_decyzyjna[, j+1] + macierz_decyzyjna[, j+2]) / 6
    k <- k + 1
  }

  # Normalizacja (P_ij)
  sumy_kolumn <- colSums(macierz_ostra)
  sumy_kolumn[sumy_kolumn == 0] <- 1 # Unikamy dzielenia przez zero
  P <- sweep(macierz_ostra, 2, sumy_kolumn, "/")

  # Obliczanie Entropii (E_j)
  k_const <- 1 / log(nrow(macierz_decyzyjna))
  E <- numeric(ncol(P))

  for(j in 1:ncol(P)) {

    p_vals <- P[, j]
    p_vals <- p_vals[p_vals > 0] # Ignorujemy zera dla logarytmu
    if(length(p_vals) == 0) {
      E[j] <- 1
    } else {
      E[j] <- -k_const * sum(p_vals * log(p_vals))
    }
  }

  # Obliczanie wag (d_j i w_j)
  d <- 1 - E
  if(sum(d) == 0) return(rep(1/length(d), length(d))) # Zabezpieczenie
  w <- d / sum(d)

  return(w)
}

#' @title Wewnętrzny procesor wag
#' @description Decyduje, skąd wziąć wagi (Ręczne vs BWM).
#' @keywords internal
.pobierz_finalne_wagi <- function(macierz, wagi, bwm_kryteria, bwm_najlepsze, bwm_najgorsze) {

  n_kryteriow <- ncol(macierz) / 3

  # Opcja 1: Wagi podane ręcznie (np. z Entropii lub eksperckie)
  if (!missing(wagi) && !is.null(wagi)) {
    if (length(wagi) == n_kryteriow) {
      # Rozszerzamy wagi ostre na rozmyte (w, w, w)
      return(rep(wagi, each = 3))
    }
    if (length(wagi) != ncol(macierz)) {
      stop("Długość wektora 'wagi' musi odpowiadać liczbie kolumn macierzy (3 * n_kryteriow) lub liczbie kryteriów.")
    }
    return(wagi)
  }

  # Opcja 2: Obliczenie BWM
  if (!missing(bwm_najlepsze) && !missing(bwm_najgorsze)) {

    # Pobieramy nazwy kryteriow
    if (missing(bwm_kryteria)) {
      if (!is.null(attr(macierz, "nazwy_kryteriow"))) {
        bwm_kryteria <- attr(macierz, "nazwy_kryteriow")
      } else {
        bwm_kryteria <- paste0("C", 1:n_kryteriow)
        message("Nie znaleziono nazw kryteriów. Używam domyślnych: ", paste(bwm_kryteria, collapse=", "))
      }
    }

    message("Obliczanie wag metodą BWM...")
    wynik_bwm <- oblicz_wagi_bwm(bwm_kryteria, bwm_najlepsze, bwm_najgorsze)
    wagi_ostre <- wynik_bwm$wagi_kryteriow

    if (length(wagi_ostre) != n_kryteriow) {
      stop("Liczba wag z BWM nie zgadza się z liczbą kryteriów w macierzy.")
    }

    # Konwersja na wagi rozmyte (w, w, w)
    wagi_rozmyte <- rep(wagi_ostre, each = 3)
    return(wagi_rozmyte)
  }

  stop("Musisz podać wektor 'wagi' LUB parametry 'bwm_najlepsze' i 'bwm_najgorsze'.")
}

# ------ Metody Fuzzy ------


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
    detale = list(S_rozmyte = S_rozmyte, R_rozmyte = R_rozmyte, Q_rozmyte = Q_rozmyte),
    parametry = list(v = v),
    metoda = "VIKOR"
  )

  class(wynik) <- "rozmyty_vikor_promo_wynik"
  return(wynik)
}


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
    detale = list(D_poz_rozmyte = D_poz_rozmyte, D_neg_rozmyte = D_neg_rozmyte, R_rozmyte = R_rozmyte),
    metoda = "TOPSIS"
  )
  class(wynik) <- "rozmyty_topsis_promo_wynik"
  return(wynik)
}
