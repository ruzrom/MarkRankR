#' @title Teoria Dominacji dla Rankingu
#' @description
#' Funkcja pomocnicza. Wyznacza ranking konsensusu na podstawie reguły większości.
#' Iteracyjnie sprawdza, która alternatywa najczęściej wygrywa na danej pozycji.
#'
#' @param macierz_rang Macierz (wiersze = alternatywy, kolumny = metody).
#' @return Wektor numeryczny z finalnym rankingiem.
#' @keywords internal
.oblicz_ranking_dominacji <- function(macierz_rang) {

  n <- nrow(macierz_rang)
  n_metod <- ncol(macierz_rang)
  finalny_ranking <- rep(0, n)

  # Maska dostepnych alternatyw (na początku wszystkie są dostępne)
  dostepne <- rep(TRUE, n)

  for (pozycja in 1:n) {
    # Pobieramy rangi tylko dla dostepnych alternatyw (reszte zamieniamy na Inf)
    macierz <- macierz_rang
    macierz[!dostepne, ] <- Inf

    # Kto ma najlepszą (najniższą) rangę w każdej metodzie?
    kandydaci <- apply(macierz, 2, which.min)

    tabela_cz <- table(kandydaci)

    maks_gl <- max(tabela_cz)
    najlepsze <- as.numeric(names(tabela_cz)[tabela_cz == maks_gl])

    if (length(najlepsze) == 1) {
      najlepszy_idx <- najlepsze
    } else {
      # Brak zgody
      # Sprawdzamy, który kandydat ma min sumę rang
      sumy <- rowSums(macierz_rang[najlepsze, , drop = FALSE])

      najlepszy_idx <- najlepsze[which.min(sumy)]
    }

    finalny_ranking[najlepszy_idx] <- pozycja
    dostepne[najlepszy_idx] <- FALSE
  }

  return(finalny_ranking)
}

#' @title Rozmyty Meta-Ranking
#' @description
#' Agreguje wyniki z metod Fuzzy VIKOR, TOPSIS i COPRAS, aby stworzyć
#' jeden, robustny ranking konsensusu.
#'
#' @param macierz_decyzyjna Rozmyta macierz danych.
#' @param typy_kryteriow Wektor typów ("min", "max").
#' @param wagi (Opcjonalnie) Wagi kryteriów.
#' @param bwm_najlepsze (Opcjonalnie) Wektor BWM Best-to-Others.
#' @param bwm_najgorsze (Opcjonalnie) Wektor BWM Others-to-Worst.
#' @param v Parametr dla VIKOR (domyślnie 0.5).
#'
#' @return Lista zawierająca ramkę danych z porównaniem rankingów oraz macierz korelacji.
#' @importFrom RankAggreg BruteAggreg RankAggreg
#' @importFrom stats cor
#' @export
rozmyty_meta_ranking <- function(macierz_decyzyjna,
                                 typy_kryteriow,
                                 wagi = NULL,
                                 bwm_najlepsze = NULL,
                                 bwm_najgorsze = NULL,
                                 v = 0.5) {

  # 1. Sprawdzenie wag (jesli brak BWM i brak wag recznych -> licz Entropie)
  if (is.null(wagi) && (is.null(bwm_najlepsze) || is.null(bwm_najgorsze))) {
    message("Brak wag i parametrów BWM. Obliczam wagi metodą Entropii...")
    wagi_surowe <- .oblicz_wagi_entropii(macierz_decyzyjna)
    wagi <- rep(wagi_surowe, each = 3)
  }

  # 2. Uruchomienie poszczególnych metod
  # Przygotowujemy liste argumentow wspolnych
  args_baza <- list(macierz_decyzyjna = macierz_decyzyjna, typy_kryteriow = typy_kryteriow)
  if (!is.null(wagi)) args_baza$wagi <- wagi
  if (!is.null(bwm_najlepsze)) {
    args_baza$bwm_najlepsze <- bwm_najlepsze
    args_baza$bwm_najgorsze <- bwm_najgorsze

    # Pobieramy nazwy kryteriow z atrybutu macierzy, zeby BWM zadzialal
    args_baza$bwm_kryteria <- attr(macierz_decyzyjna, "nazwy_kryteriow")
  }

  # VIKOR
  args_vikor <- c(args_baza, list(v = v))
  res_vikor <- do.call(rozmyty_vikor_promo, args_vikor)

  # COPRAS
  res_copras <- do.call(rozmyty_copras_promo, args_baza)

  # TOPSIS
  res_topsis <- do.call(rozmyty_topsis_promo, args_baza)


  # 3. Ekstrakcja Rankingów (same wektory liczb całkowitych)
  r_vikor <- res_vikor$wyniki$Ranking
  r_copras <- res_copras$wyniki$Ranking
  r_topsis <- res_topsis$wyniki$Ranking

  # 4. Agregacja Rankingów

  # A. Suma Rang (Im mniej tym lepiej)
  suma_pkt <- r_vikor + r_copras + r_topsis
  ranking_suma <- rank(suma_pkt, ties.method = "first")

  # B. Teoria Dominacji
  macierz_rang = cbind(r_vikor, r_copras, r_topsis)
  ranking_dominacja <- .oblicz_ranking_dominacji(macierz_rang)

  # C. RankAggreg (Algorytm Brute Force)
  macierz_dla_ra <- rbind(
    order(r_vikor),
    order(r_copras),
    order(r_topsis)
  )

  n_alt <- nrow(macierz_decyzyjna)

  # Jeśli mało alternatyw (<10), używamy Brute Force (dokładny).
  # Jeśli dużo, używamy Algorytmu Aggreg (przybliżony, ale szybszy).
  if (n_alt <= 10) {
    # verbose=FALSE zeby nie zasmiecac konsoli
    ra_wynik <- RankAggreg::BruteAggreg(macierz_dla_ra, n_alt, distance = "Spearman")
  } else {
    ra_wynik <- RankAggreg::RankAggreg(macierz_dla_ra, n_alt, method = "GA", distance = "Spearman", verbose = FALSE)
  }

  # Konwersja wyniku RankAggreg (lista indeksów) na wektor rang
  top_lista <- ra_wynik$top.list
  wektor_ra <- numeric(n_alt)

  # Mapowanie: top_lista[1] to indeks zwyciezcy -> dostaje range 1
  for(pozycja in 1:n_alt) {
    indeks_alternatywy <- as.numeric(top_lista[pozycja])
    wektor_ra[indeks_alternatywy] <- pozycja
  }

  # 5. Zestawienie wyników
  porownanie_df <- data.frame(
    Alternatywa = rownames(macierz_decyzyjna),
    R_VIKOR = r_vikor,
    R_COPRAS = r_copras,
    R_TOPSIS = r_topsis,
    Meta_Suma = ranking_suma,
    Meta_Dominacja = ranking_dominacja,
    Meta_Agregacja = wektor_ra
  )

  # Macierz korelacji Spearmana (czy metody są zgodne?)
  macierz_kor <- cor(porownanie_df[,-1], method = "spearman")

  wynik <- list(
    porownanie = porownanie_df,
    korelacje = macierz_kor
  )

  return(wynik)
}
