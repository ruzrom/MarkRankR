#' @title Wewnętrzny parser składni
#' @description Funkcja pomocnicza do interpretowania modelu, podanego przez użytkownika.
#' Zamienia tekst "Kryterium =~ zmienna1 + zmienna2" na listę.
#' @keywords internal
.parsuj_skladnie_mcda <- function(skladnia) {

  czysta_skladnia <- gsub("\n", "", skladnia)

  linie <- strsplit(czysta_skladnia, ";")[[1]]
  mapowanie <- list()

  for (linia in linie) {

    if (trimws(linia) == "") next

    czesci <- strsplit(linia, "=~")[[1]] # Dzielimy za operatorem "=~"
    if (length(czesci) == 2) {
      nazwa_kryterium <- trimws(czesci[1])
      elementy <- trimws(strsplit(czesci[2], "\\+")[[1]]) # Dzielimy część wg "+"
      mapowanie[[nazwa_kryterium]] <- elementy
    }

  }

  return(mapowanie)
}


#' @title Wewnętrzny Skaler Saaty'ego
#' @description Przekształca dowolną skalę na skalę Saaty'ego 1-9.
#' @keywords internal
.skaluj_do_saaty <- function(wektor) {

  if (any(wektor < 0, na.rm = TRUE)) stop("Wykryto wartości ujemne w danych wejściowych.")

  # Zamiana kodow bledow (np. 99) i brakow danych (NA) na 0
  wektor[is.na(wektor) | wektor == 99] <- 0

  # Maska dla poprawnych wartosci
  maska_poprawne <- wektor > 0
  wartosci <- wektor[maska_poprawne]

  if (length(wartosci) == 0) return(wektor)

  min_v <- min(wartosci)
  max_v <- max(wartosci)

  if (min_v == max_v) {
    wektor[maska_poprawne] <- 1
  } else {
    wektor[maska_poprawne] <- 1 + (wartosci - min_v) * (8 / (max_v - min_v))
  }

  return(wektor)
}


#' @title Wewnętrzna funkcja rozmywająca (Fuzzifier)
#' @description Zamienia liczbę rzeczywistą (Crisp) na Trójkątną Liczbę Rozmytą (TFN).
#' TFN to trójka (l, m, u), gdzie m = x, l = x-1, u = x+1.
#' @keywords internal
.rozmyj_wektor <- function(wektor) {

  # min to 1
  l <- pmax(1, wektor - 1)
  # middle
  m <- wektor
  # max to 9
  u <- pmin(9, wektor + 1)

  jest_zerem <- (wektor == 0)
  l[jest_zerem] <- 0; m[jest_zerem] <- 0; u[jest_zerem] <- 0

  return(cbind(l, m, u))
}


#' Przygotowanie Danych do Rozmytej Analizy MCDA
#'
#' @description Funkcja przekształca surowe dane ankietowe w rozmytą macierz decyzyjną.
#' Oblicza wyniki zmiennych kompozytowych na podstawie składni, skaluje je do przedziału 1-9,
#' agreguje odpowiedzi ekspertów (jeśli dotyczy) i dokonuje rozmycia (fuzzification).
#'
#' @param dane Ramka danych (data frame) zawierająca surowe zmienne.
#' @param skladnia Ciąg znaków definiujący kryteria (np. "Koszt =~ k1 + k2").
#' @param kolumna_alternatyw Nazwa kolumny identyfikującej alternatywy.
#' Jeśli NULL, każdy wiersz traktowany jest jako osobna alternatywa.
#' @param funkcja_agregacji Funkcja używana do scalania opinii ekspertów (domyślnie: mean).
#' @return Macierz o wymiarach ($m \times 3n$), gdzie m to liczba alternatyw.
#' @export
przygotuj_dane_mcda <- function(dane, skladnia, kolumna_alternatyw = NULL, funkcja_agregacji = mean) {

  if (!is.data.frame(dane)) stop("Argument 'dane' musi być ramką danych (data frame).")

  # 1. Parsowanie składni
  mapowanie <- .parsuj_skladnie_mcda(skladnia)
  nazwy_kryteriow <- names(mapowanie)

  # 2. Obliczanie zmiennych kompozytowych i skalowanie (dla każdego wiersza/eksperta)
  tymczasowe_wyniki <- data.frame(row_id = 1:nrow(dane))

  for (kryt in nazwy_kryteriow) {
    zmienne <- mapowanie[[kryt]]

    # Sprawdzenie czy zmienne istnieja w danych
    brakujace <- zmienne[!zmienne %in% names(dane)]

    if (length(brakujace) > 0) stop(paste("Brakuje zmiennych w danych:", paste(brakujace, collapse=", ")))

    # Obliczanie sredniej dla kryterium (Composite Score)
    if (length(zmienne) > 1) {
      surowy_wynik <- rowMeans(dane[, zmienne, drop = FALSE], na.rm = TRUE)
    } else {
      surowy_wynik <- dane[[zmienne]]
    }

    # Skalowanie do 1-9
    tymczasowe_wyniki[[kryt]] <- .skaluj_do_saaty(surowy_wynik)
  }

  # 3. Agregacja (Eksperci -> Alternatywy)
  if (!is.null(kolumna_alternatyw)) {
    if (!kolumna_alternatyw %in% names(dane)) stop("Nie znaleziono kolumny alternatyw w danych.")

    tymczasowe_wyniki$ID_Alternatywy <- dane[[kolumna_alternatyw]]

    # Agregacja wg ID Alternatywy
    dane_zagregowane <- aggregate(. ~ ID_Alternatywy, data = tymczasowe_wyniki[, -1], FUN = funkcja_agregacji)

    # Sortowanie i czyszczenie
    dane_zagregowane <- dane_zagregowane[order(dane_zagregowane$ID_Alternatywy), ]
    nazwy_wierszy <- dane_zagregowane$ID_Alternatywy
    macierz_wynikow <- as.matrix(dane_zagregowane[, nazwy_kryteriow])

  } else {
    # Brak agregacji (1 wiersz = 1 alternatywa)
    macierz_wynikow <- as.matrix(tymczasowe_wyniki[, nazwy_kryteriow])
    nazwy_wierszy <- 1:nrow(macierz_wynikow)
  }

  # 4. Rozmywanie (Crisp -> Fuzzy Triangular)
  lista_decyzyjna <- list()

  for (i in seq_along(nazwy_kryteriow)) {
    kryt <- nazwy_kryteriow[i]
    lista_decyzyjna[[kryt]] <- .rozmyj_wektor(macierz_wynikow[, i])
  }

  finalna_macierz <- do.call(cbind, lista_decyzyjna)
  rownames(finalna_macierz) <- nazwy_wierszy

  # Zapisujemy metadane jako atrybut macierzy
  attr(finalna_macierz, "nazwy_kryteriow") <- nazwy_kryteriow

  return(finalna_macierz)
}
