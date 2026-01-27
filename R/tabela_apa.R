#' @title Generowanie Tabeli APA
#' @description
#' Funkcja przekształca wyniki analizy MCDA (TOPSIS, VIKOR, WASPAS, Meta-Ranking)
#' w sformatowaną tabelę zgodną ze standardem APA, gotową do publikacji w Wordzie.
#'
#' @param x Obiekt wynikowy z funkcji pakietu (np. `rozmyty_topsis_wynik`).
#' @param tytul Opcjonalny tytuł tabeli.
#' @return Obiekt klasy `flextable` gotowy do druku lub zapisu do Worda.
#' @importFrom rempsyc nice_table
#' @importFrom flextable autofit save_as_docx
#' @export
tabela_apa <- function(x, tytul = NULL) {
  UseMethod("tabela_apa")
}

#' @export
tabela_apa.rozmyty_copras_promo_wynik <- function(x, tytul = "Wyniki metody Fuzzy COPRAS") {
  df <- x$wyniki

  names(df) <- c("Alternatywa", "Priorytet (Qi)", "Użyteczność (Ui)", "Ranking")

  df$`Priorytet (Qi)` <- round(df$`Priorytet (Qi)`, 4)
  df$`Użyteczność (Ui)` <- paste0(round(df$`Użyteczność (Ui)`, 2), "%")

  rempsyc::nice_table(
    df,
    title = c("Tabela 1", tytul),
    note = c("Uwaga. Qi - względny stopień priorytetu, Ui - stopień użyteczności wyrażony w procentach względem najlepszej alternatywy.")
  )
}


#' @export
tabela_apa.rozmyty_vikor_promo_wynik <- function(x, tytul = "Wyniki metody Fuzzy VIKOR") {
  df <- x$wyniki

  names(df) <- c("Alternatywa", "S (Grupa)", "R (Zal)", "Q (Kompromis)", "Ranking")

  df$`S (Grupa)` <- round(df$`S (Grupa)`, 3)
  df$`R (Zal)` <- round(df$`R (Zal)`, 3)
  df$`Q (Kompromis)` <- round(df$`Q (Kompromis)`, 4)

  rempsyc::nice_table(
    df,
    title = c("Tabela 2", tytul),
    note = c("Uwaga. S: użyteczność grupy, R: indywidualny żal, Q: indeks kompromisu (im mniej tym lepiej).")
  )
}


#' @export
tabela_apa.list <- function(x, tytul = "Meta-Ranking (Konsensus)") {
  # Obsługa Meta-Rankingu
  if(is.null(x$porownanie)) stop("To nie jest obiekt meta-rankingu.")

  df <- x$porownanie

  # Usuwamy "podłogi" z nazw kolumn (np. Meta_Suma -> Meta Suma)
  names(df) <- gsub("_", " ", names(df))

  rempsyc::nice_table(
    df,
    title = c("Tabela 3", tytul),
    note = c("Zestawienie rang uzyskanych różnymi metodami oraz rankingi konsensusu.")
  )
}
