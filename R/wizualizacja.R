#' @title Wewnętrzny motyw graficzny
#' @description Ujednolicony styl wykresów dla całego pakietu.
#' @import ggplot2
#' @keywords internal
.motyw_mcda <- function() {
  list(
    theme_light(base_size = 12),
    scale_fill_gradient(low = "#90A4AE", high = "#2E7D32"), # Od szaro-niebieskiego do zieleni
    scale_size_continuous(range = c(4, 16)),
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "grey40", size = 11),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      legend.position = "right",
      axis.title = element_text(face = "bold")
    )
  )
}


#' Mapa Strategiczna VIKOR
#'
#' @description Wizualizacja typu cIPMA.
#' Oś X: Efektywność grupowa (odwrócone S). Oś Y: Ryzyko/Żal (R).
#' Wielkość bąbla: Siła kompromisu (zależna od Q).
#'
#' @param x Obiekt klasy `rozmyty_vikor_promo_wynik`.
#' @param ... Dodatkowe argumenty (ignorowane).
#' @import ggplot2
#' @import ggrepel
#' @export
plot.rozmyty_vikor_promo_wynik <- function(x, ...) {
  df <- x$wyniki

  # 1. Matematyka wykresu: Odwracamy S (żeby im więcej tym lepiej na osi X)
  s_min <- min(df$Def_S); s_max <- max(df$Def_S)

  # Normalizacja do 0-100
  df$Wydajnosc <- ((s_max - df$Def_S) / (s_max - s_min)) * 100

  # Wielkość bąbla (odwrócone Q - im mniejsze Q tym większy bąbel, bo to lżejszy kompromis)
  q_inv <- 1 - ((df$Def_Q - min(df$Def_Q)) / (max(df$Def_Q) - min(df$Def_Q)))
  df$Rozmiar <- (q_inv + 0.1)^3 # Potęgowanie dla lepszego kontrastu wizualnego

  # Środki do wyznaczenia ćwiartek
  srodek_perf <- median(df$Wydajnosc, na.rm=TRUE)
  srodek_ryzyko <- median(df$Def_R, na.rm=TRUE)

  ggplot(df, aes(x = Wydajnosc, y = Def_R)) +

    # Tło dla strefy Lidera (Prawa dolna ćwiartka: Duża wydajność, Małe ryzyko)
    annotate("rect", xmin=srodek_perf, xmax=Inf, ymin=-Inf, ymax=srodek_ryzyko, fill="#E8F5E9", alpha=0.5) +

    # Linie podziału
    geom_vline(xintercept = srodek_perf, linetype = "dashed", color = "grey50") +
    geom_hline(yintercept = srodek_ryzyko, linetype = "dashed", color = "grey50") +

    # Etykiety stref
    annotate("text", x = max(df$Wydajnosc), y = min(df$Def_R), label = "STABILNY LIDER\n(Wysoka Efekt., Niskie Ryzyko)",
             hjust=1, vjust=0, size=3, fontface="bold.italic", color="darkgreen") +

    annotate("text", x = min(df$Wydajnosc), y = max(df$Def_R), label = "UNIKAĆ\n(Niska Efekt., Wysokie Ryzyko)",
             hjust=0, vjust=1, size=3, fontface="italic", color="#B71C1C") +

    # Bąble
    geom_point(aes(size = Rozmiar, fill = Wydajnosc), shape = 21, color = "black", alpha = 0.8) +
    geom_text_repel(aes(label = paste0("Alt ", Alternatywa)), box.padding = 0.5) +

    scale_x_continuous(expand = expansion(mult = 0.2)) +

    labs(
      title = "Mapa Strategiczna VIKOR",
      subtitle = "Zielona strefa = Najlepszy kompromis.",
      x = "Indeks Wydajności Grupy (odwrócone S)",
      y = "Indeks Ryzyka / Żalu (R)",
      size = "Dominacja",
      fill = "Wynik"
    ) +
    .motyw_mcda()
}


#' Mapa Strategiczna COPRAS
#'
#' @description Wizualizacja wyników metody COPRAS w układzie dwuwymiarowym.
#' Oś X: Suma korzyści (S+). Oś Y: Suma kosztów (S-).
#' Wielkość bąbla: Stopień użyteczności (Ui).
#'
#' @param x Obiekt klasy `rozmyty_copras_promo_wynik`.
#' @param ... Dodatkowe argumenty.
#' @import ggplot2
#' @import ggrepel
#' @export
plot.rozmyty_copras_promo_wynik <- function(x, ...) {
  df <- x$wyniki

  df$S_plus_def <- (x$detale$S_plus[, 1] + x$detale$S_plus[, 2] + x$detale$S_plus[, 3]) / 3
  df$S_minus_def <- (x$detale$S_minus[, 1] + x$detale$S_minus[, 2] + x$detale$S_minus[, 3]) / 3

  srodek_x <- median(df$S_plus_def)
  srodek_y <- median(df$S_minus_def)

  ggplot(df, aes(x = S_plus_def, y = S_minus_def)) +

    annotate("rect", xmin = srodek_x, xmax = Inf, ymin = -Inf, ymax = srodek_y,
             fill = "#E8F5E9", alpha = 0.5) +

    annotate("rect", xmin = -Inf, xmax = srodek_x, ymin = srodek_y, ymax = Inf,
             fill = "#FFEBEE", alpha = 0.5) +

    geom_vline(xintercept = srodek_x, linetype = "dashed", color = "grey60") +
    geom_hline(yintercept = srodek_y, linetype = "dashed", color = "grey60") +

    annotate("text", x = max(df$S_plus_def), y = min(df$S_minus_def),
             label = "LIDER EFEKTYWNOŚCI\n(Wysokie S+, Niskie S-)",
             hjust = 1, vjust = 0, size = 3.5, fontface = "bold.italic", color = "darkgreen") +

    annotate("text", x = min(df$S_plus_def), y = max(df$S_minus_def),
             label = "NIEOPŁACALNE\n(Niskie S+, Wysokie S-)",
             hjust = 0, vjust = 1, size = 3.5, fontface = "italic", color = "#B71C1C") +

    geom_point(aes(size = Uzytecznosc_Ui, fill = Uzytecznosc_Ui),
               shape = 21, color = "black", alpha = 0.8) +

    geom_text_repel(aes(label = Alternatywa),
                    fontface = "bold", box.padding = 0.6) +

    labs(
      title = "Mapa Strategiczna COPRAS",
      subtitle = "Analiza relacji korzyści (S+) do poniesionych kosztów (S-).",
      x = "Wydajność / Korzyści (S+)",
      y = "Koszt / Obciążenie (S-)",
      size = "Użyteczność (%)",
      fill = "Wynik Ui"
    ) +
    .motyw_mcda()
}


# Fix dla ostrzeżeń R CMD check o zmiennych globalnych w ggplot2
utils::globalVariables(c(
  "Def_S", "Def_R", "Def_Q",                  # VIKOR
  "S_plus_def", "S_minus_def", "Uzytecznosc_Ui", # COPRAS
  "Wydajnosc", "Rozmiar",                     # Parametry wizualne VIKOR
  "OdlegloscWizualna", "Spojnosc",            # Inne parametry graficzne
  "Alternatywa"                               # Klucz alternatyw
))
