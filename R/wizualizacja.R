#' @title Wewnętrzny motyw graficzny
#' @description Ujednolicony styl wykresów dla całego pakietu.
#' @import ggplot2
#' @importFrom stats median
#' @keywords internal
.motyw_mcda <- function() {
  base_font <- "sans"

  list(
    theme_light(base_size = 11, base_family = base_font),
    scale_fill_gradient(low = "#E8F5E9", high = "#1B5E20"),
    scale_size_continuous(range = c(3, 10)),
    theme(
      plot.title = element_text(face = "bold", size = 14, color = "black", margin = margin(b = 6), family = base_font),
      plot.subtitle = element_text(color = "grey40", size = 10, margin = margin(b = 12), family = base_font),
      plot.caption = element_text(color = "grey50", size = 8, family = base_font),

      axis.title = element_text(face = "bold", size = 10, color = "#222222", family = base_font),
      axis.text = element_text(color = "grey30", size = 9, family = base_font),
      axis.line = element_line(color = "grey75", linewidth = 0.5),

      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      panel.grid.major = element_line(color = "grey88", linewidth = 0.5),
      panel.grid.minor = element_line(color = "grey93", linewidth = 0.3),

      legend.position = "right",
      legend.title = element_text(face = "bold", size = 9, family = base_font),
      legend.text = element_text(size = 8, family = base_font),

      plot.margin = margin(15, 20, 15, 15)
    )
  )
}

#' Mapa Strategiczna VIKOR
#'
#' @description Wizualizacja wyników metody VIKOR w układzie dwuwymiarowym.
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
  df$Rozmiar <- (q_inv + 0.1)^3

  # Środki do wyznaczenia ćwiartek
  srodek_perf <- median(df$Wydajnosc, na.rm=TRUE)
  srodek_ryzyko <- median(df$Def_R, na.rm=TRUE)

  ggplot(df, aes(x = Wydajnosc, y = Def_R)) +

    # Tło dla strefy Lidera (Prawa dolna ćwiartka: Duża wydajność, Małe ryzyko)
    annotate("rect", xmin=srodek_perf, xmax=Inf, ymin=-Inf, ymax=srodek_ryzyko, fill="#88E788", alpha=0.2) +
    annotate("rect", xmin = -Inf, xmax = srodek_perf, ymin = srodek_ryzyko, ymax = Inf, fill = "#FF7F7F", alpha = 0.2) +

    # Linie podziału
    geom_vline(xintercept = srodek_perf, linetype = "dashed", color = "grey50") +
    geom_hline(yintercept = srodek_ryzyko, linetype = "dashed", color = "grey50") +

    # Etykiety stref
    annotate("text", x = max(df$Wydajnosc), y = min(df$Def_R), label = "STABILNY LIDER\n(Wysoka Efekt., Niskie Ryzyko)",
             hjust = 1.05, vjust = -0.5, size = 3, fontface = "bold.italic", color = "#2E7D32", family = "sans") +
    annotate("text", x = max(df$Wydajnosc), y = max(df$Def_R), label = "SZANSA\n(Wysoka Efekt., Wysokie Ryzyko)",
             hjust = 1.05, vjust = 1.5, size = 3, fontface = "bold.italic", color = "#E65100", family = "sans") +
    annotate("text", x = min(df$Wydajnosc), y = min(df$Def_R), label = "BEZPIECZNA PRZECIĘTNOŚĆ\n(Niska Efekt., Niskie Ryzyko)",
             hjust = -0.05, vjust = -0.5, size = 3, fontface = "bold.italic", color = "#37474F", family = "sans") +
    annotate("text", x = min(df$Wydajnosc), y = max(df$Def_R), label = "UNIKAĆ\n(Niska Efekt., Wysokie Ryzyko)",
             hjust = -0.05, vjust = 1.5, size = 3, fontface = "bold.italic", color = "#C62828", family = "sans") +

    # Bąble
    geom_point(aes(size = Rozmiar, fill = Wydajnosc), shape = 21, color = "black", alpha = 0.8) +
    geom_text_repel(aes(label = paste0("Alt ", Alternatywa)), box.padding = 0.5) +

    scale_x_continuous(expand = expansion(mult = 0.15)) +
    scale_y_continuous(expand = expansion(mult = 0.15)) +

    labs(
      title = "Mapa Strategiczna VIKOR",
      subtitle = "Zielona strefa = Najlepszy kompromis. Czerwona strefa = Najgorsze opcje",
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
#'
#' @param x Obiekt klasy `rozmyty_copras_promo_wynik`.
#' @param ... Dodatkowe argumenty.
#' @import ggplot2
#' @import ggrepel
#' @export
plot.rozmyty_copras_promo_wynik <- function(x, ...) {
  df <- x$wyniki

  # Wielkość bąbla
  df$Rozmiar <- (df$Uzytecznosc_Ui / 100) * 10

  df$S_plus_def <- rowMeans(x$detale$S_plus)
  df$S_minus_def <- rowMeans(x$detale$S_minus)

  srodek_x <- median(df$S_plus_def, na.rm=TRUE) # korzyść
  srodek_y <- median(df$S_minus_def, na.rm=TRUE) # koszt

  ggplot(df, aes(x = S_plus_def, y = S_minus_def)) +

    annotate("rect", xmin = srodek_x, xmax = Inf, ymin = -Inf, ymax = srodek_y,
             fill = "#88E788", alpha = 0.2) +

    annotate("rect", xmin = -Inf, xmax = srodek_x, ymin = srodek_y, ymax = Inf,
             fill = "#FF7F7F", alpha = 0.2) +

    geom_vline(xintercept = srodek_x, linetype = "dashed", color = "grey50") +
    geom_hline(yintercept = srodek_y, linetype = "dashed", color = "grey50") +

    annotate("text", x = max(df$S_plus_def), y = min(df$S_minus_def),
             label = "LIDER EFEKTYWNOŚCI\n(Wysoka korzyść, Niski koszt)",
             hjust = 1.05, vjust = -0.5, size = 3, fontface = "bold.italic", color = "#2E7D32", family = "sans") +

    annotate("text", x = min(df$S_plus_def), y = max(df$S_minus_def),
             label = "NIEOPŁACALNE\n(Niska korzyść, Wysoki koszt)",
             hjust = -0.05, vjust = 1.5, size = 3, fontface = "bold.italic", color = "#C62828", family = "sans") +

    # Bąble
    geom_point(aes(size = Rozmiar, fill = Uzytecznosc_Ui),
               shape = 21, color = "black", alpha = 0.8) +

    geom_text_repel(aes(label = paste0("Alt ", Alternatywa)), box.padding = 0.5) +

    scale_x_continuous(expand = expansion(mult = 0.15)) +
    scale_y_continuous(expand = expansion(mult = 0.15)) +

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


#' Mapa Strategiczna TOPSIS
#'
#' @description Wizualizacja wyników metody TOPSIS w układzie dwuwymiarowym.
#'
#' @param x Obiekt klasy `rozmyty_topsis_promo_wynik`.
#' @param ... Dodatkowe argumenty.
#' @import ggplot2
#' @import ggrepel
#' @export
plot.rozmyty_topsis_promo_wynik <- function(x, ...) {
  df <- x$wyniki

  # Wielkość bąbla
  df$Rozmiar <- (df$Score)^4

  # Środki do podziału przestrzeni
  cel_x <- max(df$D_minus, na.rm = TRUE) * 1.02
  cel_y <- min(df$D_plus, na.rm = TRUE) * 0.98

  # Odległość wizualna
  # "Jak daleko ta opcja znajduje się od Złotego Diamentu?"
  df$WizOdl = sqrt((df$D_minus - cel_x)^2 + (df$D_plus - cel_y)^2)

  ggplot(df, aes(x = D_minus, y = D_plus)) +

    geom_segment(aes(xend = cel_x, yend = cel_y), linetype = "dotted", color = "grey50") +

    geom_label(aes(x = (D_minus + cel_x) / 2,
                   y = (D_plus + cel_y) / 2,
                   label = sprintf("%.3f", WizOdl)),
               size = 3, nudge_y = 0.002, fontface = "italic", color = "grey30", fill = "white", label.size = 0, alpha = 0.8) +

    # Bąble
    geom_point(aes(size = Rozmiar, fill = Score), shape = 21, color = "black", alpha = 0.9) +
    geom_text_repel(aes(label = paste0("Alt ", Alternatywa)), box.padding = 0.5) +


    annotate("point", x = cel_x, y = cel_y, shape=18, size=5, color="#D4AF37") +
    annotate("text", x = cel_x, y = cel_y, label="IDEAŁ", vjust=1.8, size=3.2, fontface="bold") +

    scale_x_continuous(expand = expansion(mult = c(0.1, 0.15))) +
    scale_y_continuous(expand = expansion(mult = c(0.15, 0.1))) +

    labs(
      title = "Mapa Strategiczna TOPSIS",
      subtitle = "Analiza relacji odległości od rozwiązań idealnych.",
      x = "Odległość od anty-ideału (D-)",
      y = "Odległość od ideału (D+)",
      size = "Bliskość^4",
      fill = "Score"
    ) +
    .motyw_mcda()
}


# Fix dla ostrzeżeń R CMD check o zmiennych globalnych w ggplot2
utils::globalVariables(c(
  "Def_S", "Def_R", "Def_Q",                  # VIKOR
  "S_plus_def", "S_minus_def", "Uzytecznosc_Ui", # COPRAS
  "D_plus", "D_minus", "Score",               # TOPSIS
  "Wydajnosc", "Rozmiar", "WizOdl",           # Parametry wizualne
  "OdlegloscWizualna", "Spojnosc",            # Inne parametry graficzne
  "Alternatywa"                               # Klucz alternatyw
))
