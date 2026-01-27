set.seed(408)

# Zalożenia:
# 5 kryteriów
# 4 alternatyw (typy promocji)
# 10 ekspertów marketingowych

promocje <- c("Rabat", "Konkurs", "Program_Lojalnosciowy", "Kupony")

promocje_dane_surowe <- data.frame(

  # --- Identyfikatory ---
  EkspertID = rep(1:10, each = length(promocje)),
  Alternatywa = rep(promocje, times = 10),

  # --- Kryterium 1: Koszt wdrożenia (PLN) ---
  koszt_wdrozenia = runif(40, 5000, 80000),

  # --- Kryterium 2: Wpływ na wzrost sprzedaży (Skala Likerta 1-7) ---
  # 7 - bardzo wysoki wzrost
  wzrost_sprzedazy = sample(1:7, 40, replace = TRUE),

  # --- Kryterium 3: Lajalność klientów (Skala Likerta 1-7) ---
  lojalnosc_klientow = sample(1:7, 40, replace = TRUE),

  # --- Kryterium 4: Atrakcyjność dla klienta (Skala Likerta 1-9) ---
  atrakcyjnosc_klienta = sample(1:9, 40, replace = TRUE),

  # --- Kryterium 5: Łatwość realizacji (Skala Likerta 1-7) ---
  # Symulacja błędu dla testu czyszczenia
  latwosc_realizacji = sample(c(1:7,99), 40, replace = TRUE, prob = c(rep(0.135,7),0.055))

)

usethis::use_data(promocje_dane_surowe, overwrite = TRUE)

