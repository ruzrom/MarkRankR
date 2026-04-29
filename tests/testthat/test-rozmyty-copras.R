test_that("rozmyty_copras_promo follows standard COPRAS on defuzzified data", {
  macierz <- cbind(
    c(2, 4), c(2, 4), c(2, 4),
    c(4, 2), c(4, 2), c(4, 2)
  )

  rownames(macierz) <- c("A1", "A2")
  attr(macierz, "nazwy_kryteriow") <- c("Koszt", "Korzysc")

  wynik <- rozmyty_copras_promo(
    macierz_decyzyjna = macierz,
    typy_kryteriow = c("min", "max"),
    wagi = c(0.5, 0.5)
  )

  expect_equal(as.numeric(wynik$detale$P_i), c(1 / 3, 1 / 6), tolerance = 1e-8)
  expect_equal(as.numeric(wynik$detale$R_i), c(1 / 6, 1 / 3), tolerance = 1e-8)
  expect_equal(as.numeric(wynik$detale$Q_i), c(2 / 3, 1 / 3), tolerance = 1e-8)
  expect_identical(wynik$wyniki$Ranking, c(1L, 2L))
})
