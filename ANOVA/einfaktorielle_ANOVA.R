# source: https://www.youtube.com/watch?v=GctGncQrJew


# generate data set


df <- dplyr::tibble(
  Ruhepuls = c(95, 91, 65, 77, 98, 105, 100, 110, 115, 98, 115, 115, 120,
               100, 115, 115, 98, 120, 80, 100, 120, 125, 135, 145, 130, 120,
               95, 91, 98, 77, 98, 105, 110, 80, 75, 80, 65, 75, 86),
  Trainingsgruppe = c(rep(0, 13), rep(1, 13), rep(2, 13))
)
