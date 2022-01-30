# Quelle: https://www.youtube.com/watch?v=ZJB_Ya964tY


# Pakete laden ------------------------------------------------------------

library(tidyverse)



# Datengenerierung --------------------------------------------------------

df <- dplyr::tibble(
  Pflanzenwachstum = c(30, 33, 35, 37, 37, 37.5, 38, 40, 41, 41, 42, 42.5, 43, 44,
                       29, 30, 30, 29, 31, 32, 31, 30, 31, 29, 32, 30, 29, 30,
                       25, 26, 26.5, 27, 27, 27.5, 28, 28, 28, 29, 29.5, 30, 30, 30.5),
  Dünger = as_factor(c(rep("Dünger", 21), rep("Dünger", 21))),
  Temp = as_factor(c(rep("warm", 14), rep("sehr kalt", 14), rep("kalt", 14)))
)



# Voraussetzungen ANOVA ---------------------------------------------------

# 1) Intervallskalierte abhängige Variable (Ruhepuls)

# 2) Normalverteilung der Residuen 
#    Alternativ: normalverteilte abhängige Variable je Gruppe

# 3) Varianzhomogenität zwischen den Gruppen

