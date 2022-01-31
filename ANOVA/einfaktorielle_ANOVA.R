# Quelle: https://www.youtube.com/watch?v=GctGncQrJew


# Pakete laden ------------------------------------------------------------

library(tidyverse)



# Datengenerierung --------------------------------------------------------

# Wieso macht es einen Unterschied, ob die Daten der unabhängigen Variablen
# als Faktoren eingelesen werden oder nicht? (Vgl. Resultate der ANOVA
# unterscheiden sich...)

df <- dplyr::tibble(
  Puls = c(45, 60, 65, 57, 44, 35, 45, 85, 58, 68, 72, 75, 60,
           100, 125, 145, 110, 120, 115, 100, 120, 125, 135, 145, 130, 120,
           100, 110, 100, 95, 98, 125, 120, 118, 95, 100, 119, 125, 140),
  Trainingsgruppe = c(rep(0, 13), rep(1, 13), rep(2, 13))
)



# Voraussetzungen ANOVA ---------------------------------------------------

# 1) Intervallskalierte abhängige Variable

# 2) Varianzhomogenität zwischen den Gruppen

# 3) Normalverteilung der Residuen 
#    Alternativ: normalverteilte abhängige Variable je Gruppe



# Vorausssetzungen prüfen -------------------------------------------------

### Intervallskalierung

# abhängige Variable = Ruhepuls


### Varianzhomogenität 

# 1) deskripitve Analyse
psych::describeBy(x = df$Puls, group = df$Trainingsgruppe)
# ist hier auf den ersten Blick gegeben  vgl. sd = Wurzel der Varianz (genauerer Test Levene-Test)


# 2) Zusatz Levene-Test 
# prüft, ob die Varianz bei 2 oder mehr Gruppen gleich ist (Voraussetzung für t-Test oder ANOVA)
# H0: Varianzgleichheit

car::leveneTest(y = df$Puls, group = df$Trainingsgruppe)

# Levene-Test ist nicht sigifikant (F-Wert: 0.1274, Pr(>F): 0.8807)
# H0 kann nicht (!) verworfen werden!
# => Varianzhomogenität ist gegeben!
# Der Levene-Test wird in der Regel mit dem Median durchgeführt, da dieser
# robuster ist als der Mittelwert...



# Durchführung der ANOVA --------------------------------------------------

ANOVA_Training <- aov(formula = Puls ~ Trainingsgruppe, data = df)

summary(ANOVA_Training)

# signifikanter F-Test (F value: 33.64 Pr(>F): 1.17e-06 => Wahrscheinlichkeit, dass solch ein grosser F-Wert empirisch vorkommt,
# beträgt 1.17e-04%)
# Signifikante Unterschiede zwischen den Gruppen!
# ABER wir können noch nicht sagen zwischen welchen Gruppen!



# Post-hoc-Test (paarweise Vergleichstests!) ------------------------------

# Problem: Alpha-Fehler-Kumulierung => wir können nicht einfach so mehrere t-Tests an der gleichen Stichprobe durchführen
# bei n Tests und einem Alpha-Niveau von 5% steigt der Alpha-Fehler auf 1 - (0.95)^n
# vgl. 3 t-Tests: 1 - (0.95)^3 = 1-0.857 = 0.143 => die Wahrscheinlichkeit für den Alpha-Fehler steigt auf 14.3%!
# Alpha-Fehler-Korrektur mittels paarweisen t-Tests!

pairwise.t.test(df$Puls, df$Trainingsgruppe, p.adjust.method = "bonferroni")

# Signifikanzmatrix
# Gruppe 0 und 1 sind signifikant verschieden (p-value: 7.3e-13)
# Gruppe 0 und 2 sind signifikant verschieden (p-vlaue: 1.4e-10)
# Gruppe 1 und 2 sind nicht (!) signifikant verschieden (p-value: 0.16)

# P value adjustment! => um den Alpha-Kumulierungs-Fehler zu korrigieren kann man entweder direkt das Alpha anpassen,
# oder den P-Wert! => hier wird der P-Wert angepasst (bonferroni als konservativste Variante...).



# Voraussetzungen prüfen 2.0 ----------------------------------------------

# Normalverteilung der Residuen

# modelr + ggplot2
df <- df %>% 
  modelr::add_residuals(ANOVA_Training) %>% 
  modelr::add_predictions(ANOVA_Training) %>% 
  mutate(resid_std = (resid - mean(resid)) / sd(resid))

df %>% 
  ggplot(aes(resid_std)) + 
  geom_histogram(bins = 8)

# modelr + base R
hist(df$resid_std)

# base R
hist(rstandard(ANOVA_Training))


# Zusatz: Q-Q-Plot für die Residuen
plot(ANOVA_Training, 2)
# für eine perfkete Normalverteilung müssten alle standardisierten Residuen auf der Geraden sein...
# ist hier nicht ganz erfüllt, aber OK (Normalverteilung muss nicht perfekt sein...)

