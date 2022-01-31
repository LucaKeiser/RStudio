# Quelle: https://www.youtube.com/watch?v=ZJB_Ya964tY


# Pakete laden ------------------------------------------------------------

library(tidyverse)



# Datengenerierung --------------------------------------------------------

df <- dplyr::tibble(
  Pflanzenwachstum = c(30, 30.5, 31.5, 32, 32, 32.5, 32.8, 31, 32, 32.5, 34, 35.5, 35.8, 36,
                       29, 30.5, 30.5, 31, 31.5, 32, 32, 30, 31, 32, 32.5, 33, 33.5, 34.5,
                       29, 30, 30.5, 31, 31.3, 31.5, 31.5, 30, 31.5, 31.8, 32, 32.8, 33, 33.5),
  Dünger = as_factor(c(rep("Kein Dünger", 7), rep("Dünger", 7),
                       rep("Kein Dünger", 7), rep("Dünger", 7),
                       rep("Kein Dünger", 7), rep("Dünger", 7))),
  Temp = as_factor(c(rep("warm", 14), rep("normal", 14), rep("sehr kalt", 14)))
)




# Voraussetzungen ANOVA ---------------------------------------------------

# 1) Intervallskalierte abhängige Variable (Pflanzenwachstum)
#    Quasimetrische abhängige Variable auch OK (Skalen)

# 2) Varianzhomogenität zwischen den Gruppen

# 3) Normalverteilung der Residuen 
#    Alternativ: normalverteilte abhängige Variable je Gruppe




# Vorausssetzungen prüfen -------------------------------------------------

### Intervallskalierung

# abhängige Variable = Pflanzenwachstum


### Varianzhomogenität 

# 1) deskripitve Analyse

# Deskriptive Analyse
psych::describeBy(x = df$Pflanzenwachstum, group = list(df$Dünger, df$Temp))

# plot
df %>% 
  ggplot(aes(x = Temp, y = Pflanzenwachstum, color = Dünger)) + 
  geom_boxplot()

# ist hier auf den ersten Blick gegeben  vgl. sd = Wurzel der Varianz (genauerer Test Levene-Test)


# 2) Zusatz Levene-Test 
# prüft, ob die Varianz bei 2 oder mehr Gruppen gleich ist (Voraussetzung für t-Test oder ANOVA)
# H0: Varianzgleichheit

car::leveneTest(Pflanzenwachstum ~ Temp * Dünger, data = df)

# Levene-Test ist nicht sigifikant (F-Wert: 1.7044, Pr(>F): 0.1586)
# H0 kann nicht (!)  verworfen werden!
# => Varianzhomogenität ist gegeben!
# Der Levene-Test wird in der Regel mit dem Median durchgeführt, da dieser
# robuster ist als der Mittelwert...



# Durchführung der ANOVA --------------------------------------------------

ANOVA_Pflanzen <- aov(formula = Pflanzenwachstum ~ Temp + Dünger + Temp * Dünger, data = df)

summary(ANOVA_Pflanzen)


# Interaktionsterm ist nicht signifikant => kann entfernt werden
# Beachtemn: Wäre der Interaktionsterm signifikant, darf nur dieser
# interpretiert werden (c.p.)

ANOVA_Pflanzen_2 <- aov(formula = Pflanzenwachstum ~ Temp + Dünger, data = df)

summary(ANOVA_Pflanzen_2)

# Temperatur ist auf dem 5%-Level signifikant (F-Wert: 4.035, P-Wert: 0.025766) 
# Dünger ist auf dem 0.1%-Level signifikant (F-Wert: 17.024, P-Wert: 0.000194)

# Post-hoc-Test (TukeyHSD) -----------------------------------------------
# HSD = honest significat difference
# Kontrolliert ebenfalls für die Alpha-Fehler-Kumulierung!

TukeyHSD(ANOVA_Pflanzen_2)

# 1. Hauptfaktor (Temp)
# Unterschied zwischen normal und warm ist nicht (!) signifikant (p-adj.: 0.0910643)
# Unterschied zwischen sehr kalt und warm ist signifikant (p-adj.: 0.0287327))
# Unterschied zwischen sehr kalt und normal ist nicht (!) signifikant (p-adj.: 0.8643500)

# siehe 95%-Konfidenzintervalle für die Gruppenunterschiede

# 2. Hauptfaktor (Dünger)
# Unterschied zwischen Dünger und Kein Dünger ist signifikant (p-adj.: 0.000194).

# siehe 95%-Konfidenzintervalle für die Gruppenunterschiede



# Voraussetzungen prüfen 2.0 ----------------------------------------------

# Normalverteilung der Residuen

# modelr + ggplot2
df <- df %>% 
  modelr::add_residuals(ANOVA_Pflanzen_2) %>% 
  modelr::add_predictions(ANOVA_Pflanzen_2) %>% 
  mutate(resid_std = (resid - mean(resid)) / sd(resid))

df %>% 
  ggplot(aes(resid_std)) + 
  geom_histogram(bins = 8)

# modelr + base R
hist(df$resid_std)

# base R
hist(rstandard(ANOVA_Pflanzen_2))


# Zusatz: Q-Q-Plot für die Residuen
plot(ANOVA_Pflanzen_2, 2)
# für eine perfkete Normalverteilung müssten alle standardisierten Residuen auf der Geraden sein...
# ist hier nicht ganz erfüllt, aber OK (Normalverteilung muss nicht perfekt sein...)

