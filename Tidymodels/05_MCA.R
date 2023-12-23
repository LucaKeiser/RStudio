
# multiple correspondence analysis (MCA) ----------------------------------





# load packages -----------------------------------------------------------
library(tidyverse)
library(FactoMineR)
library(ggrepel)
library(glue)
theme_set(theme_minimal())




# load data ---------------------------------------------------------------
ESS_10 <- read_csv(here::here("datasets",
                              "ESS10",
                              "ESS10.csv")) %>% 
  filter(cntry == "CH")





# EDA ---------------------------------------------------------------------
ESS_10_small <- ESS_10 %>% 
  select(idno, contains("trst"), contains("stf"), 
         imwbcnt, imbgeco, imueclt, loylead, 
         happy, lrscale, agea, gndr)

ESS_10_small %>% 
  glimpse()

ESS_10_small_rec <- ESS_10_small %>% 
  mutate(ppltrst = ifelse(ppltrst <= 5, "No Trust People", "Trust Pepole"),
         trstprl = ifelse(trstprl <= 5, "No Trust Parliament", "Trust Parliament"),
         trstlgl = ifelse(trstlgl <= 5, "No Trust Legal System", "Trust Legal System"),
         trstplc = ifelse(trstplc <= 5, "No Trust Police", "Trust Police"),
         trstplt = ifelse(trstplt <= 5, "No Trust Politicians", "Trust Politicians"),
         trstprt = ifelse(trstprt  <= 5, "No Trust Parties", "Trust Parties"),
         trstep = ifelse(trstep <= 5, "No Trust EU", "Trust EU"),
         trstun = ifelse(trstun <= 5, "No Trust UN", "Trust UN"),
         trstsci = ifelse(trstsci <= 5, "No Trust Science", "Trust Science"),
         stfeco = ifelse(stfeco <= 5, "Not Satisfied Economy", "Satisfied Economy"),
         stfgov = ifelse(stfgov <= 5, "Not Satisfied Government", "Satisfied Government"),
         stfdem = ifelse(stfdem <= 5, "Not Satisfied Democracy", "Satisfied Democracy"),
         stfedu = ifelse(stfedu <= 5, "Not Satisfied Education", "Satisfied Education"),
         stfhlth = ifelse(stfhlth <= 5, "Not Satisfied Health System", "Satisfied Health System"),
         stfmjob = ifelse(stfmjob <= 5, "Not Satisfied Job", "Satisfied Job"),
         stflife = ifelse(stflife <= 5, "Not Satisfied Life", "Satisfied Life"),
         imwbcnt = ifelse(imwbcnt <= 5, "Immigrants Worse Place to Live", "Immigrants Better Place to Live"),
         imbgeco = ifelse(imbgeco <= 5, "Immigrants Bad for Economy", "Immigrants Good for Economy"),
         imueclt = ifelse(imueclt <= 5, "Immigrants Undermine Cultural Life", "Immigrants Enrich Cultural Life"),
         loylead = ifelse(loylead >= 3, "Strong Leader Yes", "Strong Lsader No"),
         happy = ifelse(happy <= 5, "Unhappy", "Happy"),
         lrscale = ifelse(lrscale <= 5, "Left", "Right"),
         agea = case_when(agea < 35 ~ "under 35",
                          between(agea, 35, 55) ~ "between 35 and 55",
                          agea > 55 ~ "above 55"),
         gndr = ifelse(gndr == 1, "Male", "Female")) %>% 
  mutate(across(-idno, ~as_factor(.)))

ESS_10_small_rec %>% 
  skimr::skim()





# create model ------------------------------------------------------------
model <- MCA(ESS_10_small_rec %>% 
               select(-idno),
             graph = FALSE,
             ncp = 4)

knitr::kable(round(model$eig, 2))

m_columns <- tibble(
  variable = rownames(model$var$coord),
  dim_1 = as.numeric(model$var$coord[,1]),
  dim_2 = as.numeric(model$var$coord[,2]),
  dim_3 = as.numeric(model$var$coord[,3]),
  dim_4 = as.numeric(model$var$coord[,4])
)

m_rows <- tibble(
  variable = ESS_10_small_rec$idno,
  dim_1 = as.numeric(model$ind$coord[,1]),
  dim_2 = as.numeric(model$ind$coord[,2]),
  dim_3 = as.numeric(model$ind$coord[,3]),
  dim_4 = as.numeric(model$ind$coord[,4])
)


# Dim 1 and Dim 2
m_rows %>% 
  ggplot(aes(x = dim_1, y = dim_2)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) + 
  geom_jitter(alpha = 0.25,
              width = 0.25,
              height = 0.25) +
  geom_text_repel(aes(label = variable),
                  max.overlaps = 1,
                  size = 2.5) +
  geom_density_2d(color = "grey80") + 
  geom_label_repel(aes(label = variable,
                       fill = variable),
                   max.overlaps = Inf,
                   size = 4,
                   data = m_columns,
                   show.legend = FALSE) + 
  labs(title = "\nMultiple Correspondence Analysis", 
       subtitle = "Data: ESS 10 for Switzerland (n = 1523)\n",
       x = glue("Dimension 1 ({round(model$eig[1,2], 2)}%)"),
       y = glue("Dimension 2 ({round(model$eig[2,2], 2)}%)"))

# Dim 1 and Dim 3
m_rows %>% 
  ggplot(aes(x = dim_1, y = dim_3)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) + 
  geom_jitter(alpha = 0.25,
              width = 0.25,
              height = 0.25) +
  geom_text_repel(aes(label = variable),
                  max.overlaps = 1,
                  size = 2.5) +
  geom_density_2d(color = "grey80") + 
  geom_label_repel(aes(label = variable,
                       fill = variable),
                   max.overlaps = Inf,
                   size = 4,
                   data = m_columns,
                   show.legend = FALSE) + 
  labs(title = "\nMultiple Correspondence Analysis", 
       subtitle = "Data: ESS 10 for Switzerland (n = 1523)\n",
       x = glue("Dimension 1 ({round(model$eig[1,2], 2)}%)"),
       y = glue("Dimension 3 ({round(model$eig[3,2], 2)}%)"))

# Dim 2 and Dim 3
m_rows %>% 
  ggplot(aes(x = dim_2, y = dim_3)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) + 
  geom_jitter(alpha = 0.25,
              width = 0.25,
              height = 0.25) +
  geom_text_repel(aes(label = variable),
                  max.overlaps = 1,
                  size = 2.5) +
  geom_density_2d(color = "grey80") + 
  geom_label_repel(aes(label = variable,
                       fill = variable),
                   max.overlaps = Inf,
                   size = 4,
                   data = m_columns,
                   show.legend = FALSE) + 
  labs(title = "\nMultiple Correspondence Analysis", 
       subtitle = "Data: ESS 10 for Switzerland (n = 1523)\n",
       x = glue("Dimension 2 ({round(model$eig[2,2], 2)}%)"),
       y = glue("Dimension 3 ({round(model$eig[3,2], 2)}%)"))

