setwd("~/RProjects/ReprezentacjaPN")

library(tidyverse)
library(rvest)
library(stringr)
library(lubridate)



# theme
theme_set(theme_minimal() +
            theme(plot.title = element_text(family = NULL, face = "bold", size = 18, color = "black"),
                  plot.subtitle = element_text(family = NULL, face = "plain", size = 12, color = "black"),
                  plot.caption = element_text(family = NULL, face = "italic", size = 9, color = "darkgray"),
                  plot.background = element_rect(fill="#efefef", color="#aaaaaa"),
                  panel.background = element_rect(fill = "white", color="black"),
                  strip.text.x = element_text(face = "bold")))

# wczytanie danych z pliku
mecze_df <- readRDS("mecze_df.RDS")

# bez meczy, które dopiero będą
mecze_df <- mecze_df %>% filter(!is.na(gospodarz_bramki))

# bilans bramek miesiąc po miesiącu
mecze_df %>%
  group_by(rok, miesiac) %>%
  mutate(bramki_strata = sum(bramki_stracone)) %>%
  mutate(bramki_zysk = sum(bramki_strzelone)) %>%
  ungroup() %>%
  distinct(rok, miesiac, bramki_strata, bramki_zysk) %>%
  ggplot() +
  geom_bar(aes(make_date(rok, miesiac, 1), bramki_zysk), fill="green", stat = "identity") +
  geom_bar(aes(make_date(rok, miesiac, 1), -bramki_strata), fill="red", stat = "identity") +
  geom_smooth(aes(make_date(rok, miesiac, 1), bramki_zysk-bramki_strata), se=FALSE) +
  labs(title = "Bilans bramek w rozgrywkach polskiej reprezentacji piłki nożnej",
       x = "Data", y = "Bramki (strzelone, stracone)",
       subtitle = "Liczba bramek strzelonych (zielone) i straconych (czerowne) w danym miesiącu.\nLinia niebieska to trend różnicy bramek",
       caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy")


# wyniki z poszczególnymi przeciwnikami
mecze_df %>%
  group_by(przeciwnik) %>%
  mutate(bramki_strata = sum(bramki_stracone)) %>%
  mutate(bramki_zysk = sum(bramki_strzelone)) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  distinct(przeciwnik, bramki_strata, bramki_zysk, n) %>%
  mutate(roznica = bramki_zysk - bramki_strata) %>%
  arrange(roznica) %>%
  mutate(przeciwnik = paste0(przeciwnik, " (", n, ")")) %>%
  mutate(przeciwnik = factor(przeciwnik, levels = przeciwnik)) %>%
  ggplot() +
  geom_bar(aes(przeciwnik,
               roznica,
               fill = ifelse(roznica < 0, "red", "green")),
           stat = "identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("green" = "green", "red" = "red")) +
  coord_flip() +
  labs(x = "Przeciwnik (liczba spotkań)", y = "Przewaga w zdobytych bramkach przez Polskę",
       title = "Komu strzelamy więcej niż tracimy?",
       subtitle = "Różnica w bramkach strzelonych i straconych przez reprezentację Polski\nw piłce nożnej we wszystkich spotkaniach z danym przeciwnikiem",
       caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy")




# wyniki z poszczególnymi przeciwnikami
mecze_df %>%
  filter(przeciwnik == "Dania") %>%
  mutate(roznica = bramki_strzelone - bramki_stracone) %>%
  ggplot() +
  geom_bar(aes(data_meczu, bramki_strzelone), fill = "green", stat="identity") +
  geom_bar(aes(data_meczu, -bramki_stracone), fill = "red", stat="identity") +
  geom_smooth(aes(data_meczu, roznica), se=FALSE) +
  geom_point(aes(data_meczu, roznica)) +
  labs(x = "", y = "Liczba bramek",
       title = "Historia rozgrywek z Danią",
       subtitle = "Bramki strzelone przez Reprezentację (zielone) i stracone (czerwone).\nCzarne punkty to różnica z punktu widzenia Polski, niebieska linia to trend różnicy",
       caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy")


# średnie wyniki z przeciwnikami
mecze_df %>%
  group_by(przeciwnik) %>%
  summarise(strata = mean(bramki_stracone), zysk = mean(bramki_strzelone)) %>%
  ungroup() %>%
  arrange(zysk, desc(strata)) %>%
  mutate(przeciwnik=factor(przeciwnik, levels=przeciwnik)) %>%
  ggplot() +
  geom_bar(aes(przeciwnik, -strata), fill="red", stat="identity") +
  geom_bar(aes(przeciwnik, zysk), fill="green", stat="identity") +
  coord_flip()

# średnie wyniki rok po roku
mecze_df %>%
  group_by(rok) %>%
  summarise(strata = mean(bramki_stracone), zysk = mean(bramki_strzelone)) %>%
  ungroup() %>%
  ggplot() +
  geom_bar(aes(rok, -strata), fill="red", stat="identity") +
  geom_bar(aes(rok, zysk), fill="green", stat="identity") +
  geom_smooth(aes(rok, zysk-strata))




# popularność wyników
mecze_df %>%
  count(bramki_stracone, bramki_strzelone) %>%
  ggplot() +
  geom_tile(aes(bramki_stracone, bramki_strzelone, fill = n), color = "white") +
  scale_x_continuous(breaks = 0:10) +
  scale_y_continuous(breaks = 0:10)

