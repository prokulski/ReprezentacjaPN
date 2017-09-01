setwd("~/RProjects/ReprezentacjaPN")

library(tidyverse)
library(rvest)
library(stringr)
library(lubridate)

url_base <- "http://www.hppn.pl/reprezentacja/mecze/rok-"

mecze_df <- data_frame()

for(rok in 1921:2017) {
  page <- read_html(sprintf("%s%d", url_base, rok))

  mecze <- page %>% html_nodes("div.national-match-area")

  # daty meczy
  data_meczu <- mecze %>% html_node("div.nmTop") %>% html_text() %>% trimws() %>% str_sub(1, 10) %>% dmy()

  # rodzaj meczu
  rodzaj <- mecze %>% html_node("div.nmTop") %>% html_node("nobr") %>% html_text()

  # miejsce
  # miejsce <- mecze %>% html_node("div.nmTop") %>% html_text() %>% trimws() %>% str_sub(12, -1) %>% str_extract("\r([^\r])+\r") %>% str_replace_all("[\n\r\t]", "") %>% trimws()

  # home team
  gospodarz <- mecze %>% html_node("div.match") %>% html_node("div.home-team") %>% html_text()

  # visitor-team
  gosc <- mecze %>% html_node("div.match") %>% html_node("div.visitor-team") %>% html_text()

  # result
  wynik <- mecze %>% html_node("div.match") %>% html_node("div.result") %>% html_text() %>% trimws()


  mecze_df <- bind_rows(mecze_df,
                        data_frame(data_meczu, gospodarz, gosc, wynik, rodzaj))
}

# dla brakujcych lat dostajemy informacje z 2017 roku - stad duble, ktore trzeba usunac
mecze_df <- distinct(mecze_df)

mecze %>% html_node("div.nmTop") %>% html_text() %>% trimws() %>% str_extract("\n([^\n])+\n") %>% str_replace_all("[\n\r\t]", "") %>% trimws


# poprawka karnych
mecze_df$wynik <- ifelse(str_detect(mecze_df$wynik, "k. "),
                         str_extract(mecze_df$wynik, "\\(k. [0-9:]+\\)") %>%
                           str_replace("\\(k. ", "") %>%
                           str_replace("\\)", ""),
                         mecze_df$wynik)

# poprawka dogrywek
mecze_df$wynik <- ifelse(str_detect(mecze_df$wynik, "d. "),
                         str_extract(mecze_df$wynik, "\\(d. [0-9:]+\\)") %>%
                           str_replace("\\(d. ", "") %>%
                           str_replace("\\)", ""),
                         mecze_df$wynik)

# rozdzielenie bramek
mecze_df <- mecze_df %>%
  separate(wynik, c("gospodarz_bramki", "gosc_bramki"), sep=":")

mecze_df$gospodarz_bramki <- as.numeric(mecze_df$gospodarz_bramki)
mecze_df$gosc_bramki <- as.numeric(mecze_df$gosc_bramki)

# bramki strzelone i stracone przez Polskę
mecze_df <- mecze_df %>%
  mutate(bramki_strzelone = ifelse(gospodarz == "Polska", gospodarz_bramki, gosc_bramki),
         bramki_stracone = ifelse(gospodarz == "Polska", gosc_bramki, gospodarz_bramki))

# przeciwnik
mecze_df$przeciwnik <- ifelse(mecze_df$gospodarz == "Polska", mecze_df$gosc, mecze_df$gospodarz)

# rozbicie dat - przydatne do agregacji
mecze_df <- mecze_df %>%
  mutate(rok = year(data_meczu),
         miesiac = month(data_meczu),
         dzien = day(data_meczu))


# zapisanie danych w pliku
saveRDS(mecze_df, file = "mecze_df.RDS")
