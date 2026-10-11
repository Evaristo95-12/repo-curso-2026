install.packages(c("edgar", "dplyr", "stringr", "tidyr", "tidytext", "textdata", "ggplot2"))

library(edgar)      # descarga reportes de la SEC (lo pide la consigna)
library(dplyr)      # filtrar, agrupar, contar, unir tablas
library(stringr)    # trabajar con texto (str_detect, etc.)
library(tidyr)      # pivot_wider (dar vuelta tablas)
library(tidytext)   # unnest_tokens, get_stopwords, get_sentiments, tf-idf
library(ggplot2)    # gráficos

#En EDGAR cada empresa tiene un número de identificación: el CIK.
# Estos los verifiqué en sec.gov. Las 5 cotizan en el NYSE.

empresas <- tibble(
  ticker = c("YPF",    "PAM",     "TEO",    "BMA",     "TGS"),
  cik    = c("904851", "1469395", "932470", "1347426", "931427")
)
empresas

#DESCARGAR LOS REPORTES ANUALES CON edgar

ua <- "Evaristo Bardullas eebardullas@gmail.com"   # <-- CAMBIAR por tus datos
getFilings(
  cik.no       = empresas$cik,
  form.type    = "20-F",
  filing.year  = 2026,
  quarter      = c(1, 2),
  downl.permit = "y",
  useragent    = ua
)

#LEER LOS ARCHIVOS DESCARGADOS

archivos <- list.files(pattern = "20-F", recursive = TRUE, full.names = TRUE)
archivos
leer <- function(archivo) {
  tibble(archivo = archivo,
         text    = readLines(archivo, warn = FALSE, skipNul = TRUE))
}

lineas <- bind_rows(lapply(archivos, leer))

#PREPARAR EL TEXTO

reportes <- lineas |>
  mutate(cik = str_extract(basename(archivo), "^[0-9]+")) |>
  left_join(empresas, by = "cik") |>
  group_by(ticker) |>
  mutate(documento = cumsum(str_detect(text, "<DOCUMENT>"))) |>
  ungroup() |>
  filter(documento == 1) |>
  mutate(text = str_remove_all(text, "<[^>]+>"))

#TOKENIZAR: UNA PALABRA POR FILA

tidy_reportes <- reportes |>
  unnest_tokens(output = word, input = text) |>
  filter(!str_detect(word, "[0-9]"))
mis_stopwords <- bind_rows(
  get_stopwords(),
  tibble(word = c("nbsp", "amp", "quot", "lt", "gt"))
)

cleaned_reportes <- tidy_reportes |>
  anti_join(mis_stopwords)

#ANÁLISIS DE TEXTO

simples <- c("inflation", "devaluation", "default", "restructuring", "imf", "embi")
dobles  <- c("exchange controls", "capital controls", "price controls", "country risk")

bigramas <- reportes |>
  unnest_tokens(output = bigram, input = text, token = "ngrams", n = 2)
incidencia <- bind_rows(
  tidy_reportes |> filter(word %in% simples) |> count(ticker, termino = word),
  bigramas      |> filter(bigram %in% dobles) |> count(ticker, termino = bigram))
wordcounts <- tidy_reportes |>
  group_by(ticker) |>
  summarize(words = n())
incidencia <- incidencia |>
  left_join(wordcounts, by = "ticker") |>
  mutate(cada_10mil = n / words * 10000)
incidencia |>
  select(ticker, termino, n) |>
  pivot_wider(names_from = termino, values_from = n, values_fill = 0)
ggplot(incidencia, aes(cada_10mil, termino, fill = ticker)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(ticker)) +
  labs(x = "Menciones cada 10.000 palabras", y = NULL,
       title = "Incidencia de términos de riesgo argentino")
tfidf <- cleaned_reportes |>
  count(ticker, word) |>
  bind_tf_idf(word, ticker, n)
tfidf |>
  group_by(ticker) |>
  slice_max(tf_idf, n = 10) |>
  mutate(word = reorder(word, tf_idf)) |>
  ggplot(aes(tf_idf, word, fill = ticker)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(ticker), scales = "free_y") +
  labs(x = "tf-idf", y = NULL, title = "Palabras más distintivas de cada empresa")
 
#ANÁLISIS DE SENTIMIENTO CON LOUGHRAN-McDONALD

loughran <- get_sentiments("loughran")
loughran
sentimientos <- tidy_reportes |>
  inner_join(loughran, relationship = "many-to-many") |>
  count(ticker, sentiment) |>
  left_join(wordcounts, by = "ticker") |>
  mutate(cada_10mil = n / words * 10000)
sentimientos
ggplot(sentimientos, aes(cada_10mil, sentiment, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(ticker)) +
  labs(x = "Palabras cada 10.000", y = NULL,
       title = "Sentimientos en los 20-F (Loughran-McDonald)")
lm_word_counts <- tidy_reportes |>
  inner_join(loughran, relationship = "many-to-many") |>
  count(word, sentiment, sort = TRUE)
lm_word_counts |>
  group_by(sentiment) |>
  slice_max(n, n = 10) |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(sentiment), scales = "free_y") +
  labs(x = "Contribución al sentimiento", y = NULL)

