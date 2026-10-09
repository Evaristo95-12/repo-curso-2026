install.packages(c("tidyverse", "tidytext", "textdata", "wordcloud",
                   "reshape2", "topicmodels", "igraph", "ggraph", "readxl"))

library(dplyr)       # [CLASE] filter, count, mutate, group_by, joins
library(stringr)     # [CLASE] str_detect (buscar patrones en texto)
library(tidyr)       # [CLASE] pivot_wider, separate
library(ggplot2)     # [CLASE] gráficos
library(readr)       # leer el .csv
library(tidytext)    # [CLASE] unnest_tokens, get_stopwords, get_sentiments
library(wordcloud)   # [CLASE] nubes de palabras
library(reshape2)    # [CLASE] acast, para comparison.cloud
library(topicmodels) # (nuevo) topic modelling
library(igraph)      # (nuevo) redes
library(ggraph)      # (nuevo) dibujar redes

#Carga de datos------------------------------------------------------------------------------------------------------------------

ruta  <- file.choose()        # abre una ventana: elegí el archivo .csv
datos <- read_csv(ruta)       # lee el .csv y lo guarda como "datos"

glimpse(datos)   # MIRÁ: cuántas filas (notas) hay y qué columnas tiene


#Armo una tabla llamada nota---------------------------------------------------------------------------------------------------------

notas <- datos |>
  mutate(
    id   = row_number(),                            # número de nota: 1, 2, 3...
    anio = as.numeric(format(as.Date(date), "%Y")), # año, sacado de la fecha
    text = str_replace_all(article_text, "’", "'")  # unifica apóstrofes (’ -> ')
  ) |>
  select(id, title, date, anio, text)

notas

##Veo si hay texto que se repita mucho-----

renglones <- notas |>
  unnest_tokens(output = renglon, input = text, token = "lines", to_lower = FALSE) |>
  mutate(renglon = str_trim(renglon)) |>
  filter(renglon != "")

renglones_repetidos <- renglones |>
  distinct(id, renglon) |>               # cada renglón una vez por nota
  count(renglon, sort = TRUE, name = "notas_donde_aparece") |>
  filter(notas_donde_aparece >= 3)       # aparece en 3 notas o más

renglones_repetidos   # >>> MIRÁ: estos son los renglones de plantilla.
#     Describilos en la tarea (¿qué son? ¿saludos? ¿firmas?)

# Rearmamos cada nota sin esos renglones
notas <- renglones |>
  anti_join(renglones_repetidos, by = "renglon") |>
  group_by(id) |>
  summarize(text = paste(renglon, collapse = "\n")) |>
  right_join(select(notas, id, title, date, anio), by = "id") |>
  mutate(text = coalesce(text, "")) |>   # por si alguna nota quedó vacía
  arrange(id)

#Tokenizo palabras por filas----------------------------------------------------------------------------------------------------

tidy_notas <- notas |>
  unnest_tokens(output = word, input = text)

tidy_notas    # cada fila es ahora una palabra, con el id de su nota
# Palabras vacías.
cleaned_notas <- tidy_notas |>
  anti_join(get_stopwords(), by = "word") |>
  filter(!str_detect(word, "[0-9]"))      # saca palabras con números (años, cifras)

# ¿Hay palabras que aparecen en CASI TODAS las notas?
# Una palabra que está en el 70% de las notas o más no sirve para
# distinguir una nota de otra (suele ser el nombre de la empresa, del
# público al que apunta, etc.). Las buscamos con los datos:
total_notas <- n_distinct(notas$id)

palabras_omnipresentes <- cleaned_notas |>
  distinct(id, word) |>
  count(word, sort = TRUE, name = "notas_donde_aparece") |>
  mutate(proporcion = notas_donde_aparece / total_notas) |>
  filter(proporcion >= 0.7)

palabras_omnipresentes
cleaned_notas <- cleaned_notas |>
  anti_join(palabras_omnipresentes, by = "word")
stopwords_todas <- get_stopwords() |>
  bind_rows(select(palabras_omnipresentes, word))

cleaned_notas <- cleaned_notas |>
  anti_join(palabras_omnipresentes, by = "word")

#PREGUNTAS---------------------------------------------------------------------------------------------------------------------------


##Pregunta 1 ¿De qué se trata el corpus? ¿Son comparables los documentos?----------------------------------------------------------------------------------------------------------------------------
###Período que cubre y cuántas notas hay por año----
notas |>
  count(anio) |>
  ggplot(aes(anio, n)) +
  geom_col() +
  labs(title = "Notas por año", x = NULL, y = "Cantidad de notas")
###Largo de cada nota-----
wordcounts <- tidy_notas |>
  group_by(id) |>
  summarize(words = n())
####Hay un total de 146 observacion en 2 variables----
summary(wordcounts$words)   # >>> MIRÁ: mínimo, mediana, promedio y máximo

ggplot(wordcounts, aes(words)) +
  geom_histogram(bins = 30) +
  labs(title = "Largo de las notas", x = "Palabras por nota", y = "Cantidad de notas")
####Notas mas largas y mas cortas----
wordcounts |>
  left_join(select(notas, id, title, date), by = "id") |>
  arrange(words)
#¿hay notas muy distintas al resto? Si los largos varían mucho,conviene comparar proporciones (como el "ratio" de clase) y no conteos absolutos 

###Las palabras más comunes----
cleaned_notas |>
  count(word, sort = TRUE)

##PREGUNTA 2: Aspectos destacados del corpus-----------------------------------------------------------------------------------------------
####Gráfico de las 20 palabras más frecuentes----
cleaned_notas |>
  count(word, sort = TRUE) |>
  slice_max(n, n = 20) |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(n, word)) +
  geom_col() +
  labs(title = "Palabras más frecuentes", x = "Frecuencia", y = NULL)
####Nube de palabras----
cleaned_notas |>
  count(word) |>
  with(wordcloud(word, n, max.words = 100))
####¿Cambian los temas con los años? Palabras más características de cada año----
cleaned_notas |>
  count(anio, word) |>
  bind_tf_idf(word, anio, n) |>
  group_by(anio) |>
  slice_max(tf_idf, n = 10, with_ties = FALSE) |>
  ungroup() |>
  mutate(word = reorder_within(word, tf_idf, anio)) |>
  ggplot(aes(tf_idf, word, fill = factor(anio))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(anio), scales = "free") +
  scale_y_reordered() +
  labs(title = "Palabras más características de cada año (TF-IDF)", x = "tf-idf", y = NULL)
##PREGUNTA 3: Sentimiento---------------------------------------------------------------------------------------------------------------------
####Palabra es positive o negative----
bing <- get_sentiments("bing")
bing_word_counts <- tidy_notas |>
  inner_join(bing, by = "word", relationship = "many-to-many") |>
  count(word, sentiment, sort = TRUE)

bing_word_counts   # >>> MIRÁ las primeras 20 o 30. Para cada una preguntate:
#     ¿en estas notas expresa una opinión, o es solo un
#     tema o una palabra con otro significado?

# Si encontrás palabras mal clasificadas, escribilas acá entre comillas,
# separadas por comas. Ejemplo:  palabras_a_excluir <- c("palabra1", "palabra2")
# Si no encontrás ninguna, dejalo vacío así:
palabras_a_excluir <- c()

bing <- bing |>
  filter(!word %in% palabras_a_excluir)
notas_sentiment <- tidy_notas |>
  inner_join(bing, by = "word", relationship = "many-to-many") |>
  count(id, anio, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  mutate(sentiment = positive - negative)
ggplot(notas_sentiment, aes(id, sentiment, fill = factor(anio))) +
  geom_col() +
  labs(title = "Sentimiento por nota (Bing)", x = "Nota",
       y = "Palabras positivas - negativas", fill = "Año")
# Palabras que más aportan a cada sentimiento (ya con el ajuste)

tidy_notas |>
  inner_join(bing, by = "word", relationship = "many-to-many") |>
  count(word, sentiment, sort = TRUE) |>
  group_by(sentiment) |>
  slice_max(n, n = 10) |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(sentiment), scales = "free_y") +
  labs(x = "Contribución al sentimiento", y = NULL)
# Nube comparativa positivo vs negativo
tidy_notas |>
  inner_join(bing, by = "word") |>
  count(word, sentiment, sort = TRUE) |>
  acast(word ~ sentiment, value.var = "n", fill = 0) |>
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 100)
# Notas más negativas, normalizado por largo
bingnegative <- bing |>
  filter(sentiment == "negative")
tidy_notas |>
  semi_join(bingnegative, by = "word") |>
  group_by(id) |>
  summarize(negativewords = n()) |>
  left_join(wordcounts, by = "id") |>
  mutate(ratio = negativewords / words) |>
  left_join(select(notas, id, title, date), by = "id") |>
  arrange(desc(ratio))  
####graduaciones ----

afinn <- get_sentiments("afinn") |>
  filter(!word %in% palabras_a_excluir)   # mismo ajuste que con Bing

notas_afinn <- tidy_notas |>
  inner_join(afinn, by = "word") |>
  group_by(id, anio) |>
  summarize(sentiment_afinn = sum(value))

ggplot(notas_afinn, aes(id, sentiment_afinn, fill = factor(anio))) +
  geom_col() +
  labs(title = "Sentimiento por nota (AFINN)", x = "Nota",
       y = "Puntaje total", fill = "Año")
# Comparar los dos diccionarios: ¿dan resultados parecidos?
comparacion <- inner_join(notas_sentiment, notas_afinn, by = c("id", "anio"))
cor(comparacion$sentiment, comparacion$sentiment_afinn)
# Promedio por año con cada diccionario
comparacion |>
  group_by(anio) |>
  summarize(bing = mean(sentiment), afinn = mean(sentiment_afinn))

##PREGUNTA 4: Topic modelling con k = 10 y k = 15-------------------------------------------------------------------------------------------

# El modelo LDA necesita una tabla "notas x palabras" con los conteos.
# cast_dtm() la arma a partir de nuestra tabla ordenada.
dtm <- cleaned_notas |>
  count(id, word) |>
  cast_dtm(id, word, n)
lda_10 <- LDA(dtm, k = 10, control = list(seed = 1234))
tidy(lda_10, matrix = "beta") |>
  group_by(topic) |>
  slice_max(beta, n = 8, with_ties = FALSE) |>
  ungroup() |>
  mutate(term = reorder_within(term, beta, topic)) |>
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(topic), scales = "free") +
  scale_y_reordered() +
  labs(title = "Tópicos, k = 10", y = NULL)
lda_15 <- LDA(dtm, k = 15, control = list(seed = 1234))

tidy(lda_15, matrix = "beta") |>
  group_by(topic) |>
  slice_max(beta, n = 8, with_ties = FALSE) |>
  ungroup() |>
  mutate(term = reorder_within(term, beta, topic)) |>
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(topic), scales = "free") +
  scale_y_reordered() +
  labs(title = "Tópicos, k = 15", y = NULL)
temas_10 <- tidy(lda_10, matrix = "gamma") |>
  group_by(document) |>
  slice_max(gamma, n = 1) |>
  ungroup() |>
  mutate(id = as.integer(document)) |>
  left_join(select(notas, id, title), by = "id")

temas_10 |> count(topic)                  # cuántas notas por tópico
temas_10 |> arrange(topic) |> select(topic, title) |> print(n = 50)


##PREGUNTA 5 (AVANZADO): Redes de bigramas-----------------------------------------------------------------------------------------------------
bigramas <- notas |>
  unnest_tokens(output = bigram, input = text, token = "ngrams", n = 2) |>
  filter(!is.na(bigram)) |>
  separate(bigram, c("word1", "word2"), sep = " ")   # separa el par en 2 columnas
####Bigramas más frecuentes----
bigram_counts <- bigramas |>
  filter(!word1 %in% stopwords_todas$word,
         !word2 %in% stopwords_todas$word,
         !str_detect(word1, "[0-9]"),
         !str_detect(word2, "[0-9]")) |>
  count(word1, word2, sort = TRUE)

bigram_counts      # >>> MIRÁ: los pares de palabras más usados

# Red con los 50 bigramas más frecuentes: cada palabra es un punto y cada
# flecha une palabras que van juntas. Si querés más o menos, cambiá el 50.
set.seed(2024)
bigram_counts |>
  slice_max(n, n = 50, with_ties = FALSE) |>
  graph_from_data_frame() |>
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = arrow(length = unit(2, "mm")), end_cap = circle(2, "mm")) +
  geom_node_point(color = "lightblue", size = 4) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void() +
  labs(title = "Red de bigramas más frecuentes")
####Bigramas con negaciones----

negation_words <- c("not", "no", "never", "without")

bigramas |>
  filter(word1 %in% negation_words) |>
  count(word1, word2, sort = TRUE)   # >>> MIRÁ: todas las negaciones

negated_words <- bigramas |>
  filter(word1 %in% negation_words) |>
  inner_join(afinn, by = c(word2 = "word")) |>   # solo las que tienen sentimiento
  count(word1, word2, value, sort = TRUE)

negated_words

negated_words |>
  mutate(contribution = n * value) |>
  slice_max(abs(contribution), n = 20, with_ties = FALSE) |>
  mutate(word2 = paste(word1, word2),
         word2 = reorder(word2, contribution)) |>
  ggplot(aes(contribution, word2, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Palabras precedidas por una negación",
       x = "Sentimiento x frecuencia (AFINN)", y = NULL)

# Red de negaciones (los 40 pares más frecuentes)
set.seed(2024)
bigramas |>
  filter(word1 %in% negation_words, !word2 %in% stopwords_todas$word) |>
  count(word1, word2, sort = TRUE) |>
  slice_max(n, n = 40, with_ties = FALSE) |>
  graph_from_data_frame() |>
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE) +
  geom_node_point(color = "salmon", size = 4) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  labs(title = "Red de bigramas con negaciones")















