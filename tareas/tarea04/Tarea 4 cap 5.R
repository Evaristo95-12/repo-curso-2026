
# Ejercicio 1 -------------------------------------------------------------
##Which carrier has the worst average delays? Challenge: can you disentangle the effects of bad airports vs. bad carriers? Why/why not? (Hint: think about flights |> group_by(carrier, dest) |> summarize(n()))----
flights |>
  group_by(carrier) |>
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE)) |>
  arrange(desc(avg_delay))
# Ejercicio 2 ----
##Find the flights that are most delayed upon departure to each destination.----
flights |>
  group_by(dest) |>
  slice_max(dep_delay, n = 1) |>
  relocate(dest)
# Ejercicio 3 ----
##How do delays vary over the course of the day? Illustrate your answer with a plot.----
flights |>
  group_by(hour) |>
  summarize(dep_delay = mean(dep_delay, na.rm = TRUE)) |>
  ggplot(aes(x = hour, y = dep_delay)) +
  geom_line() +
  geom_point()
# Ejercicio 4----
##What happens if you supply a negative n to slice_min() and friends?----
###Con un n negativo, en vez de devolverte las n filas con el valor más chico o más grande, te devuelve todas las filas excepto esas n----
# Ejercicio 5----
##Explain what count() does in terms of the dplyr verbs you just learned. What does the sort argument to count() do?----
###agrupa por las columnas que le pases y cuenta cuántas filas hay en cada grupo. El argumento sort = TRUE hace que el resultado quede ordenado de forma descendente por esa cantidad----
# Ejercicio 6----
##A)Write down what you think the output will look like, then check if you were correct, and describe what group_by() does.----
###Agrega a la tabla la etiqueta de agrupamiento.group_by por sí solo no hace nada visible sobre los datos, solo prepara el terreno para que los verbos siguientes----
##B)Write down what you think the output will look like, then check if you were correct, and describe what arrange() does. Also, comment on how it’s different from the group_by() in part (a).----
###La diferencia con group_by. Arrange reordena físicamente las filas, mientras que group_by no toca el orden ni los datos, solo marca los grupos para usarlos después----
##C)Write down what you think the output will look like, then check if you were correct, and describe what the pipeline does.----
###Acá sí cambia una fila por cada valor único de y, con el promedio de x dentro de ese grupo----
##D)Write down what you think the output will look like, then check if you were correct, and describe what the pipeline does. Then, comment on what the message says.----
###Ahora agrupa por la combinación de y y z. El mensaje aparece porque, al agrupar por dos variables, summarize() solo saca el último nivel de agrupamiento z y el resultado queda todavía agrupado por y----
##E)Write down what you think the output will look like, then check if you were correct, and describe what the pipeline does. How is the output different from the one in part (d)?----
###Mismos 3 valores calculados, pero ahora el resultado queda completamente sin agrupar y no aparece el mensaje de aviso porque se ordeno que hacer con ese argumento----
##F)Write down what you think the outputs will look like, then check if you were correct, and describe what each pipeline does. How are the outputs of the two pipelines different?----
###La primera canalización da el mismo resultado de 3 filas del punto d. La segunda no colapsa nada----