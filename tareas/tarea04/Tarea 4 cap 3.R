
# Ejercicio 1 -------------------------------------------------------------
##Compare dep_time, sched_dep_time, and dep_delay. How would you expect those three numbers to be related?----
flights |> select(dep_time, sched_dep_time, dep_delay)
# Ejercicio 2----
##Brainstorm as many ways as possible to select dep_time, dep_delay, arr_time, and arr_delay from flights.----
flights |> select(dep_time, dep_delay, arr_time, arr_delay)
flights |> select(starts_with("dep_"), starts_with("arr_"))
flights |> select(dep_time:arr_delay, !starts_with("sched"))
# Ejercicio 3----
##What happens if you specify the name of the same variable multiple times in a select() call?----
###No tira error select simplemente ignora la repetición y la columna aparece una sola vez en el resultado, en la posición de su primera mención----
# Ejercicio 4----
##What does the any_of() function do? Why might it be helpful in conjunction with this vector?----
###any_of es otro ayudante de select pensado para seleccionar columnas a partir de un vector de nombres guardado aparte, en vez de escribirlos uno por uno. Su ventaja frente a escribir los nombres a mano es que no tira error si algún nombre del vector no existe como columna en los datos simplemente lo ignora----
# Ejercicio 5----
##Does the result of running the following code surprise you? How do the select helpers deal with upper and lower case by default? How can you change that default?----
flights |> select(contains("TIME"))
###Sí sorprende: aunque escribiste "TIME" en mayúsculas y las columnas están en minúsculas, el código igual las encuentra. Esto pasa porque los ayudantes de selección ignoran mayúsculas/minúsculas por defecto. Para cambiar ese comportamiento y exigir coincidencia exacta, se agrega el argumento ignore.case = FALSE----
# Ejercicio 6----
##Rename air_time to air_time_min to indicate units of measurement and move it to the beginning of the data frame----
flights |> 
  rename(air_time_min = air_time) |> 
  relocate(air_time_min)
# Ejercicio 7----
##Why doesn’t the following work, and what does the error mean?----
flights |> 
  select(tailnum) |> 
  arrange(arr_delay)
###Porque select(tailnum) devuelve un nuevo data frame que contiene únicamente la columna tailnum, select descarta todas las demás columnas que no mencionás. Entonces, cuando el arrange de la segunda línea intenta ordenar por arr_delay, esa columna ya no existe en los datos que le llegan . El error object 'arr_delay' not found es justo eso: R no encuentra ninguna columna con ese nombre. Para que funcione, habría que incluir arr_delay en el select(), o directamente ordenar antes de seleccionar----