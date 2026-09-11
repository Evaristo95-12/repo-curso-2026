#1.5 Visualización de relaciones
##1.5.1 Una variable numérica y una variable categórica
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot()
ggplot(penguins, aes(x = body_mass_g, color = species)) +
  geom_density(linewidth = 0.75)
###También hemos personalizado el grosor de las líneas utilizando el linewidthargumento para que destaquen un poco más sobre el fondo.
###Además, podemos asignar valores speciesa ambas colorestéticas filly usar la alphaestética para añadir transparencia a las curvas de densidad rellenas.
ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) +
  geom_density(alpha = 0.5)
##1.5.2 Dos variables categóricas
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar()
###El segundo gráfico, un gráfico de frecuencia relativa creado mediante la configuración position = "fill"en el geom
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")
###Al crear estos gráficos de barras, asignamos la variable que se separará en barras a la xestética, y la variable que cambiará los colores dentro de las barras a la fillestética. Desafortunadamente, ggplot2 etiqueta el eje y "count"por defecto, pero esto es algo que podemos anular agregando una labs()capa donde especificamos la etiqueta del eje y como "proportion".
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill") +
  labs(y = "proportion")
##1.5.3 Dos variables numéricas
###Hasta ahora has aprendido sobre diagramas de dispersión (creados con geom_point()) y curvas suavizadas (creadas con geom_smooth()) para visualizar la relación entre dos variables numéricas.
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
##1.5.4 Tres o más variables
###En el siguiente diagrama de dispersión, los colores de los puntos representan especies y las formas de los puntos representan islas.
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = island))
### dividir el gráfico en facetas , subgráficos que muestran cada uno un subconjunto de los datos
### Para segmentar tu gráfico por una sola variable, usa facet_wrap()
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)
##Ejericicio 1. El mpgmarco de datos que viene incluido con el paquete ggplot2 contiene 234 observaciones recopiladas por la Agencia de Protección Ambiental de EE. UU. sobre 38 modelos de automóviles. ¿Qué variables mpgson categóricas? ¿Qué variables son numéricas? (Sugerencia: escriba ?mpgpara leer la documentación del conjunto de datos). ¿Cómo puede ver esta información cuando ejecuta mpg?
str(mpg)
### Variable categorica:manufacturer,model,trans,drv,fl,class
### Variable numerica: displ,year,cyl,cty,hwy 
##Ejericicio 2.Crea un diagrama de dispersión de hwyvs.  displusando el mpgmarco de datos. Luego, asigna una tercera variable numérica a color, luego a size, luego a ambas colory size, luego a shape. ¿Cómo se comportan estas estéticas de manera diferente para variables categóricas vs. numéricas?
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()
ggplot(mpg, aes(x = displ, y = hwy, color = cyl)) +
  geom_point()
###Con una variable numérica arma un degradé continuo
ggplot(mpg, aes(x = displ, y = hwy, size = cyl)) +
  geom_point()
### con una variable numérica funciona muy bien, porque el tamaño es algo que naturalmente se puede escalar de menor a mayor.
ggplot(mpg, aes(x = displ, y = hwy, color = cyl, size = cyl)) +
  geom_point()
### Por lo mismo que los puntos anteriores, solo que ahora estan combinados
ggplot(mpg, aes(x = displ, y = hwy, shape = cyl)) +
  geom_point()
### no existe una forma de mapear una variable numérica a formas
##Ejericicio 3. En el diagrama de dispersión de hwyvs.  displ, ¿qué sucede si asignas una tercera variable a linewidth?
ggplot(mpg, aes(x = displ, y = hwy, linewidth = cyl)) +
  geom_point()
ggplot(mpg, aes(x = displ, y = hwy, linewidth = model)) +
  geom_point()
### si pongo una variable categorica arroja un aviso, No se recomienda utilizar el grosor de línea para una variable discreta.
##Ejericicio 4. Crea un diagrama de dispersión de bill_depth_mm vs.  bill_length_mm y colorea los puntos según species. ¿Qué revela agregar color por especie sobre la relación entre estas dos variables? ¿Y qué hay de la faceta por species?
ggplot(
  data = penguins,
  mapping = aes(x = bill_depth_mm, y = bill_length_mm, color = species))
### Esto nos ayuda a ver donde esta situada cada especie en el grafico, asi podemos ver mas facilmente e intuaitivamente donde se acumulan
ggplot(
  data = penguins,
  mapping = aes(x = bill_length_mm, y = bill_depth_mm)
) +
  geom_point() +
  facet_wrap(~species)
### Nos permite ver lo mismo que el color pero de forma mas sencilla
##Ejercicio 5. ¿Qué sucede si asignas la misma variable a múltiples estéticas?
ggplot(mpg, aes(x = displ, y = hwy, color = cyl, size = cyl)) +
  geom_point()
###R puede generar el grafico, lo que pasa es que la informacion queda redundante porque esta expresada en dos formas distintas 
##Ejercicio 6. ¿Por qué aparecen dos leyendas separadas? ¿Cómo lo solucionarías para combinarlas?
ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm, y = bill_depth_mm, 
    color = species, shape = species
  )
) +
  geom_point() +
  labs(color = "Species")
###ggplot2 los trata como si fueran dos leyendas de cosas distintas y las dibuja separadas.
###La solución es agregar también el título de shape en el labs(), con el mismo texto exacto que usaste para color
##Ejercicio 7.Crea los dos siguientes gráficos de barras apiladas. ¿Qué pregunta puedes responder con el primero? ¿Qué pregunta puedes responder con el segundo?
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")
ggplot(penguins, aes(x = species, fill = island)) +
  geom_bar(position = "fill")
###Con el primero puedo responde ¿Que porcentaje hay en cada isla por especie? 
###Con el segundo puedo responde ¿Donde habita cada especie? ¿Como se distribuyen por isla?






