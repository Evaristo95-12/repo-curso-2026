library(tidyverse)
install.packages('palmerpenguins')
install.packages('ggthemes')
library(ggthemes)
library('palmerpenguins')
#Estos comando son para visualizar como esta compuesto penguins
palmerpenguins::penguins
penguins
# el comando glimpse(penguins) me va a permitir visulizar las variables de otra manera 
glimpse(penguins)
#row son filar y columns columnas
#para obtener mas informacion tengo que poner en la consola ?penguins 
#ahora voy a crear el grafico con las variables Body mass, flipper length y la especie. Para esto voy a usar el comando ggplot
ggplot(data = penguins)
#Sale un grafico en gris si solo lo hago correr asi para poder verlo bien voy a tener que hacerle arreglos
#Primero voy a modificar la estetica con el comando mapping. El mapping argumento siempre se define en la aes()función, y los xargumentos yy aes()especifican qué variables se asignarán a los ejes x e y
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
)
#ahora ya se ve una cuadricula pero aun se ve el grafico en gris
# Estos objetos geométricos están disponibles en ggplot2 con funciones que comienzan con geom_. geoms de barra ( geom_bar());eoms de línea ( geom_line());geoms de diagrama de caja ( geom_boxplot());geoms de punto ( geom_point())
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point()
#Hay que tener cuidado porque geom aca contine outside o valores perdidos
#ahora voy a agregar la variable de la species al grafico y la voy a diferenciar por colores
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point()
#Me volvio a salir el mismo aviso que antes
#ahora veo el gradico con las tres especies, la masa corporal y el largo de las aletas
#Ahora agreguemos una capa más: una curva suave que muestre la relación entre la masa corporal y la longitud de las aletas.
#añadiremos un nuevo geom como capa sobre nuestro geom de punto: geom_smooth(). Y especificaremos que queremos dibujar la línea de mejor ajuste basándonos en un modelo llineal mcon method = "lm".
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point() +
  geom_smooth(method = "lm")
#Ahora veo el grafico anterior pero con tres linea que reprefentan la regresion sobre cada especie
#Como queremos que los puntos se coloreen según la especie, pero no queremos que las líneas se separen para ellas, debemos especificarlo solo color = speciespara geom_point().
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species)) +
  geom_smooth(method = "lm")
#Aún necesitamos usar formas diferentes para cada especie de pingüino y mejorar las etiquetas.
# Por lo tanto, además del color, también podemos recurrir speciesa la shapeestética.
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm")
#Ahora veo las especies representadas por colores y formas geometricas
#Algunos de los argumentos de labs()pueden ser autoexplicativos: titleagrega un título y subtitleagrega un subtítulo al gráfico. Otros argumentos coinciden con las asignaciones estéticas, xes la etiqueta del eje x, yes la etiqueta del eje y, y colory shapedefinen la etiqueta para la leyenda. Además, podemos mejorar la paleta de colores para que sea segura para daltónicos con la scale_color_colorblind()función del paquete ggthemes.
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()
#ejericio 1. ¿Cuántas filas tiene penguins? ¿Cuántas columnas?
## tiene 344 filas y 8 columnas
#ejercicio 2. ¿Qué describe la bill_depth_mmvariable en el penguinsmarco de datos?.
## un número que indica la profundidad del pico
#ejercicio 3. Crea un diagrama de dispersión de bill_depth_mmvs.  bill_length_mm. Es decir, crea un diagrama de dispersión con bill_depth_mmen el eje y y bill_length_mmen el eje x. Describe la relación entre estas dos variables.
ggplot(
  data = penguins,
  mapping = aes(x = bill_depth_mm, y = bill_length_mm)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "bill_depth_mm vs bill_length_mm",
    x = "bill_depth_mm", y = "bill_length_mm",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()
#ejercicio 4. ¿Qué sucede si haces un diagrama de dispersión de species vs.  bill_depth_mm? ¿Cuál podría ser una mejor opción de geom?
ggplot(
  data = penguins,
  mapping = aes(x = species, y = bill_depth_mm)
) +
  geom_point(aes(color = species, shape = species)) 
## lo que pasa es que se ven solo unas lineas verticales las cuales no aportan mucha informacion. Lo mejor en este caso seria usa un grafico de cajas 
ggplot(
  data = penguins,
  mapping = aes(x = species)
) +
  geom_bar(aes(color = species, shape = species)) 
#Ejercicio 5. ¿Por qué se produce el siguiente error y cómo lo solucionarías?
ggplot(data = penguins) + 
  geom_point()
##lo que pasa es que no especificaste los parametros. Tenes que poner minimamente mapping y especificar x o y, esto como minimo y se soliciona. Sino no sabe que tomar como parametros 
#Ejercicio 6.¿Qué hace el na.rm argumento en geom_point()? ¿Cuál es el valor predeterminado del argumento? Crea un diagrama de dispersión donde uses correctamente este argumento configurado en TRUE.
##El argumento na.rm en geom_point() (y en la mayoría de los geom_* de ggplot2) controla qué hace la función cuando hay valores faltantes (NA) en las variables que estás graficando.
##Con na.rm = TRUE, pasa exactamente lo mismo (las filas con NA se excluyen del gráfico), pero ggplot2 lo hace en silencio, sin mostrarte esa advertencia. Es útil cuando ya sabés que tenés NA en tus datos y no querés que la consola se llene de warnings cada vez que corrés el gráfico.
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(na.rm = TRUE)
#Ejercicio 7.Añade el siguiente título al gráfico que hiciste en el ejercicio anterior: “Los datos provienen del paquete palmerpenguins”. Sugerencia: Consulta la documentación de labs().
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(na.rm = TRUE) +
  labs(
    title = "Los datos provienen del paquete palmerpenguins")
#Ejercicio 8. Recrea la siguiente visualización. ¿A qué estética se le debe bill_depth_mm asignar? ¿Y se debe asignar a nivel global o a nivel geométrico?
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = bill_depth_mm)) +
  geom_smooth(method = "loess") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "bill_depth_mm"
  ) 
#Ejercicio 9.Ejecuta este código mentalmente y predice cómo será el resultado. Luego, ejecuta el código en R y comprueba tus predicciones.
## Este codigo haria un grafico con los ejes X= Flipper e y= Body, le pondria colores por a la variable isla y seria un grafico de puntos. Smooth lo que haria seria quitar lo NA y te avisa donde los quita
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
) +
  geom_point() +
  geom_smooth(se = FALSE)
#no pense que pondria linea de tendencia
#Ejercicio 10. ¿Se verán diferentes estos dos gráficos? ¿Por qué sí o por qué no?
# No se tnedrian que ver diferentes. Si bien el segundo es mas largo, lo que pasa es que no usa mapping por lo que tiene que definir todas las variablen en geom
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point() +
  geom_smooth()

ggplot() +
  geom_point(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  ) +
  geom_smooth(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  )
#Como indique no se ven diferentes cuando uno los corre 


