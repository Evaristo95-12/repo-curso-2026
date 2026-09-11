#1.4 Visualización de distribuciones
##1.4.1 Una variable categórica
ggplot(penguins, aes(x = species)) +
  geom_bar()
###En los gráficos de barras de variables categóricas con niveles no ordenados, suele ser preferible reordenar las barras según sus frecuencias. Para ello, es necesario transformar la variable en un factor (Con la funcion fct_infreq()) y luego reordenar los niveles de ese factor.
ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar()
##1.4.2 Una variable numérica
###Una variable es numérica (o cuantitativa) si puede tomar un amplio rango de valores numéricos y es lógico sumar, restar o calcular promedios con dichos valores. Las variables numéricas pueden ser continuas o discretas.
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)
###Puedes ajustar el ancho de los intervalos en un histograma con el argumento `binwidth`, que se mide en las unidades de la xvariable. 
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 60)
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)
###Una visualización alternativa para distribuciones de variables numéricas es un gráfico de densidad. Un gráfico de densidad es una versión suavizada de un histograma y una alternativa práctica, especialmente para datos continuos que provienen de una distribución suave subyacente.Con la funcion geom_density()
ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()
##Ejercicio 1. Crea un gráfico de barras speciesde penguins, donde asignas speciesa la yestética. ¿En qué se diferencia este gráfico?
ggplot(penguins, aes(y = species)) +
   geom_bar() 
##Ejercicio 2. ¿En qué se diferencian los dos gráficos siguientes? ¿Qué estética, coloro fill, es más útil para cambiar el color de las barras?
ggplot(penguins, aes(x = species)) +
  geom_bar(color = "red")

ggplot(penguins, aes(x = species)) +
  geom_bar(fill = "red")
####Es mas util el segundo grafico. Creo que barras rojas son mas utiles que barras grises bordeadas de rojo
##Ejercicio 3. ¿Qué hace el bins argumento geom_histogram()?
### Cambia el ancho de las barras, sobre el eje X. Esto es para que se puedan ver mejor y diferenciar mejor
##Ejercicio 4. Crea un histograma de la caratvariable en el diamondsconjunto de datos disponible al cargar el paquete tidyverse. Experimenta con diferentes anchos de intervalo. ¿Qué ancho de intervalo revela los patrones más interesantes?
library(tidyverse)
ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.18)
###Creo que los patrones mas interesantes se ven con los anchos de 0.12 y 0.18