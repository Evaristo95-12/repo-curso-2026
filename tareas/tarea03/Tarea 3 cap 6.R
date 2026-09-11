#1.6 Guardar tus parcelas
##es para exportarlo como imagen
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
ggsave(filename = "penguin-plot.png")
##Ejercicio 1. Ejecuta las siguientes líneas de código. ¿Cuál de los dos gráficos se guarda como mpg-plot.png? ¿Por qué?
ggplot(mpg, aes(x = class)) +
  geom_bar()
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point()
ggsave("mpg-plot.png")
###Se guarda el segundo porque tiene el comando de guardado ggsave
##Ejercicio 2. ¿Qué necesitas cambiar en el código anterior para guardar el gráfico como PDF en lugar de PNG? ¿Cómo podrías averiguar qué tipos de archivos de imagen funcionarían en ggsave()?
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
ggsave(filename = "penguin-plot.pdf")
###ya confirme que se guardo correctamente como pdf. Para saberlo ejecute en la consola el comando getwd()