# Instalar una sola vez si hace falta:
# install.packages(c("data.table", "ggplot2"))

library(data.table)
library(ggplot2)

# Leer solo las columnas que necesitamos.
# Esto es importante porque el archivo es muy grande.
entidades <- fread(
  "entidadesunificado.csv",
  select = c(
    "numero_correlativo",
    "anio",
    "fecha_corte",
    "dada_de_baja",
    "descripcion_tipo_societario"
  ),
  colClasses = list(
    character = c("numero_correlativo", "fecha_corte")
  ),
  na.strings = c("", "NA")
)

# Si dada_de_baja contiene "S", la entidad esta dada de baja.
# Si esta vacio, la consideramos activa.
entidades[, estado := ifelse(
  !is.na(dada_de_baja) & dada_de_baja == "S",
  "Dadas de baja",
  "Activas"
)]

# Contar cuantas entidades hay por año y por estado.
resumen_anual <- entidades[
  , .(cantidad = .N),
  by = .(anio, estado)
]

# Calcular el porcentaje de cada estado dentro de cada anio.
resumen_anual[, porcentaje := cantidad / sum(cantidad), by = anio]

# Ordenar la leyenda y las partes de cada barra.
resumen_anual[, estado := factor(
  estado,
  levels = c("Activas", "Dadas de baja")
)]

# GRAFICO CANTIDADENTIDADES #######################################################
grafico <- ggplot(
  resumen_anual,
  aes(x = factor(anio), y = cantidad, fill = estado)
) +
  geom_col() +
  geom_text(
    aes(
      label = scales::label_percent(
        accuracy = 0.1,
        decimal.mark = ","
      )(porcentaje)
    ),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3.5
  ) +
  scale_fill_manual(
    values = c(
      "Activas" = "#2E86AB",
      "Dadas de baja" = "#E76F51"
    )
  ) +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Total de entidades desde 2017 a 2026",
    subtitle = "Separadas entre activas y dadas de baja",
    x = "A\u00F1o",
    y = "Cantidad de entidades",
    fill = "Estado"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom"
  )

# Mostrar el grafico en RStudio.
print(grafico)

# Guardar una copia como imagen PNG.
ggsave(
  "cantidadentidadesporcentaje.png",
  plot = grafico,
  width = 10,
  height = 6,
  dpi = 300
)


# GRAFICO TIPODEENTIDADES #######################################################

# Contar las entidades activas de cada tipo en cada anio.
tipos_por_anio <- entidades[
  estado == "Activas" & !is.na(descripcion_tipo_societario),
  .(cantidad = .N),
  by = .(anio, descripcion_tipo_societario)
]

# Buscar los cinco tipos mas numerosos del ultimo anio.
ultimo_anio <- max(tipos_por_anio$anio)

tipos_principales <- tipos_por_anio[
  anio == ultimo_anio
][order(-cantidad)][
  1:5,
  descripcion_tipo_societario
]

# Los tipos que no estan entre los cinco principales se agrupan como OTROS.
tipos_por_anio[, tipo_grafico := ifelse(
  descripcion_tipo_societario %in% tipos_principales,
  descripcion_tipo_societario,
  "OTROS TIPOS"
)]

# Sumar y calcular el porcentaje de cada tipo dentro de cada anio.
evolucion_tipos <- tipos_por_anio[
  , .(cantidad = sum(cantidad)),
  by = .(anio, tipo_grafico)
]

evolucion_tipos[, porcentaje := cantidad / sum(cantidad), by = anio]

# Mantener el mismo orden y los mismos colores en todos los anios.
evolucion_tipos[, tipo_grafico := factor(
  tipo_grafico,
  levels = c(tipos_principales, "OTROS TIPOS")
)]

# Crear el grafico de evolucion porcentual.
grafico_tipos <- ggplot(
  evolucion_tipos,
  aes(x = factor(anio), y = porcentaje, fill = tipo_grafico)
) +
  geom_col() +
  geom_text(
    aes(
      label = ifelse(
        porcentaje >= 0.025,
        scales::label_percent(
          accuracy = 0.1,
          decimal.mark = ","
        )(porcentaje),
        ""
      )
    ),
    position = position_stack(vjust = 0.5),
    color = "black",
    fontface = "bold",
    size = 3
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(
    labels = scales::label_percent(
      accuracy = 1,
      decimal.mark = ","
    )
  ) +
  labs(
    title = "Evoluci\u00F3n de los tipos de entidades activas",
    subtitle = paste(
      "Cinco tipos principales de",
      ultimo_anio,
      "y agrupaci\u00F3n del resto"
    ),
    x = "A\u00F1o",
    y = "Porcentaje de entidades activas",
    fill = "Tipo de entidad"
  ) +
  theme_minimal() +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom"
  )

# Mostrar el segundo grafico en RStudio.
print(grafico_tipos)

# Guardar el segundo grafico.
ggsave(
  "tipodeentidades.png",
  plot = grafico_tipos,
  width = 12,
  height = 8,
  dpi = 300
)

# GRAFICO Tipos de Entidades y CantidadesPorcentaje #######################################################

# Crear los grupos que apareceran dentro de cada barra.
entidades[, grupo_mega := ifelse(
  estado == "Dadas de baja",
  "DADAS DE BAJA",
  ifelse(
    descripcion_tipo_societario %in% tipos_principales,
    descripcion_tipo_societario,
    "OTRAS ACTIVAS"
  )
)]

# Contar las entidades de cada grupo por anio.
resumen_mega <- entidades[
  , .(cantidad = .N),
  by = .(anio, grupo_mega)
]

# Calcular el porcentaje de cada grupo dentro de su anio.
resumen_mega[, porcentaje := cantidad / sum(cantidad), by = anio]

# Calcular el total para escribirlo arriba de cada barra.
totales_mega <- resumen_mega[
  , .(total = sum(cantidad)),
  by = anio
]

# Mantener el mismo orden de grupos en todos los anios.
niveles_mega <- c(
  tipos_principales,
  "OTRAS ACTIVAS",
  "DADAS DE BAJA"
)

resumen_mega[, grupo_mega := factor(
  grupo_mega,
  levels = niveles_mega
)]

# Elegir los colores.
colores_mega <- c(
  setNames(
    c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854"),
    tipos_principales
  ),
  "OTRAS ACTIVAS" = "#FFD92F",
  "DADAS DE BAJA" = "#D73027"
)

# Crear el grafico combinado.
mega_grafico <- ggplot(
  resumen_mega,
  aes(x = factor(anio), y = cantidad, fill = grupo_mega)
) +
  geom_col() +
  geom_text(
    aes(
      label = ifelse(
        porcentaje >= 0.015,
        scales::label_percent(
          accuracy = 0.1,
          decimal.mark = ","
        )(porcentaje),
        ""
      )
    ),
    position = position_stack(vjust = 0.5),
    color = "black",
    fontface = "bold",
    size = 2.8
  ) +
  geom_text(
    data = totales_mega,
    aes(
      x = factor(anio),
      y = total,
      label = scales::label_number(
        big.mark = ".",
        decimal.mark = ","
      )(total)
    ),
    inherit.aes = FALSE,
    vjust = -0.5,
    fontface = "bold",
    size = 3.2
  ) +
  scale_fill_manual(values = colores_mega) +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Cantidad total, estado y tipo de entidades",
    subtitle = paste(
      "La altura muestra el total anual.",
      "Los colores y porcentajes explican su composici\u00F3n."
    ),
    x = "A\u00F1o",
    y = "Cantidad total de entidades",
    fill = "Estado y tipo"
  ) +
  theme_minimal() +
  guides(fill = guide_legend(nrow = 4, byrow = TRUE)) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom"
  )

# Mostrar el mega grafico en RStudio.
print(mega_grafico)

# Guardar el mega grafico.
ggsave(
  "megagrafico.png",
  plot = mega_grafico,
  width = 13,
  height = 9,
  dpi = 300
)


# GRAFICO DE ANTIGUEDAD ########################################################

# Convertir la fecha de corte en una fecha que R pueda calcular.
entidades[, fecha := as.Date(paste0(fecha_corte, "-01"))]

# Darle un numero de orden a cada fecha de corte disponible.
fechas_disponibles <- sort(unique(entidades$fecha))
entidades[, numero_corte := match(fecha, fechas_disponibles)]

# Ordenar el historial de cada entidad desde el corte mas antiguo al mas nuevo.
setorder(entidades, numero_correlativo, numero_corte)

# Marcar si la entidad estaba activa en cada corte.
entidades[, esta_activa := estado == "Activas"]

# Comienza una nueva racha cuando:
# 1. cambia la entidad,
# 2. cambia entre activa y dada de baja, o
# 3. la entidad no aparece en el corte anterior.
entidades[, nueva_racha :=
  numero_correlativo != shift(numero_correlativo) |
  esta_activa != shift(esta_activa) |
  numero_corte != shift(numero_corte) + 1L
]

# La primera fila siempre comienza una racha.
entidades[is.na(nueva_racha), nueva_racha := TRUE]
entidades[, numero_racha := cumsum(nueva_racha)]

# Para cada racha activa, buscar la primera fecha en la que aparece activa.
entidades[
  esta_activa == TRUE,
  inicio_actividad := min(fecha),
  by = numero_racha
]

# Calcular la antiguedad usando dias para respetar los distintos meses de corte.
entidades[
  esta_activa == TRUE,
  antiguedad_anios := as.numeric(fecha - inicio_actividad) / 365.25
]

# Crear las categorias solicitadas.
entidades[, categoria_antiguedad := "BAJA"]

entidades[
  esta_activa == TRUE & antiguedad_anios < 1,
  categoria_antiguedad := "MENOS_1"
]

entidades[
  esta_activa == TRUE & antiguedad_anios >= 1 & antiguedad_anios < 3,
  categoria_antiguedad := "MENOS_3"
]

entidades[
  esta_activa == TRUE & antiguedad_anios >= 3 & antiguedad_anios < 5,
  categoria_antiguedad := "MENOS_5"
]

entidades[
  esta_activa == TRUE & antiguedad_anios >= 5 & antiguedad_anios < 7,
  categoria_antiguedad := "MENOS_7"
]

# Esta ultima categoria incluye siete anios o mas para no dejar casos afuera.
entidades[
  esta_activa == TRUE & antiguedad_anios >= 7,
  categoria_antiguedad := "MAS_7"
]

# Contar cada categoria por anio y calcular su porcentaje.
resumen_antiguedad <- entidades[
  , .(cantidad = .N),
  by = .(anio, categoria_antiguedad)
]

resumen_antiguedad[
  , porcentaje := cantidad / sum(cantidad),
  by = anio
]

# Totales anuales para mostrarlos arriba de las barras.
totales_antiguedad <- resumen_antiguedad[
  , .(total = sum(cantidad)),
  by = anio
]

# Orden de las categorias dentro de las barras.
niveles_antiguedad <- c(
  "MENOS_1",
  "MENOS_3",
  "MENOS_5",
  "MENOS_7",
  "MAS_7",
  "BAJA"
)

resumen_antiguedad[, categoria_antiguedad := factor(
  categoria_antiguedad,
  levels = niveles_antiguedad
)]

# Crear el grafico de antiguedad.
grafico_antiguedad <- ggplot(
  resumen_antiguedad,
  aes(x = factor(anio), y = cantidad, fill = categoria_antiguedad)
) +
  geom_col() +
  geom_text(
    aes(
      label = ifelse(
        porcentaje >= 0.015,
        scales::label_percent(
          accuracy = 0.1,
          decimal.mark = ","
        )(porcentaje),
        ""
      )
    ),
    position = position_stack(vjust = 0.5),
    color = "black",
    fontface = "bold",
    size = 2.8
  ) +
  geom_text(
    data = totales_antiguedad,
    aes(
      x = factor(anio),
      y = total,
      label = scales::label_number(
        big.mark = ".",
        decimal.mark = ","
      )(total)
    ),
    inherit.aes = FALSE,
    vjust = -0.5,
    fontface = "bold",
    size = 3.2
  ) +
  scale_fill_manual(
    values = c(
      "BAJA" = "#D73027",
      "MENOS_1" = "#D9F0FF",
      "MENOS_3" = "#A6CEE3",
      "MENOS_5" = "#6BAED6",
      "MENOS_7" = "#4292C6",
      "MAS_7" = "#2171B5"
    ),
    breaks = c(
      "BAJA",
      "MENOS_1",
      "MENOS_3",
      "MENOS_5",
      "MENOS_7",
      "MAS_7"
    ),
    labels = c(
      "Dada de baja",
      "Menos de 1 a\u00F1o",
      "Menos de 3 a\u00F1os",
      "Menos de 5 a\u00F1os",
      "Menos de 7 a\u00F1os",
      "M\u00E1s de 7 a\u00F1os"
    )
  ) +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Antig\u00FCedad de las entidades por a\u00F1o",
    subtitle = paste(
      "La antig\u00FCedad se reinicia cuando una entidad vuelve a estar activa",
      "despu\u00E9s de una baja o una ausencia."
    ),
    x = "A\u00F1o",
    y = "Cantidad de entidades",
    fill = "Antig\u00FCedad",
    caption = paste(
      "C\u00E1lculo basado en numero_correlativo",
      "y en las fechas de corte disponibles."
    )
  ) +
  theme_minimal() +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )

# Mostrar el grafico en RStudio.
print(grafico_antiguedad)

# Guardar el grafico.
ggsave(
  "antiguedad.png",
  plot = grafico_antiguedad,
  width = 13,
  height = 9,
  dpi = 300
)
