library(data.table)
library(ggplot2)
library(scales)

# ------------------------------------------------------------------------------
# 1. Carga y preparación del dataset
# ------------------------------------------------------------------------------
ruta_csv <- if (file.exists("data/entidadesunificado.csv")) {
  "data/entidadesunificado.csv"
} else {
  "entidadesunificado.csv"
}

entidades <- fread(
  ruta_csv,
  select = c("numero_correlativo", "anio", "dada_de_baja", "descripcion_tipo_societario"),
  colClasses = list(character = c("numero_correlativo")),
  na.strings = c("", "NA")
)

entidades[, anio := as.integer(anio)]
entidades[, es_baja := !is.na(dada_de_baja) & dada_de_baja == "S"]

# Tipos societarios principales
top_tipos <- c("SOCIEDAD DE RESPONSABILIDAD LIMITADA", "SOC. ANONIMA", "SOCIEDAD POR ACCIONES SIMPLIFICADA")
entidades[descripcion_tipo_societario %in% top_tipos, tipo_label := fcase(
  descripcion_tipo_societario == "SOCIEDAD DE RESPONSABILIDAD LIMITADA", "S.R.L.",
  descripcion_tipo_societario == "SOC. ANONIMA", "S.A.",
  descripcion_tipo_societario == "SOCIEDAD POR ACCIONES SIMPLIFICADA", "S.A.S."
)]

# Identificar año de constitución (primera aparición en el registro)
primer_registro <- entidades[
  , .(anio_nacimiento = min(anio)), 
  by = numero_correlativo
]
entidades <- merge(entidades, primer_registro, by = "numero_correlativo", all.x = TRUE)


# ==============================================================================
# GRÁFICO 1 (Derecha Arriba): Evolución Acumulada de Bajas por Cohorte (%)
# ==============================================================================
df_cohortes <- entidades[
  anio_nacimiento %in% 2018:2023 & anio >= anio_nacimiento
]
df_cohortes[, t_anios := anio - anio_nacimiento]

# Conteo y variación acumulada
resumen_bajas <- df_cohortes[
  , .(bajas_periodo = sum(es_baja)),
  by = .(Cohorte = paste("Cohorte", anio_nacimiento), t_anios)
][order(Cohorte, t_anios)]

resumen_bajas[, bajas_base := bajas_periodo[t_anios == 0], by = Cohorte]
resumen_bajas[bajas_base == 0, bajas_base := 1]
resumen_bajas[, indice_crecimiento := ((bajas_periodo - bajas_base) / bajas_base) * 100]

puntos_finales <- resumen_bajas[, .SD[which.max(t_anios)], by = Cohorte]

grafico_cohortes_bajas <- ggplot(
  resumen_bajas, 
  aes(x = t_anios, y = indice_crecimiento, color = Cohorte, group = Cohorte)
) +
  geom_hline(yintercept = 0, color = "#1a1a1a", linewidth = 0.8) +
  geom_line(linewidth = 1.6, alpha = 0.95) +
  geom_point(size = 3.0) +
  geom_text(
    data = puntos_finales,
    aes(x = t_anios, y = indice_crecimiento, label = paste0(format(round(indice_crecimiento, 1), decimal.mark = ","), "%")),
    vjust = -0.8, hjust = 0.4, fontface = "bold", size = 4.2, show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Cohorte 2018" = "#1A252C",
      "Cohorte 2019" = "#E69F00",
      "Cohorte 2020" = "#009ADE",
      "Cohorte 2021" = "#D52B1E",
      "Cohorte 2022" = "#2D8A4E",
      "Cohorte 2023" = "#9B59B6"
    )
  ) +
  scale_x_continuous(breaks = 0:max(resumen_bajas$t_anios), expand = expansion(mult = c(0.02, 0.10))) +
  scale_y_continuous(labels = function(x) paste0(format(x, big.mark = "."), "%"), expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Evolución Acumulada de Bajas Empresariales por Cohorte",
    subtitle = "Variación porcentual acumulada de entidades dadas de baja desde el año de constitución (t = 0)",
    x = "Cantidad de años desde la inscripción (t = 0)",
    y = "Crecimiento acumulado de bajas (%)",
    caption = "Fuente: Inspección General de Justicia (IGJ) | ECON-520 FCE-UBA",
    color = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#111827", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 11.5, color = "#4B5563", margin = margin(b = 16)),
    legend.position = "top",
    legend.justification = "center",
    legend.text = element_text(face = "bold", size = 10),
    panel.grid.major.y = element_line(color = "#E5E7EB", linewidth = 0.6),
    axis.line = element_line(color = "#374151", linewidth = 0.8),
    axis.text = element_text(face = "bold", color = "#374151", size = 10.5),
    axis.title.x = element_text(face = "italic", margin = margin(t = 8), size = 11.5),
    axis.title.y = element_text(face = "bold", size = 11.5, margin = margin(r = 8)),
    plot.margin = margin(15, 20, 15, 15)
  )

# Guardar en alta definición
ggsave("grafico_cohortes_bajas.png", plot = grafico_cohortes_bajas, width = 10, height = 5.8, dpi = 300)


# ==============================================================================
# GRÁFICO 2 (Derecha Abajo): Boxplot de Longevidad por Tipo Societario
# ==============================================================================
longevidad_entidades <- entidades[
  !is.na(tipo_label),
  .(
    anio_inicio = min(anio),
    anios_observados = (max(anio) - min(anio)) + 1L
  ),
  by = .(numero_correlativo, tipo_label)
][anio_inicio >= 2018]

grafico_boxplot_longevidad <- ggplot(
  longevidad_entidades, 
  aes(x = tipo_label, y = anios_observados, fill = tipo_label)
) +
  geom_boxplot(width = 0.5, alpha = 0.75, outlier.alpha = 0.2, outlier.size = 1.2, color = "#1F2937") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3.5, fill = "white", color = "#111827") +
  scale_fill_manual(
    values = c(
      "S.R.L." = "#4B5563",
      "S.A."   = "#D97706",
      "S.A.S." = "#0284C7"
    )
  ) +
  scale_y_continuous(breaks = seq(1, max(longevidad_entidades$anios_observados), by = 1)) +
  labs(
    title = "Distribución de la Longevidad Empresarial por Tipo Societario",
    subtitle = "Años de permanencia activa en el padrón para entidades constituidas desde 2018 (Rombo blanco = Media)",
    x = "Tipo Societario",
    y = "Años de permanencia observada",
    caption = "Fuente: Inspección General de Justicia (IGJ) | ECON-520 FCE-UBA"
  ) +
  theme_gray(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#111827", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 11.5, color = "#4B5563", margin = margin(b = 15)),
    legend.position = "none",
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(color = "#374151", size = 11, face = "bold"),
    panel.grid.major = element_line(color = "white", linewidth = 0.8),
    panel.grid.minor = element_line(color = "white", linewidth = 0.4),
    plot.margin = margin(15, 20, 15, 15)
  )

# Guardar en alta definición
ggsave("grafico_boxplot_longevidad.png", plot = grafico_boxplot_longevidad, width = 9.5, height = 5.8, dpi = 300)