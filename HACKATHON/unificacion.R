# Instalar una sola vez:
# install.packages("data.table")

library(data.table)

directorio <- "D:/WindowsCarpetaDescarga/drive-download-20260913T003417Z-1-001"

salida <- file.path(
  directorio,
  "agjentidadesunificado.csv"
)

archivos_esperados <- c(
  "2017-12.csv",
  "2018-12.csv",
  "2019-12.csv",
  "2020-12.csv",
  "2021-12.csv",
  "2022-06.csv",
  "2023-09.csv",
  "2024-12.csv",
  "2025-12.csv",
  "2026-06.csv"
)

archivos <- file.path(directorio, archivos_esperados)

# Verificar que estén todos los archivos
faltantes <- archivos[!file.exists(archivos)]

if (length(faltantes) > 0) {
  stop(
    "Faltan estos archivos:\n",
    paste(faltantes, collapse = "\n")
  )
}

columnas_base <- c(
  "numero_correlativo",
  "tipo_societario",
  "descripcion_tipo_societario",
  "razon_social",
  "dada_de_baja",
  "codigo_baja",
  "detalle_baja",
  "cuit"
)

leer_archivo <- function(archivo) {
  
  nombre_archivo <- basename(archivo)
  fecha_corte <- sub("\\.csv$", "", nombre_archivo)
  
  anio <- as.integer(substr(fecha_corte, 1, 4))
  mes <- substr(fecha_corte, 6, 7)
  
  # El archivo 2024 no tiene encabezado
  if (nombre_archivo == "2024-12.csv") {
    
    datos <- fread(
      archivo,
      header = FALSE,
      colClasses = "character",
      encoding = "UTF-8",
      showProgress = TRUE
    )
    
    setnames(datos, columnas_base)
    
  } else {
    
    datos <- fread(
      archivo,
      header = TRUE,
      colClasses = "character",
      encoding = "UTF-8",
      showProgress = TRUE
    )
  }
  
  # El archivo 2017 no tiene la columna CUIT
  if (!"cuit" %in% names(datos)) {
    datos[, cuit := NA_character_]
  }
  
  # Controlar que no existan columnas inesperadas
  columnas_inesperadas <- setdiff(names(datos), columnas_base)
  
  if (length(columnas_inesperadas) > 0) {
    stop(
      "Columnas inesperadas en ",
      nombre_archivo,
      ": ",
      paste(columnas_inesperadas, collapse = ", ")
    )
  }
  
  # Homogeneizar el orden de las columnas
  setcolorder(datos, columnas_base)
  
  # Agregar información temporal y de procedencia
  datos[, `:=`(
    anio = anio,
    mes = mes,
    fecha_corte = fecha_corte,
    archivo_origen = nombre_archivo
  )]
  
  datos
}

# Leer los diez archivos
bases <- lapply(archivos, leer_archivo)

# Unirlos sin eliminar registros repetidos entre años
base_unificada <- rbindlist(
  bases,
  use.names = TRUE,
  fill = TRUE
)

# Controles
cat("Cantidad total de registros:", format(nrow(base_unificada), big.mark = "."), "\n")
cat("Cantidad de columnas:", ncol(base_unificada), "\n")

print(
  base_unificada[
    ,
    .N,
    by = .(archivo_origen, anio, mes, fecha_corte)
  ]
)

# Exportar el resultado
fwrite(
  base_unificada,
  file = salida,
  sep = ",",
  quote = "auto",
  bom = TRUE,
  na = ""
)

cat("Archivo generado en:\n", salida, "\n")