#Cargar librerias:
  library(igraph)
library(igraphdata)

data(UKfaculty)
g <- UKfaculty


# Caracteristicas de las redes --------------------------------------------

#¿Cuántos nodos y aristas tiene? summary(g)
cat("--- PREGUNTA 1.1: NODOS Y ARISTAS ---\n")
summary(g)

n_nodos   <- vcount(g)
n_aristas <- ecount(g)

cat("Cantidad de nodos (N):", n_nodos, "\n")
cat("Cantidad de aristas (L):", n_aristas, "\n\n")

# ¿Qué atributos tienen los nodos y las aristas? Es dirigida o no dirigida? Es ponderada o no ponderada?

# Atributos
cat("Atributos de los nodos:", vertex_attr_names(g), "\n")
cat("Atributos de las aristas:", edge_attr_names(g), "\n")

# Tipo de red
es_dirigida <- is_directed(g)
es_ponderada <- is_weighted(g)

cat("¿Es dirigida?:", ifelse(es_dirigida, "Sí (D en summary)", "No"), "\n")
cat("¿Es ponderada?:", ifelse(es_ponderada, "Sí (W en summary)", "No"), "\n\n")

# ¿La red está conectada? Si no, ¿cuántos componentes tiene y qué tamaño tiene el más grande?
esta_conectada <- is_connected(g, mode = "weak")
comp <- components(g, mode = "weak")

cat("¿Está conectada (débilmente)?:", ifelse(esta_conectada, "Sí", "No"), "\n")
cat("Número de componentes:", comp$no, "\n")
cat("Tamaño del componente más grande:", max(comp$csize), "de", n_nodos, "nodos\n\n")


# Como se conecta ---------------------------------------------------------

#Grado promedio y la mediana; Son parecidos o muy distintos? Qué dice eso sobre la red?
grados_totales <- degree(g, mode = "all")

grado_promedio <- mean(grados_totales)
grado_mediana  <- median(grados_totales)

cat("Grado promedio <k>:", round(grado_promedio, 2), "\n")
cat("Mediana de grado:", grado_mediana, "\n")
cat("Diagnóstico: Media y mediana son casi idénticas (~20), lo que indica una\n")
cat("distribución simétrica sin sesgos extremos en la conectividad docente.\n\n")

# Histograma de grados. ¿Tiene cola larga?
hist(grados_totales, 
     breaks = 15, 
     col = "skyblue", 
     border = "white",
     main = "Distribución de Grado - UKfaculty",
     xlab = "Número de conexiones (k)", 
     ylab = "Frecuencia de nodos")

cat("Diagnóstico: No presenta cola larga de ley de potencia; se asemeja a una campana\n")
cat("concentrada alrededor de la media, propia de redes profesionales cerradas.\n\n")

# Cuáles son  los 5 nodos más conectados (hubs?)  Tiene sentido que sean hubs?
top5_hubs <- head(sort(grados_totales, decreasing = TRUE), 5)
print(top5_hubs)

cat("Diagnóstico: Sí tiene sentido; representan a las autoridades de la facultad,\n")
cat("directores de departamento o docentes con rol articulador interdisciplinario.\n\n")

#Densidad? Es una red densa o esparsa?
densidad_red <- edge_density(g)

cat("Densidad:", round(densidad_red, 4), "(~12.6% de enlaces posibles)\n")
cat("Diagnóstico: Es una red relativamente densa para ser un entorno social humano,\n")
cat("evidenciando alta cohesión interna en el claustro.\n\n")


# ESTRUCTURA DE LA RED ----------------------------------------------------

# Coeficiente de clustering global. Es alto o bajo?
c_global <- transitivity(g, type = "global")

cat("Clustering global C:", round(c_global, 4), "\n")
cat("Diagnóstico: Es alto (~32%), lo que muestra una fuerte tendencia a formar\n")
cat("tríadas cerradas (los contactos de un profesor suelen conocerse entre sí).\n\n")



#Cuál es la distancia promedio entre nodos?
d_promedio <- mean_distance(g, directed = TRUE, unconnected = TRUE)

cat("Distancia promedio <d>:", round(d_promedio, 2), "pasos\n")
cat("Diagnóstico: Distancia muy corta (~2.16 saltos), confirmando el efecto\n")
cat("de 'Mundo Pequeño' (Small-World) en la difusión de información.\n\n")


#Cuántas comunidades encontró
g_undir <- as.undirected(g, mode = "collapse")
com <- cluster_louvain(g_undir)

cat("Número de comunidades (length):", length(com), "\n")
cat("Tamaño de cada comunidad (sizes):\n")
print(sizes(com))

# Gráfico de comunidades en pestaña Plots
set.seed(42)
plot(com, g_undir,
     vertex.size = 5 + (grados_totales * 0.3),
     vertex.label = NA,
     edge.arrow.size = 0.2,
     main = "Comunidades detectadas en UKfaculty (Louvain)")


