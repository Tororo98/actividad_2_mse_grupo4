###############################################################################
# Sección: Paquetes y Configuración
###############################################################################
library(ggplot2)
library(dplyr)

###############################################################################
# Sección: Cálculo de probabilidad teórica
###############################################################################

# Parámetro de la distribución de Poisson
lambda <- 5

# Valor de interés
x <- 3

# Calcular la probabilidad teórica de P(X=3) usando la fórmula de Poisson.
# La fórmula es P(X=x) = (lambda^x * exp(-lambda)) / x!
# En R, esto se puede calcular fácilmente con la función dpois().
prob_teorica <- dpois(x, lambda = lambda)
prob_teorica

###############################################################################
# Sección: Simulación con una muestra
###############################################################################

# Tamaño de la muestra
n_muestra <- 1000

# Generar una muestra aleatoria de tamaño n_muestra
set.seed(42) # Se fija una semilla para reproducibilidad
muestra_1 <- rpois(n_muestra, lambda = lambda)

# Calcular la frecuencia relativa para X=3
frecuencia_absoluta <- sum(muestra_1 == 3)
frecuencia_relativa <- frecuencia_absoluta / n_muestra
frecuencia_relativa

###############################################################################
# Sección: Análisis de la variabilidad entre muestras
###############################################################################

# Número de muestras a generar
num_muestras <- 100
n_muestra_variabilidad <- 1000

# Vector para almacenar las frecuencias relativas
frecuencias_relativas <- numeric(num_muestras)

# Generar 100 muestras y calcular la frecuencia relativa para cada una
set.seed(123) # Nueva semilla para esta sección
for (i in 1:num_muestras) {
  muestra <- rpois(n_muestra_variabilidad, lambda = lambda)
  frecuencias_relativas[i] <- sum(muestra == 3) / n_muestra_variabilidad
}

# Crear un data frame para el gráfico de dispersión
df_variabilidad <- data.frame(
  Indice = 1:num_muestras,
  Frecuencia = frecuencias_relativas
)

# Construir el gráfico de dispersión
grafico_variabilidad <- ggplot(df_variabilidad, aes(x = Indice, y = Frecuencia)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_hline(yintercept = prob_teorica, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Frecuencia Relativa de X=3 en 100 Muestras de Tamaño 1000",
    x = "Muestra (índice 1-100)",
    y = "Frecuencia Relativa (f_n(X=3))"
  ) +
  theme_minimal()

# Imprimir el gráfico (o guardar si lo desea)
print(grafico_variabilidad)

###############################################################################
# Sección: Impacto del tamaño muestral
###############################################################################

# Tamaños de muestra a considerar
tamanos_muestra <- c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)

# Vector para almacenar las frecuencias relativas
frecuencias_por_tamano <- numeric(length(tamanos_muestra))

# Calcular la frecuencia relativa para cada tamaño de muestra
set.seed(456) # Nueva semilla para esta sección
for (i in 1:length(tamanos_muestra)) {
  n <- tamanos_muestra[i]
  muestra <- rpois(n, lambda = lambda)
  frecuencias_por_tamano[i] <- sum(muestra == 3) / n
}

# Crear un data frame para el gráfico de dispersión
df_impacto_tamano <- data.frame(
  TamanoMuestral = tamanos_muestra,
  Frecuencia = frecuencias_por_tamano
)

# Construir el gráfico de dispersión
grafico_impacto_tamano <- ggplot(df_impacto_tamano, aes(x = TamanoMuestral, y = Frecuencia)) +
  geom_point(color = "darkgreen", alpha = 0.8) +
  geom_line(color = "darkgreen", alpha = 0.5) +
  geom_hline(yintercept = prob_teorica, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Frecuencia Relativa vs. Tamaño Muestral",
    x = "Tamaño de la Muestra (n)",
    y = "Frecuencia Relativa (f_n(X=3))"
  ) +
  theme_minimal()

# Imprimir el gráfico
print(grafico_impacto_tamano)

###############################################################################
# Sección: Convergencia de la media muestral
###############################################################################

# Número de muestras
num_muestras_media <- 100
n_muestra_media <- 1000

# Vector para almacenar los promedios muestrales
promedios_muestrales <- numeric(num_muestras_media)

# Generar 100 muestras y calcular el promedio para cada una
set.seed(789) # Nueva semilla para esta sección
for (i in 1:num_muestras_media) {
  muestra <- rpois(n_muestra_media, lambda = lambda)
  promedios_muestrales[i] <- mean(muestra)
}

# Crear un data frame para el gráfico de dispersión
df_media <- data.frame(
  Indice = 1:num_muestras_media,
  Promedio = promedios_muestrales
)

# Construir el gráfico de dispersión
grafico_media <- ggplot(df_media, aes(x = Indice, y = Promedio)) +
  geom_point(color = "purple", alpha = 0.7) +
  geom_hline(yintercept = lambda, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Promedio Muestral en 100 Muestras de Tamaño 1000",
    x = "Muestra (índice 1-100)",
    y = "Promedio Muestral"
  ) +
  theme_minimal()

# Imprimir el gráfico
print(grafico_media)

###############################################################################
# Sección: Impacto del tamaño muestral en la media
###############################################################################

# Tamaños de muestra a considerar (los mismos que antes)
tamanos_muestra_media <- c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)

# Vector para almacenar los promedios muestrales
promedios_por_tamano <- numeric(length(tamanos_muestra_media))

# Calcular el promedio muestral para cada tamaño
set.seed(1011) # Nueva semilla para esta sección
for (i in 1:length(tamanos_muestra_media)) {
  n <- tamanos_muestra_media[i]
  muestra <- rpois(n, lambda = lambda)
  promedios_por_tamano[i] <- mean(muestra)
}

# Crear un data frame para el gráfico de dispersión
df_impacto_media <- data.frame(
  TamanoMuestral = tamanos_muestra_media,
  Promedio = promedios_por_tamano
)

# Construir el gráfico de dispersión
grafico_impacto_media <- ggplot(df_impacto_media, aes(x = TamanoMuestral, y = Promedio)) +
  geom_point(color = "orange", alpha = 0.8) +
  geom_line(color = "orange", alpha = 0.5) +
  geom_hline(yintercept = lambda, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Promedio Muestral vs. Tamaño Muestral",
    x = "Tamaño de la Muestra (n)",
    y = "Promedio Muestral"
  ) +
  theme_minimal()

# Imprimir el gráfico
print(grafico_impacto_media)