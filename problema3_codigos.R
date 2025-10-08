###############################################################################
# Sección: Paquetes y Configuración (Problema 3)
###############################################################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(bookdown)
# Paquete necesario para el test de normalidad Shapiro-Wilk (base R)

# Parámetro de la distribución Exponencial
lambda <- 0.16

# Parámetros poblacionales teóricos
media_poblacional <- 1 / lambda
varianza_poblacional <- 1 / (lambda^2)
desviacion_poblacional <- sqrt(varianza_poblacional)

lambda <- 0.16
curve(dexp(x, rate = lambda), from = 0, to = 30, col = "darkblue", lwd = 2, 
      main = "Función de Densidad Exponencial (λ=0.16)",
      xlab = "Tiempo de Funcionamiento (meses)",
      ylab = "Densidad de Probabilidad")
abline(v = 1/lambda, col = "red", lty = 2)
legend("topright", legend = c("Media Poblacional (6.25)"), col = "red", lty = 2, cex = 0.8)

###############################################################################
# Sección: c) Comparación de parámetros, estimadores y estimaciones
###############################################################################

num_muestras_c <- 10
n_c <- 200
set.seed(123)

# Data frame para almacenar resultados
resultados_c <- data.frame(
  Muestra = 1:num_muestras_c,
  Media_Muestral = numeric(num_muestras_c),
  Varianza_Muestral = numeric(num_muestras_c)
)

# Generar 10 muestras y calcular estadísticas
muestras_c <- list()
for (i in 1:num_muestras_c) {
  muestra <- rexp(n_c, rate = lambda)
  muestras_c[[i]] <- muestra
  
  resultados_c$Media_Muestral[i] <- mean(muestra)
  resultados_c$Varianza_Muestral[i] <- var(muestra)
}

# Histograma y curva de densidad para la primera muestra (para el Rmd)
hist_muestra1 <- ggplot(data.frame(x = muestras_c[[1]]), aes(x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red", linewidth = 1) +
  labs(
    title = paste("Distribución de la Muestra 1 (n=", n_c, ")"),
    x = "Tiempo de Funcionamiento (meses)",
    y = "Densidad"
  ) +
  theme_minimal()

ggsave("graphs/hist_muestra1_n200.png", hist_muestra1, width = 8, height = 5)

###############################################################################
# Sección: d) Aplicación del Teorema del Límite Central para n=200
###############################################################################

num_muestras_d <- 100
n_d <- 200
set.seed(456)

# Generar las 100 medias muestrales
medias_muestrales_d <- numeric(num_muestras_d)
for (i in 1:num_muestras_d) {
  muestra <- rexp(n_d, rate = lambda)
  medias_muestrales_d[i] <- mean(muestra)
}

# Cálculo de estadísticas descriptivas
promedio_medias_d <- mean(medias_muestrales_d)
varianza_medias_d <- var(medias_muestrales_d)

# Varianza teórica de las medias muestrales (Error Estándar al cuadrado)
varianza_teorica_d <- varianza_poblacional / n_d

# Test de normalidad Shapiro-Wilk
test_normalidad_d <- shapiro.test(medias_muestrales_d)

# Histograma de las 100 medias muestrales
hist_medias_d <- ggplot(data.frame(x = medias_muestrales_d), aes(x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 15, fill = "lightgreen", color = "black") +
  geom_density(color = "blue", linewidth = 1) +
  geom_vline(xintercept = media_poblacional, linetype = "dashed", color = "red", linewidth = 1) +
  labs(
    title = paste("Distribución de las 100 Medias Muestrales (n=", n_d, ")"),
    x = "Media Muestral",
    y = "Densidad"
  ) +
  theme_minimal()

ggsave("graphs/hist_medias_n200.png", hist_medias_d, width = 8, height = 5)

###############################################################################
# Sección: e) Aplicación del Teorema del Límite Central variando n
###############################################################################

tamanos_n_e <- c(5, 10, 80, 200, 500, 2000)
num_muestras_e <- 100
set.seed(789)

# Data frame para almacenar resultados de la convergencia
convergencia_resultados_e <- data.frame(
  n = tamanos_n_e,
  Promedio_Medias = numeric(length(tamanos_n_e)),
  Varianza_Medias = numeric(length(tamanos_n_e)),
  Varianza_Teorica = varianza_poblacional / tamanos_n_e,
  P_Valor_Normalidad = numeric(length(tamanos_n_e))
)

# Lista para almacenar los gráficos (para el Rmd)
graficos_e <- list()

for (i in 1:length(tamanos_n_e)) {
  n_actual <- tamanos_n_e[i]
  medias_muestrales_n <- numeric(num_muestras_e)
  
  for (j in 1:num_muestras_e) {
    muestra <- rexp(n_actual, rate = lambda)
    medias_muestrales_n[j] <- mean(muestra)
  }
  
  # Almacenar estadísticas
  convergencia_resultados_e$Promedio_Medias[i] <- mean(medias_muestrales_n)
  convergencia_resultados_e$Varianza_Medias[i] <- var(medias_muestrales_n)
  convergencia_resultados_e$P_Valor_Normalidad[i] <- shapiro.test(medias_muestrales_n)$p.value
  
  # Generar histograma
  grafico <- ggplot(data.frame(x = medias_muestrales_n), aes(x)) +
    geom_histogram(aes(y = after_stat(density)), bins = 15, fill = "coral", color = "black") +
    geom_density(color = "red", linewidth = 1) +
    geom_vline(xintercept = media_poblacional, linetype = "dashed", color = "blue", linewidth = 1) +
    labs(
      title = paste("Distribución de Medias Muestrales (n = ", n_actual, ")"),
      x = "Media Muestral",
      y = "Densidad"
    ) +
    theme_minimal()
  
  graficos_e[[i]] <- grafico
  ggsave(paste0("graphs/hist_medias_n", n_actual, ".png"), grafico, width = 8, height = 5)
}

# Graficar la convergencia de la varianza
grafico_convergencia_var <- ggplot(convergencia_resultados_e, aes(x = n)) +
  geom_line(aes(y = Varianza_Medias, color = "Varianza Observada"), linewidth = 1) +
  geom_point(aes(y = Varianza_Medias, color = "Varianza Observada"), size = 3) +
  geom_line(aes(y = Varianza_Teorica, color = "Varianza Teórica (σ²/n)"), linetype = "dashed", linewidth = 1) +
  geom_point(aes(y = Varianza_Teorica, color = "Varianza Teórica (σ²/n)"), size = 3) +
  scale_x_continuous(trans = 'log10', breaks = tamanos_n_e) +
  labs(
    title = "Convergencia de la Varianza Muestral al Aumentar n",
    x = "Tamaño de la Muestra (n) en escala logarítmica",
    y = "Varianza de las Medias Muestrales",
    color = "Tipo de Varianza"
  ) +
  theme_minimal()

ggsave("graphs/convergencia_varianza_tlc.png", grafico_convergencia_var, width = 10, height = 6)
