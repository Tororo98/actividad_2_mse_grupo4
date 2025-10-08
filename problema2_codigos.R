###############################################################################
# Problema 2: Estimadores en Distribución Gamma
###############################################################################

# Paquetes y Configuración
library(ggplot2)
library(dplyr)
library(tidyr)
library(bookdown)

# Parámetros de la distribución Gamma
alpha <- 3
sigma <- 2
media_verdadera <- alpha * sigma

###############################################################################
# Sección: a) Simulación de estimadores
###############################################################################

num_muestras_a <- 100
n_a <- 10

resultados_a <- data.frame(
  Muestra = 1:num_muestras_a,
  mu_hat_1 = numeric(num_muestras_a),
  mu_hat_2 = numeric(num_muestras_a),
  mu_hat_3 = numeric(num_muestras_a),
  mu_hat_4 = numeric(num_muestras_a)
)

set.seed(123)
for (i in 1:num_muestras_a) {
  muestra <- rgamma(n_a, shape = alpha, scale = sigma)
  resultados_a$mu_hat_1[i] <- mean(muestra)
  resultados_a$mu_hat_2[i] <- (min(muestra) + max(muestra)) / 2
  resultados_a$mu_hat_3[i] <- min(muestra)
  resultados_a$mu_hat_4[i] <- sum(muestra) / (n_a + 1)
}

resultados_a_long <- resultados_a %>%
  pivot_longer(cols = starts_with("mu_hat"), names_to = "Estimador", values_to = "Valor")

grafico_a <- ggplot(resultados_a_long, aes(x = Valor, fill = Estimador)) +
  geom_density(alpha = 0.6) +
  geom_vline(xintercept = media_verdadera, linetype = "dashed", color = "red", size = 1) +
  facet_wrap(~ Estimador, scales = "free_x") +
  labs(
    title = "Distribución de los Estimadores (n = 10)",
    x = "Valor del Estimador",
    y = "Densidad"
  ) +
  theme_minimal()

ggsave("graphs/distribucion_estimadores.png", grafico_a, width = 10, height = 6)

###############################################################################
# Sección: b) Insesgadez
###############################################################################

medias_estimadores <- resultados_a %>%
  summarise(across(starts_with("mu_hat"), ~ mean(.x))) %>%
  pivot_longer(cols = everything(), names_to = "Estimador", values_to = "Media_Estimada")

medias_estimadores$Media_Verdadera <- media_verdadera

grafico_b <- ggplot(medias_estimadores, aes(x = Estimador, y = Media_Estimada)) +
  geom_point(color = "blue", size = 4) +
  geom_segment(aes(x = Estimador, xend = Estimador, y = Media_Verdadera, yend = Media_Estimada),
               color = "gray", linetype = "dashed") +
  geom_hline(yintercept = media_verdadera, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Comparación de Medias de los Estimadores vs. Media Verdadera",
    x = "Estimador",
    y = "Media de la Distribución Muestral"
  ) +
  theme_minimal()

ggsave("graphs/insesgadez.png", grafico_b, width = 8, height = 6)

###############################################################################
# Sección: c) Consistencia
###############################################################################

tamanos_muestra_c <- c(5, 10, 20, 30, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
num_simulaciones_c <- 500

resultados_c <- data.frame(
  n = tamanos_muestra_c,
  mu_hat_1_var = numeric(length(tamanos_muestra_c)),
  mu_hat_2_var = numeric(length(tamanos_muestra_c)),
  mu_hat_3_var = numeric(length(tamanos_muestra_c)),
  mu_hat_4_var = numeric(length(tamanos_muestra_c))
)

set.seed(456)
for (i in 1:length(tamanos_muestra_c)) {
  n_actual <- tamanos_muestra_c[i]
  
  estimaciones_n <- data.frame(
    mu_hat_1 = numeric(num_simulaciones_c),
    mu_hat_2 = numeric(num_simulaciones_c),
    mu_hat_3 = numeric(num_simulaciones_c),
    mu_hat_4 = numeric(num_simulaciones_c)
  )
  
  for (j in 1:num_simulaciones_c) {
    muestra <- rgamma(n_actual, shape = alpha, scale = sigma)
    estimaciones_n$mu_hat_1[j] <- mean(muestra)
    estimaciones_n$mu_hat_2[j] <- (min(muestra) + max(muestra)) / 2
    estimaciones_n$mu_hat_3[j] <- min(muestra)
    estimaciones_n$mu_hat_4[j] <- sum(muestra) / (n_actual + 1)
  }
  
  resultados_c$mu_hat_1_var[i] <- var(estimaciones_n$mu_hat_1)
  resultados_c$mu_hat_2_var[i] <- var(estimaciones_n$mu_hat_2)
  resultados_c$mu_hat_3_var[i] <- var(estimaciones_n$mu_hat_3)
  resultados_c$mu_hat_4_var[i] <- var(estimaciones_n$mu_hat_4)
}

resultados_c_long <- resultados_c %>%
  pivot_longer(cols = starts_with("mu_hat"), names_to = "Estimador", values_to = "Varianza")

grafico_c <- ggplot(resultados_c_long, aes(x = n, y = Varianza, color = Estimador)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  labs(
    title = "Variabilidad de los Estimadores vs. Tamaño Muestral",
    x = "Tamaño de la Muestra (n)",
    y = "Varianza del Estimador"
  ) +
  theme_minimal()

ggsave("graphs/consistencia.png", grafico_c, width = 10, height = 6)

###############################################################################
# Sección: d) Eficiencia
###############################################################################

varianzas_estimadores <- resultados_a %>%
  summarise(across(starts_with("mu_hat"), ~ var(.x))) %>%
  pivot_longer(cols = everything(), names_to = "Estimador", values_to = "Varianza")

cv_estimadores <- resultados_a_long %>%
  group_by(Estimador) %>%
  summarise(CV = sd(Valor) / abs(mean(Valor)))

grafico_d_var <- ggplot(varianzas_estimadores, aes(x = Estimador, y = Varianza, fill = Estimador)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Varianza Muestral de los Estimadores (n=10)",
    x = "Estimador",
    y = "Varianza"
  ) +
  theme_minimal()

grafico_d_cv <- ggplot(cv_estimadores, aes(x = Estimador, y = CV, fill = Estimador)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Coeficiente de Variación de los Estimadores (n=10)",
    x = "Estimador",
    y = "Coeficiente de Variación"
  ) +
  theme_minimal()

ggsave("graphs/eficiencia_varianza.png", grafico_d_var, width = 8, height = 6)
ggsave("graphs/eficiencia_cv.png", grafico_d_cv, width = 8, height = 6)
