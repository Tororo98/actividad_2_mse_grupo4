#Librerias
library(ggplot2)
library(boot)
library(dplyr)
library(tidyr)
library(knitr)
library(readxl)
library(mice)
library(summarytools)
library(goftest)
library(PMCMRplus)
library(fBasics)

###############################################################################
###############################################################################
#Problema 4: Estimación de intervalos de confianza
##############################################################################
#############################################################################

##############################################################################
#a. Normalidad
###############################################################################

###############################################################################
#Elabora un histograma y una curva de densidad de la muestra. 
#¿Podría considerarse normal la población de origen de la muestra?
###############################################################################

# Muestra de eficiencia energética (kWh/L)
muestra <- c(6.12, 5.87, 5.45, 6.33, 5.71, 6.04, 5.92, 5.65, 6.18, 
             5.78, 5.95, 6.21, 5.63, 5.79, 6.11, 5.88, 6.02, 5.76, 5.85, 6.10)

(length(muestra))
# Histograma básico
hist(muestra, 
     main = "Histograma de la muestra", 
     xlab = "Valores", 
     ylab = "Frecuencia", 
     col = "lightblue", 
     border = "black")

# Curva de densidad
muestra1 <- muestra

set.seed(124)  # Así se asegura que los números aleatorios sean siempre los mismos
# Muestra simulada con misma media y sd
muestra2 <- rnorm(length(muestra), mean = mean(muestra), sd = sd(muestra))

# Construir dataframe
df <- data.frame(
  valores = c(muestra1, muestra2),
  grupo = factor(rep(c("Muestra", "Distribucion normal"), each = length(muestra)))
)

# Graficar curvas de densidad
ggplot(df, aes(x = valores, color = grupo, fill = grupo)) +
  geom_density(alpha = 0.4, size = 1) +
  labs(title = "Curvas de densidad superpuestas",
       x = "Valores",
       y = "Densidad") +
  theme_minimal()

###############################################################################
#Aplica pruebas para evaluar la normalidad. 
#Expón claramente las hipótesis nula y alternativa. 
#Determina el nivel de significancia adecuado. 
#Argumenta el valor del nivel de significancia que uses.
###############################################################################

#Hipotesis nula H_0: Los datos siguen una distribución normal.

#Hipótesis alternativa H_1: Los datos no siguen una distribución normal.

#NIVEL DE SIGNIFICANCIA: 0.05

#En este estudio definimos α = 0.05 para la prueba de normalidad
#(Shapiro–Wilk) porque ofrece un equilibrio entre el riesgo de error 
# Tipo I (falsos positivos) y la potencia del test porque dado que el 
#tamaño muestral es n = 20, mantener α = 0.05 evita usar un nivel demasiado 
# estricto (α = 0.01), que reduciría aún más la potencia, y al mismo tiempo 
#impide recurrir a un umbral muy flexible (α = 0.10).

#Criterio de decisión:
  
#Si el valor−p es mayor que α=0.05, no se rechaza H0, 
#lo que sugiere que los datos podrían seguir una distribución normal.

#Si el valor−p es menor que α=0.05, se rechaza H0 en favor de HA, 
#indicando que los datos no siguen una distribución normal.


shapiro.test(muestra)

##############################################################################
# Comenta si los resultados sugieren la viabilidad de usar métodos 
# paramétricos para calcular intervalos de confianza.
##############################################################################

# Teniendo en cuenta lo obtenido en la prueba de Shapiro-Wilk, se puede 
# asumir que los datos presentan un comportamiento cercano a una distribución 
# normal. Sin embargo, el tamaño de la muestra es reducido, lo que limita 
# la aplicación confiable del Teorema del Límite Central por lo que se podria
#calcular intervalos de confianza pero con t-student


##############################################################################
#b. Método paramétrico
##############################################################################

##############################################################################
#Si se cumplen las condiciones para su aplicación 
#calcula un intervalo de confianza paramétrico.
##############################################################################

#Para que se cumplan las condiciones, es necesario calcular el intervalo de
#confianza parametrico con t-student teniendo en cuenta que n=20 para la
#media

# Definir valores
x_bar <- mean(muestra)   # Media muestral
s <- sd(muestra)         # Desviación estándar muestral
n <- length(muestra)     # Tamaño de la muestra
alpha <- 0.05            # Nivel de significancia (1 - 0.95)

# Calcular el valor crítico t
t_alpha_2 <- qt(1 - alpha/2, df = n - 1)

# Calcular el margen de error
error <- t_alpha_2 * (s / sqrt(n))

# Construir el intervalo de confianza
IC_lower <- x_bar - error
IC_upper <- x_bar + error

# Mostrar resultados
IC <- c(IC_lower, IC_upper)
names(IC) <- c("Límite Inferior", "Límite Superior")
print(IC)


###############################################################################
#Compara e interpreta este intervalo con los obtenidos mediante métodos 
#no paramétricos.
##############################################################################

##############################################################################
# Intervalo de confianza BCa (sesgo-acelerado) para la media con bootstrap
##############################################################################

set.seed(123)       # Reproducibilidad
B <- 5000           # Número de réplicas bootstrap
alpha <- 0.05       # Nivel de significancia (95% de confianza)

# Función estadística: la media
boot_mean <- function(data, idx) mean(data[idx])

# Ejecutar bootstrap sobre tu vector "muestra"
bobj <- boot(data = muestra, statistic = boot_mean, R = B)

# Intervalo de confianza BCa
IC_bca <- boot.ci(bobj, conf = 1 - alpha, type = "bca")$bca[4:5]

# Mostrar resultados
cat("Media muestral:           ", mean(muestra), "\n")
cat("IC 95% BCa (media):       [", IC_bca[1], ", ", IC_bca[2], "]\n")

# El intervalo de confianza paramétrico (t-Student) para la media 
# fue [5.813, 6.022], mientras que el intervalo de confianza no paramétrico 
# obtenido mediante bootstrap BCa fue [5.822, 6.012].
# El intervalo de confianza bootstrap, que no depende de supuestos de normalidad, respalda 
# la validez del intervalo paramétrico. Dado que ambos intervalos son 
# prácticamente coincidentes, se confirma que incluso sin dichas suposiciones 
# se obtiene una estimación muy similar y sugiere que los métodos paramétricos 
# tradicionales son adecuados para estos datos.

#############################################################################
#c. Procedimiento bootstrap:
#############################################################################

##############################################################################
#Calcula 2 tipos de intervalos de confianza para la media mediante Bootstrap.
##############################################################################

### Percentil Bootstrap

set.seed(123)           # Reproducibilidad
B <- 5000               # réplicas
alpha <- 0.05           # Nivel de significancia (95% de confianza)

# estadístico: la media
boot_mean <- function(data, idx) mean(data[idx])

# ejecutar bootstrap sobre tu vector 'muestra'
bobj <- boot(data = muestra, statistic = boot_mean, R = B)

# IC percentil al 95%
IC_perc <- boot.ci(bobj, conf = 1 - alpha, type = "perc")$percent[4:5]

cat("Media muestral:           ", mean(muestra), "\n")
cat("IC 95% Percentil (media): [", IC_perc[1], ", ", IC_perc[2], "]\n")

### Normal bootstrap

##############################################################################
# IC Normal Bootstrap para la MEDIA (usando 'boot')
##############################################################################

set.seed(123)  # Reproducibilidad
B <- 5000      # réplicas
alpha <- 0.05  # Nivel de significancia (95% de confianza)

boot_mean <- function(data, idx) mean(data[idx])
bobj <- boot(data = muestra, statistic = boot_mean, R = B)

IC_norm <- boot.ci(bobj, conf = 1 - alpha, type = "norm")$normal[2:3]

cat("Media muestral:          ", mean(muestra), "\n")
cat("IC 95% Normal Bootstrap: [", IC_norm[1], ", ", IC_norm[2], "]\n")

# Los intervalos de confianza obtenidos mediante Bootstrap Percentil 
# ([5.8220, 6.0125]) y Bootstrap Normal ([5.8218, 6.0137]) son prácticamente 
# coincidentes, con diferencias mínimas en sus extremos (<0.002). 
# Esto indica que ambos métodos son consistentes y que, en este caso, la 
# distribución de la media muestral no presenta sesgos o asimetrías relevantes. 
# Por tanto, cualquiera de los dos métodos resulta adecuado para estimar el 
# intervalo de confianza de la media.


###############################################################################
###############################################################################
#Problema 5: Análisis de potencia, tamaño del efecto 
#y errores tipo I y II en una prueba de hipótesis
##############################################################################
#############################################################################

##############################################################################
# Curvas de potencia vs tamaño muestral (prueba t bilateral, una muestra)
# H0: mu = 120,  H1: mu != 120,  sigma = 15,  alpha = 0.10
##############################################################################

# Parámetros poblacionales y de diseño
mu0   <- 120        # media bajo H0
sigma <- 15         # D.E poblacional
alpha <- 0.10       # nivel de significación
n_vals <- c(10,20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800,900,1000)

# Tamaños del efecto (Cohen's d)
d_vals <- c(0.2, 0.5, 0.8)   # pequeño, mediano, grande

# Malla n x d y cálculo de potencia
grid <- expand.grid(n = n_vals, d = d_vals) %>%
  mutate(delta = d * sigma,  # OJO diferencia absoluta en unidades originales
         power = mapply(function(n, delta){
           power.t.test(n = n,
                        delta = delta,
                        sd = sigma,
                        sig.level = alpha,
                        type = "one.sample",
                        alternative = "two.sided")$power
         }, n, delta))

# Tabla (opcional)
kable(grid %>% arrange(d, n),
      digits = 4,
      caption = "Potencia (1-β) por tamaño muestral y tamaño del efecto (prueba t bilateral, α = 0.10)")

# Gráfico: Potencia vs Tamaño muestral, una curva por d
ggplot(grid, aes(x = n, y = power, color = factor(d), group = d)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_discrete(name = "Tamaño del efecto (d)",
                       labels = c("0.2 (pequeño)", "0.5 (mediano)", "0.8 (grande)")) +
  labs(title = "Curvas de potencia (prueba t bilateral, una muestra)",
       subtitle = expression(paste(mu[0]," = 120,  ", alpha, " = 0.10,  ", sigma, " = 15")),
       x = "Tamaño muestral (n)",
       y = "Potencia (1 - β)") +
  theme_minimal()

##############################################################################
# Curvas de potencia vs tamaño muestral (prueba t bilateral, una muestra)
# H0: mu = 120,  H1: mu != 120,  sigma = 15,  alpha = 0.05
##############################################################################

# Parámetros poblacionales y de diseño
mu0   <- 120        # media bajo H0
sigma <- 15         # D.E poblacional
alpha <- 0.05       # nivel de significación
n_vals <- c(10,20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800,900,1000)

# Tamaños del efecto (Cohen's d)
d_vals <- c(0.2, 0.5, 0.8)   # pequeño, mediano, grande

# Malla n x d y cálculo de potencia
grid <- expand.grid(n = n_vals, d = d_vals) %>%
  mutate(delta = d * sigma,  # OJO diferencia absoluta en unidades originales
         power = mapply(function(n, delta){
           power.t.test(n = n,
                        delta = delta,
                        sd = sigma,
                        sig.level = alpha,
                        type = "one.sample",
                        alternative = "two.sided")$power
         }, n, delta))

# Tabla (opcional)
kable(grid %>% arrange(d, n),
      digits = 4,
      caption = "Potencia (1-β) por tamaño muestral y tamaño del efecto (prueba t bilateral, α = 0.05)")

# Gráfico: Potencia vs Tamaño muestral, una curva por d
ggplot(grid, aes(x = n, y = power, color = factor(d), group = d)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_discrete(name = "Tamaño del efecto (d)",
                       labels = c("0.2 (pequeño)", "0.5 (mediano)", "0.8 (grande)")) +
  labs(title = "Curvas de potencia (prueba t bilateral, una muestra)",
       subtitle = expression(paste(mu[0]," = 120,  ", alpha, " = 0.05,  ", sigma, " = 15")),
       x = "Tamaño muestral (n)",
       y = "Potencia (1 - β)") +
  theme_minimal()

###############################################################################
#Graficas separadas
###############################################################################

# ---- Función para un gráfico por d (con color fijo y sin leyenda) ----
make_plot <- function(df, titulo){
  ggplot(df, aes(x = n, y = power, color = factor(d))) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0.8, linetype = "dashed") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_color_manual(values = c("0.2" = "red", "0.5" = "green", "0.8" = "blue")) +
    labs(title = titulo,
         subtitle = expression(paste(mu[0]," = 120,  ", alpha, " = 0.05,  ", sigma, " = 15")),
         x = "Tamaño muestral (n)",
         y = "Potencia (1 - β)") +
    theme_minimal() +
    theme(legend.position = "none")
}

# ---- Gráficos separados ----
p_small  <- make_plot(subset(grid, d == 0.2), "Potencia para d = 0.2 (pequeño)")
p_medium <- make_plot(subset(grid, d == 0.5), "Potencia para d = 0.5 (mediano)")
p_large  <- make_plot(subset(grid, d == 0.8), "Potencia para d = 0.8 (grande)")

p_small
p_medium
p_large

##############################################################################
# Curvas de potencia vs tamaño muestral (prueba t bilateral, una muestra)
# H0: mu = 120,  H1: mu != 120,  sigma = 15,  alpha = 0.01
##############################################################################

# Parámetros poblacionales y de diseño
mu0   <- 120        # media bajo H0
sigma <- 15         # D.E poblacional
alpha <- 0.01       # nivel de significación
n_vals <- c(10,20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800,900,1000)

# Tamaños del efecto (Cohen's d)
d_vals <- c(0.2, 0.5, 0.8)   # pequeño, mediano, grande

# Malla n x d y cálculo de potencia
grid <- expand.grid(n = n_vals, d = d_vals) %>%
  mutate(delta = d * sigma,  # OJO diferencia absoluta en unidades originales
         power = mapply(function(n, delta){
           power.t.test(n = n,
                        delta = delta,
                        sd = sigma,
                        sig.level = alpha,
                        type = "one.sample",
                        alternative = "two.sided")$power
         }, n, delta))

# Tabla (opcional)
kable(grid %>% arrange(d, n),
      digits = 4,
      caption = "Potencia (1-β) por tamaño muestral y tamaño del efecto (prueba t bilateral, α = 0.01)")

# Gráfico: Potencia vs Tamaño muestral, una curva por d
ggplot(grid, aes(x = n, y = power, color = factor(d), group = d)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_discrete(name = "Tamaño del efecto (d)",
                       labels = c("0.2 (pequeño)", "0.5 (mediano)", "0.8 (grande)")) +
  labs(title = "Curvas de potencia (prueba t bilateral, una muestra)",
       subtitle = expression(paste(mu[0]," = 120,  ", alpha, " = 0.01,  ", sigma, " = 15")),
       x = "Tamaño muestral (n)",
       y = "Potencia (1 - β)") +
  theme_minimal()

###############################################################################
#Graficas separadas
################################################################################



###############################################################################
#Preguntas orientadoras para el análisis
###############################################################################

##############################################################################
# ¿Qué tamaño de muestra se requiere para alcanzar una potencia del 80% 
# al detectar un efecto grande(d=0.8)?
###############################################################################

# de significancia 0.10:
#  El tamaño mínimo de muestra requerido se encuentra entre 10 y 20.

#Nivel de significancia 0.05:
#  El tamaño mínimo de muestra requerido también se encuentra entre 10 y 20.

#Nivel de significancia 0.01:
#  El tamaño mínimo de muestra requerido aumenta, y se encuentra entre 20 y 30.

###############################################################################
# ¿Cuántas observaciones se necesitan para detectar un efecto mediano (d=0.5) 
# con una potencia adecuada?
###############################################################################

# Para α = 0.10, se alcanza una potencia del 80% con un tamaño muestral 
# entre 20 y 30 observaciones.
# Para α = 0.05, se alcanza una potencia del 80% con un tamaño muestral 
# entre 30 y 40 observaciones.
# Para α = 0.01, se alcanza una potencia del 80% con un tamaño muestral 
# entre 50 y 60 observaciones.

#Se considra que la convención de usar 80 % de potencia tiene décadas de aceptación 
#(Cohen, 1988), estudios recientes indican que sigue siendo el referente 
#principal en análisis de potencia a priori 
#(Vankelecom et al., 2025; Buckley, 2024). 
#Sin embargo, trabajos como el de Nakagawa et al. (2024) sugieren que en 
#casos específicos podría requerirse mayor potencia o ajustar este umbral 
#según los riesgos de errores tipo II u otros costos del estudio.


################################################################################
# Para un efecto grande, ¿qué sucede al aumentar el 
# tamaño de muestra más allá de 300 observaciones? ¿Es eficiente?
################################################################################

# Para α = 0.10, lo que sucede es que la potencia llega a 100%, no es 
#eficiente porque ya llego al maximo de potencia cuando llego a las 50 
#observaciones

# Para α = 0.05, lo que sucede es que la potencia llega a 100%, no es 
#eficiente porque ya llego al maximo de potencia cuando llego a las 60 
#observaciones

#Para α = 0.01, lo que sucede es que la potencia llega a 100%, no es 
#eficiente porque ya llego al maximo de potencia cuando llego a las 70 
#observaciones

##############################################################################
#Explica cómo influye el tamaño del efecto sobre la potencia estadística.
##############################################################################

#A mayor tamaño del efecto, mayor es la potencia para un mismo tamaño muestral. 
#De manera equivalente, para alcanzar una potencia determinada 
#(por ejemplo, 0.80), el tamaño muestral requerido disminuye conforme aumenta el efecto.

################################################################################
#¿Cómo afecta el tamaño muestral a la potencia estadística?
###############################################################################

#El tamaño muestral es un factor decisivo en la potencia estadística: 
#a mayor n, mayor potencia, y la magnitud del efecto condiciona cuán 
#rápido se alcanza el umbral de potencia deseado. 
#Efectos grandes se detectan con muestras pequeñas, mientras 
#que efectos pequeños exigen tamaños muestrales muy grandes para evitar errores tipo II.

################################################################################
# ¿En qué punto aumentar el tamaño muestral deja de proporcionar 
# beneficios significativos en la potencia?
################################################################################

# El incremento en el tamaño muestral mejora notablemente la potencia 
# estadística cuando las muestras son pequeñas. Sin embargo, 
# los beneficios adicionales se reducen conforme la potencia se aproxima a 1. 


# En los resultados obtenidos, para un α = 0.10 el aumento deja 
# de ser relevante a partir de aproximadamente 500 muestras cuando el efecto
# es pequeño, 70 muestras cuando el efecto es mediano y 30 muestras
# cuando el efecto es grande. 

# En el caso de α = 0.05 el aumento deja de ser relevante a partir de 
# aproximadamente 500 muestras con efecto pequeño, 
# 80 muestras cuando efecto es mediano y 40 cuando el efecto es grande. 

# Por ultimo, para α = 0.01 el aumento deja de ser relevante a partir de 
# aproximadamente 700 muestras cuando el efecto es pequeño, 100 muestras
#cuando el efecto es mediano y 50 cuando el efecto es grande 

##############################################################################
# Con base en los gráficos obtenidos y tu análisis, propón un tamaño de 
# muestra y un nivel de significancia apropiados para diseñar el estudio.
#############################################################################

# Se considera que un tamaño de muestra de 300 sujetos es adecuado 
# porque es el mínimo, en comparación con otros tamaños evaluados, que 
# garantiza una potencia superior al 80% para los niveles de significancia 
# analizados ( 𝛼 = 0.10 , 0.05 , 0.01 α=0.10,0.05,0.01). 
# Asimismo, se selecciona un nivel de significancia de 0.10, ya que, 
# frente a valores más estrictos (𝛼 = 0.05 α=0.05 y 𝛼 = 0.01 α=0.01), 
# este proporciona la mayor potencia estadística en donde se alcanzan
# los valores de 0.9650, 1.0000 y 1.0000, lo que garantiza una alta 
# probabilidad de detectar diferencias reales en la población.

#############################################################################
# Problema 6: Prueba de hipótesis e intervalo de confianza
# Teniendo en cuenta el contexto y los datos utilizados en el 
# informe estadístico desarrollado en la Actividad 1, 
# formule tres preguntas de investigación relevantes. 
# Para cada pregunta, realice el análisis correspondiente utilizando 
# pruebas de hipótesis e intervalos de confianza. 
# Además, verifique el cumplimiento de los supuestos necesarios para 
# garantizar la validez de los procedimientos estadísticos aplicados.
#############################################################################

#############################################################
#Importar base de datos
#############################################################
data <- readxl::read_excel("data_actividad1.xlsx",sheet=1) #Lectura de .xlsx

# Eliminar duplicados, conservando la primera aparición
data1 <- distinct(data)

############################################################################
############################################################################
#Manejo de valores atípicos
############################################################################
############################################################################

############################################################################
#Datos atipicos de variable Age
##########################################################################

# Copia base
data2 <- data1

# 1) Asegurar tipo numérico correcto para Age
if (is.factor(data2$Age))  data2$Age <- as.numeric(as.character(data2$Age))
if (is.character(data2$Age)) data2$Age <- as.numeric(data2$Age)

# 2) Limpiar imposibles antes de imputar
data2 <- data2 %>%
  mutate(Age = ifelse(Age < 0 | Age > 120, NA, Age))

# 3) Preparar datos para mice
data_mice <- data2 %>%
  mutate(across(where(is.character), as.factor))

vars_imputar <- "Age"
predictores_candidatos <- c(
  "Income","MntWines","MntMeatProducts","MntFishProducts","MntSweetProducts",
  "MntGoldProds","MntRegularProds","Kidhome","Teenhome","Recency","MntTotal",
  "NumDealsPurchases","NumWebPurchases","NumCatalogPurchases","NumStorePurchases",
  "NumWebVisitsMonth","Complain","Response","Customer_Days","AcceptedCmpOverall"
)
predictores <- intersect(predictores_candidatos, names(data_mice))
dt <- data_mice[, unique(c(vars_imputar, predictores)), drop = FALSE]

# Método: solo Age
meth <- make.method(dt); meth[] <- ""
meth["Age"] <- "pmm"   # o "rf"

# Matriz de predicción
pred <- make.predictorMatrix(dt)
diag(pred) <- 0
pred["Age","Age"] <- 0

# (Clave) Regla post-imputación: mantener Age en [0, 120]
post <- make.post(dt)
post["Age"] <- "imp[[j]][ imp[[j]] < 0 | imp[[j]] > 120 ] <- NA"

set.seed(123)
imp <- mice(dt, m = 5, maxit = 10, method = meth,
            predictorMatrix = pred, post = post, seed = 123)

# 4) Sustituir e inspeccionar
comp <- complete(imp, 1)
data2$Age <- comp$Age

############################################################################
#Datos atipicos de variable MntRegularProds
############################################################################

# --- 0) Asegurar tipo NUMÉRICO real ---
# (si es factor/character, conviértelo correctamente)
if (is.factor(data2$MntRegularProds))  data2$MntRegularProds <- as.numeric(as.character(data2$MntRegularProds))
if (is.character(data2$MntRegularProds)) data2$MntRegularProds <- as.numeric(data2$MntRegularProds)

# --- 1) Limpieza previa: NO permitir negativos en observados ---
data2 <- data2 %>%
  mutate(MntRegularProds = ifelse(MntRegularProds < 0, NA, MntRegularProds))

# --- 2) Preparar datos para mice ---
data_mice <- data2 %>% mutate(across(where(is.character), as.factor))

vars_imputar <- "MntRegularProds"
predictores  <- c("Income","Age","MntWines","MntMeatProducts",
                  "MntFishProducts","MntSweetProducts","MntGoldProds",
                  "Kidhome","Teenhome","Recency","MntTotal")

dt <- data_mice[, unique(c(vars_imputar, intersect(predictores, names(data_mice)))), drop = FALSE]

# --- 3) Configuración mice: solo imputamos MntRegularProds con PMM ---
meth <- make.method(dt); meth[] <- ""
meth["MntRegularProds"] <- "pmm"  # también puedes probar "rf"

pred <- make.predictorMatrix(dt)
diag(pred) <- 0
pred["MntRegularProds","MntRegularProds"] <- 0

# (CLAVE) 4) Regla post-imputación: NO permitir negativos en cada iteración
post <- make.post(dt)
# Si PMM llega a proponer un valor < 0, lo marcamos NA para que se vuelva a imputar
post["MntRegularProds"] <- "imp[[j]][ imp[[j]] < 0 ] <- NA_real_"

set.seed(123)
imp <- mice(dt, m = 5, maxit = 10, method = meth,
            predictorMatrix = pred, post = post, seed = 123)

# --- 5) Reemplazar en data2 y verificación ---
comp <- complete(imp, 1)
data2$MntRegularProds <- comp$MntRegularProds

##############################################################################
#Imputacion de datos cualitativos: Marital
#############################################################################

#La columna marital como se comprobo anteriormente, tiene filas en donde
#hay "solo ceros", es decir, no pertenece a ninguna de las categorias
#lo cual no es logico asi que se procede a eliminar esas columnas en donde
#solo hay ceros o cuando hay mas de un 1

dataframe_marital <- list(
  marital_divorced    = data1$marital_Divorced,
  marital_married      = data1$marital_Married,
  marital_single = data1$marital_Single,
  marital_together     = data1$marital_Together,
  marital_widow        = data1$marital_Widow
)

#1) Deteccion de filas problematicas

# columnas dummy
mar_cols <- c("marital_Divorced","marital_Married",
              "marital_Single","marital_Together","marital_Widow")

data2 <- data2 %>%
  # 1) número de unos por fila
  mutate(n_ones = rowSums(across(all_of(mar_cols)) == 1, na.rm = TRUE),
         # 2) bandera: si algún valor no es 0 ni 1
         bad_values = rowSums(across(all_of(mar_cols), ~ !.x %in% c(0,1)), na.rm = TRUE),
         # 3) clasificar cada fila
         marital_flag = case_when(
           bad_values > 0         ~ "valores_invalidos",   # ej. 2, 100, etc.
           n_ones == 0            ~ "solo_ceros",
           n_ones > 1             ~ "multiples_unos",
           n_ones == 1            ~ "valida",
           TRUE                   ~ "indeterminado"
         ))

#2)Limpieza
# Cualquier fila problemática -> convertir todas marital_X en NA
data2 <- data2 %>%
  mutate(across(all_of(mar_cols),
                ~ ifelse(marital_flag == "valida", .x, NA)))

freq(data2$marital_Divorced,cumul = FALSE)

#3)imputacion
# Construir la columna categórica Marital:
#    - filas con exactamente un 1 -> categoría correspondiente
#    - filas con 0, >1 o NA en dummies -> NA (se imputará)
n_ones <- rowSums(data2[, mar_cols] == 1, na.rm = TRUE)

data2 <- data2 %>%
  mutate(
    Marital = case_when(
      n_ones == 1 & marital_Divorced == 1 ~ "Divorced",
      n_ones == 1 & marital_Married  == 1 ~ "Married",
      n_ones == 1 & marital_Single   == 1 ~ "Single",
      n_ones == 1 & marital_Together == 1 ~ "Together",
      n_ones == 1 & marital_Widow    == 1 ~ "Widow",
      TRUE                                ~ NA_character_
    )
  )

# 3) IMPUTACIÓN con mice (imputamos solo Marital)
#    Prepara predictores: ajusta esta lista a tus columnas disponibles
predictores_candidatos <- c(
  "Age","Income","MntWines","MntMeatProducts","MntFishProducts",
  "MntSweetProducts","MntGoldProds","MntRegularProds","Kidhome",
  "Teenhome","Recency","NumDealsPurchases","NumWebPurchases",
  "NumCatalogPurchases","NumStorePurchases","NumWebVisitsMonth",
  "Complain","Response","Customer_Days","AcceptedCmpOverall","MntTotal"
)

# Convierte character a factor (recomendación mice) y arma dataset para imputar
data_mice <- data2 %>% mutate(across(where(is.character), as.factor))
vars <- c("Marital", intersect(predictores_candidatos, names(data_mice)))
dt   <- data_mice[, vars, drop = FALSE]

# Configura método: solo imputamos Marital (factor multinomial)
meth <- make.method(dt); meth[] <- ""
meth["Marital"] <- "polyreg"  # para variable categórica con >2 niveles

# Matriz de predicción (puedes usar quickpred para seleccionar por correlación)
pred <- quickpred(dt, mincor = 0.1, include = "Marital")
diag(pred) <- 0

set.seed(123)
imp <- mice(dt, m = 5, method = meth, predictorMatrix = pred, seed = 123)

# Sustituye Marital imputado en data2
data2$Marital <- complete(imp, 1)$Marital

# 4) REGENERAR DUMMIES limpias y consistentes desde Marital
data2 <- data2 %>%
  mutate(
    marital_Divorced = as.integer(Marital == "Divorced"),
    marital_Married  = as.integer(Marital == "Married"),
    marital_Single   = as.integer(Marital == "Single"),
    marital_Together = as.integer(Marital == "Together"),
    marital_Widow    = as.integer(Marital == "Widow")
  )

# 5) VERIFICACIÓN: cada fila debe tener exactamente un 1
chk <- rowSums(data2[, mar_cols])
table(chk, useNA = "ifany")  # idealmente todo 1

############################################################################
#freq(data2$marital_Divorced) #los valores de 100 ya no estan
#Se ha limpiado e impitado valores de 100 en marital_Divorced
#############################################################################

mar_cols <- c("marital_Divorced","marital_Married",
              "marital_Single","marital_Together","marital_Widow")
niveles <- c("Single","Married","Together","Divorced","Widow")

# 1) Reconstruir Marital desde dummies (limpia valores fuera de 0/1)
data2 <- data2 %>%
  mutate(across(all_of(mar_cols), ~ ifelse(.x %in% c(0,1), .x, NA_integer_)))

n_ones <- rowSums(data2[, mar_cols, drop=FALSE] == 1, na.rm = TRUE)

data2 <- data2 %>%
  mutate(Marital = case_when(
    n_ones == 1 & marital_Divorced == 1 ~ "Divorced",
    n_ones == 1 & marital_Married  == 1 ~ "Married",
    n_ones == 1 & marital_Single   == 1 ~ "Single",
    n_ones == 1 & marital_Together == 1 ~ "Together",
    n_ones == 1 & marital_Widow    == 1 ~ "Widow",
    TRUE                                ~ NA_character_
  )) %>%
  mutate(Marital = factor(Marital, levels = niveles))

# 2) Imputar SOLO los NA de Marital con mice (polyreg)
predictores <- c("Age","Income","MntWines","MntMeatProducts","MntFishProducts",
                 "MntSweetProducts","MntGoldProds","MntRegularProds",
                 "Kidhome","Teenhome","Recency","NumDealsPurchases",
                 "NumWebPurchases","NumCatalogPurchases","NumStorePurchases",
                 "NumWebVisitsMonth","Complain","Response","Customer_Days",
                 "AcceptedCmpOverall","MntTotal")

dt <- data2 %>%
  mutate(across(where(is.character), as.factor)) %>%
  select(Marital, any_of(predictores))

meth <- make.method(dt); meth[] <- ""
meth["Marital"] <- "polyreg"

pred <- make.predictorMatrix(dt)
diag(pred) <- 0
pred["Marital","Marital"] <- 0

set.seed(123)
imp <- mice(dt, m = 5, maxit = 10, method = meth, predictorMatrix = pred, seed = 123)

data2$Marital <- complete(imp, 1)$Marital
data2$Marital <- factor(data2$Marital, levels = niveles)  # asegurar niveles

# 3) FALLOVER (por si aún quedan NA): rellenar con muestreo proporcional
restan <- which(is.na(data2$Marital))
if (length(restan) > 0) {
  dist <- prop.table(table(na.omit(data2$Marital)))
  set.seed(123)
  data2$Marital[restan] <- sample(names(dist), length(restan), replace = TRUE,
                                  prob = as.numeric(dist))
  data2$Marital <- factor(data2$Marital, levels = niveles)
}

# 4) Regenerar dummies consistentes
data2 <- data2 %>%
  mutate(
    marital_Divorced = as.integer(Marital == "Divorced"),
    marital_Married  = as.integer(Marital == "Married"),
    marital_Single   = as.integer(Marital == "Single"),
    marital_Together = as.integer(Marital == "Together"),
    marital_Widow    = as.integer(Marital == "Widow")
  )

# 5) Chequeos rápidos
cat("NA en Marital:", sum(is.na(data2$Marital)), "\n")
print(table(rowSums(data2[, mar_cols, drop=FALSE])))



marital <- sapply(seq_len(nrow(data2)), function(i) {
  nombres_con_uno <- names(dataframe_marital)[
    sapply(dataframe_marital, function(v) v[i] == 1) # Recorre en el orden de la lista
  ]
  
  if (length(nombres_con_uno) == 0) {
    "solo ceros"
  } else {
    paste(nombres_con_uno, collapse = ", ")
  }
})


##############################################################################
#Imputacion: Reemplazar valores faltantes por mice y random forest
#Primero: Imputacion Income
##############################################################################

set.seed(123)

# 1) Preparar datos para mice (trabajamos sobre data2)
data_mice <- data2 %>%
  mutate(across(where(is.character), as.factor))

# 2) Variables a imputar
vars_imputar <- c("Income")

# 3) Configurar métodos y predictores
meth <- make.method(data_mice)
meth[] <- ""                    # por defecto no imputar
meth[vars_imputar] <- "rf"      # random forest para Income (puedes usar "pmm" si prefieres)

pred <- quickpred(data_mice, mincor = 0.1, include = vars_imputar)
diag(pred) <- 0                 # no predecirse a sí misma

# 4) Ejecutar mice
imp <- mice(data_mice, m = 5, maxit = 20,
            method = meth, predictorMatrix = pred, seed = 123)

# 5) Extraer una versión completa y pasar la imputación a data2
comp_rf <- complete(imp, 1)
data2$Income <- comp_rf$Income

###############################################################################
#Imputacion MntWines
###############################################################################

set.seed(123)

# 1) Prepara datos para mice (sobre data2)
data_mice <- data2 %>%
  mutate(across(where(is.character), as.factor))

# 2) Variables a imputar
vars_imputar <- c("MntWines")   # puedes incluir más, ej: c("Income","MntWines")

# 3) Configurar métodos/predictores
meth <- make.method(data_mice)
meth[] <- ""                     # por defecto no imputar nada
meth[vars_imputar] <- "rf"       # imputar MntWines con random forest

# 4) Configurar predictores
pred <- quickpred(data_mice, mincor = 0.1, include = vars_imputar)
diag(pred) <- 0                  # una variable no se predice a sí misma

# 5) Ejecutar mice con random forest
imp <- mice(data_mice, m = 5, maxit = 20,
            method = meth, predictorMatrix = pred, seed = 123)

# 6) Extraer imputación completa y guardar en data2
comp_rf <- complete(imp, 1)
data2$MntWines <- comp_rf$MntWines

sum(is.na(data2$Income))
sum(is.na(data2$MntWines))

summarytools::freq(data2$Age)
summarytools::freq(data2$MntRegularProds)
summarytools::freq(data2$marital_Divorced)
View(data2)

data2$Marital[70]
summarytools::freq(data2$Marital)

data2$NumHijos <- data2$Kidhome + data2$Teenhome

data2$education_2nCycle <- data2$`education_2n Cycle`

data2 <- data2 %>%
  mutate(
    Education = case_when(
      education_2nCycle == 1 ~ "2nd Cycle",
      education_Basic == 1 ~ "Basic",
      education_Graduation == 1 ~ "Graduation",
      education_Master == 1 ~ "Master",
      education_PhD == 1 ~ "PhD",
      TRUE ~ NA_character_
    )
  )

##############################################################################
##############################################################################
#Situacion metodos no parametricos
##############################################################################
##############################################################################

##############################################################################
### Income vs Nvl educativo
##############################################################################

###############################################################################
#Test de normalidad
################################################################################
fBasics::dagoTest(data2$Income)

##############################################################################
#prueba de Kruskal–Wallis
#############################################################################

#Hipotesis 

#H_0 (nula): La distribución de Income es la misma en todos los grupos de Education.
#H_1 (alternativa): Al menos un grupo tiene una distribución (o mediana) diferente de Income.

# Se fija un nivel de significancia de 0.001 debido al tamaño de la muestra
# Se estableció un nivel de significancia de 0.001 con el fin de 
# reducir la probabilidad de falsos positivos y garantizar que 
# únicamente diferencias en la distribucion realmente relevantes superen el 
# umbral de significancia.

data3 <- data2 %>%
  select(Income, Education)
View(data3)

# Asegúrate de que Education es factor
data3$Education <- as.factor(data3$Education)

# Kruskal-Wallis
kruskal.test(Income ~ Education, data = data3)

# Se rechaza la hipotesis nula debido a que p-value es menor a 0.001 lo cual
# indica que al menos la distribucion de un grupo educativo difiere de los demás, por lo que se 
# requiere un análisis post-hoc para identificar cuáles pares de niveles educativos difieren significativamente.

# Para ello se hara el post-hoc de Nemenyi
posthoc <- kwAllPairsNemenyiTest(Income ~ Education, data = data3)

# Ver resultados
summary(posthoc)

# Los resultados del análisis post-hoc muestran que los ingresos 
# difieren de manera significativa entre los niveles educativos más 
# bajos (2nd Cycle y Basic) y los niveles superiores 
# (Graduation, Master y PhD), con p-valores muy pequeños que 
# indican diferencias consistentes. 
# En cambio, entre los niveles más altos de formación 
# (Graduation, Master y PhD) no se observan diferencias 
# estadísticamente significativas, lo que sugiere que el principal 
# salto en los ingresos se produce al pasar de una educación básica a 
# una universitaria, mientras que los estudios de posgrado no implican 
# incrementos sustanciales en el ingreso respecto al nivel de pregrado.

#############################################################################
#Intervalos de confianza
#############################################################################

# Aseguramos el orden del factor Education
data3$Education <- factor(data3$Education,
                          levels = c("Basic", "2nd Cycle", "Graduation", "Master", "PhD"))

# Función bootstrap para la MEDIANA
boot_median <- function(data, indices) {
  d <- data[indices]
  median(d, na.rm = TRUE)
}

# Parámetros
set.seed(123)   # reproducibilidad
B <- 5000       # número de réplicas bootstrap
alpha <- 0.05   # nivel de confianza (95%)

# Calcular IC BCa para cada nivel
resultados <- lapply(levels(data3$Education), function(niv) {
  subset_data <- data3$Income[data3$Education == niv]
  subset_data <- subset_data[!is.na(subset_data)]   # quitar NA para estabilidad
  n_g <- length(subset_data)
  
  # Si no hay datos suficientes, devolvemos NA
  if (n_g < 2) {
    return(data.frame(
      Education = niv,
      n = n_g,
      Median = ifelse(n_g == 1, subset_data, NA_real_),
      IC_low = NA_real_,
      IC_high = NA_real_
    ))
  }
  
  boot_obj <- boot(data = subset_data, statistic = boot_median, R = B)
  ci <- boot.ci(boot_obj, conf = 1 - alpha, type = "bca")
  
  data.frame(
    Education = niv,
    n = n_g,
    Median = median(subset_data),
    IC_low = ci$bca[4],
    IC_high = ci$bca[5]
  )
})

# Unir en data frame y mostrar
df_resultados <- bind_rows(resultados)
print(df_resultados)

###############################################################################
### Income vs n hijos
###############################################################################
data2 <- data2 %>%
  mutate(KidTeenHome = Kidhome + Teenhome)

data4 <- data2 %>%
  select(Income, KidTeenHome)

#prueba de Kruskal–Wallis

#Hipotesis 

#H_0 (nula): La distribución de Income es la misma en todos los grupos de Education.
#H_1 (alternativa): Al menos un grupo tiene una distribución (o mediana) diferente de Income.

# Se fija un nivel de significancia de 0.001 debido al tamaño de la muestra
# Se estableció un nivel de significancia de 0.001 con el fin de 
# reducir la probabilidad de falsos positivos y garantizar que 
# únicamente diferencias en la distribucion realmente relevantes superen el 
# umbral de significancia.

data4$KidTeenHome <- as.factor(data4$KidTeenHome)

# Aplicamos Kruskal-Wallis
kruskal.test(Income ~ KidTeenHome, data = data4)

# Post-hoc Nemenyi
posthoc2 <- kwAllPairsNemenyiTest(Income ~ KidTeenHome, data = data4)

# Ver resultados
summary(posthoc2)

# El análisis post-hoc de Nemenyi mostró que los ingresos 
# difieren significativamente entre quienes no tienen hijos y quienes tienen 
# 1, 2 o 3 hijos (p < 0.001). En cambio, entre los grupos con hijos 
# (1 vs 2, 1 vs 3, 2 vs 3) no se encontraron diferencias significativas, lo que 
# indica que la principal brecha de ingresos se produce entre los hogares sin 
# hijos y los que tienen al menos un hijo.

##############################################################################
# Intervalos de confianza
##############################################################################

# Parámetros
set.seed(123)   # reproducibilidad
B <- 5000       # número de réplicas bootstrap
alpha <- 0.05   # IC 95%

# Aseguramos que KidTeenHome sea un factor ordenado por número de hijos
data4$KidTeenHome <- factor(data4$KidTeenHome,
                            levels = sort(unique(data4$KidTeenHome)))

# Calcular IC BCa por número de hijos
resultados <- lapply(levels(data4$KidTeenHome), function(niv) {
  subset_data <- data4$Income[data4$KidTeenHome == niv]
  subset_data <- subset_data[!is.na(subset_data)]   # quitar NA
  n_g <- length(subset_data)
  
  if (n_g < 2) {
    return(data.frame(
      KidTeenHome = niv,
      n = n_g,
      Median = ifelse(n_g == 1, subset_data, NA_real_),
      IC_low = NA_real_,
      IC_high = NA_real_
    ))
  }
  
  boot_obj <- boot(data = subset_data, statistic = boot_median, R = B)
  ci <- boot.ci(boot_obj, conf = 1 - alpha, type = "bca")
  
  data.frame(
    KidTeenHome = niv,
    n = n_g,
    Median = median(subset_data),
    IC_low = ci$bca[4],
    IC_high = ci$bca[5]
  )
})

# Combinar resultados en data frame
df_resultados <- bind_rows(resultados)
print(df_resultados)

###############################################################################
#MntSum vs AcceptedCmpOverall
#############################################################################

#Creacion de columna MntSum
data2 <- data2 %>%
  mutate(MntSum = MntWines + MntFruits + MntMeatProducts +
           MntFishProducts + MntSweetProducts + MntGoldProds)

# Revision de normalidad
nortest::cvm.test(data2$MntSum)

#Creamos data 5 para aislarlos
data5 <- data2 %>%
  select(MntSum, AcceptedCmpOverall)

#Hipotesis de prueba

#H_0 (nula): La distribución de MntSum es la misma en todos los grupos de AcceptedCmpOverall.
#H_1 (alternativa): Al menos un grupo tiene una distribución (o mediana) diferente de MntSum.

# Se fija un nivel de significancia de 0.001 debido al tamaño de la muestra
# Se estableció un nivel de significancia de 0.001 con el fin de 
# reducir la probabilidad de falsos positivos y garantizar que 
# únicamente diferencias en la distribucion realmente relevantes superen el 
# umbral de significancia.

data5$AcceptedCmpOverall <- as.factor(data5$AcceptedCmpOverall)

# Aplicamos Kruskal-Wallis
kruskal.test(MntSum ~ AcceptedCmpOverall, data = data5)

# Se rechaza la hipotesis nula debido a que p-value es menor a 0.001 lo cual
# indica que al menos la distribucion de un grupo que acepto la campaña difiere de los demás, por lo que se 
# requiere un análisis post-hoc para identificar cuáles grupos difieren significativamente.

posthoc <- kwAllPairsNemenyiTest(MntSum ~ AcceptedCmpOverall, data = data5)
summary(posthoc)

# Los resultados del análisis post-hoc de Nemenyi muestran que los clientes que no 
# aceptaron ninguna campaña presentan un gasto total significativamente menor en 
# comparación con todos los grupos que aceptaron al menos una campaña 
# (p < 0.001 en todos los casos). Además, entre quienes sí aceptaron, se 
# observa una diferencia significativa únicamente entre los que aceptaron 
# una campaña y los que aceptaron dos (p < 0.001), mientras que no se encuentran
# diferencias estadísticamente significativas entre los grupos que 
# aceptaron dos, tres o cuatro campañas. En conjunto, esto indica que el mayor 
# contraste en el gasto total se da entre no haber aceptado campañas frente a 
# haber aceptado al menos una, con un incremento adicional al pasar de una a 
# dos campañas aceptadas.

##############################################################################
# Intervalos de confianza
##############################################################################

# Función bootstrap para la mediana
boot_median <- function(data, indices) {
  d <- data[indices]
  median(d, na.rm = TRUE)
}

set.seed(123)   # reproducibilidad
B <- 5000       # número de réplicas
alpha <- 0.05   # IC 95%

# Aseguramos que AcceptedCmpOverall sea factor
data2$AcceptedCmpOverall <- as.factor(data2$AcceptedCmpOverall)

# Calcular IC BCa por cada categoría
resultados <- lapply(levels(data2$AcceptedCmpOverall), function(niv) {
  subset_data <- data2$MntSum[data2$AcceptedCmpOverall == niv]
  subset_data <- subset_data[!is.na(subset_data)]
  n_g <- length(subset_data)
  
  if (n_g < 2) {
    return(data.frame(
      AcceptedCmpOverall = niv,
      n = n_g,
      Median = ifelse(n_g == 1, subset_data, NA_real_),
      IC_low = NA_real_,
      IC_high = NA_real_
    ))
  }
  
  boot_obj <- boot(data = subset_data, statistic = boot_median, R = B)
  
  ci <- try(boot.ci(boot_obj, conf = 1 - alpha, type = "bca"), silent = TRUE)
  
  if (inherits(ci, "try-error") || is.null(ci$bca)) {
    return(data.frame(
      AcceptedCmpOverall = niv,
      n = n_g,
      Median = median(subset_data),
      IC_low = NA_real_,
      IC_high = NA_real_
    ))
  } else {
    return(data.frame(
      AcceptedCmpOverall = niv,
      n = n_g,
      Median = median(subset_data),
      IC_low = ci$bca[4],
      IC_high = ci$bca[5]
    ))
  }
})

# Combinar en data frame
df_resultados <- bind_rows(resultados)
print(df_resultados)
