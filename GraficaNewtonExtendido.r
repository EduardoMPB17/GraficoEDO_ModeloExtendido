#Codigo en R que simula graficamente el comportamiento de la temperatura de un
#sistema(PC) con respecto al tiempo, dependiendo de los valores simulado de
#k1 y k2 

# Cargar las librerias necesaria
install.packages("deSolve")
library(deSolve)
  
# Definir parámetros del problema
k1 <- 2.0   # Constante de disipación(valor de prueba)
k2 <- 1.0   # Constante de acoplamiento(valor de prueba)
T_amb <- 50 # Temperatura ambiente
T0 <- 80    # Temperatura inicial
T0_prime <- 0 # Derivada inicial (cambio de temperatura)

# Función que define el sistema de ecuaciones diferenciales
modelo_temperatura <- function(t, y, params) {
  T <- y[1]         # Temperatura
  T_prime <- y[2]   # Derivada de la temperatura
  dTdt <- T_prime
  dT2dt2 <- -k1 * T_prime - k2 * (T - T_amb)
  list(c(dTdt, dT2dt2))
}
  
# Condiciones iniciales
y_inicial <- c(T = T0, T_prime = T0_prime)
  
# Tiempo de simulación
tiempo <- seq(0, 10, by = 0.01) # Desde t=0 hasta t=10 con pasos de 0.01(10s)
  
# Resuelve el sistema de ecuaciones diferenciales
sol <- ode(y = y_inicial, times = tiempo, func = modelo_temperatura, parms = NULL)

# Grafica la solución
sol <- as.data.frame(sol) # Convertir la solución a un dataframe para graficar

# Crea el gráfico
plot(sol$time, sol$T, type = "l", col = "blue", lwd = 2, xlab = "Tiempo (s)", ylab = "Temperatura (°C)",
     main = "Evolución de la temperatura del sistema")
abline(h = T_amb, col = "red", lty = 2, lwd = 2) # Línea para T_amb
legend("topright", legend = c("Temperatura T(t)", "Temperatura ambiente T_amb"),
       col = c("blue", "red"), lty = c(1, 2), lwd = c(2, 2))
grid()
