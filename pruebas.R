# Función para cargar/guardar datos desde un archivo CSV:
cargar_datos <- function() {
  # Instalamos las librerias dplyr, readxl para la manipulación de datos .xlsx y la lectura de los mismos.
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
  }
  library(ggplot2)
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    install.packages("dplyr")
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    install.packages("readxl")
  }
  library(dplyr)
  library(readxl)
  # Colocamos el url o link del archivo local en la maquina donde
  # se halla el archivo .xlsx para la extracción de datos.
  url <- "C:/Users/emilv/OneDrive/Documents/R-Studio/Deber 1-Intervalos Grupal 2/Calificaciones.xlsx"
  calificaciones <- readxl::read_excel(url)
  return (calificaciones)

}
calificaciones <- cargar_datos()

filtar_datos <-function(calificaciones) {
  
  # Tomamos todos los datos filtrados de "Medicina" para tener una evidencia en la aproximacion de medias, varianzas y proporciones
  # como tambien saber la cantidad de N datos disponibles para hallar las respectivas muestras aleatorias y buscar sus I.Confianza

  nombre_carrera <- calificaciones$nombre_carrera
  
  resultado_global<- calificaciones$resultado_global
  
  
  return(data.frame(nombre_carrera,resultado_global))
}

data_filtrada <- filtar_datos(calificaciones)

# Función para calcular media, varianza y proporción del archivo .xlsx:
calcular_estadisticas <- function(data_filtrada) {
  media_poblacional <- mean(data_filtrada$resultado_global)
  varianza_poblacional <- var(data_filtrada$resultado_global)

  count_medicina <- data_filtrada%>% filter(nombre_carrera == "MEDICINA") %>% select()%>% count()
  count_carreras <-data_filtrada%>% select(nombre_carrera)%>% count()

  #medicina_count <- as.integer(medicina_count)
  proporcion_medicina_poblacional <- as.double(count_medicina / count_carreras)
  
  
  cat("Calculando estadísticas del CSV...\n")
  # Devolvemos una lista de los valores aproximados del .csv por "Medicina"
  return(data.frame(media_poblacional, varianza_poblacional, proporcion_medicina_poblacional))
}
parametros <- calcular_estadisticas(data_filtrada)
print(calcular_estadisticas(data_filtrada))



muestrar <- function(valores){
  lista_muestras <- list()
  
  # Generar 100 muestras de 500 datos cada una
  for (i in 1:100) {
    muestra <- sample(valores, 500, replace = FALSE)
    lista_muestras[[i]] <- muestra
  }
  return(lista_muestras)
}
muestras_resultado_global<-muestrar(data_filtrada$resultado_global)
muestras_proporcion<-muestrar(data_filtrada$nombre_carrera)







calcular_intervalos_media <- function(muestras_media, confianza = 0.95) {
  lista_intervalo <- list()
  
  for (i in seq_along(muestras_media)) {
    media_muestra <- mean(muestras_media[[i]])
    desviacion_estandar_muestra <- sd(muestras_media[[i]])
    error_estandar <- qt((1 + confianza) / 2, df = length(muestras_media[[i]]) - 1) * desviacion_estandar_muestra / sqrt(length(muestras_media[[i]]))
    
    intervalo <- c(media_muestra - error_estandar, media_muestra + error_estandar)
    lista_intervalo[[i]] <- intervalo
  }
  
  return(lista_intervalo)
}
# Calcular intervalos de confianza para la media
intervalos_media <- calcular_intervalos_media(muestras_media, confianza = 0.95)
print(intervalos_media)

calcular_intervalos_varianza <- function(muestras_varianza, confianza = 0.95) {
  lista_intervalo <- list()
  
  for (i in seq_along(muestras_varianza)) {
    varianza_muestra <- var(muestras_varianza[[i]])
    chi_cuadrado_superior <- qchisq((1 + confianza) / 2, df = length(muestras_varianza[[i]]) - 1)
    chi_cuadrado_inferior <- qchisq((1 - confianza) / 2, df = length(muestras_varianza[[i]]) - 1)
    
    intervalo <- c((length(muestras_varianza[[i]]) - 1) * varianza_muestra / chi_cuadrado_superior,
                   (length(muestras_varianza[[i]]) - 1) * varianza_muestra / chi_cuadrado_inferior)
    lista_intervalo[[i]] <- intervalo
  }
  
  return(lista_intervalo)
}
# Calcular intervalos de confianza para la varianza
intervalos_varianza <- calcular_intervalos_varianza(muestras_varianza, confianza = 0.95)

print(intervalos_varianza)
# Calcular intervalos de confianza para la varianza


calcular_intervalos_proporcion <- function(muestras_proporcion, confianza = 0.95) {
  lista_intervalo <- list()
  
  for (i in seq_along(muestras_proporcion)) {
    proporciones_muestra <- sum(muestras_proporcion[[i]] == "MEDICINA") / length(muestras_proporcion[[i]])
    z_score <- qnorm((1 + confianza) / 2)
    n <- length(muestras_proporcion[[i]])
    
    intervalo <- c((2 * n * proporciones_muestra + z_score^2 - 
                      z_score * sqrt(4 * n * proporciones_muestra * (1 - proporciones_muestra) + z_score^2)) / (2 * (n + z_score^2)),
                   (2 * n * proporciones_muestra + z_score^2 + 
                      z_score * sqrt(4 * n * proporciones_muestra * (1 - proporciones_muestra) + z_score^2)) / (2 * (n + z_score^2)))
    lista_intervalo[[i]] <- intervalo
  }
  
  return(lista_intervalo)
}

# Calcular intervalos de confianza para la proporción
intervalos_proporcion <- calcular_intervalos_proporcion(muestras_proporcion, confianza = 0.95)
print(intervalos_proporcion)
































graficador <- function(secuencia_n, valores_min, valores_max, media_poblacional) {
  
# Crear un data.frame de ejemplo con 10 intervalos
datos_intervalos <- data.frame(
  n<- secuencia_n,
  inicio<- valores_min,  # Ejemplo: intervalos generados aleatoriamente entre 40 y 60
  fin <- valores_max, # Ejemplo: intervalos generados aleatoriamente entre 60 y 80
  media <- media_poblacional
)


# Crear el gráfico de líneas para representar los intervalos
ggplot(datos_intervalos) +
  geom_segment(aes(x = inicio, y = n, xend = fin, yend = n), color = "blue", size = 1) +
  geom_vline(xintercept = media, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Gráfico de Líneas que Representan 10 Intervalos", y = NULL, x = "Eje X") +
  theme_minimal()

}
graficador(seq(1,100), sample(seq(1, 10), 100, replace = TRUE), sample(seq(11, 20), 100, replace = TRUE),10)






library(ggplot2)

graficador <- function(secuencia_n, intervalos, media_poblacional) {
  datos_intervalos <- data.frame(
    n = secuencia_n,
    inicio = sapply(intervalos, function(x) x[1]),
    fin = sapply(intervalos, function(x) x[2]),
    media = media_poblacional
  )
  
  ggplot(datos_intervalos) +
    geom_segment(aes(x = inicio, y = n, xend = fin, yend = n), color = "blue", size = 1) +
    geom_vline(xintercept = media_poblacional, linetype = "dashed", color = "red", size = 1) +
    labs(title = "Gráfico de Líneas que Representan Intervalos", y = NULL, x = "Eje X") +
    theme_minimal()
}


graficador(seq(1, 100), intervalos_proporcion, parametros$proporcion_medicina_poblacional)
graficador(seq(1, 100), intervalos_media, parametros$media_poblacional)
graficador(seq(1, 100), intervalos_varianza, parametros$varianza_poblacional)


