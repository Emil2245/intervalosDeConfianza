# Función para cargar/guardar datos desde un archivo CSV:
cargar_datos <- function() {
  # Instalamos las librerias stats, dplyr, tidyr para la manipulación
  # de datos .xlsx y la lectura de los mismos.
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    install.packages("dplyr")
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    install.packages("readxl")
  }
  library(dplyr)
  library(stats)
  # Colocamos el url o link del archivo local en la maquina donde
  # se halla el archivo .csv para la extracción de datos.
  url <- "C:/Users/emilv/OneDrive/Documents/R-Studio/Deber 1-Intervalos Grupal 2/Calificaciones.xlsx"
  calificaciones <- readxl::read_excel(url)
  
  # Dividimos cada columna por su item requerido o categoría específica
  final_resultado_global <- calificaciones$resultado_global
  final_nombre_carrera <- calificaciones$nombre_carrera
  
}

cargar_datos()

# Función para calcular media, varianza y proporción del archivo .CSV:
calcular_estadisticas_csv <- function() {
  # Tomamos todos los datos filtrados de "Medicina" para tener una
  # evidencia en la aproximacion de medias, varianzas y proporciones
  # como tambien saber la cantidad de N datos disponibles para hallar
  # las respectivas muestras aleatorias y buscar sus I.Confianza
  media_medicina <- mean(as.numeric(datos_medicina$Resultado_Global))
  varianza_medicina <- var(as.numeric(datos_medicina$Resultado_Global))
  proporcion_medicina <- (nrow(datos_medicina) / 10000) * 100
  
  cat("Media (csv): ", media_medicina, "\n")
  cat("Varianza (csv): ", varianza_medicina, "\n")
  cat("Proporción (Csv):", proporcion_medicina, "\n")
  cat("Calculando estadísticas del CSV...\n")
  
  # Devolvemos una lista de los valores aproximados del .csv por "Medicina"
  return(list(media = media_medicina, varianza = varianza_medicina, proporcion = proporcion_medicina))
}

# Función para calcular media, varianza y proporción de una muestra aleatoria:
calcular_estadisticas_muestra_aleatoria <- function() {
  # Es necesario hacer pruebas de estadisticos para la media, varianza y
  # proporción (muestral) de los datos almacenados de "Medicina", observando
  # que exista una ligera aproximacion a las medidas reales del archivo .csv por 
  # "Medicina" para una m1 (muestra) de tamaño 500.
  muestra_comun <- datos_medicina %>% sample_n(500, replace = FALSE)
  media_muestra_comun <- mean(as.numeric(muestra_comun$Resultado_Global))
  varianza_muestra_comun <- var(as.numeric(muestra_comun$Resultado_Global))
  proporcion_muestra_comun <- nrow(muestra_comun) / nrow(datos_medicina) * 100
  
  cat("Media (1 sola muestra): ", media_muestra_comun, "\n")
  cat("Varianza (1 sola muestra): ", varianza_muestra_comun, "\n")
  cat("Proporción (1 sola muestra): ", proporcion_muestra_comun, "\n")
  cat("Calculando estadísticas de una muestra aleatoria...\n")
  
  # Devolvemos un resumen de los estadisticos de prueba en una sola muestra
  return(list(media = media_muestra_comun, varianza = varianza_muestra_comun, proporcion = proporcion_muestra_comun))
}

# Función para obtener muestras de datos:
obtener_muestras <- function() {
  # Solicitamos al usuario el número/tamaño n de muestras que desea calcular con 500 observaciones
  num_muestras <- as.integer(readline(prompt = "Ingrese el número de muestras: "))
  
  # Validamos que el número de muestras sea mayor a 0
  if (num_muestras <= 0) {
    cat("Por favor, ingrese un número de muestras válido mayor que cero.\n")
    return()
  }
  
  # Inicializamos listas para almacenar las distintas muestras con tamaño 500
  muestras_media <- list()
  muestras_proporcion <- list()
  muestras_varianza <- list()
  
  # Generamos las muestras de forma aleatoria tomando los datos almacenados de "Medicina"
  for (i in 1:num_muestras) {
    # Permitimos que los datos replicados no se sobreescriban
    muestra_media <- datos_medicina %>%
      sample_n(500, replace = FALSE)
    
    muestra_proporcion <- datos_medicina %>%
      sample_n(500, replace = FALSE)
    
    muestra_varianza <- datos_medicina %>%
      sample_n(500, replace = FALSE)
    
    # Guardamos las distintas n muestras en las listas
    muestras_media[[paste0("Muestra", i)]] <- muestra_media
    muestras_proporcion[[paste0("Muestra", i)]] <- muestra_proporcion
    muestras_varianza[[paste0("Muestra", i)]] <- muestra_varianza
  }
  
  confianza <- c(0.68, 0.95, 0.997)
  
  # Crear una función para calcular la media, proporción y varianza
  calcular_estadisticas_muestra <- function(muestra, nombre_muestra) {
    media_muestra <- mean(as.numeric(muestra$Resultado_Global))
    proporcion_muestra <- nrow(muestra) / nrow(datos_medicina)
    varianza_muestra <- var(as.numeric(muestra$Resultado_Global))
    
    cat("Muestra", nombre_muestra, "Media:", media_muestra, "\n")
    cat("Muestra", nombre_muestra, "Proporción:", proporcion_muestra, "\n")
    cat("Muestra", nombre_muestra, "Varianza:", varianza_muestra, "\n\n")
    
    # Devolver un vector con las estadísticas
    return(c(media = media_muestra, proporcion = proporcion_muestra, varianza = varianza_muestra))
  }
  
  # Crear una lista para almacenar los resultados de todas las muestras
  resultados_muestras <- list()
  
  # Generar y calcular estadísticas para cada muestra
  for (i in 1:num_muestras) {
    nombre_muestra <- paste("muestra", i)
    muestra <- datos_medicina %>% sample_n(500, replace = FALSE)
    resultados_muestras[[nombre_muestra]] <- calcular_estadisticas_muestra(muestra, nombre_muestra)
  }
  
  # Imprimir los resultados
  for (i in 1:num_muestras) {
    nombre_muestra <- paste("muestra", i)
    cat("Resultados de", nombre_muestra, ":\n")
    cat("Media:", resultados_muestras[[nombre_muestra]]["media"], "\n")
    cat("Proporción:", resultados_muestras[[nombre_muestra]]["proporcion"], "\n")
    cat("Varianza:", resultados_muestras[[nombre_muestra]]["varianza"], "\n\n")
  }
  
  cat("Obteniendo", num_muestras, "muestras de 500 datos...\n")
}

# Función para calcular intervalos de confianza (muestras) y mostrarlos:
calcular_intervalos_confianza <- function() {
  # Solicitamos al usuario el número/tamaño n de muestras que desea calcular con 500 observaciones
  num_muestras2 <- as.integer(readline(prompt = "Ingrese el número de muestras: "))
  
  # Validamos que el número de muestras sea mayor a 0
  if (num_muestras2 <= 0) {
    cat("Por favor, ingrese un número de muestras válido mayor que cero.\n")
    return()
  }
  # El numero fijo de n muestras que nuestro codigo puede ejecutar es
  # de 9 en 9, es decir de las n muestras distintas, puede calcular apenas 9
  # y mostrarlas en consola con 3 tipos de niveles de confianza, si se desea
  # un nuevo calculo para los IC se debe volver a ejecutar este código y tomara
  # de nuevo otras 9 muestras distintas de nuestros datos almacenados en las listas
  
  num_muestras<- num_muestras2
  # --------------------------------- MEDIA -------------------------------------
  print(">>>>>>>>>>>>>>>>>>MEDIA<<<<<<<<<<<<<<<<<<<<<<<<<<")
  # Nivel de confianza (68%):
  resultados_intervalos_confianza_68 <- list()
  longitudes_intervalos_68 <- numeric(num_muestras)
  
  # Calculamos intervalo de confianza para cada muestra con 68%
  for (i in 1:num_muestras) {
    muestra_media <- muestras_media[[paste0("muestra", i)]]
    
    media_muestral <- mean(as.numeric(muestra_media$Resultado_Global))
    desviacion_estandar_poblacional <- sqrt(var(as.numeric(datos_medicina$Resultado_Global)))
    n <- nrow(muestra_media)
    
    nivel_confianza <- confianza[1]
    alpha <- 1 - nivel_confianza
    z_critico <- qnorm(alpha / 2)
    error_estandar <- desviacion_estandar_poblacional / sqrt(n)
    intervalo_confianza <- c(media_muestral - z_critico * error_estandar, media_muestral + z_critico * error_estandar)
    
    # Guardamos resultados
    resultados_intervalos_confianza_68[[paste("muestra", i)]] <- intervalo_confianza
    
    # Guardamos longitudes
    longitudes_intervalos_68[i] <- intervalo_confianza[1] - intervalo_confianza[2]
  }
  
  # Imprimimos resultados:
  # Guardamos las Longitudes para graficarlas en un diagrama representativo de las
  # n muestras con I.Confianza
  for (i in 1:num_muestras) {
    cat("Nivel de confianza del 68%", ":\n")
    nombre_muestra <- paste("muestra", i)
    cat("Resultados del intervalo de confianza para", nombre_muestra, ":\n")
    cat("Intervalo de Confianza: [", resultados_intervalos_confianza_68[[nombre_muestra]], "]\n")
    cat("Longitud: {", longitudes_intervalos_68[i], "}\n\n")
  }
  #---------------------
  # Nivel de confianza (95%)
  resultados_intervalos_confianza_95 <- list()
  longitudes_intervalos_95 <- numeric(num_muestras)
  
  # Calculamos el intervalo de confianza para cada muestra con 95%
  for (i in 1:num_muestras) {
    muestra_media <- muestras_media[[paste0("muestra", i)]]
    
    media_muestral <- mean(as.numeric(muestra_media$Resultado_Global))
    desviacion_estandar_poblacional <- sqrt(var(as.numeric(datos_medicina$Resultado_Global)))
    n <- nrow(muestra_media)
    
    nivel_confianza <- confianza[2]
    alpha <- 1 - nivel_confianza
    z_critico <- qnorm(alpha / 2)
    error_estandar <- desviacion_estandar_poblacional / sqrt(n)
    intervalo_confianza <- c(media_muestral - z_critico * error_estandar, media_muestral + z_critico * error_estandar)
    
    # Guardamos resultados
    resultados_intervalos_confianza_95[[paste("muestra", i)]] <- intervalo_confianza
    
    # Guardamos longitudes
    longitudes_intervalos_95[i] <- intervalo_confianza[1] - intervalo_confianza[2]
  }
  
  # Imprimir resultados 
  # Guardamos la longitud para graficarlas en un diagrama representativo
  for (i in 1:num_muestras) {
    cat("Nivel de confianza del 95%", ":\n")
    nombre_muestra <- paste("muestra", i)
    cat("Resultados del intervalo de confianza para", nombre_muestra, ":\n")
    cat("Intervalo de Confianza: [", resultados_intervalos_confianza_95[[nombre_muestra]], "]\n")
    cat("Longitud: {", longitudes_intervalos_95[i], "}\n\n")
  }
  #---------------------
  # Nivel de confianza (99,7%):
  resultados_intervalos_confianza_997 <- list()
  longitudes_intervalos_997 <- numeric(num_muestras)
  
  # Calculamos intervalo de confianza para cada muestra con 99,7%
  for (i in 1:num_muestras) {
    muestra_media <- muestras_media[[paste0("muestra", i)]]
    
    media_muestral <- mean(as.numeric(muestra_media$Resultado_Global))
    desviacion_estandar_poblacional <- sqrt(var(as.numeric(datos_medicina$Resultado_Global)))
    n <- nrow(muestra_media)
    
    nivel_confianza <- confianza[3]
    alpha <- 1 - nivel_confianza
    z_critico <- qnorm(alpha / 2)
    error_estandar <- desviacion_estandar_poblacional / sqrt(n)
    intervalo_confianza <- c(media_muestral - z_critico * error_estandar, media_muestral + z_critico * error_estandar)
    
    # Guardamos resultados
    resultados_intervalos_confianza_997[[paste("muestra", i)]] <- intervalo_confianza
    
    # Guardamos longitudes
    longitudes_intervalos_997[i] <- intervalo_confianza[1] - intervalo_confianza[2]
  }
  
  # Imprimimos resultados 
  # Guardamos las longitudes para representarlas en un diagrama
  for (i in 1:num_muestras) {
    cat("Nivel de confianza del 99,7%", ":\n")
    nombre_muestra <- paste("muestra", i)
    cat("Resultados del intervalo de confianza para", nombre_muestra, ":\n")
    cat("Intervalo de Confianza: [", resultados_intervalos_confianza_997[[nombre_muestra]], "]\n")
    cat("Longitud: {", longitudes_intervalos_997[i], "}\n\n")
  }
  print("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")
  #------------------------ VARIANZA -------------------------------
  print(">>>>>>>>>>>>>>>>>>VARIANZA<<<<<<<<<<<<<<<<<<<<<<<<<<")
  # Nivel de confianza (68%)
  resultados_intervalos_varianza_68 <- list()
  longitudes_intervalos_varianza_68 <- numeric(num_muestras)
  
  # Calculamos intervalo de confianza para la varianza en cada muestra con 68%
  for (i in 1:num_muestras) {
    muestra_varianza <- muestras_varianza[[paste0("muestra", i)]]
    
    varianza_muestral <- var(as.numeric(muestra_varianza$Resultado_Global))
    n <- nrow(muestra_varianza)
    
    nivel_confianza <- confianza[1]
    alpha <- 1 - nivel_confianza
    
    # Cuantiles de chi-cuadrado superior e inferior
    chi_cuadrado_inf <- qchisq(alpha / 2, df = n - 1)
    chi_cuadrado_sup <- qchisq(1 - alpha / 2, df = n - 1)
    
    intervalo_varianza <- c(((n - 1) * varianza_muestral) / chi_cuadrado_sup, ((n - 1) * varianza_muestral) / chi_cuadrado_inf)
    
    # Guardamos resultados
    resultados_intervalos_varianza_68[[paste("muestra", i)]] <- intervalo_varianza
    
    # Guardamos longitud
    longitudes_intervalos_varianza_68[i] <- intervalo_varianza[2] - intervalo_varianza[1]
  }
  
  # Imprimimos resultados 
  # Guardamos las longitudes para realizar un diagrama representativo
  for (i in 1:num_muestras) {
    cat("Nivel de confianza del 68%", ":\n")
    nombre_muestra <- paste("muestra", i)
    cat("Resultados del intervalo de confianza para la Varianza de", nombre_muestra, ":\n")
    cat("Intervalo de Confianza: [", resultados_intervalos_varianza_68[[nombre_muestra]], "]\n")
    cat("Longitud: {", longitudes_intervalos_varianza_68[i], "}\n\n")
  }
  #---------------------------
  # Nivel de confianza (95%):
  resultados_intervalos_varianza_95 <- list()
  longitudes_intervalos_varianza_95 <- numeric(num_muestras)
  
  # Calculamos intervalo de confianza para la varianza en cada muestra con 95%
  for (i in 1:num_muestras) {
    muestra_varianza <- muestras_varianza[[paste0("muestra", i)]]
    
    varianza_muestral <- var(as.numeric(muestra_varianza$Resultado_Global))
    n <- nrow(muestra_varianza)
    
    nivel_confianza <- confianza[2]
    alpha <- 1 - nivel_confianza
    
    # Cuantiles de chi-cuadrado superior e inferior
    chi_cuadrado_inf <- qchisq(alpha / 2, df = n - 1)
    chi_cuadrado_sup <- qchisq(1 - alpha / 2, df = n - 1)
    
    intervalo_varianza <- c(((n - 1) * varianza_muestral) / chi_cuadrado_sup, ((n - 1) * varianza_muestral) / chi_cuadrado_inf)
    
    # Guardamos resultados
    resultados_intervalos_varianza_95[[paste("muestra", i)]] <- intervalo_varianza
    
    # Guardamos longitudes
    longitudes_intervalos_varianza_95[i] <- intervalo_varianza[2] - intervalo_varianza[1]
  }
  
  # Imprimimos resultados 
  # Guardamos las longitudes para realizar un diagrama representativo
  for (i in 1:num_muestras) {
    cat("Nivel de confianza del 95%", ":\n")
    nombre_muestra <- paste("muestra", i)
    cat("Resultados del intervalo de confianza para la Varianza de", nombre_muestra, ":\n")
    cat("Intervalo de Confianza: [", resultados_intervalos_varianza_95[[nombre_muestra]], "]\n")
    cat("Longitud: {", longitudes_intervalos_varianza_95[i], "}\n\n")
  }
  #----------------------------
  #Nivel de confianza (99.7%):
  resultados_intervalos_varianza_997 <- list()
  longitudes_intervalos_varianza_997 <- numeric(num_muestras)
  
  # Calculamos intervalo de confianza para la varianza en cada muestra con 99,7%
  for (i in 1:num_muestras) {
    muestra_varianza <- muestras_varianza[[paste0("muestra", i)]]
    
    varianza_muestral <- var(as.numeric(muestra_varianza$Resultado_Global))
    n <- nrow(muestra_varianza)
    
    nivel_confianza <- confianza[3]
    alpha <- 1 - nivel_confianza
    
    # Cuantiles de chi-cuadrado superior e inferior
    chi_cuadrado_inf <- qchisq(alpha / 2, df = n - 1)
    chi_cuadrado_sup <- qchisq(1 - alpha / 2, df = n - 1)
    
    intervalo_varianza <- c(((n - 1) * varianza_muestral) / chi_cuadrado_sup, ((n - 1) * varianza_muestral) / chi_cuadrado_inf)
    
    # Guardamos resultados
    resultados_intervalos_varianza_997[[paste("muestra", i)]] <- intervalo_varianza
    
    # Guardamos longitud
    longitudes_intervalos_varianza_997[i] <- intervalo_varianza[2] - intervalo_varianza[1]
  }
  
  # Imprimimos resultados 
  # Guardamos las longitudes para realizar un diagrama representativo
  for (i in 1:num_muestras) {
    cat("Nivel de confianza del 99,7%", ":\n")
    nombre_muestra <- paste("muestra", i)
    cat("Resultados del intervalo de confianza para la Varianza de", nombre_muestra, ":\n")
    cat("Intervalo de Confianza: [", resultados_intervalos_varianza_997[[nombre_muestra]], "]\n")
    cat("Longitud: {", longitudes_intervalos_varianza_997[i], "}\n\n")
  }
  print("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")
  #----------------------------PROPORCION--------------------------
  print(">>>>>>>>>>>>>>>>>>PROPORCION<<<<<<<<<<<<<<<<<<<<<<<<<<")
  # Nivel de confianza (68%):
  resultados_intervalos_proporcion_68 <- list()
  longitudes_intervalos_proporcion_68 <- numeric(num_muestras)
  
  # Calculamos intervalo de confianza para la proporción en cada muestra con 68%
  for (i in 1:num_muestras) {
    muestra_proporcion <- muestras_proporcion[[paste0("muestra", i)]]
    
    proporcion_muestral <- nrow(muestra_proporcion) / nrow(datos_medicina)
    tamano_muestra <- nrow(muestra_proporcion)
    
    nivel_confianza <- confianza[1]
    alpha <- 1 - nivel_confianza
    valor_critico_Z <- qnorm(alpha / 2)
    
    # Calculamos el error estándar
    error_estandar_proporcion <- sqrt(proporcion_muestral * (1 - proporcion_muestral) / tamano_muestra)
    
    intervalo_confianza_proporcion <- c(proporcion_muestral - valor_critico_Z * error_estandar_proporcion,
                                        proporcion_muestral + valor_critico_Z * error_estandar_proporcion)
    
    # Guardamos resultados
    resultados_intervalos_proporcion_68[[paste("muestra", i)]] <- intervalo_confianza_proporcion
    
    # Guardamos longitudes
    longitudes_intervalos_proporcion_68[i] <- intervalo_confianza_proporcion[1] - intervalo_confianza_proporcion[2]
  }
  
  # Imprimimos resultados 
  # Guardamos las longitudes para realizar un diagrama representativo
  for (i in 1:num_muestras) {
    cat("Nivel de confianza del 68%", ":\n")
    nombre_muestra <- paste("muestra", i)
    cat("Resultados del intervalo de confianza para la Proporción de", nombre_muestra, ":\n")
    cat("Intervalo de Confianza: [", resultados_intervalos_proporcion_68[[nombre_muestra]], "]\n")
    cat("Longitud: {", longitudes_intervalos_proporcion_68[i], "}\n\n")
  }
  #----------------------------------------------------------------
  # Nivel de confianza (95%):
  resultados_intervalos_proporcion_95 <- list()
  longitudes_intervalos_proporcion_95 <- numeric(num_muestras)
  
  # Calculamos intervalo de confianza para la proporción en cada muestra con 95%
  for (i in 1:num_muestras) {
    muestra_proporcion <- muestras_proporcion[[paste0("muestra", i)]]
    
    proporcion_muestral <- nrow(muestra_proporcion) / nrow(datos_medicina)
    tamano_muestra <- nrow(muestra_proporcion)
    
    nivel_confianza <- confianza[2]
    alpha <- 1 - nivel_confianza
    valor_critico_Z <- qnorm(alpha / 2)
    
    # Calculamos el error estándar
    error_estandar_proporcion <- sqrt(proporcion_muestral * (1 - proporcion_muestral) / tamano_muestra)
    
    intervalo_confianza_proporcion <- c(proporcion_muestral - valor_critico_Z * error_estandar_proporcion,
                                        proporcion_muestral + valor_critico_Z * error_estandar_proporcion)
    
    # Guardamos resultados
    resultados_intervalos_proporcion_95[[paste("muestra", i)]] <- intervalo_confianza_proporcion
    
    # Guardamos longitudes
    longitudes_intervalos_proporcion_95[i] <- intervalo_confianza_proporcion[1] - intervalo_confianza_proporcion[2]
  }
  
  # Imprimimos resultados 
  # Guardamos las longitudes para representarlas en un diagrama
  for (i in 1:num_muestras) {
    cat("Nivel de confianza del 95%", ":\n")
    nombre_muestra <- paste("muestra", i)
    cat("Resultados del intervalo de confianza para la Proporción de", nombre_muestra, ":\n")
    cat("Intervalo de Confianza: [", resultados_intervalos_proporcion_95[[nombre_muestra]], "]\n")
    cat("Longitud: {", longitudes_intervalos_proporcion_95[i], "}\n\n")
  }
  #------------------------------------------------
  # Nivel de confianza (99.7%):
  resultados_intervalos_proporcion_997 <- list()
  longitudes_intervalos_proporcion_997 <- numeric(num_muestras)
  
  # Calculamos intervalo de confianza para la proporción en cada muestra con 99,7%
  for (i in 1:num_muestras) {
    muestra_proporcion <- muestras_proporcion[[paste0("muestra", i)]]
    
    proporcion_muestral <- nrow(muestra_proporcion) / nrow(datos_medicina)
    tamano_muestra <- nrow(muestra_proporcion)
    
    nivel_confianza <- confianza[3]
    alpha <- 1 - nivel_confianza
    valor_critico_Z <- qnorm(alpha / 2)
    
    # Calculamos el error estándar
    error_estandar_proporcion <- sqrt(proporcion_muestral * (1 - proporcion_muestral) / tamano_muestra)
    
    intervalo_confianza_proporcion <- c(proporcion_muestral - valor_critico_Z * error_estandar_proporcion,
                                        proporcion_muestral + valor_critico_Z * error_estandar_proporcion)
    
    # Guardamos resultados
    resultados_intervalos_proporcion_997[[paste("muestra", i)]] <- intervalo_confianza_proporcion
    
    # Guardamos longitudes
    longitudes_intervalos_proporcion_997[i] <- intervalo_confianza_proporcion[1] - intervalo_confianza_proporcion[2]
  }
  
  # Imprimimos resultados 
  # Guardamos las longitudes para representarlas en un diagrama
  for (i in 1:num_muestras) {
    cat("Nivel de confianza del 99,7%", ":\n")
    nombre_muestra <- paste("muestra", i)
    cat("Resultados del intervalo de confianza para la Proporción de", nombre_muestra, ":\n")
    cat("Intervalo de Confianza: [", resultados_intervalos_proporcion_997[[nombre_muestra]], "]\n")
    cat("Longitud: {", longitudes_intervalos_proporcion_997[i], "}\n\n")
  }
  print("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")
  print("Calculando intervalos de confianza en cada muestra...")
}

# Función para graficar los valores y guardar en un PDF:
graficar_valores <- function() {
  
  # fijamos un tamaño de longitudes maximas que se pueden graficar en
  # nuestro diagrama, que de igual forma tomara las 9 primeras longitudes
  # encontradas de unas n muestras distintas almacenadas en una lista
  n<- 9
  # Nombramos all archivo PDF
  nombre_pdf <- "Graficas_Intervalos_Confianza.pdf"
  
  # Abrimos el archivo PDF para insertar las graficas respectivas
  pdf(nombre_pdf)
  
  #----------------------MEDICINA--------------------------
  # GRAFICAS MEDIA (68%):
  # Graficmos las longitudes de los intervalos de confianza en relación con la media poblacional
  plot(1:n, longitudes_intervalos_68, type = "n", xlab = "Muestra", ylab = "Longitud IC",
       main = paste("Longitudes de IC para Media de Medicina (68%)"))
  
  # Dibujamos la línea horizontal que representa la media poblacional
  abline(h = mean(longitudes_intervalos_68), col = "red", lty = 2, lwd = 2)
  
  # Agregamos rectángulos en lugar de círculos
  for (i in 1:n) {
    rect(i - 0.1, longitudes_intervalos_68[i] - 0.1, i + 0.1, longitudes_intervalos_68[i] + 0.1, col = "blue", border = NA)
  }
  
  # Ajustamos la posición horizontal del texto para centrarlo
  text(mean(longitudes_intervalos_68), max(longitudes_intervalos_68) + 0.2, "", pos = 4, col = "red", xpd = TRUE)
  
  # GRAFICAS MEDIA (95%):
  # Graficamos las longitudes de los intervalos de confianza en relación con la media poblacional
  plot(1:n, longitudes_intervalos_95, type = "n", xlab = "Muestra", ylab = "Longitud IC",
       main = "Longitudes de IC para Media de Medicina (95%)")
  
  # Dibujamos la línea horizontal que representa la media poblacional
  abline(h = mean(longitudes_intervalos_95), col = "red", lty = 2, lwd = 2)
  
  # Agregamos rectángulos 
  for (i in 1:n) {
    rect(i - 0.1, longitudes_intervalos_95[i] - 0.1, i + 0.1, longitudes_intervalos_95[i] + 0.1, col = "blue", border = NA)
  }
  
  # Ajustamos la posición horizontal del texto para centrarlo
  text(mean(longitudes_intervalos_95), max(longitudes_intervalos_95) + 0.2, "", pos = 4, col = "red", xpd = TRUE)
  
  # GRAFICAS MEDIA (99,7%):
  # Graficamos las longitudes de los intervalos de confianza en relación con la media poblacional
  plot(1:n, longitudes_intervalos_997, type = "n", xlab = "Muestra", ylab = "Longitud IC",
       main = "Longitudes de IC para Media de Medicina (99,7%)")
  
  # Dibujamos la línea horizontal que representa la media poblacional
  abline(h = mean(longitudes_intervalos_997), col = "red", lty = 2, lwd = 2)
  
  # Agregamos rectángulos 
  for (i in 1:n) {
    rect(i - 0.1, longitudes_intervalos_997[i] - 0.1, i + 0.1, longitudes_intervalos_997[i] + 0.1, col = "blue", border = NA)
  }
  
  # Ajustamos la posición horizontal del texto para centrarlo
  text(mean(longitudes_intervalos_997), max(longitudes_intervalos_997) + 0.2, "", pos = 4, col = "red", xpd = TRUE)
  
  #--------------------------------------------
  # GRAFICAS VARIANZA (68%):
  # Graficamos las longitudes de los intervalos de confianza en relación con la varianza poblacional
  plot(1:n, longitudes_intervalos_varianza_68, type = "n", xlab = "Muestra", ylab = "Longitud IC",
       main = "Longitudes de IC para Varianza de Medicina (68%)")
  
  # Dibujamos la línea horizontal que representa la varianza poblacional
  abline(h = mean(longitudes_intervalos_varianza_68), col = "red", lty = 2, lwd = 2)
  
  # Agregamos rectángulos s
  for (i in 1:n) {
    rect(i - 0.1, longitudes_intervalos_varianza_68[i] - 0.1, i + 0.1, longitudes_intervalos_varianza_68[i] + 0.1, col = "blue", border = NA)
  }
  
  # Ajustamos la posición horizontal del texto para centrarlo
  text(mean(longitudes_intervalos_varianza_68), max(longitudes_intervalos_varianza_68) + 0.2, "", pos = 2, col = "red", xpd = TRUE)
  
  # GRAFICAS VARIANZA (95%):
  # Graficamos las longitudes de los intervalos de confianza en relación con la varianza poblacional
  plot(1:n, longitudes_intervalos_varianza_95, type = "n", xlab = "Muestra", ylab = "Longitud IC",
       main = "Longitudes de IC para Varianza de Medicina (95%)")
  
  # Dibujamos la línea horizontal que representa la varianza poblacional
  abline(h = mean(longitudes_intervalos_varianza_95), col = "red", lty = 2, lwd = 2)
  
  # Agregamos rectángulos
  for (i in 1:n) {
    rect(i - 0.1, longitudes_intervalos_varianza_95[i] - 0.1, i + 0.1, longitudes_intervalos_varianza_95[i] + 0.1, col = "blue", border = NA)
  }
  
  # Ajustamos la posición horizontal del texto para centrarlo
  text(mean(longitudes_intervalos_varianza_95), max(longitudes_intervalos_varianza_95) + 0.2, "", pos = 4, col = "red", xpd = TRUE)
  
  # GRAFICAS VARIANZA (99,7%):
  # Graficamos las longitudes de los intervalos de confianza en relación con la varianza poblacional
  plot(1:n, longitudes_intervalos_varianza_997, type = "n", xlab = "Muestra", ylab = "Longitud IC",
       main = "Longitudes de IC para Varianza de Medicina (99,7%)")
  
  # Dibujamos la línea horizontal que representa la varianza poblacional
  abline(h = mean(longitudes_intervalos_varianza_997), col = "red", lty = 2, lwd = 2)
  
  # Agregamos rectángulos
  for (i in 1:n) {
    rect(i - 0.1, longitudes_intervalos_varianza_997[i] - 0.1, i + 0.1, longitudes_intervalos_varianza_997[i] + 0.1, col = "blue", border = NA)
  }
  
  # Ajustamos la posición horizontal del texto para centrarlo
  text(mean(longitudes_intervalos_varianza_997), max(longitudes_intervalos_varianza_997) + 0.2, "", pos = 4, col = "red", xpd = TRUE)
  
  # GRAFICAS PROPORCION (68%%):
  # Graficamos las longitudes de los intervalos de confianza en relación con la proporcion poblacional
  plot(1:n, longitudes_intervalos_proporcion_68, type = "n", xlab = "Muestra", ylab = "Longitud IC",
       main = "Longitudes de IC para Proporcion de Medicina (68%)")
  
  # Dibujamos la línea horizontal que representa la proporcion poblacional
  abline(h = mean(longitudes_intervalos_proporcion_68), col = "red", lty = 2, lwd = 2)
  
  # Agregamos rectángulos 
  for (i in 1:n) {
    rect(i - 0.1, longitudes_intervalos_proporcion_68[i] - 0.1, i + 0.1, longitudes_intervalos_proporcion_68[i] + 0.1, col = "blue", border = NA)
  }
  
  # Ajustamos la posición horizontal del texto para centrarlo
  text(mean(longitudes_intervalos_proporcion_68), max(longitudes_intervalos_proporcion_68) + 0.2, "", pos = 4, col = "red", xpd = TRUE)
  
  # GRAFICAS PROPORCION (95%%):
  # Graficamos las longitudes de los intervalos de confianza en relación con la proporcion poblacional
  plot(1:n, longitudes_intervalos_proporcion_95, type = "n", xlab = "Muestra", ylab = "Longitud IC",
       main = "Longitudes de IC para Proporcion de Medicina (95%)")
  
  # Dibujamos la línea horizontal que representa la proporcion poblacional
  abline(h = mean(longitudes_intervalos_proporcion_95), col = "red", lty = 2, lwd = 2)
  
  # Agregamos rectángulos en lugar de círculos
  for (i in 1:n) {
    rect(i - 0.1, longitudes_intervalos_proporcion_95[i] - 0.1, i + 0.1, longitudes_intervalos_proporcion_95[i] + 0.1, col = "blue", border = NA)
  }
  
  # Ajusta la posición horizontal del texto para centrarlo
  text(mean(longitudes_intervalos_proporcion_95), max(longitudes_intervalos_proporcion_95) + 0.2, "", pos = 4, col = "red", xpd = TRUE)
  
  # GRAFICAS PROPORCION (99,7%%):
  # Graficamos las longitudes de los intervalos de confianza en relación con la proporcion poblacional
  plot(1:n, longitudes_intervalos_proporcion_997, type = "n", xlab = "Muestra", ylab = "Longitud IC",
       main = "Longitudes de IC para Proporcion de Medicina (99,7%)")
  
  # Dibujamos la línea horizontal que representa la proporcion poblacional
  abline(h = mean(longitudes_intervalos_proporcion_997), col = "red", lty = 2, lwd = 2)
  
  # Agregamos rectángulos
  for (i in 1:n) {
    rect(i - 0.1, longitudes_intervalos_proporcion_997[i] - 0.1, i + 0.1, longitudes_intervalos_proporcion_997[i] + 0.1, col = "blue", border = NA)
  }
  
  # Ajustamos la posición horizontal del texto para centrarlo
  text(mean(longitudes_intervalos_proporcion_997), max(longitudes_intervalos_proporcion_997) + 0.2, "", pos = 4, col = "red", xpd = TRUE)
  
  # Cerramos el archivo PDF
  dev.off()
  #-----------------------------------------
  print("Graficando valores y guardando en un PDF...")
}

