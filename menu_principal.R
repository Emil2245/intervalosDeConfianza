source("C:/Users/emilv/OneDrive/Documents/R-Studio/Deber 1-Intervalos Grupal 2/Verkade_Parra_Pozo.R")
cargar_datos()
# Menú principal:
menu_principal <- function() {
  # Desarrollamos una emulacion de un while - switch
  # para desarrollar una aplicacion intuitiva para el usuario
  # digite opciones que le permitan calcular estadisticos, muestras
  # I.Confianza y graficas de determinadas muestras
  opcion <- 0
  while (opcion != 7) {
    cat("------- Menú Principal -------\n")
    cat("1) Cargar Datos\n")
    cat("2) Calcular Media, Varianza, Proporción del CSV\n")
    cat("3) Calcular Media, Varianza, Proporción de una Muestra Aleatoria\n")
    cat("4) Obtener n Muestras de 500 Datos\n")
    cat("5) Calcular n Intervalos de Confianza\n")
    cat("6) Graficar solamente 9 Muestras/Valores y Guardar en PDF\n")
    cat("7) Salir\n")
    
    opcion <- as.numeric(readline("Ingrese la opción deseada: "))
    
    switch(opcion,
           "1" = cargar_datos(),
           "2" = calcular_estadisticas_csv(),
           "3" = calcular_estadisticas_muestra_aleatoria(),
           "4" = obtener_muestras(),
           "5" = calcular_intervalos_confianza(),
           "6" = graficar_valores(),
           "7" = cat("Saliendo del programa...\n"),
           print("Opción no válida. Inténtelo de nuevo.")
    )
  }
}

# Iniciamos el programa:
# Si es la primera vez que se abre este archivo en R, debemos
# seleccionar todo el contenido de este archivo y ejecutarlo una vez,
# para que se ejecuten todas las sentencias/librerias y funciones de R
menu_principal()
