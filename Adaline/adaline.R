# Declaración de variables finales
APRENDIZAJE = 0.1
PRECISION = 5
ESTABILIDAD = 5
MINIMOS = c(102, 0, 0, 121.800003, 0, 801, 594, 1, 2.33)
MAXIMOS = c(540, 359.399994, 200.100006, 247, 32.200001, 1145, 992.599976, 365, 82.599998)
CICLOS_MAXIMOS = 150

# Creación de dataframes
entrenamiento_csv = read.csv("entrenamiento.csv", header = FALSE, sep = ";", quote = "", dec = ",")
validacion_csv = read.csv("validacion.csv", header = FALSE, sep = ";", quote = "", dec = ",")
test_csv = read.csv("test.csv", header = FALSE, sep = ";", quote = "", dec = ",")
errores_ent_val_csv = data.frame(
    Ciclo = numeric(0),
    Error_Entrenamiento = numeric(0),
    Error_Validación = numeric(0)
)
errores_aprendizaje_csv = data.frame(
    Tasa_Aprendizaje = numeric(0),
    Error_Entrenamiento = numeric(0),
    Error_Validación = numeric(0),
    Error_Test = numeric(0),
    Error_Test_Desnormalizado = numeric(0)
)


# Inicialización de variables
pesos = round(runif(8, 0, 1), 2)
umbral = round(runif(1, 0, 1), 2)
contador_parada = 0
numero_racha = 0
ciclo = 0
tiempo = Sys.time()

# Aprendizaje
while(ciclo < CICLOS_MAXIMOS){
    ciclo = ciclo + 1
    errores_ent_val_csv[ciclo, 1] = ciclo
    for (i in 1 : nrow(entrenamiento_csv)) {
        salida = sum(entrenamiento_csv[i, 1 : 8] * pesos) + umbral
        error = entrenamiento_csv[i, 9] - salida
        pesos = pesos + APRENDIZAJE * error * entrenamiento_csv[i, 1:8]
        umbral = umbral + APRENDIZAJE * error
    }

    errores_ent_val_csv[ciclo, 2] = mean(apply(entrenamiento_csv, 1, function(fila, weight = pesos, threshold = umbral) (fila[9] - (sum(fila[1 : 8] * weight) + threshold))^2))
    cat("Entrenamiento = ", errores_ent_val_csv[ciclo, 2], "\t")

    errores_ent_val_csv[ciclo, 3] = mean(apply(validacion_csv, 1, function(fila, weight = pesos, threshold = umbral) (fila[9] - (sum(fila[1 : 8] * weight) + threshold))^2))
    cat("Validacion = ", errores_ent_val_csv[ciclo, 3], "\n")

    # Comprobación de la condición de parada.
    if(floor(errores_ent_val_csv[ciclo,3] * 10^PRECISION) == numero_racha){
        contador_parada = contador_parada + 1
        if(contador_parada == ESTABILIDAD){
            break
        }
    } else {
        contador_parada = 0
        numero_racha = floor(errores_ent_val_csv[ciclo,3] * 10^PRECISION);
    }
}

# Cálculo del error de test al finalizar el aprendizaje.
error_test = mean(apply(test_csv, 1, function(fila, weight = pesos, threshold = umbral) (fila[9] - (sum(fila[1 : 8] * weight) + threshold))^2))
error_test_desnormalizado = error_test * (MAXIMOS[9] - MINIMOS[9]) + MINIMOS[9]
cat("Error de test = ", error_test, "\n")

# Escritura y desnormalizacion de las salidas de test de aprendizaje y esperadas
salidas_test = as.data.frame(apply(test_csv, 1, function(fila, weight = pesos, threshold = umbral) sum(fila[1:8] * weight) + threshold))
test_desnormalizado = apply(salidas_test, 1, function(fila, maximo = MAXIMOS, minimo = MINIMOS) fila[1] * (maximo[9] - minimo[9]) + minimo[9])
salida_esperada_desnormalizada = apply(test_csv[9], 1, function(fila, maximo = MAXIMOS, minimo = MINIMOS) fila[1] * (maximo[9] - minimo[9]) + minimo[9])
write.table(test_desnormalizado, file = paste(paste("test_desnormalizado", format(tiempo,'%Y%m%d_%H%M%S'), sep = "_"), ".csv"), sep = ";", col.names = TRUE, dec = ",", row.names = FALSE)

# Creación del gráfico de comparación de las salidas
matplot(1:length(test_desnormalizado), cbind(test_desnormalizado, salida_esperada_desnormalizada), type = "p", col = c("black", "black"), xlab = "Patrones", ylab = "Salidas", pch = c(1, 16), bg = "white")
segments(1:length(test_desnormalizado), test_desnormalizado, 1:length(test_desnormalizado), salida_esperada_desnormalizada)
legend("topleft", legend = c("Salida del aprendizaje", "Salida esperada"), col = c("black", "black"), pch = c(1, 16), cex=0.8, bg = "white")
dev.print(png, paste(paste("plot_comparacion_salidas", format(tiempo,'%Y%m%d_%H%M%S'), sep = "_"), ".png"), width=750, height=500)

# Creación del gráfico de barras del error
matplot(test_desnormalizado - salida_esperada_desnormalizada, type = "h", bg = "white", xlab = "Patrones", ylab = "Error cuadratico medio")
abline(h = mean(test_desnormalizado - salida_esperada_desnormalizada), col = "blue", lty = 3)
legend("topleft", legend = c("Error de test desnormalizado", "Media del error desnormalizado"), col = c("black", "blue"), lty = c(1, 3), cex=0.8, bg = "white")
dev.print(png, paste(paste("plot_error_test", format(tiempo,'%Y%m%d_%H%M%S'), sep = "_"), ".png"), width=750, height=500)


# Escritura de las tablas de errores de entrenamiento y validación
cat("Entrenamiento terminado. Escribiendo archivo csv \n")
write.table(errores_ent_val_csv, file = paste(paste("errores_ent_val", format(tiempo,'%Y%m%d_%H%M%S'), sep = "_"), ".csv"), sep = ";", col.names = TRUE, dec = ",", row.names = FALSE)

# Creación del gráfico de la evolución del aprendizaje
matplot(errores_ent_val_csv[1], cbind(errores_ent_val_csv[, 2 : 3]), type = "l", xlab = "Ciclos", ylab = "Errores", bg = "white")
legend("topright", legend = c("Error de entrenamiento", "Error de validacion"), col = c("black", "red"), lty=1:2, cex=0.8, bg = "white")
dev.print(png, paste(paste("plot_aprendizaje", format(tiempo,'%Y%m%d_%H%M%S'), sep = "_"), ".png"), width=750, height=500)


# Escritura de la tabla con los tres errores medios al finalizar el aprendizaje
errores_aprendizaje_csv[1, 1] = APRENDIZAJE
errores_aprendizaje_csv[1, 2] = errores_ent_val_csv[ciclo, 2]
errores_aprendizaje_csv[1, 3] = errores_ent_val_csv[ciclo, 3]
errores_aprendizaje_csv[1, 4] = error_test
errores_aprendizaje_csv[1, 5] = error_test_desnormalizado
write.table(errores_aprendizaje_csv, file = "errores_aprendizaje.csv", sep = ";", col.names = FALSE, dec = ",", row.names = FALSE, append = T)
write.table(errores_aprendizaje_csv, file = paste(paste("errores_aprendizaje", format(tiempo,'%Y%m%d_%H%M%S'), sep = "_"), ".csv"), sep = ";", col.names = TRUE, dec = ",", row.names = FALSE)

# Escritura de los pesos y el umbral
write.table(rbind(pesos, umbral), file = paste(paste("pesos_umbral", format(tiempo,'%Y%m%d_%H%M%S'), sep = "_"), ".csv"), sep = ";", col.names = TRUE, dec = ",", row.names = FALSE)
