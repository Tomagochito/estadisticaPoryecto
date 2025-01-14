install.packages("ggplot2")
install.packages("DescTools")
library(DescTools)
library(readxl)
library(ggplot2)
library(e1071)
numIntentos <- 5
datos <- read_excel("C:/Users/TOMAS/Downloads/datosEstadistica.xlsx")

cantidadExitosParadoDerecha <- datos$`Total Exitos Parado con mano derecha`
cantidadExitosParadoIzquierda <- datos$`Total Exitos Parado con mano izquierda`
cantidadExitosAgachadoDerecha <- datos$`Total Exitos Agachado con mano derecha`
cantidadExitosAgachadoIzquierda <- datos$`Total Exitos Agachado con mano izquierda`

totalDatos2 <- c(cantidadExitosAgachadoDerecha, cantidadExitosAgachadoIzquierda,
                 cantidadExitosParadoDerecha, cantidadExitosParadoIzquierda)


media <- mean(totalDatos2)
print(paste("Media: ", media))

# 2. Error estándar de la media
error_estandar_media <- sd(totalDatos2) / sqrt(length(totalDatos2))
print(paste("Error Estándar de la Media: ", error_estandar_media))

# 3. Mediana
mediana <- median(totalDatos2)
print(paste("Mediana: ", mediana))

# 4. Moda
moda <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
moda_resultado <- moda(totalDatos2)
print(paste("Moda: ", moda_resultado))

# 5. Desviación estándar
desviacion_estandar <- sd(totalDatos2)
print(paste("Desviación Estándar: ", desviacion_estandar))

# 6. Sesgo (Instalar e1071 si no lo tienes)
# install.packages("e1071")
library(e1071)
sesgo <- skewness(totalDatos2)
print(paste("Sesgo: ", sesgo))


data.frame(totalDatos2)

cantidadDatosParadoDerecha <- length(cantidadExitosParadoDerecha)
cantidadDatosParadoIzquierda <- length(cantidadExitosParadoIzquierda)
cantidadDatosAgachadoDerecha <- length(cantidadExitosAgachadoDerecha)
cantidadDatosAgachadoIzquierda <- length(cantidadExitosAgachadoIzquierda)

cantidadDatos <- cantidadDatosParadoDerecha +
                 cantidadDatosParadoIzquierda +
                 cantidadDatosAgachadoDerecha +
                cantidadDatosAgachadoIzquierda 

probExitosParadoDerecha <- sum(cantidadExitosParadoDerecha)/(cantidadDatosParadoDerecha*5)
probExitosParadoIzquierda <- sum(cantidadExitosParadoIzquierda)/(cantidadDatosParadoIzquierda*5)
probExitosAgachadoDerecha <- sum(cantidadExitosAgachadoDerecha)/(cantidadDatosAgachadoDerecha*5)
probExitosAgachadoIzquierda <- sum(cantidadExitosAgachadoIzquierda)/(cantidadDatosAgachadoIzquierda*5)

# Dataframe tabla para exitos por categorias
dfProbabilidades <- data.frame(
  "Mano Derecha" = c(probExitosParadoDerecha, probExitosAgachadoDerecha),
  "Mano Izquierda" = c(probExitosParadoIzquierda, probExitosAgachadoIzquierda)
)
rownames(dfProbabilidades) <- c("Parado", "Agachado")

proporcionPosturaParadoAgachado <- c(
                (cantidadDatosParadoDerecha+cantidadDatosParadoIzquierda)/cantidadDatos,
                (cantidadDatosAgachadoDerecha+cantidadDatosAgachadoIzquierda)/cantidadDatos
                )

proporcionManoLanzadoraDerechaIzquierda <- c(
                (cantidadDatosParadoDerecha+cantidadDatosAgachadoDerecha)/cantidadDatos,
                (cantidadDatosParadoIzquierda+cantidadDatosAgachadoIzquierda)/cantidadDatos
                )



# Crear la tabla de frecuencias
postura <- c("Parado","Agachado")
mano <- c("Mano Derecha", "Mano Izquierda")
tfp <- data.frame( "Postura" = postura , "Proporción" = proporcionPosturaParadoAgachado )
tfm <- data.frame("Mano Usada"= mano, "Proporción" = proporcionManoLanzadoraDerechaIzquierda )


# Gráfico de barras para la proporción de posturas
ggplot(tfp, aes(x = Postura, y = Proporción, fill = Postura)) +
geom_bar(stat = "identity", color = "black", width = 0.7, fill = "gray") +  
geom_text(aes(label = format(Proporción, nsmall = 3, digits = 3)), vjust = -0.3, size = 4, color = "black") + 
labs(x="",
    y = "Proporción", 
    title = "Diagrama de Barras", 
    subtitle = '"Postura"') + 
scale_y_continuous(
    labels = scales::label_number(scale = 1, accuracy = 0.001), 
    limits = c(0, 1), 
    breaks = seq(0, 1, by = 0.100) 
) + 
theme_minimal() + 
theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),  
    plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),  
    axis.title = element_text(face = "italic", size = 12), 
    axis.text = element_text(size = 12), 
    plot.caption = element_text(face = "bold", size = 12, hjust = 0, vjust = 0, lineheight = 1),  
    panel.border = element_rect(color = "black", fill = NA, size = 1)  ,
    axis.ticks.y = element_line(size = 1),  # Añade una línea a la izquierda de los números del eje Y
    axis.ticks.length = unit(0.3, "cm"),
    panel.grid = element_blank() 
)






# Gráfico de barras para la proporción de mano utilizada
ggplot(tfm, aes(x = `mano`, y = Proporción, fill = `mano`)) +
geom_bar(stat = "identity", color = "black", width = 0.7, fill = "gray") + 
geom_text(aes(label = format(Proporción, nsmall = 3, digits = 3)), vjust = -0.3, size = 4, color = "black") +  
labs(x="",
    y = "Proporción", 
    title = "Diagrama de Barras", 
    subtitle = '"Mano utilizada"') + 
scale_y_continuous(
    labels = scales::label_number(scale = 1, accuracy = 0.001), 
    limits = c(0, 1), 
    breaks = seq(0, 1, by = 0.100)
)+
theme_minimal() + 
theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),  # Título más pequeño
    plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),  # Subtítulo más pequeño
    axis.title = element_text(face = "italic", size = 12),  # Títulos de los ejes más pequeños
    axis.text = element_text(size = 12),  # Texto de los ejes más pequeño
    plot.caption = element_text(face = "bold", size = 12, hjust = 0, vjust = 0, lineheight = 1),  # Número del gráfico más pequeño
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Agregar contorno al gráfico
    axis.ticks.y = element_line(size = 1),  # Añade una línea a la izquierda de los números del eje Y
    axis.ticks.length = unit(0.3, "cm"),
    panel.grid = element_blank() 
)



distanciaLanzamiento <- c( datos$`Distancia PD`,
                           datos$`Distancia PI`,
                           datos$`Distancia AD`,
                           datos$`Distancia AI`
)
totalDistanciaLanzamiento <- length(distanciaLanzamiento)
claseDistanciaLanzamiento <- c(1.5 , 1.75, 2, 2.25, 2.5, 2.51)
tfd <- table(cut(distanciaLanzamiento, breaks = claseDistanciaLanzamiento, right = FALSE, include.lowest = TRUE))
tfd_df <- data.frame("Distancia de Lanzamiento" = names(tfd), "Frecuencias"=as.vector(table(distanciaLanzamiento)), "Proporción" = as.vector(tfd)/totalDistanciaLanzamiento )
tfdmedia <- mean(distanciaLanzamiento)
tfdmediana <- median(distanciaLanzamiento)

tfdmoda <- Mode(distanciaLanzamiento)


tfdvarianza <- round(var(distanciaLanzamiento),3)
tfdDesviacionEstandar <- sqrt(var(distanciaLanzamiento, na.rm = T))


#Rango
tfdrango1 <- range(distanciaLanzamiento, na.rm = T)
tfdrango2 <- Range(distanciaLanzamiento, na.rm = T)

#Extremos

tfdminimo <- min(distanciaLanzamiento)
tfdmaximo <- max(distanciaLanzamiento)

#Medidas de posicion
#Cuartiles
cuartiles <- quantile(distanciaLanzamiento, na.rm = T)

cat("Cuartil 1 (Q1):", cuartiles[2], "\n")  # 25%
cat("Cuartil 3 (Q3):", cuartiles[4], "\n")  # 75%
cat("Rango intercuartílico (IQR):", cuartiles[4] - cuartiles[2], "\n")


sesgo <- skewness(distanciaLanzamiento, na.rm = TRUE)
cat("Sesgo (asimetría):", sesgo, "\n")

MedidasTendenciaCentralDispersion <- data.frame(
  Medida = c("Media", "Moda", "Desviación Estándar", "Sesgo", "Mínimo", "Máximo", 
             "Cuartil 1 (Q1)", "Mediana", "Cuartil 3 (Q3)", "Rango Intercuartílico (IQR)"),
  Valor = c(
    round(tfdmedia, 3), 
    round(tfdmoda, 3), 
    round(tfdDesviacionEstandar, 3), 
    round(sesgo, 3), 
    round(tfdminimo, 3), 
    round(tfdmaximo, 3), 
    round(cuartiles[2], 3), 
    round(tfdmediana, 3), 
    round(cuartiles[4], 3), 
    round(cuartiles[4] - cuartiles[2], 3)
  )
)
MedidasTendenciaCentralDispersion <- as.data.frame(t(MedidasTendenciaCentralDispersion))

boxplot(x = distanciaLanzamiento, 
        horizontal=TRUE,
       main="Diagrama de Caja",
        xlab = "", ylab = "",
        col = "gray",
       cex.main = 1,  # Tamaño del título principal
       font.main = 4,  # Título en cursiva (4 es para cursiva)
       cex.axis = .8  # Tamaño de los números del eje
        )
mtext('"Distancia de Lanzamiento"', side = 3, line = 0.5, cex = 1, font = 3)  # Subtítulo en cursiva

df <- data.frame(distancia = distanciaLanzamiento)
#ggplot(df, aes(x = distancia, y="0"))+
#  geom_boxplot()+
#  geom_jitter()

# Crear el histograma con las proporciones




hist(distanciaLanzamiento,
     seq(tfdminimo-0.5,tfdmaximo+0.5, by = 0.25),  
     main="Histograma de Frecuencias",
     right=FALSE, 
     label=TRUE,  
     xlim=c(1.5 ,2.75), 
     ylim=c(0,1200),
     cex.main = 1,
     xlab="",
     ylab="Frecuencia"
     )


mtext('"Distancia de Lanzamiento"', side = 3, line = 0.5, cex = 1, font = 3)  # Subtítulo en cursiva




posibles <- c(0, 1, 2, 3, 4, 5)

#promedio de exitos parado con mano derecha
exitosPD <- datos$`Total Exitos Parado con mano derecha`
promExitosPD <- mean(exitosPD)
print(promExitosPD)
exitosPD_factor <- factor(exitosPD, levels = posibles)
tabla_frecuencia_PD <- table(exitosPD_factor)
print(tabla_frecuencia_PD)

# Promedio de éxitos parado con mano izquierda
exitosPI <- datos$`Total Exitos Parado con mano izquierda`
promExitosPI <- mean(exitosPI, na.rm = TRUE)  # Ignorar NA para el cálculo del promedio
print(promExitosPI)
exitosPI_factor <- factor(exitosPI, levels = posibles)
tabla_frecuencia_PI <- table(exitosPI_factor)
print(tabla_frecuencia_PI)

# Promedio de éxitos agachado con mano derecha
exitosAD <- datos$`Total Exitos Agachado con mano derecha`
promExitosAD <- mean(exitosAD, na.rm = TRUE)  # Ignorar NA para el cálculo del promedio
print(promExitosAD)
exitosAD_factor <- factor(exitosAD, levels = posibles)
tabla_frecuencia_AD <- table(exitosAD_factor)
print(tabla_frecuencia_AD)

# Promedio de éxitos agachado con mano izquierda
exitosAI <- datos$`Total Exitos Agachado con mano izquierda`
promExitosAI <- mean(exitosAI, na.rm = TRUE)  # Ignorar NA para el cálculo del promedio
print(promExitosAI)
exitosAI_factor <- factor(exitosAI, levels = posibles)
tabla_frecuencia_AI <- table(exitosAI_factor)
print(tabla_frecuencia_AI)


tabla_frecuencia_combinada <- data.frame(
  "Éxitos" = posibles,
  "Parado Derecha (PD)" = as.numeric(tabla_frecuencia_PD),
  "Parado Izquierda (PI)" = as.numeric(tabla_frecuencia_PI),
  "Agachado Derecha (AD)" = as.numeric(tabla_frecuencia_AD),
  "Agachado Izquierda (AI)" = as.numeric(tabla_frecuencia_AI)
)

t3 <- data.frame("Exitos" = posibles,
                 "PD" = tabla_frecuencia_combinada$Parado.Derecha..PD.,
                 "AD" = tabla_frecuencia_combinada$Agachado.Derecha..AD.
                 )

chisq.test(t3)


# Ejemplo de dataframe con datos
df3 <- data.frame(
  Exitos = c(0, 1, 2, 3, 4, 5),
  PD = c(8, 0, 87, 95, 59, 12),
  AD = c(39, 3, 21, 82, 117, 77)
)
tabla_contingencia <- cbind(df3$PD, df3$AD)
resultado_chi2 <- chisq.test(tabla_contingencia)

# Ver los resultados de la prueba
print(resultado_chi2)



df4 <- data.frame(
  Exitos = c( 2, 3, 4, 5),
  PD = c(95, 95, 59, 12),
  AD = c(63, 82, 117, 77)
)
tabla_contingencia4 <- cbind(df4$PD, df3$AD)
resultado_chi2 <- chisq.test(tabla_contingencia4)

# Ver el dataframe
print(df)






totalExitosPD <- sum(tabla_frecuencia_combinada$Parado.Derecha..PD.*posibles)
totalFracasosPD <- (cantidadDatosParadoDerecha*numIntentos)-totalExitosPD

totalExitosPI <- sum(tabla_frecuencia_combinada$Parado.Izquierda..PI.*posibles)
totalFracasosPI <- (cantidadDatosParadoIzquierda*numIntentos)-totalExitosPI

totalExitosAD <- sum(tabla_frecuencia_combinada$Agachado.Derecha..AD.*posibles)
totalFracasosAD <- (cantidadDatosParadoIzquierda*numIntentos)-totalExitosAD

totalExitosAI <- sum(tabla_frecuencia_combinada$Agachado.Izquierda..AI.*posibles)
totalFracasosAI <- (cantidadDatosParadoIzquierda*numIntentos)-totalExitosAI

totalExitos <- totalExitosAD + totalExitosAI + totalExitosPD + totalExitosPI
ponderacionExitos <- c(totalExitosPD/totalExitos, 
                       totalExitosPI/totalExitos,
                       totalExitosAD/totalExitos,
                       totalExitosAI/totalExitos)

cantidaddeExitosFracasos <- data.frame(
  "Parado Derecha" = c(totalExitosPD, totalFracasosPD),
  "Parado Izquierda" = c(totalExitosPI, totalFracasosPI),
  "Agachado Derecha" = c(totalExitosAD,totalFracasosAD),
  "Agachado Izquierda" = c(totalExitosAI, totalFracasosAI)
)

rownames(cantidaddeExitosFracasos) <- c("Exitos", "Fracasos")
#tabla_frecuencia_Postura <- data.frame("Éxitos" = posibles,"Parado")

# Reemplazar NA con 0 en caso de que falten datos
tabla_frecuencia_combinada[is.na(tabla_frecuencia_combinada)] <- 0

tfd


#rm(list = ls()) ##borra todas las variables
#graphics.off()  # Cierra todos los gráficos abiertos

O <- c(160, 224, 247, 250, 219, 100)
E <- c(48, 217.2, 392.4, 352.8, 160.8, 28.8)
chi_squared <- sum((O - E)^2 / E)
print(paste("Valor de chi-cuadrado:", chi_squared))
df <- length(O) - 1
p_value <- 1 - pchisq(chi_squared, df)
print(paste("Valor p:", p_value))



tabla_exitos_postura <- data.frame(
  Postura = c("Parado", "Agachado"),
  Exitos = c(
    sum(cantidadExitosParadoDerecha, cantidadExitosParadoIzquierda),
    sum(cantidadExitosAgachadoDerecha, cantidadExitosAgachadoIzquierda)
  )
)
print(tabla_exitos_postura)

tabla_exitos_mano <- data.frame(
  Mano = c("Mano Derecha", "Mano Izquierda"),
  Exitos = c(
    sum(cantidadExitosParadoDerecha, cantidadExitosAgachadoDerecha),
    sum(cantidadExitosParadoIzquierda, cantidadExitosAgachadoIzquierda)
  )
)
print(tabla_exitos_mano)


totales <- data.frame(
  Categoria = c("Parado Derecha", "Parado Izquierda", "Agachado Derecha", "Agachado Izquierda"),
  Total = c(
    sum(datos$`Total Exitos Parado con mano derecha`),
    sum(datos$`Total Exitos Parado con mano izquierda`),
    sum(datos$`Total Exitos Agachado con mano derecha`),
    sum(datos$`Total Exitos Agachado con mano izquierda`)
  )
)

print(totales)


# Primer gráfico (Postura vs Éxitos)
ggplot(tabla_exitos_postura, aes(x = Postura, y = Exitos, fill = "gray")) +
  geom_bar(stat = "identity",fill = "gray", color = "black") +
  geom_text(aes(label = Exitos), vjust = -0.3, size = 5) +
  ylim(0,2000)+
  labs(title = "Éxitos por Postura", x = "Postura", y = "Éxitos") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Borde alrededor del gráfico
  )

# Segundo gráfico (Mano vs Éxitos)
ggplot(tabla_exitos_mano, aes(x = Mano, y = Exitos, fill = "gray")) +
  geom_bar(stat = "identity", fill = "gray",color = "black") +
  labs(title = "Éxitos por Mano Utilizada", x = "Mano", y = "Éxitos") +
  geom_text(aes(label = Exitos), vjust = -0.3, size = 5) +
  ylim(0,2000)+
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Borde alrededor del gráfico
  )

# Tercer gráfico (Total de Éxitos por Categoría)
ggplot(totales, aes(x = Categoria, y = Total, fill = "gray")) +
  geom_bar(stat = "identity", fill = "gray", color = "black", width = 0.7) +
  geom_text(aes(label = Total), vjust = -0.3, size = 5) +
  ylim(0,2000)+
  labs(
    title = "Total de Éxitos por Categoría",
    x = "Categoría",
    y = "Total de Éxitos"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Borde alrededor del gráfico
  )
