

library(readr) # Importar documentos
library(readxl) # Importar documentos
library(writexl) # Exportar documentos
library(dplyr) # Crear CODMUN
library(stringr) # Concatenar vectores
library(ggplot2) # Hacer gráficas
library(RColorBrewer) # Colores mapas

# Carga de datos de TB
Datos_Global <- read_csv("../data/data_Paula_2014_2020_epi_clusters.csv")
Datos_Global <- as.data.frame(Datos_Global)

# Carga de datos de población CV
load("../data/CupoPobMunis.RData")

# Se eliminan las 3 últimas columnas porque están repetidas
Datos_Global <- Datos_Global[,c(1:72)]

# Se transforma todo a factor
for (i in 2:72){
  Datos_Global[,i]<- as.factor(Datos_Global[,i])
}

# Se recuperan el formato de variables numéricas
Datos_Global$year_declaracion <- as.numeric(Datos_Global$year_declaracion)
Datos_Global$S_Declaracion <- as.numeric(Datos_Global$S_Declaracion)
Datos_Global$No_Edad <- as.numeric(Datos_Global$No_Edad)

# Se cambian los nombres que no son adecuados
colnames(Datos_Global)[1] <- "ID_Genom"
colnames(Datos_Global)[2] <- "Cluster_5snps"
colnames(Datos_Global)[3] <- "Cluster_10snps"
colnames(Datos_Global)[45] <- "resH.INH"
colnames(Datos_Global)[46] <- "resZ.PZA"
colnames(Datos_Global)[47] <- "resE.EMB"
colnames(Datos_Global)[48] <- "resR.RIF"
colnames(Datos_Global)[49] <- "resS.SM"

# Datos que sí tienen información epi de la encuesta 
Con_Epi <- Datos_Global[!is.na(Datos_Global$Sexo),]

# Se eliminan los individuos con NAs Municipio_Residencia
Con_Epi[is.na(Con_Epi$Municipio_Residencia),]

Datos_Bien <- Con_Epi[!is.na(Con_Epi$Municipio_Residencia),]
Datos_Bien <- Datos_Bien[Datos_Bien$Municipio_Residencia!="ALMANSA",]

# Número y porcentaje de casos de TB en españoles y extranjeros
table(Datos_Bien$Extranjero)
1864+1180
1864/(3044) #0.6123522 Locales
1180/3044   #0.3876478 Extranjeros

# Tasas según el número de población
Población_Año_Local <- (4265214 +4280478+ 4287589+ 4299129 +4298782+ 4300356 +4305222)/7
Población_Año_Extr <- (739630+ 700211+ 672379+ 642380+ 664921+703413+752131)/7

# Tasa de TB en extranjeros
1180/(739630+ 700211+ 672379+ 642380+ 664921+703413+752131)

# Tasa de TB en locales
1864/(4265214 +4280478+ 4287589+ 4299129 +4298782+ 4300356 +4305222)


###  Análisis de la transmisión

# Sacamos un data.frame con los datos de TB que tienen información
# genómica asociada
Datos_Genom <- Datos_Bien[ !is.na(Datos_Bien$ID_Genom),]

# Se crea una variable Bernoulli que nos diga si están o no en transmisión
Datos_Genom$Ber <- Datos_Genom$Cluster_5snps

Datos_Genom$Ber <- as.character(Datos_Genom$Ber)

Datos_Genom$Ber[Datos_Genom$Ber=="unique"] <- "NO"
Datos_Genom$Ber[Datos_Genom$Ber!="NO"] <- "SI"

Datos_Genom$Ber <- as.factor(Datos_Genom$Ber)
plot(Datos_Genom$Ber,  main="Casos en Transmisión Genómica")

colSums(is.na(Datos_Genom)) #MunRes 0 NAs
 
# Número de casos y porcentaje de transmisión de TB 
table(Datos_Genom$Ber)
table(Datos_Genom$Ber, Datos_Genom$Extranjero)

# Casos de TB en transmisión en extran/Casos de TB en extranjeros
234/(234+423)

#  Casos de TB en transmisión en españoles/Casos de TB en españoles
482/(482+508)

# Casos de TB en transmisión extranjeros/ Casos en transmisión
482/(482+234)

# Casos de TB en transmisión locales / Casos en transmisión
234/(482+234)


# Casos de TB en transmisión extr/ Nº extranjeros en CV
(234/7)/696437.9
# Casos de TB en transmisión locales/ Nº locales en CV
482/(4265214 +4280478+ 4287589+ 4299129 +4298782+ 4300356 +4305222)


