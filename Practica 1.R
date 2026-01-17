#---
#title: "Practica 1"
#author: "Ulises"
#output: html_document
#date: "2026-01-17"
#---

# Configuración
knitr::opts_chunk$set(echo = F)
knitr::opts_chunk$set(warning = F)
knitr::opts_chunk$set(error = F)
knitr::opts_chunk$set(message = F)

# Carga de librerías
library(readr)
library(dplyr)
library(stringr)

# Carga de datos
epa_http <- read_table("epa-http.csv", col_names = FALSE, show_col_types = F)
colnames(epa_http) <- c("IP", "Timestamp", "Tipo", "URL", "Protocolo", "ResponseCode", "Bytes")

# Cambio ResponseCode a factor 
epa_http$ResponseCode <- as.factor(epa_http$ResponseCode)

# Cambio Bytes a integer
epa_http$Bytes <- as.integer(epa_http$Bytes)

# Aplicación de stringr en Tipo
epa_http$Tipo <- str_remove(epa_http$Tipo, '^"')

# Aplicación de stringr en Protocolo
epa_http$Protocolo <- str_remove(epa_http$Protocolo, '"$')

# Mostrar datos
knitr::kable(
  head(epa_http),
  caption = "Muestra de conjunto de datos:"
)

# Ejercicio 1

# Número filas
cat(nrow(epa_http))

# Número columnas
cat(ncol(epa_http))

# Valor medio de la columna Bytes
cat(mean(epa_http$Bytes, na.rm = T))

# Ejercicio 2

# Número IPs .edu
cat(sum(grepl(".edu", epa_http$IP)))

# Ejercicio 3

# Nuevo dataframe donde solo hayan con GET
epa_get <- epa_http[epa_http$Tipo == "GET", ]

# Creo nueva columna (hora) con el valor del Timestamp
epa_get <- epa_get %>%
  mutate(
    Hora = sub("^\\[|\\]$", "", Timestamp),  # quita [ ]
    Hora = sapply(strsplit(Hora, ":"), `[`, 2)
  )

# Cuento cuantas veces aparece cada una de las franjas horarias mediante un nuevo dataframe con dos columnas, valor de las horas [0-23] y su frecuencia
frecuencia_horas <- epa_get %>%count(Hora)

# Muestro solo la primera fila y filtro de manera descendente
hora_max <- frecuencia_horas %>%
  arrange(desc(n)) %>%
  slice(1) %>%           
  pull(Hora)
cat(hora_max)

# Muestro las 3 franjas horarias con mayor numero de peticiones
knitr::kable(
  frecuencia_horas %>%
    arrange(desc(n)) %>%
    slice(1:3),
  caption = "3 horas del día con mayor número de peticiones:"
)

# Ordenación de datos
frecuencia_horas <- frecuencia_horas[order(frecuencia_horas$Hora), ]

# Generación de gráfico de barras
barplot(
  height = frecuencia_horas$n,
  names.arg = frecuencia_horas$Hora,
  col = "purple",
  xlab = "Franja horaria",
  ylab = "Nº peticiones HTTP GET",
  main = "Peticiones HTTP GET por hora"
)

# Ejercicio 4

# Filtro filas con texto ".edu" en la columna de IP
epa_edu <- epa_http[grepl("\\.edu", epa_http$IP), ]

# Filtro filas que contienen un fichero de descarga .txt
epa_edu <- epa_edu[grepl("\\.txt", epa_edu$URL), ]

# Sumo la columna de Bytes, ignorando los NA
cat(sum(epa_edu$Bytes, na.rm = T))

# Ejercicio 5

# Suma de las filas con URL de valor "/"
cat(sum(epa_http$URL == "/", na.rm = T))

# Ejercicio 6

# Suma de las filas cuyo Protocolo NO tenga de valor HTTP/0.2 
cat(sum(epa_http$Protocolo != "HTTP/0.2", na.rm = T))