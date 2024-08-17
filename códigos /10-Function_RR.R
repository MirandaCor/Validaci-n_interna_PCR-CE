# Cargar librerías necesarias
library(writexl)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(openxlsx)
library(tidyr)

# Donde leera los archivos
path <- "C:/Users/3500_1/3500_results/"
# Lista de archivos y valores de ua y phr
archivos <- list(
  list(file_path = paste0(path, "gf_positive_ALLELE_PHR.csv"), ua = 50, phr = 0.6),
  list(file_path = paste0(path,"mf_positive_ALLELE_PHR.csv"), ua = 50, phr = 0.6),
  list(file_path = paste0(path,"ngm_positive_ALLELE_PHR.csv"), ua = 50, phr = 0.6),
  list(file_path = paste0(path,"ppy_positive_ALLELE_PHR.csv"), ua = 50, phr = 0.6),
  list(file_path = paste0(path,"yfp_positive_ALLELE_PHR.csv"), ua = 50, phr = 0.6)
)

# Función para procesar archivos y guardar en formato Excel
procesar_archivo <- function(file_path, ua, phr) {
  data_input <- read.csv(file_path)
  data_input <- data_input[, c("Sample.Name", "Marker", "a1", "a1_Height", "a2", "a2_Height", "PHR")]
  
  # Filtrar filas basadas en 'ua' y 'phr'
  data_filtered <- data_input %>%
    filter((a1_Height >= ua & a2_Height >= ua) & (PHR >= phr))
  
  # Contar cuántas veces cada combinación de datos aparece en cada Sample.Name antes del filtro
  count_original <- data_input %>%
    group_by(Sample.Name, Marker, a1, a2) 
  
  # Contar cuántas veces cada combinación de datos aparece en cada Sample.Name después del filtro
  count_filtered <- data_filtered %>%
    group_by(Sample.Name, Marker, a1, a2) 
  
  # Identificar las filas que se perdieron al aplicar el filtro
  data_lost <- anti_join(count_original, count_filtered)
  
  # Crear nuevo nombre de archivo
  nuevo_nombre <- file.path(dirname(file_path), paste0(tools::file_path_sans_ext(basename(file_path)), "_ua_filtered.xlsx"))
  
  # Crear una lista de hojas para escribir en Excel
  sheet_list <- list("Input" = data_input, "Filtered Data" = data_filtered, "Lost Data" = data_lost)
  
  # Guardar el archivo modificado
  write.xlsx(sheet_list, nuevo_nombre, rowNames = FALSE)
  
  cat("Archivo procesado y guardado como:", nuevo_nombre, "\n")
}

# Procesar todos los archivos en la lista
lapply(archivos, function(archivo) {
  procesar_archivo(archivo$file_path, archivo$ua, archivo$phr)
})


