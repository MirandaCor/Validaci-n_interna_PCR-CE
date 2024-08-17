# Cargar librerías necesarias
library(ggplot2)
library(openxlsx)
library(dplyr)

folder_path <- "C:/Users/3500_1/3500_results/"

# Lista de archivos y valores de ua
archivos <- list(
  list(file_path = paste0(folder_path,"gf_sample_ALLELE_PHR.csv"), ua = 50),
  list(file_path = paste0(folder_path, "mf_sample_ALLELE_PHR.csv"), ua = 50),
  list(file_path = paste0(folder_path, "ngm_sample_ALLELE_PHR.csv"), ua = 50),
  list(file_path = paste0(folder_path, "ppy_sample_ALLELE_PHR.csv"), ua = 50),
  list(file_path = paste0(folder_path, "yfp_sample_ALLELE_PHR.csv"), ua = 50)
)

# Función para normalizar las concentraciones
normalize_concentration <- function(concentration) {
  concentration <- gsub("_", "", concentration)  # Eliminar guiones bajos
  concentration <- gsub("\\s+", "", concentration)  # Eliminar espacios en blanco
  concentration <- gsub("ng$", "", concentration, ignore.case = TRUE)  # Eliminar el sufijo ng
  concentration <- tolower(concentration)  # Convertir todo a minúsculas para unificar
  return(concentration)
}

# Función para procesar archivos y guardar en formato Excel
procesar_archivo <- function(file_path, ua) {
  # Leer los datos del archivo CSV
  data <- read.csv(file_path)
  data <- data[ , c("Sample.Name", "Marker", "a1", "a1_Height", "a2", "a2_Height", "PHR")]
  
  # Filtrar y modificar la columna PHR
  data$PHR <- ifelse(is.na(data$a1_Height) | is.na(data$a2_Height) | data$a1_Height < ua | data$a2_Height < ua,
                     ifelse(data$a1_Height > ua & !is.na(data$a1_Height), data$a1_Height,
                            ifelse(data$a2_Height > ua & !is.na(data$a2_Height), data$a2_Height, 0)), 
                     data$PHR)
  
  data$PHR[is.na(data$PHR)] <- 0
  
  # Extraer y normalizar la concentración del nombre de la muestra
  data <- data %>%
    mutate(Concentration = gsub(".*_(\\d+\\.\\d+)ng.*", "\\1", Sample.Name)) %>%
    mutate(Concentration = normalize_concentration(Concentration))
  
  # Contar cuántos PHR no se calcularon (es decir, PHR igual a 0)
  no_calculated_phr <- data %>%
    group_by(Concentration) %>%
    summarise(Count_No_PHR = sum(PHR > 1.0 | PHR == 0))
  
  # Contar cuántas filas tienen un PHR en los rangos especificados
  count_summary <- data %>%
    group_by(Concentration) %>%
    summarise(
      PHR_above_0_3 = sum(PHR >= 0.3 & PHR <= 1.0),
      PHR_above_0_4 = sum(PHR >= 0.4 & PHR <= 1.0),
      PHR_above_0_5 = sum(PHR >= 0.5 & PHR <= 1.0),
      PHR_above_0_6 = sum(PHR >= 0.6 & PHR <= 1.0),
      PHR_above_0_7 = sum(PHR >= 0.7& PHR <= 1.0)
    )
  
  # Crear un nuevo nombre de archivo Excel
  nuevo_nombre <- file.path(dirname(file_path), paste0(tools::file_path_sans_ext(basename(file_path)), "_ua.xlsx"))
  
  # Guardar los datos y el resumen en el archivo Excel
  wb <- createWorkbook()
  
  # Agregar la hoja de datos originales procesados
  addWorksheet(wb, "Processed Data")
  writeData(wb, "Processed Data", data)
  
  # Agregar la hoja con el conteo de PHR no calculados
  addWorksheet(wb, "No PHR Counts")
  writeData(wb, "No PHR Counts", no_calculated_phr)
  
  # Agregar la hoja con los conteos de PHR en los rangos especificados
  addWorksheet(wb, "PHR Counts")
  writeData(wb, "PHR Counts", count_summary)
  
  # Guardar el archivo Excel
  saveWorkbook(wb, nuevo_nombre, overwrite = TRUE)
  
  cat("Archivo procesado y guardado como:", nuevo_nombre, "\n")
}

# Procesar todos los archivos en la lista
lapply(archivos, function(archivo) procesar_archivo(archivo$file_path, archivo$ua))
