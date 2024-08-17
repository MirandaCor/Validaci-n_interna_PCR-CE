# Cargar librerías necesarias
library(writexl)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(openxlsx)
library(tidyr)

#modificar
folder_path <- "C:/Users/3500_1/3500_results/"
excel_files <- list.files(folder_path, pattern = "_sample_ALLELE_PHR_ua\\.xlsx$", full.names = TRUE)
phr_= 0.6
# Define the order of Sample.Name values
custom_order <- c("4.8", "2.4", "1.2", "0.6", "0.3", "0.15", "0.075", "0.0375", "0.01875")

process_excel <- function(file_path) {
  # Leer el archivo Excel
  data <- read.xlsx(file_path)
  
  data <- data %>%
    filter(PHR >= phr_)
  # Asegurarse de que los valores de PHR sean 0 si están vacíos
  #data$PHR[is.na(data$PHR)] <- 0
  
  # Extraer la parte numérica del nombre de la muestra
  numeric_part <- gsub("^.*_(\\d+\\.\\d+)ng$", "\\1", data$Sample.Name)
  
  # Encontrar el índice en el orden personalizado
  index <- match(numeric_part, custom_order)
  
  # Ordenar los datos según el orden personalizado
  data_sorted <- data[order(index), ]
  
  # Crear una matriz para almacenar los valores PHR
  phr_matrix <- matrix(NA, nrow = length(unique(data_sorted$Sample.Name)), 
                       ncol = length(unique(data_sorted$Marker)),
                       dimnames = list(unique(data_sorted$Sample.Name), unique(data_sorted$Marker)))
  
  
  # Bucle a través de cada fila de los datos ordenados
  for (i in 1:nrow(data_sorted)) {
    sample_name <- as.character(data_sorted[i, "Sample.Name"])  
    marker <- as.character(data_sorted[i, "Marker"])           
    phr <- data_sorted[i, "PHR"]
    
    # Asignar el valor de PHR a la celda correspondiente en la matriz
    phr_matrix[sample_name, marker] <- phr
  }
  
  # Convertir la matriz a un data frame
  phr_matrix_df <- as.data.frame(phr_matrix)
  
  # Reemplazar valores NA con 0
  phr_matrix_df[is.na(phr_matrix_df)] <- 0
  
  # Agregar Sample.Name como una columna
  phr_matrix_df$Sample.Name <- rownames(phr_matrix_df)
  
  # Reordenar las columnas para tener Sample.Name como la primera columna
  phr_matrix_df <- phr_matrix_df[, c("Sample.Name", names(phr_matrix_df)[1:(ncol(phr_matrix_df) - 1)])]
  
  # Restablecer los nombres de las filas del data frame
  rownames(phr_matrix_df) <- NULL
  
  return(phr_matrix_df)
}


# Loop through each filtered CSV file
for (file in excel_files) {
  # Process the CSV file
  phr_matrix_df <- process_excel(file)
  
  # Extract filename without extension
  filename <- tools::file_path_sans_ext(basename(file))
  
  # Write the phr_matrix to an Excel file
  write.xlsx(phr_matrix_df, paste0(folder_path, filename, "_matrix.xlsx"))
  
  cat("Archivo procesado :", filename, "\n")
}


