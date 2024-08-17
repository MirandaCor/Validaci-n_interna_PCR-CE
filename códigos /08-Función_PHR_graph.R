library(dplyr)
library(tidyr)
library(openxlsx)
library(ggplot2)
library(readxl)

# Dirección donde se leerán los archivos y asignar PHR elegido 
folder_path <- "C:/Users/3500_1/3500_result/"
excel_files <- list.files(folder_path, pattern = "matrix\\.xlsx$", full.names = TRUE)
phr <- 0.60

# Define el orden de Sample.Name
custom_order <- c("4.8", "2.4", "1.2", "0.6", "0.3", "0.15", "0.075", "0.0375", "0.01875")

analyze_and_plot <- function(data, folder_path, file_name) {
  # Remover espacios en Sample.Name
  data$Sample.Name <- gsub("\\s+(\\d+\\.\\d+)", "\\1", data$Sample.Name)
  
  # Extraer la parte numérica
  data$Numeric_Part <- gsub("^.*_(\\d+\\.\\d+)ng$", "\\1", data$Sample.Name)
  
  # Contar ceros por grupo
  na_sum_grouped <- data %>%
    select(-Sample.Name) %>% # eliminar primer columna
    group_by(Numeric_Part) %>%
    summarise(across(everything(), ~ sum(. == 0)))
  
  # Calcular suma de valores por debajo de 0.6 o el determinado por grupo
  below_point_six_sum_grouped <- data %>%
    group_by(Numeric_Part) %>% # eliminar primer columna
    summarise(across(-(1), ~ sum(. <= phr & . <= 1 & . != 0,  na.rm = TRUE)))
  
  # Crear workbook
  wb <- createWorkbook()
  
  # Agregar hojas para los datos
  addWorksheet(wb, "Sum_dropout")
  writeData(wb, sheet = "Sum_dropout", na_sum_grouped)
  
  addWorksheet(wb, "Below_phr")
  writeData(wb, sheet = "Below_phr", below_point_six_sum_grouped)
  
  # Graficar valores por debajo de 0.6
  data_long <- tidyr::pivot_longer(below_point_six_sum_grouped, -Numeric_Part, names_to = "Column", values_to = "Sum_dropout")
  
  plot_below_06 <- ggplot(data_long, aes(x = Column, y = Numeric_Part, fill = Sum_dropout)) +
    geom_tile() +
    scale_fill_gradient(low = "steelblue", high = "white", 
                        breaks = seq(0, max(data_long$Sum_dropout, na.rm = TRUE), by = 1),
                        labels = function(x) round(x, 0)) +
    labs(x = "Markers", y = "Concentrations", fill = "Below_PHR") +
    theme_minimal(base_size = 8) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA)
    )
  
    
  
  # Guardar plot_below_06 como un archivo de imagen temporal
  plot_file_1 <- tempfile(fileext = ".png")
  ggsave(plot_file_1, plot = plot_below_06, width = 10, height = 7, units = "in")
  
  addWorksheet(wb, "Plot_Below_phr")
  insertImage(wb, sheet = "Plot_Below_phr", file = plot_file_1, width = 10, height = 7, units = "in")
  
  # Graficar NA counts
  data_na <- tidyr::pivot_longer(na_sum_grouped, -Numeric_Part, names_to = "Column", values_to = "Drop_out")
  
  plot_na_count <- ggplot(data_na, aes(x = Column, y = Numeric_Part, fill = Drop_out)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(x = "Markers", y = "Concentrations", fill = "Perdida_alelicas") +
    theme_minimal(base_size = 8) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA)
    )
  
  # Guardar plot_na_count como un archivo de imagen temporal
  plot_file_2 <- tempfile(fileext = ".png")
  ggsave(plot_file_2, plot = plot_na_count, width = 10, height = 7, units = "in")
  
  addWorksheet(wb, "Plot_Sum_dropout")
  insertImage(wb, sheet = "Plot_Sum_dropout", file = plot_file_2, width = 10, height = 7, units = "in")
  
  # Guardar workbook
  saveWorkbook(wb, file.path(folder_path, paste0(file_name, "_summary_data.xlsx")), overwrite = TRUE)
  
  return(list(plot_below_06 = plot_below_06, plot_na_count = plot_na_count))
}

# Loop por cada archivo Excel
for (i in excel_files) {
  file_name <- tools::file_path_sans_ext(basename(i))
  data <- read_excel(i)
  
  if (!is.data.frame(data)) {
    next
  }
  
  result <- analyze_and_plot(data, folder_path, file_name)
  
  print(result$plot_below_06)
  print(result$plot_na_count)
}
