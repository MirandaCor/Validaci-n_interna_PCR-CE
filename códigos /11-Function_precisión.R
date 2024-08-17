library(dplyr)
library(tidyr)
library(openxlsx)

file_path <- 'C:/Users/3500_1/3500_results/'
data1 <- read.csv(paste0(file_path, 'output_rearrange_ladder_rows.csv'))

# Filtrar las filas con 'OL' en la columna Allele
data <- data1 %>% filter(!Allele %in% c('OL') ) # & !Marker %in% c( 'Yindel', 'IQCS', 'IQCL', 'AMEL')

# Función para calcular estadísticas
calculate_stats <- function(data) {
  data %>%
    group_by(Marker, Allele, Panel, Cap) %>%
    summarise(
      min_size = round(min(Size, na.rm = TRUE), 5),
      max_size = round(max(Size, na.rm = TRUE), 5),
      mean_size = round(mean(Size, na.rm = TRUE), 5),
      sd_size = round(sd(Size, na.rm = TRUE), 5),
      difference_min_max = round(max(Size, na.rm = TRUE) - min(Size, na.rm = TRUE), 5),
      .groups = 'drop'
    )
}

# Función para calcular estadísticas sin la columna Cap
calculate_stats_without_cap <- function(data) {
  data %>%
    group_by(Marker, Allele, Panel) %>%
    summarise(
      min_size = round(min(Size, na.rm = TRUE), 5),
      max_size = round(max(Size, na.rm = TRUE), 5),
      mean_size = round(mean(Size, na.rm = TRUE), 5),
      sd_size = round(sd(Size, na.rm = TRUE), 5),
      difference_min_max = round(max(Size, na.rm = TRUE) - min(Size, na.rm = TRUE), 5),
      .groups = 'drop'
    )
}

# Función para calcular el promedio de la diferencia por rangos de tamaño
calculate_mean_sd <- function(data) {
  data %>%
    mutate(size_range = cut(mean_size, breaks = seq(0, 600, by = 100), include.lowest = TRUE, right = FALSE)) %>%
    filter(!is.na(size_range)) %>%
    group_by(size_range) %>%
    summarise(
      mean_sd = round(mean(sd_size, na.rm = TRUE), 5),
      .groups = 'drop'
    ) %>%
    mutate(size_range = as.character(size_range)) %>%
    rename(Rango = size_range)
}

# Agrupar los datos por Panel y calcular estadísticas con y sin Cap
stats <- calculate_stats(data)
stats_without_cap <- calculate_stats_without_cap(data)

# Separar los resultados en diferentes data frames según el Panel
panels <- unique(stats$Panel)
panel_dfs <- lapply(panels, function(panel) {
  list(
    with_cap = stats %>% filter(Panel == panel),
    without_cap = stats_without_cap %>% filter(Panel == panel)
  )
})

names(panel_dfs) <- panels

# Crear archivos Excel separados para cada Panel
lapply(names(panel_dfs), function(panel) {
  # Crear un nuevo workbook
  wb <- createWorkbook()
  
  # Añadir hoja para las estadísticas del Panel con Cap
  addWorksheet(wb, sheetName = "Data")
  writeData(wb, sheet = "Data", data1)
  
  # Añadir hoja para las estadísticas del Panel con Cap
  addWorksheet(wb, sheetName = "Stats_With_Cap")
  writeData(wb, sheet = "Stats_With_Cap", panel_dfs[[panel]]$with_cap)
  
  # Añadir hoja para las estadísticas del Panel sin Cap
  addWorksheet(wb, sheetName = "Stats_Without_Cap")
  writeData(wb, sheet = "Stats_Without_Cap", panel_dfs[[panel]]$without_cap)
  
  # Filtrar los datos originales para el Panel actual
  panel_data <- data %>% filter(Panel == panel)
  
  
  mean_diff_stats_without_cap<- calculate_mean_sd(panel_dfs[[panel]]$without_cap)
  # Calcular el promedio de la diferencia por rangos de tamaño (sin Cap)
  mean_diff_stats <- calculate_mean_sd(panel_dfs[[panel]]$without_cap)
  
 
  
  # Añadir hoja para los promedios de la diferencia por rangos de tamaño (sin Cap)
  addWorksheet(wb, sheetName = "Mean_sd_Without_Cap")
  writeData(wb, sheet = "Mean_sd_Without_Cap", mean_diff_stats_without_cap)
  
  # Guardar el archivo Excel
  saveWorkbook(wb, file = paste0(file_path, "statistics_", panel, ".xlsx"), overwrite = TRUE)
})
