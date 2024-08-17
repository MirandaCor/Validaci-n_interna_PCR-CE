# Cargar las librerías necesarias
library(dplyr)
library(readxl)
library(writexl)
library(ggplot2)
library(openxlsx)

# Dirección de donde se desea guardar el archivo
path <- "C:/Users/3500_1/3500_results/"

# Leer el archivo CSV (asegúrate de ajustar la ruta del archivo según sea necesario)
data <- read.csv(paste0(path,"output_rearrange_neg_rows.csv"))


# Filtrar los valores específicos de la columna Marker,los valores específicos de la columna Height, aqui puedes agregar los picos
# que consideran atipicos, artefactos, recordar que justificar la eliminación 
# es importante 

data <- data %>%
  filter(!(Marker %in% c("Yindel", "AMEL", "IQCS", "IQCL") ))


#### personalizar patron
# Filtrar las filas que contienen "Neg" en el nombre de la muestra
data_neg <- data[grep("Neg", data$Sample.Name), ]

# Eliminar espacios en los nombres de las muestras
data_neg$Sample.Name <- gsub(" ", "", data_neg$Sample.Name)

# Extraer el identificador del grupo (Neg1, Neg2, etc.)
data_neg$Group <- sub("^(.*-Neg\\d+).*", "\\1", data_neg$Sample.Name)

# Extraer la parte numérica del nombre de la muestra y crear una nueva columna para identificar los grupos de tres
data_neg$Numeric_Part <- as.numeric(gsub("^.*-Neg(\\d+).*", "\\1", data_neg$Sample.Name))
################

# Crear columna para identificar grupos de tres
data_neg$Group_of_Three <- ceiling(data_neg$Numeric_Part / 3)

# Definir los kits (esto depende de tus datos y del formato de Sample.Name, ajusta según corresponda)
data_neg$Kit <- sub("^(.*)-.*$", "\\1", data_neg$Sample.Name)


# Agrupar por Kit, Dye y grupo de tres y calcular estadísticas
stats_summary <- data_neg %>%
  group_by(Kit, Dye, Group_of_Three) %>%
  summarize(
    Max_Height = max(Height, na.rm = TRUE),
    Min_Height = min(Height, na.rm = TRUE),
    Avg_Height = mean(Height, na.rm = TRUE),
    SD_Height = sd(Height, na.rm = TRUE),
    .groups = "keep"  # Eliminar la agrupación en el resultado final
  )

# Mostrar el DataFrame con los resultados
#print(stats_summary)
stats_summary$'UA=2*(max_hight-min_hight)' <-2*(stats_summary$Max_Height-stats_summary$Min_Height)
stats_summary$'LOD=avg_hight+3std_hight' <-stats_summary$Avg_Height + (3*stats_summary$SD_Height)
stats_summary$'LOQ=avg_hight+10std_hight' <-stats_summary$Avg_Height + (10*stats_summary$SD_Height)

# Convertir Kit en un factor con niveles ordenados específicamente
# stats_summary$Kit <- factor(stats_summary$Kit, levels = c("gf", "yfp", "ngm", "mf", "ppy23"))
stats_summary$Kit <- factor(stats_summary$Kit, levels = c("GF", "YFP", "NGM", "MF", "PPY23"))

# Ordenar por Kit y Group_of_Three
stats_summary <- stats_summary %>%
  arrange(Kit, Group_of_Three)

# Crear una lista de hojas de datos
sheet <- list('data' = data, 'data_neg' = data_neg, 'summary' = stats_summary)
#write_xlsx(sheet, paste0(path, '3500_UA_NEG_arrange.xlsx'))


# Crear el archivo Excel y agregar las hojas de datos
wb <- createWorkbook()
addWorksheet(wb, "Data")
addWorksheet(wb, "Data_Neg")
addWorksheet(wb, "Summary")

writeData(wb, "Data", data)
writeData(wb, "Data_Neg", data_neg)
writeData(wb, "Summary", stats_summary)

# Generar el boxplot
plot <- ggplot(data_neg, aes(x = Marker, y = Height, fill = Dye)) +
  geom_boxplot() +
  labs(title = "Boxplot de Heights por Marker y Kit", x = "Markers", y = "Height") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Kit, scales = "free_x")

# Guardar la gráfica temporalmente como archivo PNG
temp_plot_file <- paste0(path, "temp_plot.png")
ggsave(filename = temp_plot_file, plot = plot, width = 12, height = 8)

# Insertar la imagen de la gráfica en el archivo Excel
addWorksheet(wb, "Plot")
insertImage(wb, "Plot", temp_plot_file, width = 12, height = 8, startRow = 1, startCol = 1)

# Guardar el archivo Excel
saveWorkbook(wb, paste0(path, '3500_UA_NEG_arrange.xlsx'), overwrite = TRUE)

# Eliminar el archivo de imagen temporal
file.remove(temp_plot_file)

# Mostrar la gráfica en pantalla
print(plot)
