# Cargar librerías necesarias
library(dplyr)
library(writexl)
library(readr)

file_path <- "C:/Users/3500_1/3500_result/"

all_files <- list.files(file_path, pattern = "_positive_ALLELE\\.csv$", full.names = TRUE)

# Definir rangos de referencia
#GF
gf_ranges <- list(D3S1358 = c(15, 15),vWA = c(14, 14),D16S539 = c(9, 10),CSF1PO = c(10, 12),
                  TPOX = c(8, 8),D8S1179 = c(12, 12),D21S11 = c(28, 28),D18S51 = c(12, 12),
                  DYS391 = c(11, 11),D2S441 = c(14, 15),D19S433 = c(14, 15),TH01 = c(7, 9.3),
                  FGA = c(24, 24),D22S1045 = c(11, 16),D5S818 = c(11, 11),D13S317 = c(11, 11),
                  D7S820 = c(11, 12),SE33 = c(19, 25.2),D10S1248 = c(12, 12),D1S1656 = c(13, 13),
                  D12S391 = c(18, 19),D2S1338 = c(20, 23))
#MF
mf_ranges <- list(D16S539 = c(10, 10),CSF1PO = c(12, 12),D21S11 = c(28, 30),D18S51 = c(12, 12),
                  FGA = c(26, 26),D13S317 = c(11, 11),D7S820 = c(7, 7),D2S1338 = c(20, 20))
#NGM
ngm_ranges <- list(D3S1358 = c(15, 15),vWA = c(14, 14),D16S539 = c(9, 9),D8S1179 = c(12, 12),
                   D21S11 = c(28, 31),D18S51 = c(12, 12),D12S391 = c(18, 18),D2S441 = c(14, 14),
                   D19S433 = c(14, 15),TH01 = c(7, 7),FGA = c(24, 24),D22S1045 = c(11, 11),
                   SE33 = c(17, 25),D10S1248 = c(12, 12),D1S1656 = c(13, 13),D2S1338 = c(20, 20))

#PPY23
ppy_ranges <- list(DYS19 = c(14),DYS385 = c(13,16),DYS389 = c(14,31),DYS390 = c(24),
                   DYS391 = c(10),DYS392 = c(13),DYS437 = c(14),DYS393 = c(13),DYS438 = c(9),
                   DYS439 = c(12),DYS448 = c(19),DYS456 = c(17),DYS481 = c(22),
                   YS458 = c(17),DYS549 = c(13),DYS533 =c(12),DYS576 = c(18),DYS570 =c(17),
                   DYS635 = c(21),YGATAH4 =c(11),DYS643 =c(10))

#YFP
yfp_ranges <- list(DYS19 = c(15),DYS385 = c(15,15),DYS389 = c(15,15),DYS390 = c(20),DYS391 = c(15),DYS392 = c(15),DYS437 = c(15),
                   DYS393 = c(15),DYS438 = c(15),DYS439 = c(15),DYS448 = c(15),DYS456 = c(15),DYS481 = c(20),
                   DYS458 = c(15),DYS533 =c(15),DYS576 = c(15),DYS570 =c(15),DYS635 = c(20),YGATAH4 =c(15),
                   DYS627 =c(20),DYS460 = c(15),DYS518 =c(30),DYS449 =c(30),DYF387S1 = c(30,30))


filter_data <- function(sam, data) {
  unique_samples <- unique(data[c("Sample.Name", "Marker")])
  
  filtered_data <- data.frame(Sample.Name = character(),
                              Marker = character(),
                              a1 = numeric(),
                              a1_Height = numeric(),
                              a2 = numeric(),
                              a2_Height = numeric(),
                              a1_stutter_minus = numeric(),
                              a1_stutter_minus_Height = numeric(),
                              a1_stutter_plus = numeric(),
                              a1_stutter_plus_Height = numeric(),
                              a2_stutter_minus = numeric(),
                              a2_stutter_minus_Height = numeric(),
                              a2_stutter_plus = numeric(),
                              a2_stutter_plus_Height = numeric(),
                              SPHR_a1_minus1 = numeric(),
                              SPHR_a1_plus1 = numeric(),
                              SPHR_a2_minus1 = numeric(),
                              SPHR_a2_plus1 = numeric(),
                              SPHR_a1_minus2 = numeric(),
                              SPHR_a1_plus2 = numeric(),
                              SPHR_a2_minus2 = numeric(),
                              SPHR_a2_plus2 = numeric(),
                              PHR = numeric(),
                              stringsAsFactors = FALSE)
  
  for (i in 1:nrow(unique_samples)) {
    sample_marker <- unique_samples[i, ]
    sample_name <- sample_marker$Sample.Name
    marker <- sample_marker$Marker
    sample_data <- data[data$Sample.Name == sample_name & data$Marker == marker, ]
    alleles <- unique(sample_data$Allele)
    
    range_vals <- sam[[marker]]
    allele_data <- list(a1 = 0, a1_Height = 0, a2 = 0, a2_Height = 0,
                        a1_stutter_minus = 0, a1_stutter_minus_Height = 0,
                        a1_stutter_plus = 0, a1_stutter_plus_Height = 0,
                        a2_stutter_minus = 0, a2_stutter_minus_Height = 0,
                        a2_stutter_plus = 0, a2_stutter_plus_Height = 0)
    
    if (length(range_vals) >= 1) {
      a1_val <- range_vals[1]
      a2_val <- ifelse(length(range_vals) == 2, range_vals[2], NA)
      
      for (allele in alleles) {
        filtered_allele <- sample_data[sample_data$Allele == allele, ]
        
        # Asignación de a1
        if (allele == a1_val) {
          allele_data$a1 <- allele
          allele_data$a1_Height <- filtered_allele$Height[1]
        }
        # Asignación de a2
        if (!is.na(a2_val) && allele == a2_val) {
          allele_data$a2 <- allele
          allele_data$a2_Height <- filtered_allele$Height[1]
        }
        # Stutter minus para a1
        if (allele == (a1_val - 1)) {
          allele_data$a1_stutter_minus <- allele
          allele_data$a1_stutter_minus_Height <- filtered_allele$Height[1]
        }
        # Stutter plus para a1
        if (!is.na(a2_val) && allele == (a1_val + 1) && allele != a2_val) {
          allele_data$a1_stutter_plus <- allele
          allele_data$a1_stutter_plus_Height <- filtered_allele$Height[1]
        }
        # Stutter minus para a2
        if (!is.na(a2_val) && allele == (a2_val - 1) && allele != a1_val) {
          allele_data$a2_stutter_minus <- allele
          allele_data$a2_stutter_minus_Height <- filtered_allele$Height[1]
        }
        # Stutter plus para a2
        if (!is.na(a2_val) && allele == (a2_val + 1)) {
          allele_data$a2_stutter_plus <- allele
          allele_data$a2_stutter_plus_Height <- filtered_allele$Height[1]
        }
      }
    }
    
    # Calcular PHR
    PHR_value <- if (!is.na(allele_data$a1_Height) && !is.na(allele_data$a2_Height) && allele_data$a1_Height > allele_data$a2_Height) {
      allele_data$a2_Height / allele_data$a1_Height
    } else if (!is.na(allele_data$a1_Height) && !is.na(allele_data$a2_Height)) {
      allele_data$a1_Height / allele_data$a2_Height
    } else {
      NA
    }

    SPHR_a1_minus <- if (!is.na(allele_data$a1_stutter_minus)) {
      allele_data$a1_stutter_minus_Height / allele_data$a1_Height
    } else { 0 }

    SPHR_a1_plus <- if (!is.na(allele_data$a1_stutter_plus)) {
      allele_data$a1_stutter_plus_Height / allele_data$a1_Height
    } else { 0 }

    SPHR_a2_minus <- if (!is.na(allele_data$a2_stutter_minus)) {
      allele_data$a2_stutter_minus_Height / allele_data$a2_Height
    } else { 0 }

    SPHR_a2_plus <- if (!is.na(allele_data$a2_stutter_plus)) {
      allele_data$a2_stutter_plus_Height / allele_data$a2_Height
    } else { 0 }
    
    row_data <- data.frame(Sample.Name = sample_name,
                           Marker = marker,
                           a1 = allele_data$a1,
                           a1_Height = allele_data$a1_Height,
                           a2 = allele_data$a2,
                           a2_Height = allele_data$a2_Height,
                           a1_stutter_minus = allele_data$a1_stutter_minus,
                           a1_stutter_minus_Height = allele_data$a1_stutter_minus_Height,
                           a1_stutter_plus = allele_data$a1_stutter_plus,
                           a1_stutter_plus_Height = allele_data$a1_stutter_plus_Height,
                           a2_stutter_minus = allele_data$a2_stutter_minus,
                           a2_stutter_minus_Height = allele_data$a2_stutter_minus_Height,
                           a2_stutter_plus = allele_data$a2_stutter_plus,
                           a2_stutter_plus_Height = allele_data$a2_stutter_plus_Height,
                           PHR = PHR_value,
                           SPHR_a1_minus = SPHR_a1_minus,
                           SPHR_a1_plus = SPHR_a1_plus,
                           SPHR_a2_minus = SPHR_a2_minus,
                           SPHR_a2_plus = SPHR_a2_plus,
                            stringsAsFactors = FALSE)
    
    filtered_data <- rbind(filtered_data, row_data)
  }
  
  return(filtered_data)
}


for (file in all_files) {
  file_name <- tools::file_path_sans_ext(basename(file))
  suffix <- gsub("^(.*?)_positive_ALLELE$", "\\1", file_name)
  pos <- ranges_list[[suffix]]
  data <- read.csv(file)
  filtered_data <- filter_data(pos, data)
  output_file <- paste0(file_path, file_name, "_PHR.csv")
  write.csv(filtered_data, file = output_file)
}

## versión con 2 stutters############

# filter_data <- function(sam, data) {
#   unique_samples <- unique(data[c("Sample.Name", "Marker")])
#   
#   filtered_data <- data.frame(Sample.Name = character(),
#                               Marker = character(),
#                               a1 = numeric(),
#                               a1_Height = numeric(),
#                               a2 = numeric(),
#                               a2_Height = numeric(),
#                               a1_stutter_minus = numeric(),
#                               a1_stutter_minus_Height = numeric(),
#                               a1_stutter_plus = numeric(),
#                               a1_stutter_plus_Height = numeric(),
#                               a1_stutter_minus2 = numeric(),
#                               a1_stutter_minus2_Height = numeric(),
#                               a1_stutter_plus2 = numeric(),
#                               a1_stutter_plus2_Height = numeric(),
#                               a2_stutter_minus = numeric(),
#                               a2_stutter_minus_Height = numeric(),
#                               a2_stutter_plus = numeric(),
#                               a2_stutter_plus_Height = numeric(),
#                               a2_stutter_minus2 = numeric(),
#                               a2_stutter_minus2_Height = numeric(),
#                               a2_stutter_plus2 = numeric(),
#                               a2_stutter_plus2_Height = numeric(),
#                               PHR = numeric(),
#                               stringsAsFactors = FALSE)
#   
#   for (i in 1:nrow(unique_samples)) {
#     sample_marker <- unique_samples[i, ]
#     sample_name <- sample_marker$Sample.Name
#     marker <- sample_marker$Marker
#     sample_data <- data[data$Sample.Name == sample_name & data$Marker == marker, ]
#     alleles <- unique(sample_data$Allele)
#     
#     range_vals <- sam[[marker]]
#     allele_data <- list(a1 = NA, a1_Height = 0, a2 = NA, a2_Height = 0,
#                         a1_stutter_minus = NA, a1_stutter_minus_Height = 0,
#                         a1_stutter_plus = NA, a1_stutter_plus_Height = 0,
#                         a1_stutter_minus2 = NA, a1_stutter_minus2_Height = 0,
#                         a1_stutter_plus2 = NA, a1_stutter_plus2_Height = 0,
#                         a2_stutter_minus = NA, a2_stutter_minus_Height = 0,
#                         a2_stutter_plus = NA, a2_stutter_plus_Height = 0,
#                         a2_stutter_minus2 = NA, a2_stutter_minus2_Height = 0,
#                         a2_stutter_plus2 = NA, a2_stutter_plus2_Height = 0)
#     
#     if (length(range_vals) >= 1) {
#       a1_val <- range_vals[1]
#       a2_val <- ifelse(length(range_vals) == 2, range_vals[2], NA)
#       
#       for (allele in alleles) {
#         filtered_allele <- sample_data[sample_data$Allele == allele, ]
#         
#         # Asignación de a1
#         if (allele == a1_val) {
#           allele_data$a1 <- allele
#           allele_data$a1_Height <- filtered_allele$Height[1]
#         }
#         # Asignación de a2
#         if (!is.na(a2_val) && allele == a2_val) {
#           allele_data$a2 <- allele
#           allele_data$a2_Height <- filtered_allele$Height[1]
#         }
#         # Stutters para a1 (desde -1 hasta -2, +1 hasta +2)
#         if (allele == (a1_val - 1)) {
#           allele_data$a1_stutter_minus <- allele
#           allele_data$a1_stutter_minus_Height <- filtered_allele$Height[1]
#         }
#         if (allele == (a1_val - 2)) {
#           allele_data$a1_stutter_minus2 <- allele
#           allele_data$a1_stutter_minus2_Height <- filtered_allele$Height[1]
#         }
#         if (!is.na(a2_val) && allele == (a1_val + 1) && allele != a2_val) {
#           allele_data$a1_stutter_plus <- allele
#           allele_data$a1_stutter_plus_Height <- filtered_allele$Height[1]
#         }
#         if (!is.na(a2_val) && allele == (a1_val + 2) && allele != a2_val) {
#           allele_data$a1_stutter_plus2 <- allele
#           allele_data$a1_stutter_plus2_Height <- filtered_allele$Height[1]
#         }
#         # Stutters para a2 (desde -1 hasta -2, +1 hasta +2)
#         if (!is.na(a2_val) && allele == (a2_val - 1) && allele != a1_val) {
#           allele_data$a2_stutter_minus <- allele
#           allele_data$a2_stutter_minus_Height <- filtered_allele$Height[1]
#         }
#         if (!is.na(a2_val) && allele == (a2_val - 2) && allele != a1_val) {
#           allele_data$a2_stutter_minus2 <- allele
#           allele_data$a2_stutter_minus2_Height <- filtered_allele$Height[1]
#         }
#         if (!is.na(a2_val) && allele == (a2_val + 1)) {
#           allele_data$a2_stutter_plus <- allele
#           allele_data$a2_stutter_plus_Height <- filtered_allele$Height[1]
#         }
#         if (!is.na(a2_val) && allele == (a2_val + 2)) {
#           allele_data$a2_stutter_plus2 <- allele
#           allele_data$a2_stutter_plus2_Height <- filtered_allele$Height[1]
#         }
#       }
#     }
#     
#     # Calcular PHR
#     PHR_value <- if (!is.na(allele_data$a1_Height) && !is.na(allele_data$a2_Height) && allele_data$a1_Height > allele_data$a2_Height) {
#       allele_data$a2_Height / allele_data$a1_Height
#     } else if (!is.na(allele_data$a1_Height) && !is.na(allele_data$a2_Height)) {
#       allele_data$a1_Height / allele_data$a2_Height
#     } else {
#       NA
#     }
#     
#     row_data <- data.frame(Sample.Name = sample_name,
#                            Marker = marker,
#                            a1 = allele_data$a1,
#                            a1_Height = allele_data$a1_Height,
#                            a2 = allele_data$a2,
#                            a2_Height = allele_data$a2_Height,
#                            a1_stutter_minus = allele_data$a1_stutter_minus,
#                            a1_stutter_minus_Height = allele_data$a1_stutter_minus_Height,
#                            a1_stutter_plus = allele_data$a1_stutter_plus,
#                            a1_stutter_plus_Height = allele_data$a1_stutter_plus_Height,
#                            a1_stutter_minus2 = allele_data$a1_stutter_minus2,
#                            a1_stutter_minus2_Height = allele_data$a1_stutter_minus2_Height,
#                            a1_stutter_plus2 = allele_data$a1_stutter_plus2,
#                            a1_stutter_plus2_Height = allele_data$a1_stutter_plus2_Height,
#                            a2_stutter_minus = allele_data$a2_stutter_minus,
#                            a2_stutter_minus_Height = allele_data$a2_stutter_minus_Height,
#                            a2_stutter_plus = allele_data$a2_stutter_plus,
#                            a2_stutter_plus_Height = allele_data$a2_stutter_plus_Height,
#                            a2_stutter_minus2 = allele_data$a2_stutter_minus2,
#                            a2_stutter_minus2_Height = allele_data$a2_stutter_minus2_Height,
#                            a2_stutter_plus2 = allele_data$a2_stutter_plus2,
#                            a2_stutter_plus2_Height = allele_data$a2_stutter_plus2_Height,
#                            PHR = PHR_value,
#                            stringsAsFactors = FALSE)
#     
#     filtered_data <- rbind(filtered_data, row_data)
#   }
#   
#   return(filtered_data)
# }
