
#Librerias
library(dplyr)
library(writexl)
library(readr)

# save data direction
direccion <- "C:/Users/3500_1/3500_results/"

# Function
filter_and_write_pos_neg <- function(ranges, rearranged_df,filename,row_type, type_data) {
  name<- 'rearranged_df'
  expand_numbers <- function(numbers) {
    expanded_numbers <- c(numbers - 1, numbers, numbers + 1)
    # if you want to expant range for all stutter possible
    #expanded_numbers <- c(numbers - 2, numbers - 1, numbers, numbers + 1, numbers + 2)
    
    # Ensure unique values and sort them
    expanded_numbers <- sort(unique(expanded_numbers))
    
    return(expanded_numbers)
  } 
  # Iterate over each marker in ranges_pos and update the list of numbers
  for (marker in names(ranges)) {
    ranges[[marker]] <- expand_numbers(ranges[[marker]])
    
    #Empty list
    filtered_pos_rows <- list()
    filtered_neg_rows <- list()
    
    for (marker in names(ranges)) {
      # Get the allele range for the current marker
      allele_range <- as.vector(ranges[[marker]])
      # Filter rows of filtered_df for the current marker
      m_rows <- rearranged_df %>% filter(Marker == marker)
      
      for (allele in allele_range) {
        # Filter rows where Allele is within the allele_range
        filtered_rows_pos <- m_rows[m_rows$Allele %in% allele_range, ]
        # Filter rows where Allele is not within the allele_range
        filtered_rows_neg <- m_rows[!(m_rows$Allele %in% allele_range), ]
        
        # Store filtered rows in lists
        filtered_pos_rows[[marker]] <- filtered_rows_pos
        filtered_neg_rows[[marker]] <- filtered_rows_neg
        
        
      }
    }
  }
  # Combine the filtered data frames into one for each category
  pos <- do.call(rbind, filtered_pos_rows)
  neg <- do.call(rbind, filtered_neg_rows)
  
  
  
  # Check if there are any rows left
  if (nrow(pos) > 0) {
    # Write the positive data frames to CSV files
    write.csv(pos, file = paste0(direccion, row_type, "_", type_data, "_ALLELE.csv"), row.names = FALSE)
  }
  
  if (nrow(neg) > 0) {
    # Write the negative data frames to CSV files
    write.csv(neg, file = paste0(direccion, row_type, "_", type_data, "_NOALLELE.csv"), row.names = FALSE)
  }
}


############ con los anteriores archivos, 

data_type<- read.csv(paste0(direccion,"output_rearrange_pos_rows.csv"))

gf_rows <- data_type[grep("GF", data_type$Sample.Name), ]
ppy_rows <- data_type[grep("PPY", data_type$Sample.Name), ]
yfp_rows <- data_type[grepl("YFP", data_type$Sample.Name), ]
ngm_rows <- data_type[grepl("NGM", data_type$Sample.Name), ]
mf_rows <- data_type[grepl("MF", data_type$Sample.Name), ]

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

# Define ranges for each type of row
ranges_list <- list(gf = gf_ranges,mf = mf_ranges,ppy = ppy_ranges,yfp = yfp_ranges,
                    ngm = ngm_ranges)

# Define rows for each type of row
rows_list <- list(gf = gf_rows,mf = mf_rows,ppy = ppy_rows,yfp = yfp_rows,ngm = ngm_rows)

# Iterate over each type of row
for (i in seq_along(rows_list)) {
  
  row_type <- names(rows_list)[i]
  ranges <- ranges_list[[row_type]]
  rows <- rows_list[[row_type]]
  
  # Filter and write positive and negative files
  filter_and_write_pos_neg(ranges, rows,filename,row_type,type_data ='positive')
}
