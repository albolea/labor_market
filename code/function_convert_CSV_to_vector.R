convertCSV2Factor <- function(csv_file, folder = "data") {
  file_name <- paste(csv_file,"csv", sep = ".")
  temp_data <-  read.csv(here(folder, file_name), header = FALSE)$V1
  return(temp_data <- as.factor(temp_data))
}


