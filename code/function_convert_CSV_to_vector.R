convertCSV2Vector <- function(folder, csv_file) {
 temp_data <-  read.csv(here(folder, csv_file), header = FALSE)$V1
 return(temp_data <- as.factor(temp_data))
}

z <- convertCSV2Vector("data", "JWTR.csv")