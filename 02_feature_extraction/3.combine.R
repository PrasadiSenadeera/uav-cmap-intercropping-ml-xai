#can be used to combine all the excel files in a folder to an excel

library(readxl)
library(writexl)
library(dplyr)
library(openxlsx)

wd=getwd()
stat_path <- file.path(wd,"results","stats_wheat")
print(stat_path)
file_list <- list.files(path = stat_path, pattern = "*.xlsx", full.names = TRUE)
data_list <- list()


for (file in file_list) {
  temp_data <- read_excel(file)
  temp_data$Source_File <- basename(file) 
  data_list[[length(data_list) + 1]] <- temp_data
}

combined_data <- bind_rows(data_list) %>%
  distinct(Feature_ID, .keep_all = TRUE) 

output_file <- file.path(stat_path, "Combined_spectralData_W.xlsx")
if (file.exists(output_file)) {
  file.remove(output_file)
}
write_xlsx(combined_data, output_file)


