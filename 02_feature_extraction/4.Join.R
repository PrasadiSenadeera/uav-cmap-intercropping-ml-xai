library(readxl)
library(writexl)
library(dplyr)
library(openxlsx)


wd=getwd()
stat_path <- file.path(wd,"results","stats_wheat")

#Join field data to RS data
file1 <- file.path(stat_path, "Combined_spectralData_W.xlsx") #RS data
file2 <- "Z:/transfer/projects/TRIO/Python/Analysing/CombineALL/Insitu_data.xlsx" #field data

df1 <- read_excel(file1, sheet = 1)
df2 <- read_excel(file2, sheet = 1)


col_file1 <- "Feature_ID"
col_file2 <- "ID"


merged_data <- df1 %>%
  full_join(df2, by = c("Feature_ID" = "ID"))


output_file <- file.path(stat_path, "spectral_and_situData_W.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "MergedData")
writeData(wb, "MergedData", merged_data)
saveWorkbook(wb, output_file, overwrite = TRUE)

print(paste("Merged file saved at:", output_file))

############## excel for model biomass

# # Remove rows where "FM Total (g)" is NA
# cleaned_data <- merged_data %>%
#   filter(!is.na(`FM Total (g)`))
# 
# # Reorder columns
# front_columns <- c("ID_Text", "Feature_ID","Field", "Growth_stage")
# cleaned_data <- cleaned_data %>%
#   select(all_of(front_columns), everything())
# 
# 
# # Save as new Excel file
# output_file <- file.path(stat_path, "Biomass.xlsx")
# wb <- createWorkbook()
# addWorksheet(wb, "FilteredData")
# writeData(wb, "FilteredData", cleaned_data)
# saveWorkbook(wb, output_file, overwrite = TRUE)
# 
# print(paste("Filtered file saved at:", output_file))




















