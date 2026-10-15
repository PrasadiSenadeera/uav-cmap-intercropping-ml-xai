library(readxl)
library(writexl)
library(dplyr)
library(openxlsx)


wd=getwd()
stat_path <- file.path(wd,"results","stats","wheat")

#Join CHM data to RS data
file1 <- file.path(stat_path, "chm","DFH_ZE3_CHM_statistics.xlsx") #CHM data
file2 <- file.path(stat_path, "spectral", "DFH_ZE3_statistics.xlsx") #RS data

df1 <- read_excel(file1, sheet = 1)
df2 <- read_excel(file2, sheet = 1)


col_file1 <- "FID"
col_file2 <- "FID"


merged_data <- df1 %>%
  full_join(df2, by = c("FID" = "FID"))


output_file <- file.path(stat_path, "join","DFH_ZE3.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "MergedData")
writeData(wb, "MergedData", merged_data)
saveWorkbook(wb, output_file, overwrite = TRUE)

print(paste("Merged file saved at:", output_file))