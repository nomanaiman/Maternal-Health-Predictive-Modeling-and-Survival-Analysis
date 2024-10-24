# List all .sav files in the data folder
files <- list.files(path = "data", pattern = "*.sav", full.names = TRUE)

# Create an empty list to store the data frames
data_list <- list()

# Create an empty data frame to store all column information
all_columns_info <- data.frame(
  File_Name = character(),
  Column_Name = character(),
  Description = character(),
  stringsAsFactors = FALSE
)

# Loop through the files and process each one
for (file in files) {
  # Extract the file name without extension
  data_name <- tools::file_path_sans_ext(basename(file))
  
  # Load the .sav file
  data <- read_sav(file)
  
  # Store the data frame in the list with the file name as the key
  data_list[[data_name]] <- data
  
  # Extract column names and descriptions (variable labels)
  column_names <- colnames(data)
  column_descriptions <- sapply(data, function(x) attr(x, "label"))
  
  # Combine names and descriptions into a data frame
  columns_info <- data.frame(
    File_Name = data_name,
    Column_Name = column_names,
    Description = column_descriptions,
    stringsAsFactors = FALSE
  )
  
  # Append column information to the all_columns_info data frame
  all_columns_info <- rbind(all_columns_info, columns_info)
}

# Save the combined column information to an Excel file
write_xlsx(all_columns_info, "output/all_columns_info.xlsx")

# Load population data from CSV
pop_data <- read.csv("data/population.csv")

# Load Yemen shapefile
yem_shape <- st_read("data/yem_admin_shapes/yem_admbnda_adm1_govyem_cso_20191002.shp")

# Return data frames as a list
list(
  bh_data = data_list[["bh"]],
  ch_data = data_list[["ch"]],
  fs_data = data_list[["fs"]],
  hh_data = data_list[["hh"]],
  hl_data = data_list[["hl"]],
  wm_data = data_list[["wm"]],
  pop_data = pop_data
)
