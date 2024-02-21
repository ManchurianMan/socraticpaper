library(tidyverse)
library(xml2)

get_title <- function(file_path) {
  tlg <- read_xml(file_path);
  tlg %>% xml_ns_strip();
  
  # Title:
  titles <- xml_find_all(tlg, xpath = "//titleStmt/title")
  
  if (length(titles) == 0) {
    # No titles found
    title <- 'no title found'
  } else {
    # Extract the text of the first found title
    title <- xml_text(titles[1])
  }
  
  
  return(title)  
}

# Set the main directory where the subdirectories are located
main_dir <- "C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059"

# List all subdirectories
subdirs <- list.dirs(main_dir, full.names = TRUE, recursive = FALSE)

# Initialize an empty list to store data frames
df_list <- list()

# Loop through each subdirectory to process XML files
for (subdir in subdirs) {
  # List all XML files in the current subdirectory
  files <- list.files(subdir, pattern = "\\.xml$", full.names = TRUE)
  
  # Loop through each file in the subdirectory
  for (file_path in files) {
    var1 <- get_title(file_path) # Extract title using your function
    var2 <- file_path
    
    # Combine var1 and var2 into a dataframe for this file
    df <- data.frame(title = var1, filepath = var2)
    
    # Append the dataframe to the list
    df_list[[length(df_list) + 1]] <- df
  }
}

# Combine all the dataframes in the list into one dataframe
all_files <- bind_rows(df_list)
