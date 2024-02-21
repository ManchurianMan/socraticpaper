library(tidyverse)
library(xml2)
library(tibble)
library(stringr)
library(kableExtra)
library(utf8)
library(stringi)
library(patchwork)
library(ggplot2)
library(data.table)
library(XML)

eng_to_csv_take_2 <- function(path) { # modifying the Greek function:
  # Taking in the XML and touching it up:
  tlg <- read_xml(path);
  tlg %>% xml_ns_strip();
  
  xr <- xml_find_all(tlg, xpath = "//revisionDesc")
  xml_remove(xr)
  
  # Banter:
  banter <- xml_text(xml_find_all(tlg, xpath = "//said[@who]"))
  
  
  # Title:
  title <- rep(xml_text(xml_find_all(tlg, xpath = "//titleStmt/title")),
               length(banter))
  
  # Names:
  names <- gsub("Friend","Companion", xml_text(xml_find_all(tlg, xpath = "//@who")))
  
  # 
  #   # Num. o/ words per banter:
  #   banter_length <- list()
  #     for (i in 1:length(banter)) {
  #       banter_length[i] <- str_count(banter[i], "\\w+")    }
  
  
  # Response per banter:
  # banter_response <- list()
  # for (i in 1:length(banter_length)) {
  #   banter_response[i] <- (banter_length[i+1])
  # }
  # banter_response[length(banter_response)] <- 0
  
  # 
  # # Whether there's a question:
  #   question <- list()
  #   for (i in 1:length(banter)) {
  #     question[i] <- str_count(banter[i], "?")
  #   }
  #   
  
  #Turning it all into a DF:
  df <- tibble(lines = banter, 
               names = names, 
               #word_count = as.numeric(banter_length), 
               work_title = title,
               #response = as.numeric(banter_response),
               #question = as.numeric(question)
               #lecture_quotient = (word_count-response)^2
  )
  
  #df_2 <- removing_duplicates_adding_lecture_quotient(df)
  
  return(df)
}


# Looking only at Socratic Dialogues:
dialogue_list <- c("Euthyphro","Crito", "Cratylus", "Theaetetus",  "Sophist",  "Statesman","Philebus", "Phaedrus", "Alcibiades 1",  "Alcibiades 2",  "Hipparchus" ,   "Theages",  "Lysis",  "Euthydemus" ,  "Protagoras" , "Gorgias",  "Meno","Hippias Major", "Hippias Minor", "Ion", "Menexenus","Cleitophon" ,"Timaeus",  "Critias",  "Minos")


get_title <- function(file_path) {
  tlg <- read_xml(file_path);
  tlg %>% xml_ns_strip();
  
  # Title:
  title <- xml_text(xml_find_all(tlg, xpath = "//titleStmt/title"))

  
  return(trimws(title) %in% dialogue_list)  
}
# Set the working directory to where your files are located
main_dir <- "C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059"

# List all files you're interested in. Adjust the pattern as necessary.
subdirs <- list.dirs(main_dir, full.names = TRUE, recursive = FALSE)

# Initialize an empty list to store data frames
df_list <- list()

# Loop through each subdirectory
for (subdir in subdirs) {
  # List all files in the subdirectory, adjust pattern as needed
  files <- list.files(subdir, pattern = "eng2\\.xml$", full.names = TRUE)
  
  # Loop through each file in the subdirectory
  for (file_path in files) {
    if (get_title(file_path)) {
      # Apply your function to each file
      df <- eng_to_csv_take_2(file_path)
      # Append the result to the list
      df_list[[length(df_list) + 1]] <- df}
  }
}


eng_dialogues <- bind_rows(df_list)

pattern <- ".*#.*#"

eng_names <- eng_dialogues$names[!grepl(pattern,eng_dialogues$names)]
eng_names <- gsub("#","", eng_names)
eng_names <- unique(eng_names)


# Use grepl to find items matching the pattern, and negate the result to filter them out

greek_names <- socratic_dialogues$names[!grepl(pattern, socratic_dialogues$names)]
# greek_names <- gsub("#","", greek_names) this is what I'll need, but I want to 
# be able to join this dataframe to the greek one to make things easier
greek_names <- unique(greek_names)

max_length <- max(length(eng_names), length(greek_names))

# Pad the shorter list with NA to match the length of the longer list
eng_names_padded <- c(eng_names, rep(NA, max_length - length(eng_names)))
greek_names_padded <- c(greek_names, rep(NA, max_length - length(greek_names)))

# Create the dataframe
name_comparison <- data.frame(English = eng_names_padded, Greek = greek_names_padded)

# Save the names to be joined later if need be
saveRDS(name_comparison, "C:\\Users\\Ben\\Desktop\\name_comparison.rds")
