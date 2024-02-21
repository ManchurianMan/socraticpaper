## NOTE: For some reason this only works as a background job. I don't know why that is.

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

count_occurrences <- function(banter, pattern) {
  str_count(banter, pattern)
}

remove_duplicates_and_add_lecture_quotient <- function(df) {
  df$duplicate <- ave(df$names, df$names, FUN = function(x) c(0, diff(as.integer(x == lag(x)))))
  duplicate_indices <- which(df$duplicate == 1)
  
  for (i in duplicate_indices) {
    cols_to_sum <- c("ge_present", "word_count", "dh_present", "non_idiom_ge", "panu_ge", "alla_present",
                     "araq_present", "arac_present", "gar_present", "au_present", "oun_present", "mhn_present",
                     "pou_present", "kaitoi_present", "mentoi_present", "oukoun_present", "per_present", "toinun_present", "men_present")
    for (col in cols_to_sum) {
      df[[col]][i+1] <- df[[col]][i] + df[[col]][i+1]
    }
  }
  
  df <- df[duplicate_indices + 1, ]
  df$lecture_quotient <- (df$word_count - df$response)^2
  
  return(df)
}

greek_to_csv <- function(path) {
  # Read and preprocess XML
  tlg <- read_xml(path) %>% xml_ns_strip()
  xml_remove(xml_find_all(tlg, "//revisionDesc"))
  
  banter <- xml_text(xml_find_all(tlg, "//said[@who]"))
  names <- xml_text(xml_find_all(tlg, "//@who"))
  title <- rep(xml_text(xml_find_all(tlg, "//titleStmt/title")), length(banter))
  
  # Process banter
  banter_length <- count_occurrences(banter, "\\w+")
  response <- c(banter_length[-1], 0) # Shift banter length and append 0 for last
  
  patterns <- list("γε" = "ge", "πάνυ γε" = "panu_ge", "εὐ γε" = "eu_ge", "δὴ" = "dh",
                   "ἀλλά" = "alla", "ἀρα" = "ara", "γαρ" = "gar", "αὐ" = "au", "οὖν" = "oun",
                   "μὴν" = "mhn", "ποῦ" = "pou", "καιτοι" = "kaitoi", "μεντοι" = "mentoi",
                   "οὐκοῦν" = "oukoun", "περ" = "per", "τοίνυν" = "toinun", ";" = "question",
                   " μὲν" = "men")
  
  # Dynamically create columns based on patterns
  for (pattern_name in names(patterns)) {
    column_name <- patterns[pattern_name]
    df[[column_name]] <- count_occurrences(banter, pattern_name)
  }
  
  df <- tibble(lines = banter, 
               names = names, 
               word_count = as.numeric(banter_length), 
               work_title = title,
               ge_present = as.numeric(ge),
               response = as.numeric(banter_response),
               dh_present = as.numeric(dh),
               panu_ge = as.numeric(panu_ge),
               non_idiom_ge = ge_present - panu_ge - as.numeric(eu_ge),
               question = as.numeric(question),
               alla_present = as.numeric(alla),
               araq_present = as.numeric(ara_question),
               arac_present = as.numeric(ara_conj),
               gar_present = as.numeric(gar),
               au_present = as.numeric(au),
               oun_present = as.numeric(oun),
               mhn_present = as.numeric(mhn),
               pou_present = as.numeric(pou),
               kaitoi_present = as.numeric(kaitoi),
               mentoi_present = as.numeric(mentoi),
               oukoun_present = as.numeric(oukoun),
               per_present = as.numeric(per),
               toinun_present = as.numeric(toinun),
               men_present = as.numeric(men),
               nontranslatable = dh_present + mhn_present + men_present + non_idiom_ge,
               translatable = gar_present + oun_present + arac_present + per_present,
               lecture_quotient = (word_count-response)^2)
  
  # Process the XML, compute necessary columns, and remove duplicates as before
  df <- remove_duplicates_and_add_lecture_quotient(df)
  
  # Add binary columns
  df$ge_binary <- (df$non_idiom_ge > 0) * 1
  df$dh_binary <- (df$dh_present > 0) * 1
  df$question_binary <- (df$question > 0) * 1
  df$arac_binary <- (df$arac_present > 0) * 1
  df$oun_binary <- (df$oun_present > 0) * 1
  
  # Return the final dataframe
  return(df)
}



# Listing all the relevant dialogues
dialogue_list <- c("Euthyphro","Crito", 
                   "Cratylus", "Theaetetus",  
                   "Sophist",  "Statesman",
                   "Philebus", "Phaedrus", 
                   "Alcibiades 1",  "Alcibiades 2",  
                   "Hipparchus" ,   "Theages",  
                   "Lysis",  "Euthydemus" ,  
                   "Protagoras" , "Gorgias",  
                   "Meno","Hippias Major", 
                   "Hippias Minor", "Ion", 
                   "Menexenus","Cleitophon" ,
                   "Timaeus",  "Critias",  
                   "Minos")

get_title <- function(file_path) {
  tlg <- read_xml(file_path);
  tlg %>% xml_ns_strip();
  
  # Title:
  title <- xml_text(xml_find_all(tlg, xpath = "//titleStmt/title"))
  
  return(title %in% dialogue_list)  
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
  files <- list.files(subdir, pattern = "grc2\\.xml$", full.names = TRUE)
  
  # Loop through each file in the subdirectory
  for (file_path in files) {
    if (get_title(file_path)) {
    # Apply your function to each file
    df <- greek_to_csv(file_path)
    # Append the result to the list
    df_list[[length(df_list) + 1]] <- df}
  }
}

socratic_dialogues <- bind_rows(df_list)


# whether these chosen variables have non-zero observations: 

# Zooming in to look at only Socrates:
socrates_only <- socratic_dialogues[socratic_dialogues$names == "#Σωκράτης",]

# Panning over to look at his counterparts:
not_socrates <- socratic_dialogues[socratic_dialogues$names != "#Σωκράτης",]

# Saving as .rds (subsetting the DFs didn't work in RStudio, where I work, 
#so I made the subsets for each  of these in base R and saved them as an RDS file):

saveRDS(socrates_only, "C:\\Users\\Ben\\Desktop\\socrates_only.rds")
saveRDS(not_socrates, "C:\\Users\\Ben\\Desktop\\not_socrates.rds")
saveRDS(socratic_dialogues, "C:\\Users\\Ben\\Desktop\\socratic_dialogues.rds")