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

removing_duplicates_adding_lecture_quotient <- function(df) {
  df$duplicate <- 0
  for (i in 2:nrow(df)) {
    if (df$names[i] == df$names[i-1]) {
      df$duplicate[i-1] <- 1
      df$ge_present[i] <- df$ge_present[i] + df$ge_present[i-1]
      df$word_count[i] <- df$word_count[i] + df$word_count[i-1]
      df$dh_present[i] <- df$dh_present[i] + df$dh_present[i-1]
      df$non_idiom_ge[i] <- df$non_idiom_ge[i] + df$non_idiom_ge[i-1]
      df$panu_ge[i] <- df$panu_ge[i] + df$panu_ge[i-1]
      df$alla_present[i] <- df$alla_present[i] + df$alla_present[i-1]
      df$araq_present[i] <- df$araq_present[i] + df$araq_present[i-1]
      df$arac_present[i] <- df$arac_present[i] + df$arac_present[i-1]
      df$gar_present[i] <- df$gar_present[i] + df$gar_present[i-1]
      df$au_present[i] <- df$au_present[i] + df$au_present[i-1]
      df$oun_present[i] <- df$oun_present[i] + df$oun_present[i-1]
      df$mhn_present[i] <- df$mhn_present[i] + df$mhn_present[i-1]
      df$pou_present[i] <- df$pou_present[i] + df$pou_present[i-1]
      df$kaitoi_present[i] <- df$kaitoi_present[i] + df$kaitoi_present[i-1]
      df$mentoi_present[i] <- df$mentoi_present[i] + df$mentoi_present[i-1]
      df$oukoun_present[i] <- df$oukoun_present[i] + df$oukoun_present[i-1]
      df$per_present[i] <- df$per_present[i] +    df$per_present[i-1]
      df$toinun_present[i] <- df$toinun_present[i] + df$toinun_present[i-1]
      df$men_present[i] <- df$men_present[i] + df$men_present[i-1]
    }
  }
  df_2 <- subset(df, duplicate != 1)
  df_2$lecture_quotient <- (df_2$word_count - df_2$response)^2
  return(df_2)
}

greek_to_csv <- function(path) {
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
  names <- xml_text(xml_find_all(tlg, xpath = "//@who"))
  
  
  # Num. o/ words per banter:
  banter_length <- list()
  for (i in 1:length(banter)) {
    banter_length[i] <- str_count(banter[i], "\\w+")    }
  
  
  # Response per banter:
  banter_response <- list()
  for (i in 1:length(banter_length)) {
    banter_response[i] <- (banter_length[i+1])
  }
  banter_response[length(banter_response)] <- 0
  
  
  # Whether there is a ge:
  ge <- list() 
  for (i in 1:length(banter)) {
    ge[i] <- str_count(banter[i], " \U03B3(\U03b5|\U03AD)[.,;· ]| \u03B3\u02BC")
  }
  
  
  # Panu ge:
  panu_ge <- list()
  for (i in 1:length(banter)) {
    panu_ge[i] <- str_count(banter[i], "\U03C0\U03AC\U03BD\U03C5.\U03B3(\U03b5|\U03AD)")
  }
  
  # eu ge: 
  eu_ge <- list()
  for (i in 1:length(banter)) {
    eu_ge[i] <- str_count(banter[i], "\u03B5\u1F56 \U03B3(\U03b5|\U03AD)[.,;· ]|\u03B5\u1F56 \u03B3\u02BC")
  }
  
  
  # Whether there's a dh:
  dh <- list()
  for (i in 1:length(banter)) {
    dh[i] <- str_count(banter[i], "( \U03B4\U1f74)[.,;· ]|( \U03B4\U03AE)[. ,;·]")
  }
  
  # Whether there is an alla:
  alla <- list()
  for (i in 1:length(banter)) {
    alla[i] <- str_count(banter[i], "((\u1F00|\u1F04)\u03BB\u03BB(\u03AC|\u1F70|\u03B1)[. ,;·])|(\u1F00\u03BB\u03BB\u02BC)")
  }
  
  # Whether there is an ara (question):
  ara_question <- list()
  for (i in 1:length(banter)) {
    ara_question[i] <- str_count(banter[i], "\u1F06\u03C1\u03B1[. ,;·]")
  }
  
  # Whether there is an ara (conjunction):
  ara_conj <- list()
  for (i in 1:length(banter)) {
    ara_conj[i] <- str_count(banter[i], "\u1F04\u03C1\u03B1[. ,;·]")
  }
  
  # Whether there is a gar:
  gar <- list()
  for (i in 1:length(banter)) {
    gar[i] <- str_count(banter[i], "\u03B3(\u03AC|\u1F70)\u03C1[ ,;·.]")
  }
  
  # Whether there is an au:
  au <- list()
  for (i in 1:length(banter)) {
    au[i] <- str_count(banter[i], "\u03B1\u1F56[. ,;·]")
  }
  
  # Whether there is an oun:
  oun <- list()
  for (i in 1:length(banter)) {
    oun[i] <- str_count(banter[i], "\u03BF\u1F56\u03BD[. ,;·]")
  }
  
  # Whether there is a mhn:
  mhn <- list()
  for (i in 1:length(banter)) {
    mhn[i] <- str_count(banter[i], "\u03BC(\u03AE|\u1F74)\u03BD[. ,;·]")
  }
  
  # Whether there is a pou:
  pou <- list()
  for (i in 1:length(banter)) {
    pou[i] <- str_count(banter[i], "\u03C0\u03BF(\u03C5|\u03CD|\u1FE6)[. ,;·]")
  }
  
  # Whether there is a kaitoi:
  kaitoi <- list()
  for (i in 1:length(banter)) {
    kaitoi[i] <- str_count(banter[i], "\u03BA\u03B1\u03AF\u03C4\u03BF\u03B9")
  }
  
  # Whether there is a mentoi:
  mentoi <- list()
  for (i in 1:length(banter)) {
    mentoi[i] <- str_count(banter[i], "\u03BC\u03AD\u03BD\u03C4\u03BF\u03B9")
  }
  
  # Whether there is a oukoun:
  oukoun <- list()
  for (i in 1:length(banter)) {
    oukoun[i] <- str_count(banter[i], "(\u03BF\u1F50\u03BA\u03BF\u1FE6\u03BD)|(\u03BF\u1F54\u03BA\u03BF\u03C5\u03BD)")
  }
  
  # Whether there is a men by itself:
  men <- list()
  for (i in 1:length(banter)) {
    men[i] <- str_count(banter[i], " \u03BC(\u03AD|\u1F72)\u03BD[ ,;·.]") -
      str_count(banter[i], " \u03BC(\u03AD|\u1F72)\u03BD[ ,;·].*(\u03B4(\u03AD|\u1F72))[ ,;·.]") -
      str_count(banter[i], " \u03BC(\u03AD|\u1F72)\u03BD[ ].*\u03BF\u1F56\u03BD[ ,;·.]")
  }
  
  # Whether there is a -per:
  per <- list()
  for (i in 1:length(banter)) {
    per[i] <- str_count(banter[i], "\u03C0\u03B5\u03C1[ ,;·.]")
  }
  
  # Whether there is a toinun:
  toinun <- list()
  for (i in 1:length(banter)) {
    toinun[i] <- str_count(banter[i], "\u03C4\u03BF\u03AF\u03BD\u03C5\u03BD")
  }
  
  # Whether there's a question:
  question <- list()
  for (i in 1:length(banter)) {
    question[i] <- str_count(banter[i], ";")
  }
  
  
  #Turning it all into a DF:
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
               lecture_quotient = (word_count-response)^2,
               ge_binary = (non_idiom_ge > 0)*1,
               dh_binary = (dh_present > 0)*1,
               question_binary = (question > 0)*1,
               arac_binary = (arac_present > 0)*1,
               oun_binary = (oun_present > 0)*1)
  
  df_2 <- removing_duplicates_adding_lecture_quotient(df)
  
  return(df_2)

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