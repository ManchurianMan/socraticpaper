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
  names <- xml_text(xml_find_all(tlg, xpath = "//@who"))
  
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

tlg_001 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg001\\tlg0059.tlg001.perseus-eng2.xml")

tlg_003 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg003\\tlg0059.tlg003.perseus-eng2.xml")

tlg_004 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg004\\tlg0059.tlg004.perseus-eng2.xml")

tlg_005 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg005\\tlg0059.tlg005.perseus-eng2.xml")

tlg_006 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg006\\tlg0059.tlg006.perseus-eng2.xml")

tlg_007 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg007\\tlg0059.tlg007.perseus-eng2.xml")

tlg_008 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg008\\tlg0059.tlg008.perseus-eng2.xml")

tlg_010 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg010\\tlg0059.tlg010.perseus-eng2.xml")

tlg_011 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg011\\tlg0059.tlg011.perseus-eng2.xml")

tlg_012 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg012\\tlg0059.tlg012.perseus-eng2.xml")

tlg_013 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg013\\tlg0059.tlg013.perseus-eng2.xml")

tlg_014 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg014\\tlg0059.tlg014.perseus-eng2.xml")

tlg_015 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg015\\tlg0059.tlg015.perseus-eng2.xml")

tlg_017 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg017\\tlg0059.tlg017.perseus-eng2.xml")



tlg_020 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg020\\tlg0059.tlg020.perseus-eng2.xml")

tlg_021 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg021\\tlg0059.tlg021.perseus-eng2.xml")

tlg_022 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg022\\tlg0059.tlg022.perseus-eng2.xml")

tlg_023 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg023\\tlg0059.tlg023.perseus-eng2.xml")

tlg_024 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg024\\tlg0059.tlg024.perseus-eng2.xml")

tlg_025 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg025\\tlg0059.tlg025.perseus-eng2.xml")

tlg_026 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg026\\tlg0059.tlg026.perseus-eng2.xml")

tlg_027 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg027\\tlg0059.tlg027.perseus-eng2.xml")

tlg_028 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg028\\tlg0059.tlg028.perseus-eng2.xml")

tlg_029 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg029\\tlg0059.tlg029.perseus-eng2.xml")

tlg_031 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg031\\tlg0059.tlg031.perseus-eng2.xml")

tlg_032 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg032\\tlg0059.tlg032.perseus-eng2.xml")

tlg_033 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg033\\tlg0059.tlg033.perseus-eng2.xml")

tlg_034 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg034\\tlg0059.tlg034.perseus-eng2.xml")

tlg_035 <- eng_to_csv_take_2("C:\\Users\\Ben\\Documents\\GitHub\\canonical-greekLit\\data\\tlg0059\\tlg035\\tlg0059.tlg035.perseus-eng2.xml")


tlg1to36 <- rbind(tlg_001,tlg_003,tlg_004,tlg_005,tlg_006,tlg_007,tlg_008,
                  tlg_010,tlg_011,tlg_012,tlg_013,tlg_014,tlg_015,tlg_017,
                  tlg_020,tlg_021,tlg_022,tlg_023,tlg_024,tlg_025,tlg_026,
                  tlg_027,tlg_028,tlg_029,tlg_031,tlg_032,tlg_033,tlg_034, tlg_035)



# Looking only at Socratic Dialogues:
dialogue_list <- c("Euthyphro","Crito", "Cratylus", "Theaetetus",  "Sophist",  "Statesman","Philebus", "Phaedrus", "Alcibiades 1",  "Alcibiades 2",  "Hipparchus" ,   "Theages",  "Lysis",  "Euthydemus" ,  "Protagoras" , "Gorgias",  "Meno","Hippias Major", "Hippias Minor", "Ion", "Menexenus","Cleitophon" ,"Timaeus",  "Critias",  "Minos")

filtered_df <- tlg1to36[tlg1to36$work_title %in% dialogue_list,]

eng_titles <- unique(filtered_df$work_title)

socratic_dialogues <- read_rds("C:\\Users\\Ben\\Desktop\\socratic_dialogues.rds") 

greek_titles <- unique(socratic_dialogues$work_title)


# Combine into a data frame


# saveRDS(socratic_dialogues_english, "C:\\Users\\Ben\\Desktop\\socratic_dialogues_english.rds")
