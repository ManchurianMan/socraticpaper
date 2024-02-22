# Regressions
library(tidyverse)
library(xml2)
library(tibble)
library(stringr)
library(kableExtra)
library(utf8)
library(stringi)
library(patchwork)
library(jtools)
library(ggplot2)
library(data.table)
library(XML)
library(RColorBrewer)
library(stargazer)

# Reading in the data:

not_socrates <- read_rds("C:\\Users\\Ben\\Desktop\\not_socrates.rds")
socrates_only <- read_rds("C:\\Users\\Ben\\Desktop\\socrates_only.rds")
socratic_dialogues <- read_rds("C:\\Users\\Ben\\Desktop\\socratic_dialogues.rds")


# # # #
# Regressions for just Socrates:
# # # # 

# Continuous:
socrates_lecture <- (lm(lecture_quotient ~ non_idiom_ge + dh_present + oun_present + arac_present + question + word_count, data = socrates_only))

# Binary:
socrates_lecture_binary <- (lm(lecture_quotient ~ ge_binary + dh_binary + oun_binary + arac_binary + question_binary + word_count, data = socrates_only))

socrates_lecture_idioms <- lm(lecture_quotient ~ ge_present + dh_present + oun_present + arac_present + question + word_count, data = socrates_only)


regression_list <- list(socrates_lecture, socrates_lecture_binary, socrates_lecture_idioms)
regression_list_names <- c("Continuous", "Binary", "Idioms included")

regression_comparison <- as.data.frame(matrix(ncol=4, nrow=length(regression_list)))
colnames(regression_comparison) <- c("Socrates", "Adj. R^2", "AIC", "BIC")
for (i in 1:length(regression_list)) {
  regression_comparison[i,1] <- regression_list_names[i]
  regression_comparison[i,2] <- summary(regression_list[[i]])$adj.r.squared
  regression_comparison[i,3] <- AIC(regression_list[[i]])
  regression_comparison[i,4] <- BIC(regression_list[[i]])
  
}


regression_comparison %>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Charis SIL")



# LaTex regression outputs:
stargazer(socrates_lecture_idioms)

# # # # 
# Regressions for the whole corpus:
# # # #

# Continuous:
total_lecture <- (lm(lecture_quotient ~ non_idiom_ge + dh_present + oun_present + arac_present + question + word_count, data = socratic_dialogues))

# Binary:
total_lecture_binary <- (lm(lecture_quotient ~ ge_binary + dh_binary + oun_binary + arac_binary + question_binary + word_count, data = socratic_dialogues))

total_lecture_idioms <- lm(lecture_quotient ~ ge_present + dh_present + oun_present + arac_present + question + word_count, data = socratic_dialogues)


regression_list_total <- list(total_lecture, total_lecture_binary, total_lecture_idioms)
regression_list_names_total <- c("Continuous", "Binary", "Idioms included")

regression_comparison_total <- as.data.frame(matrix(ncol=4, nrow=length(regression_list)))
colnames(regression_comparison_total) <- c("Overall", "Adj. R^2", "AIC", "BIC")
for (i in 1:length(regression_list_total)) {
  regression_comparison_total[i,1] <- regression_list_names_total[i]
  regression_comparison_total[i,2] <- summary(regression_list_total[[i]])$adj.r.squared
  regression_comparison_total[i,3] <- AIC(regression_list_total[[i]])
  regression_comparison_total[i,4] <- BIC(regression_list_total[[i]])
  
}

regression_comparison_total %>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Charis SIL")
summary(socrates_lecture_idioms)
summary(total_lecture)
stargazer(total_lecture)

