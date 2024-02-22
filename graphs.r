library(forcats)
library(viridis)
library(clipr)
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

# source(file = "C:/Users/Ben/ling_soc/df_encoding_test.r", encoding = "UTF-8")
# # Subsetting the DFs didn't work in RStudio, where I work, so I made the subsets for each of these in base R and saved them as an RDS file. Here's the code I used for that: 
not_socrates <- read_rds ("C:\\Users\\Ben\\Desktop\\not_socrates.rds")
socrates_only <- read_rds("C:\\Users\\Ben\\Desktop\\socrates_only.rds")
socratic_dialogues <- read_rds("C:\\Users\\Ben\\Desktop\\socratic_dialogues.rds")



# Making the graphs: 

g1 <- socratic_dialogues %>%
  group_by(names, work_title) %>%
  summarize(words_per_dialogue = sum(word_count)) %>%
  ungroup() %>%
  group_by(work_title) %>%
  mutate(percent = words_per_dialogue/sum(words_per_dialogue),
         total_words = sum(words_per_dialogue)) %>%
  ungroup() %>%
  group_by(names) %>%
  summarize(mean_percent = mean(percent)) %>%
  arrange(order(names))%>%
  cbind(english_names) %>%
  filter(mean_percent > 0.01) %>%
  mutate(as.factor(names)) %>%
  mutate(english_names = fct_reorder(english_names, mean_percent)) %>%
  ggplot(aes(x=english_names, y=mean_percent, fill= english_names)) + 
  geom_bar(stat="identity")+
  theme(legend.position = "none")+
  labs(
    x= "",
    y = ""
  ) +
  scale_y_continuous(labels = scales::percent, limits= c(0,1.03), expand = c(0,0)) +
  scale_fill_viridis(discrete = TRUE, option = "mako", direction = -1) +
  coord_flip() +
  theme(panel.border = element_blank())

g1
ggsave(g1, filename = "testplswork.png", path = "C:/Users/Ben/Pictures/Graphs", dpi = 300, type = "cairo",
       width = 7, height = 9.5, units = "in")
