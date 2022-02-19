#importing the data

data <- read.csv("data/clean_data.csv")

#installing the necessary libraries

library(stringr)
library(stopwords)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggthemes)

#defining which stopwords to remove

stopwords_regex = paste(stopwords('German'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')

#selecting the necessary column witht he keywords

keywords <- data %>%
  select(tags)

#cleaning the keywords for the analysis

keywords <-  str_replace_all(keywords, "[[:punct:]]", "")
keywords <-  tolower(keywords)

#determining term frequency for the 100 most prominent terms within the keywords column

keyword_freq <- keywords %>%
  strsplit(" ") %>%
  unlist() %>% 
  table() %>% 
  sort(decreasing = TRUE) %>% 
  head(100)

#assigning the filtered words to a dataframe and assigning an index

keyword_freq <- as.data.frame(keyword_freq)  
keyword_freq$index <- 1:nrow(keyword_freq)
names(keyword_freq) <-  c("term", "frequency")

#selecting the 10 illustrative examples within the list of 100

keyword_freq1 <-  keyword_freq[keyword_freq$term %in% c('bebauungsplan','strandbelegung', 'scharbeutz', 'futtermittel','pkw','lkw','bundesstrasse','geschlecht','weiblich', 'covid19'), ]

keyword_freq2 <-  keyword_freq[keyword_freq$term %in% c('bebauungsplan', 'tourismus','bodennutzung', 'futtermittel', 'strandbelegung','bundesstrasse','geschlecht', 'weiblich'), ] #keywords for alternative visualisation

#plotting the 10 selected terms for illustrative purposes in a bar chart

eng <- c("development plan", "beach occupacy", "Scharbeutz (city)", "animal feed", "personal vehicle", "truck", "national road", "gender", "female", "Covid 19" )
keyword_freq1 <- cbind(keyword_freq1, eng)


tf_plot <- ggplot(keyword_freq1, 
       aes(x=frequency, 
           y=reorder(eng, frequency))) +
  geom_point(color=c("#808080","#808080","#808080","#808080", "#808080", "#808080", "#808080","#800000", "#800000","#808080"), 
             size = 3) +
  geom_segment(aes(x = 40, 
                   xend = frequency, 
                   y = reorder(eng, frequency), 
                   yend = reorder(eng, frequency)),
               color = "grey") +
  labs (x = "term frequency",
        y = "",
        title = "Term Frequency on govdata.de",
        subtitle = "Gotta set priorities",
        caption="as from over 55000 datasets") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("../outputs/tf_plot.png", width = 7, height = 6, dpi = 200, tf_plot)



################################################################################
##############Alternative visualisation of words frequency, not used in the text

library(ggrepel)

keywords_alt <- as.tibble(keyword_freq2[0:2])
keywords_alt <- keywords_alt %>% mutate(term = as.character(term),
         color = ifelse(str_detect(term, "geschlecht|weiblich"), "#800000", "#808080")) #marking gendered and nongendered words
  
eng2 <- c("development plan", "tourism", "land use", "animal feed", "beach occupacy", "national road", "gender", "female") #english translation

keywords_alt <- cbind(keywords_alt, eng2)

fig3a <- ggplot(keywords_alt, aes(x = 1.001, y = frequency)) +
  scale_size(range = c(2, 12)) +
  geom_text(aes(size = frequency, label = eng2, color = color),
                   hjust = 0) +
  xlim(1, 1.1) + #setting x-axis linits to fit long words
  scale_color_manual(values = c("#800000", "#808080")) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x=element_text(colour="white"),
        axis.text.x=element_text(colour="white"),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.text.y = element_text(margin = margin(r = 0)),
        ) +
  geom_segment(aes(x = 1.06, y = 1000, xend = 1.02, yend = 1000, color = "#800000"),
               arrow = arrow(length = unit(2, "mm"))) +
  geom_vline(xintercept = 1) +
  labs(title = "Term number in the portal Govdata.de",
       y = "Term number")

ggsave("outputs/tf-alternative.png", width = 7, height = 6, dpi = 200, fig3a)
