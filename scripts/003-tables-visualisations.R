# UTF-8 lettering, enabling e.g. potential German 'Umlaute'
Sys.setlocale("LC_ALL", "en_US.UTF-8")

#packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidytext, stopwords, foreach, forcats, waffle, gridExtra, directlabels, grid, ggrepel)

df <- read.csv("data/clean_data.csv")

df <- df %>% mutate(date = lubridate::ymd(date))

##########################TOTAL DATASETS
#not used in the article

#data sets in total -- with and without gender

total <- df %>%
  rowwise() %>%
  mutate(gendered = +any(
    str_detect(c_across(title:tags), "frauen|weiblich|geschlecht"), na.rm = TRUE)) %>% #looking for keywords in title, description and tags
  group_by(gendered) %>%
  summarise(n = n()) %>%
  mutate(freq = prop.table(n),
         perc = freq * 100) %>%
  round(2)

#waffle plot, total percentage of plots

total <- c(94, 6)
waffle(total, rows = 10,
       colors = c("#808080", "#FF69B4"),
       title = "Percentage of gendered vs non gendered datasets",
       legend_pos = "none")


#################################
####data sets per year -- with and without gender

####tables - gendered vs non-gendered

year <- df %>%
  rowwise() %>%
  mutate(gendered = +any(
    str_detect(c_across(title:tags), "frauen|weiblich|geschlecht"), na.rm = TRUE)) %>% #looking for keywords in title, description and tags
  mutate(gendered = as.factor(gendered)) %>%
  group_by(gendered, year = lubridate::year(date)) %>%
  summarise(n = n()) %>%
  mutate(freq = prop.table(n),
         cum_sum = cumsum(n),
         cum_sum_00 = cum_sum/1000,
         cum_perc = cum_sum/n *100) %>% #cummulative sum in thousands
  mutate(across(where(is.numeric), round, 2))


perc <- df %>%
  rowwise() %>%
  mutate(gendered = +any(
    str_detect(c_across(title:tags), "frauen|weiblich|geschlecht"), na.rm = TRUE)) %>% #looking for keywords in title, description and tags
  mutate(gendered = as.factor(gendered)) %>%
  group_by(year = lubridate::year(date)) %>%
  mutate(n_year = n()) %>%
  ungroup() %>%
  group_by(gendered, year, n_year) %>%
  summarise(n = n()) %>%
  ungroup()%>%
  complete(year, nesting(gendered), fill = list(n = 0)) %>% #completes the missing years for gendered
  fill(n_year) %>% #fills year from top to bottom
  ungroup() %>%
  group_by(gendered) %>%
  mutate(cum_n = cumsum(n),
         cum_year = cumsum(n_year),
         cum_perc = cum_n / cum_year * 100) %>%
  mutate(across(where(is.numeric), round, 2))

####plots - gendered vs non-gendered


p1 <- year %>%
  mutate(gendered = ifelse(gendered == 1, "Gendered", "Not gendered")) %>%
  ggplot(aes(x = year, y = cum_sum_00, colour = gendered)) +
  geom_point(aes(color = gendered), shape = 15, size = 3) +
  scale_x_continuous(breaks = seq(2013, 2021, 1)) +
  geom_line(aes(color = gendered)) +
  annotate(geom="text", x=2020.5, y=9, label="Gendered",
           color="#800000") +
  annotate(geom="text", x=2019.2, y=20, label="Non-gendered",
           color="#808080") +
  scale_color_manual(values = c("#800000", "#808080")) +
  labs(title = "Cumulative number of gendered and non-gendered data sets per year",
       y = "Cummulative number (thousands)",
       x = "") +
  theme_minimal() +
  geom_hline(yintercept = 0) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

#to add static annotations to plot
y2015 <- grobTree(textGrob("27%", x=0.23,  y=0.89, hjust=0,
                          gp=gpar(col="#800000", fontsize=12)))
y2018 <- grobTree(textGrob("9%", x=0.58,  y=0.86, hjust=0,
                           gp=gpar(col="#800000", fontsize=12)))
y2021 <- grobTree(textGrob("6%", x=0.92,  y=0.84, hjust=0,
                           gp=gpar(col="#800000", fontsize=12)))



p2<- perc %>%
  mutate(gendered = ifelse(gendered == 1, "Gen", "Nge")) %>%
  ggplot(aes(x = year, y = fct_reorder(gendered, desc(cum_perc)))) +
  geom_point(aes(color = gendered, size = cum_perc), shape = 15) +
  scale_x_continuous(breaks = seq(2013, 2021, 1)) +
  scale_size(range = c(.1, 24)) +
  scale_color_manual(values = c("#800000", "#808080")) +
  labs(title = "Ratio of gendered and non-gendered data sets per year",
       y = "Percent",
       x="") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y=element_text(colour="white"),
        axis.text.y=element_text(colour="white")) +
  annotation_custom(y2015) +
  annotation_custom(y2018) +
  annotation_custom(y2021)

fig1 <- grid.arrange(p1, p2, ncol = 1)
fig1

ggsave("outputs/datasets-yearly.png", width = 7, height = 6, dpi = 200, fig1)


#################################
####data sets per topic -- with and without gender

# add columns per topic to check whether topic is present in dataset
groups_df <- df %>%
  mutate(wt = str_detect(groups,"wissenschaft und technologie")) %>%
  mutate(gb = str_detect(groups,"bevoelkerung und gesellschaft")) %>%
  mutate(bks = str_detect(groups,"bildung kultur und sport")) %>%
  mutate(en = str_detect(groups,"energie")) %>%
  mutate(ge = str_detect(groups,"gesundheit")) %>%
  mutate(it = str_detect(groups,"internationale themen")) %>%
  mutate(lfn = str_detect(groups,"landwirtschaft fischerei forstwirtschaft und nahrungsmittel")) %>%
  mutate(ros = str_detect(groups,"regierung und oeffentlicher sektor")) %>%
  mutate(rs = str_detect(groups,"regionen und staedte")) %>%
  mutate(um = str_detect(groups,"umwelt")) %>%
  mutate(ve = str_detect(groups,"verkehr")) %>%
  mutate(wf = str_detect(groups,"wirtschaft und finanzen")) %>%
  mutate(jros = str_detect(groups,"justiz rechtssystem und oeffentliche sicherheit"))
  
# transform logical to numeric
groups_df[,7:19] <- lapply(groups_df[,7:19], as.numeric)

#data set containg a variable "gendered" that shows whether or not the data set has gender
gendered_groups <- groups_df %>%
  rowwise() %>%
  mutate(gendered = +any(
    str_detect(c_across(title:tags), "frauen|weiblich|geschlecht"), na.rm = TRUE)) %>% #looking for keywords in title, description and tags
  ungroup()

#calculating percentage of gendered and non-gendered datasets per topic
gendered_topics <- foreach(i = 1:ncol(gendered_groups[,7:19]), .combine = rbind) %do% {
  gendered_groups %>%
    filter(!!as.symbol(names(gendered_groups[, 6+i])) == 1) %>%
    group_by(gendered) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n),
           perc = freq * 100) %>%
    mutate(across(.cols = freq:perc, .fns = ~ round(., 2))) %>%
    mutate(topic = names(gendered_groups[, 6+i])) %>%
    relocate(topic)
}

#peek into the table
gendered_topics

#list of topics sorted by percentage in gendered
list <- gendered_topics %>% filter(gendered == 1) %>% arrange(desc(perc)) %>% pull(topic) #ordered list of topics by perc in gedered datasets


#extracting percentages of gendered and non-gendered data sets per topic to plot 
x <- list()

rm(i)
for (i in 1:length(list)) {
  perc <- gendered_topics %>%
    filter(topic == list[i]) %>%
    pull(perc) %>%
    round(0)
  x[[i]] <- perc
}

#creating a vector of titles for plots
title_list <- c("Health", "Justice", "Society", "Science", "Education", "Government", "Economy", "Regions", 
               "Transport", "Energy", "Agriculture", "Environment" )

#plotting waffle plots in a loop for all topics
rm(i)
for (i in 1:length(x)){
  name = paste0("w", i)
  chart <- waffle(x[[i]], rows = 10,
                 colors = c("#808080", "#800000"),
                 title = title_list[i],
                 legend_pos = "none")
  assign(name, chart)
}

#combining plots in a grid
fig_1 <- grid.arrange(w1, w2, w3, w4, ncol = 4)
fig_2 <- grid.arrange(w5, w6, w7, w8, ncol = 4)
fig_3 <- grid.arrange(w9, w10, w11, w12, ncol = 4)

fig2 <- grid.arrange(fig_1, fig_2, fig_3,
                     top = textGrob("Ratio of gendered datasets per topic\n",
                                    x = 0,
                                    just = "left",
                                    gp = gpar(fontsize = 18)))
fig2

#saving plot
ggsave("outputs/datasets-topics.png", width = 7, height = 6, dpi = 200, fig2)



###############################EXPLORING WORDS THAT SEPARATE TOPICS
#not used in the article

#which topic are most common
gendered_topics %>%
  filter(gendered == 1) %>%
  arrange(desc(perc))

#importing german stop words
stop_german <- data.frame(word = stopwords::stopwords("de"), stringsAsFactors = FALSE)

#cleaning dataset for topic models - removing gendered words, as we know that these separate the groups and removing numbers
topic_analysis <- gendered_groups %>%
  mutate(description = str_remove_all(description, "frauen|weiblich|geschlecht")) %>%
  mutate(description = str_remove_all(description, "[:digit:]"))
    

##getting a dataframe with words that distinguish gendered and non gendered datasets per most common topics 

list <- c("ge", "jros", "gb", "wt") #character vector of topics to extract

gendered_words <- foreach(i = 1:length(list), .combine = rbind) %do% {
  topic_analysis %>%
    filter(!!as.symbol(list[i]) == 1) %>% #filters for topic
    mutate(gendered = as.factor(gendered)) %>%
    unnest_tokens(word, description) %>% #unnesting words in descriptions
    count(gendered, word, sort = TRUE) %>% #counts word frequencies
    anti_join(stop_german) %>% #removes stop words
    bind_tf_idf(word, gendered, n) %>% #applies tf_idf function to calculate distinctiveness of words
    mutate(topic = list[i]) %>% #adds a column with topic name
    relocate(topic) #moves topic column to the front
} 

#words that separate gendered and non gendered data sets for health
gendered_words %>%
  filter(topic == "wt") %>% #filters for topic
  group_by(gendered) %>%
  slice_max(tf_idf, n = 15) %>% #gets the 15 most distinctive words
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = gendered)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic + gendered, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

#words that separate gendered and non gendered data sets for justice
gendered_words %>%
  filter(topic == "jros") %>% #filters for topic
  group_by(gendered) %>%
  slice_max(tf_idf, n = 15) %>% #gets the 15 most distinctive words
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = gendered)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic + gendered, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

#words that separate gendered and non gendered data sets for society
gendered_words %>%
  filter(topic == "gb") %>% #filters for topic
  group_by(gendered) %>%
  slice_max(tf_idf, n = 15) %>% #gets the 15 most distinctive words
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = gendered)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic + gendered, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

#words that separate gendered and non gendered data sets for science
gendered_words %>%
  filter(topic == "wt") %>% #filters for topic
  group_by(gendered) %>%
  slice_max(tf_idf, n = 15) %>% #gets the 15 most distinctive words
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = gendered)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic + gendered, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)