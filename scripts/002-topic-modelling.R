##### data analysis ######

# Set working directory to source file location
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

# UTF-8 lettering, enabling e.g. potential German 'Umlaute'
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# install necessary packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,topicmodels,quanteda,kableExtra,gridExtra,webshot)

# load dataset
clean_df <- read_csv("../data/clean_data.csv")

groups_df <- clean_df %>%
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

##### prepare data for topic analysis #####

## 1 topic model compare all gender non-containing datasets with all gender containing datasets
# add column to check whether datasets contains gender-related words
full_df <- groups_df %>%
  rowwise() %>%
  mutate(gendered = +any(
    str_detect(c_across(title:tags), "frauen|weiblich|geschlecht"), na.rm = TRUE))


# divide datasets

full_gender <- subset(full_df, gendered == 1, select = c(1:6))
full_gender <- full_gender[,2:6]
full_gender <- tibble::rowid_to_column(full_gender, "id")

full_nongender <- subset(full_df, gendered == 0, select = c(1:6))
full_nongender <- full_nongender[,2:6]
full_nongender <- tibble::rowid_to_column(full_nongender, "id")


# reduce datasets to id and description
gender_tpm1_df <- full_gender[,c("id","description")]
gender_tpm1_df <- gender_tpm1_df %>%
  mutate(description = str_remove_all(description,"frauen|weiblich|geschlecht"))
         
nongender_tpm1_df <- full_nongender[,c("id","description")]


# transform both  into quanteda corpora
gender_tpm1_corp <- corpus(gender_tpm1_df,docid_field = "id",text_field = "description")
nongender_tpm1_corp <- corpus(nongender_tpm1_df,docid_field = "id",text_field = "description")


# tokenize the corpus
gender_tpm1_tokens <- tokens(gender_tpm1_corp)
nongender_tpm1_tokens <- tokens(nongender_tpm1_corp)


# transform tokens object into document feature matrix
gender_tpm1_dfm <- dfm(gender_tpm1_tokens,remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = stopwords("german"))
nongender_tpm1_dfm <- dfm(nongender_tpm1_tokens,remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = stopwords("german"))


# set the number of themes
number.themes <- 10


# transform dfm into form that is suitable for topicmodel package
gender_tpm1 <- convert(gender_tpm1_dfm, to = "topicmodels")
nongender_tpm1 <- convert(nongender_tpm1_dfm, to = "topicmodels")


# perform the LDA
gender_lda.model <- LDA(gender_tpm1, number.themes)
nongender_lda.model <- LDA(nongender_tpm1, number.themes)

# show 10 most related words per topic in a dataframe
results_gender_lda <- as.data.frame(terms(gender_lda.model, 10))
results_nongender_lda <- as.data.frame(terms(nongender_lda.model, 10))


##### topic model 2 inside group gesundheit - health ######

# divide datasets

ges_gender <- subset(full_df, gendered == 1 & ge == 1 , select = c(1:6))
ges_gender <- ges_gender[,2:6]
ges_gender <- tibble::rowid_to_column(ges_gender, "id")

ges_nongender <- subset(full_df, gendered == 0 & ge == 1, select = c(1:6))
ges_nongender <- ges_nongender[,2:6]
ges_nongender <- tibble::rowid_to_column(ges_nongender, "id")


# reduce datasets to id and description
gender_tpm1_df <- ges_gender[,c("id","description")]
gender_tpm1_df <- gender_tpm1_df %>%
  mutate(description = str_remove_all(description,"frauen|weiblich|geschlecht")) %>%
  mutate(description = str_remove_all(description,"fuer"))

nongender_tpm1_df <- ges_nongender[,c("id","description")]
nongender_tpm1_df <- nongender_tpm1_df %>%
  mutate(description = str_remove_all(description,"fuer"))


# transform both  into quanteda corpora
gender_tpm1_corp <- corpus(gender_tpm1_df,docid_field = "id",text_field = "description")
nongender_tpm1_corp <- corpus(nongender_tpm1_df,docid_field = "id",text_field = "description")


# tokenize the corpus
gender_tpm1_tokens <- tokens(gender_tpm1_corp)
nongender_tpm1_tokens <- tokens(nongender_tpm1_corp)


# transform tokens object into document feature matrix
gender_tpm1_dfm <- dfm(gender_tpm1_tokens,remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = stopwords("german"))
nongender_tpm1_dfm <- dfm(nongender_tpm1_tokens,remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = stopwords("german"))


# set the number of themes
number.themes <- 10


# transform dfm into form that is suitable for topicmodel package
gender_tpm1 <- convert(gender_tpm1_dfm, to = "topicmodels")
nongender_tpm1 <- convert(nongender_tpm1_dfm, to = "topicmodels")


# perform the LDA
set.seed(0)
gender_lda.model <- LDA(gender_tpm1, number.themes, control = list(seed=0))
nongender_lda.model <- LDA(nongender_tpm1, number.themes, control = list(seed=0))

# show 10 most related words per topic in a dataframe
results_genderges_lda <- as.data.frame(terms(gender_lda.model, 10))
results_nongenderges_lda <- as.data.frame(terms(nongender_lda.model, 10))

##### topic model 3 inside group "bevoelkerung und gesellschaft" - society ######

# divide datasets

bg_gender <- subset(full_df, gendered == 1 & gb == 1 , select = c(1:6))
bg_gender <- bg_gender[,2:6]
bg_gender <- tibble::rowid_to_column(bg_gender, "id")

bg_nongender <- subset(full_df, gendered == 0 & gb == 1, select = c(1:6))
bg_nongender <- bg_nongender[,2:6]
bg_nongender <- tibble::rowid_to_column(bg_nongender, "id")


# reduce datasets to id and description
genderbg_tpm1_df <- bg_gender[,c("id","description")]
genderbg_tpm1_df <- genderbg_tpm1_df %>%
  mutate(description = str_remove_all(description,"frauen|weiblich|geschlecht")) %>%
  

nongenderbg_tpm1_df <- bg_nongender[,c("id","description")]


# transform both  into quanteda corpora
genderbg_tpm1_corp <- corpus(genderbg_tpm1_df,docid_field = "id",text_field = "description")
nongenderbg_tpm1_corp <- corpus(nongenderbg_tpm1_df,docid_field = "id",text_field = "description")


# tokenize the corpus
genderbg_tpm1_tokens <- tokens(genderbg_tpm1_corp)
nongenderbg_tpm1_tokens <- tokens(nongenderbg_tpm1_corp)


# transform tokens object into document feature matrix
genderbg_tpm1_dfm <- dfm(genderbg_tpm1_tokens,remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = stopwords("german"))
nongenderbg_tpm1_dfm <- dfm(nongenderbg_tpm1_tokens,remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = stopwords("german"))


# transform dfm into form that is suitable for topicmodel package
genderbg_tpm1 <- convert(genderbg_tpm1_dfm, to = "topicmodels")
nongenderbg_tpm1 <- convert(nongenderbg_tpm1_dfm, to = "topicmodels")


# perform the LDA
genderbg_lda.model <- LDA(genderbg_tpm1, number.themes)
nongenderbg_lda.model <- LDA(nongenderbg_tpm1, number.themes)

# show 10 most related words per topic in a dataframe
results_genderbg_lda <- as.data.frame(terms(genderbg_lda.model, 10))
results_nongenderbg_lda <- as.data.frame(terms(nongenderbg_lda.model, 10))

##### topic model 4 inside group "justiz rechtssystem und oeffentliche sicherheit" - judiciary and public security ######

# divide datasets

jros_gender <- subset(full_df, gendered == 1 & jros == 1 , select = c(1:6))
jros_gender <- jros_gender[,2:6]
jros_gender <- tibble::rowid_to_column(jros_gender, "id")

jros_nongender <- subset(full_df, gendered == 0 & jros == 1, select = c(1:6))
jros_nongender <- jros_nongender[,2:6]
jros_nongender <- tibble::rowid_to_column(jros_nongender, "id")


# reduce datasets to id and description
genderjros_tpm1_df <- jros_gender[,c("id","description")]
genderjros_tpm1_df <- genderjros_tpm1_df %>%
  mutate(description = str_remove_all(description,"frauen|weiblich|geschlecht"))

nongenderjros_tpm1_df <- jros_nongender[,c("id","description")]


# transform both  into quanteda corpora
genderjros_tpm1_corp <- corpus(genderjros_tpm1_df,docid_field = "id",text_field = "description")
nongenderjros_tpm1_corp <- corpus(nongenderjros_tpm1_df,docid_field = "id",text_field = "description")


# tokenize the corpus
genderjros_tpm1_tokens <- tokens(genderjros_tpm1_corp)
nongenderjros_tpm1_tokens <- tokens(nongenderjros_tpm1_corp)


# transform tokens object into document feature matrix
genderjros_tpm1_dfm <- dfm(genderjros_tpm1_tokens,remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = stopwords("german"))
nongenderjros_tpm1_dfm <- dfm(nongenderjros_tpm1_tokens,remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = stopwords("german"))

# transform dfm into form that is suitable for topicmodel package
genderjros_tpm1 <- convert(genderjros_tpm1_dfm, to = "topicmodels")
nongenderjros_tpm1 <- convert(nongenderjros_tpm1_dfm, to = "topicmodels")


# perform the LDA
genderjros_lda.model <- LDA(genderjros_tpm1, number.themes)
nongenderjros_lda.model <- LDA(nongenderjros_tpm1, number.themes)

# show 10 most related words per topic in a dataframe
jrosresults_gender_lda <- as.data.frame(terms(genderjros_lda.model, 10))
jrosresults_nongender_lda <- as.data.frame(terms(nongenderjros_lda.model, 10))

##### topic model 5 inside group "wissenschaft und technologie" - science and technology ######

# divide datasets

wt_gender <- subset(full_df, gendered == 1 & wt == 1 , select = c(1:6))
wt_gender <- wt_gender[,2:6]
wt_gender <- tibble::rowid_to_column(wt_gender, "id")

wt_nongender <- subset(full_df, gendered == 0 & wt == 1, select = c(1:6))
wt_nongender <- wt_nongender[,2:6]
wt_nongender <- tibble::rowid_to_column(wt_nongender, "id")


# reduce datasets to id and description
genderwt_tpm1_df <- wt_gender[,c("id","description")]
genderwt_tpm1_df <- genderwt_tpm1_df %>%
  mutate(description = str_remove_all(description,"frauen|weiblich|geschlecht"))

nongenderwt_tpm1_df <- wt_nongender[,c("id","description")]


# transform both  into quanteda corpora
genderwt_tpm1_corp <- corpus(genderwt_tpm1_df,docid_field = "id",text_field = "description")
nongenderwt_tpm1_corp <- corpus(nongenderwt_tpm1_df,docid_field = "id",text_field = "description")


# tokenize the corpus
genderwt_tpm1_tokens <- tokens(genderwt_tpm1_corp)
nongenderwt_tpm1_tokens <- tokens(nongenderwt_tpm1_corp)


# transform tokens object into document feature matrix
genderwt_tpm1_dfm <- dfm(genderwt_tpm1_tokens,remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = stopwords("german"))
nongenderwt_tpm1_dfm <- dfm(nongenderwt_tpm1_tokens,remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = stopwords("german"))


# transform dfm into form that is suitable for topicmodel package
genderwt_tpm1 <- convert(genderwt_tpm1_dfm, to = "topicmodels")
nongenderwt_tpm1 <- convert(nongenderwt_tpm1_dfm, to = "topicmodels")


# perform the LDA
genderwt_lda.model <- LDA(genderwt_tpm1, number.themes)
nongenderwt_lda.model <- LDA(nongenderwt_tpm1, number.themes)

# show 10 most related words per topic in a dataframe
wtresults_gender_lda <- as.data.frame(terms(genderwt_lda.model, 10))
wtresults_nongender_lda <- as.data.frame(terms(nongenderwt_lda.model, 10))



##### visualize topicmodel results health #####

# subset interesting topics
genderges_lda_vis <- results_genderges_lda[,c(1,6,7)]
nongenderges_lda_vis <- results_nongenderges_lda[,c(3,5,7)]

# export the subsetted data
write.csv(genderges_lda_vis,paste0(getwd(),"/outputs/table_1_gender_ges.csv"), row.names = FALSE)
write.csv(nongenderges_lda_vis,paste0(getwd(),"/outputs/table_2_nongender_ges.csv"), row.names = FALSE)


# visualize datasets that contain gender
table1 <- genderges_lda_vis %>%
  kbl() %>%
  kable_minimal(bootstrap_options = "condensed",font_size = 10, position = "center",full_width = FALSE) %>%
  #kable_styling(bootstrap_options = "condensed",font_size = 10) %>%
  add_header_above(c("Selection out of Ten: Topics Modelled \n for the Healh Datasets That Include Gender" = 3), color = "#808080") %>%
  row_spec(0,color = "#FF69B4") %>%
  row_spec(c(1:10),color = "#808080") 


# visualize datasets that not contain gender
table2 <- nongenderges_lda_vis %>%
  kbl() %>%
  kable_minimal(bootstrap_options = "condensed",font_size = 10,full_width = FALSE) %>%
  #kable_styling(bootstrap_options = "condensed",font_size = 10) %>%
  add_header_above(c("Selection out of Ten: Topics Modelled \n for the Healh Datasets That Do Not Include Gender" = 3), color = "#808080") %>%
  row_spec(c(0:10),color = "#808080")


##translated tables

#creating a table for gendered data and saving it
g_topic1 <- c("numbers", "cases", "registered ('gemeldeten')", "registered ('gemeldete')", "data", "Corona virus", "communicated", "fields", "field separator", "dataset")
g_topic6 <- c("care/nursing", "own", "living", "dataset", "satisfaction", "living situation", "help", "experiences", "consultation", "following")
g_topic7 <- c("districts", "cities", "district-free", "male", "selected", "year", "causes of death", "specialist departments", "died", "regional")

table_gendered <- as_tibble(cbind(g_topic1, g_topic6, g_topic7))
table_gendered <- rename(table_gendered, "Topic 1" = g_topic1, "Topic 6" = g_topic6, "Topic 7" = g_topic7)
write.csv(table_gendered,"../outputs/table_1_gender_ges_eng.csv", row.names = FALSE)

#non-gendered
n_topic3 <- c("Corona virus", "Germany", "inhabitant", "Schleswig-Holstein", "https", "per", "www.schleswig-holstein.de", "name", "(of the) district", "focus areas")
n_topic5 <- c("district", "Kleve (city)", "quantity", "Aachen (city)", "pharmacies", "cities region", "people", "register", "list", "Wesel (city)")
n_topic7 <- c("data", "of", "liability", "noise exposure", "noise mapping", "metropolitan areas", "mapped", "strategic", "dataset", "http")

table_nongen <- as_tibble(cbind(n_topic3, n_topic5, n_topic7))
table_nongen <- rename(table_nongen, "Topic 3" = n_topic3, "Topic 5" = n_topic5, "Topic 7" = n_topic7)
write.csv(table_nongen,"../outputs/table_2_nongender_ges_eng.csv", row.names = FALSE)

