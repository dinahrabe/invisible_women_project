# Final data science project

We used metadata in rdf form to analyze the prevalence of gender in 54682 data sets from ['govdata.de'](https://www.govdata.de/). The data was retrieved through an API and cleaned. For the analysis different parts of the data set were subset in order to filter for gendered data. Four different data analysis approaches were performed: calculating the percentage for the overall data sets and gendered data thereof and comparing on an annual basis, filtering the amount of gendered and non-gendered data sets per topic, unsupervised topic modeling with LDA on the descriptions, term frequency analysis on the keywords. We found that gendered data sets only make up a small amount (~6 percent) of overall data sets and are confined to 7 out of 12 topics. Furthermore, the most prevalent data appears to be treating construction and traffic. Our analysis has shown that there is a stark gender data gap and in order to be inclusive the portal would need to apply (inclusive) standards on the data published.

The data journalism article can be accessed [here](https://github.com/intro-to-data-science-21/data-project-invisible_women/blob/main/invisible_women_dr_hb_alw.html).

## Data folder

- Contains the raw data we extracted from the API on ['govdata.de'](https://www.govdata.de/)
- Contains the cleaned dataset we used for further analysis

## Scripts folder - functionality

### 00-wrangling
- API query
- Selecting the metadata used for the project
- Join them in a dataframe

### 001-data-cleaning
- Removing punctuation, words to lowercase

### 002-topic-modelling
- Cleaning data for LDA algorithm
- Applying LDA algorithm

### 003-tables-visualisations
- Visualizing accumulation of datasets over time
- Visualizing gendered data presence in topics

### 004-term_freq
- Visualizing term frequency within the datasets' keywords

## Individual contributors

* Dinah Rabe(d.rabe@mpp.hertie-school.org)

* Helena Bakic(211553@mds.hertie-school.org)

* Anna-Lisa Wirth (213286@mds.hertie-school.org)

## License

The material in this repository is made available under the [MIT license](http://opensource.org/licenses/mit-license.php).