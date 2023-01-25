# Where are the Women?

This repository contains the data and scripts used to produce a data journalism article on the (missing) gender representation in the data available on Germanys Open Data Portal "GovData". This article was produced as final assignment for the course "Introduction to Data Science" at the Hertie School.

## Summary:

Motivated by the Book "Invisible Women" by Caroline Criado-Perez we used metadata in rdf form to analyze the prevalence of gender in 54682 data sets from ['govdata.de'](https://www.govdata.de/). The data was retrieved through an API and cleaned. For the analysis different parts of the data set were subset in order to filter for gendered data. Four different data analysis approaches were performed: calculating the percentage for the overall data sets and gendered data thereof and comparing on an annual basis, filtering the amount of gendered and non-gendered data sets per topic, unsupervised topic modeling with LDA on the descriptions, term frequency analysis on the keywords. 

The data journalism article can be accessed [here](https://rawcdn.githack.com/dinahrabe/invisible_women_project/815c2de465d30d6c548c140cd431b1b4033c3249/invisible_women_dr_hb_alw.html).

## Data folder

- Contains the raw data we extracted from the API on ['govdata.de'](https://www.govdata.de/) as the basis of our analysis. The data was retrieved in Nov/Dec of 2021.
- Contains the cleaned dataset we used for further analysis

## Scripts folder

This folder contains the individual scripts for the different steps of our pipeline, including: data wrangling, data cleaning:  topic modelling, visualizations and the term-frequency analysis.

## Findings

We find that the first gendered datasets were published in 2015 on the GovData Portal. These gendered data sets currently only make up a small amount (~6 percent) of overall data sets and are confined to 7 out of 12 topics. Furthermore, the most prevalent data appears to be treating construction and traffic. Our analysis has shown that there is a stark gender data gap in German Open Data and we recommend to develop and enforce (inclusive) standards on the data published to close this gap.


## Individual contributors

* Dinah Rabe(d.rabe@mpp.hertie-school.org)

* Helena Bakic(211553@mds.hertie-school.org)

* Anna-Lisa Wirth (213286@mds.hertie-school.org)

## License

The material in this repository is made available under the [MIT license](http://opensource.org/licenses/mit-license.php).

## Note
This repository was worked on in a github classroom environment and therefore had to be re-located into my personal github, thereby the commit-history was lost.
