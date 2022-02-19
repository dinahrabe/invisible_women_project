if (!require("pacman")) install.packages("pacman")
pacman::p_load(jsonlite, tidyjson, httr, doParallel, foreach)


## set up parallel core use for getting the raw data
n_cores <- detectCores()-1
cl <- makeCluster(
  n_cores,
  type = 'PSOCK'
)

## write function to deregister the cores after use
unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

## getting the meta data of all 55000 datasets
# Register cluster
registerDoParallel(cl)

# create empty list for json objects
x <- list()

# for loop to go over the urls and extend the limit each time and store the object in the json_list
# tryout with 1:5 objects - final version has to be with 1:55

json_list <- foreach(i=1:55,.packages = c('httr','jsonlite', 'tidyjson')) %dopar% {
  
  endp_one <- "https://www.govdata.de/ckan/api/action/current_package_list_with_resources?limit=1000&offset="
  endp_two <- as.character(1+1000*(i-1))
  endp <- paste0(endp_one,endp_two)
  resp <- GET(endp)
  x <- fromJSON(content(resp, as = "text"))
  
}

# De-register cluster
registerDoSEQ()
unregister_dopar()

######### GETTING THE RELEVANT DATA OUT OF THE JSON OBJECTS #########

## initialise two functions to get tags and groups

# function to get the names of the groups
get_groups <- function(x){
  list(my_groups = x$display_name)
} 

# function to get the names of the tags
get_tags <- function(x){
  list(my_tags = x$name)
} 

# Register cluster
registerDoParallel(cl)

## building loops to get out the needed attributes and combine them in a list
## list of 56 dataframes and then bind_rows together (dplyr)

final_df <- foreach(i = 1:length(json_list), .combine = rbind,.packages = c('httr','jsonlite', 'tidyjson','tidyverse'))%dopar%{
  
  ##### GET THE TAGS #####
  # reduce the json to the list of tags
  json_tags <- json_list[[i]]$result$tags
  
  # apply the function to the reduced json
  tags_list <- lapply(json_tags, get_tags)
  
  # create a dataframe of the tag names with the id of the element (needed for matching with other dataframes at the end)
  df_tags <- dplyr::bind_rows(tags_list,.id = "id")
  
  
  ##### GET THE TITLES #######
  # get the titles in a dataframe
  df_titles <- as.data.frame(json_list[[i]]$result$title)
  
  # change column name
  colnames(df_titles)[1] <- "titles"
  
  # add an id column
  df_titles <- tibble::rowid_to_column(df_titles, "id")
  
  
  ##### GET THE GROUPS #####
  # reduce the json to the list of groups
  json_groups <- json_list[[i]]$result$groups
  
  # apply the get_groups function to the reduced json
  groups_list <- lapply(json_groups, get_groups)
  
  # create a dataframe of the groups names with the id of the element (needed for matching with other dataframes at the end)
  df_groups <- dplyr::bind_rows(groups_list,.id = "id")
  
  
  ##### GET THE DESCRIPTION #####
  
  # get the descriptions in a dataframe
  df_description <- as.data.frame(json_list[[i]]$result$notes)
  # change column name
  colnames(df_description)[1] <- "description"
  # add an id column
  df_description <- tibble::rowid_to_column(df_description, "id")
  
  ##### GET THE DATE - METADATA CREATED #####
  
  # get the descriptions in a dataframe
  df_date <- as.data.frame(json_list[[i]]$result$metadata_created)
  # change column name
  colnames(df_date)[1] <- "metadata_created"
  # add id column
  df_date <- tibble::rowid_to_column(df_date, "id")
  # transform datatype to date
  df_date$metadata_created <-  as.Date(substr(df_date$metadata_created,1,10))
  
  
  ##### BINDING DATAFRAMES TOGETHER #####
  assign('c_results',setNames(data.frame(matrix(ncol = 6,nrow = nrow(json_list[[i]]$result))),c('id','title','description','tags','groups','date')))
  
  # add id column
  c_results$id <- unique(df_titles$id)
  
  # add tags and groups
  for(j in 1:nrow(c_results)){
    c_results[j,'tags'] <- toString(df_tags[which(df_tags[,'id']==j),'my_tags'])
    c_results[j,'groups'] <- toString(df_groups[which(df_groups[,'id']==j),'my_groups'])
    c_results[j,'title'] <- toString(df_titles[which(df_titles[,'id']==j),'titles'])
    c_results[j,'description'] <- toString(df_description[which(df_description[,'id']==j),'description'])
    c_results[j,'date'] <- df_date[which(df_date[,'id']==j),'metadata_created']
  }
  # transform date column
  c_results[,'date'] <- as.Date(c_results[,'date'],origin = "1970-01-01")
  
  c_results
}

# De-register cluster
registerDoSEQ()
unregister_dopar()

write.csv(final_df,paste0(getwd(),"/data/raw_data.csv"), row.names = FALSE)



