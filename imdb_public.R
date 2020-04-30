# IMDb Public Data 
#Import libraries
library(tidyverse)
library(readxl)
library(scales)
library(splitstackshape)
library(openxlsx)

#### Importing Data ####
#Title AKA
df_title_aka <- read_tsv("title.akas.tsv", na = "\\N", quote = '') %>%
  select(titleId, ordering, title, region, language, types, attributes, isOriginalTitle) %>%
  filter(region %in% c("US", "GB", NA))

#Title Basic
df_title_basic <- read_tsv("title.basics.tsv", na = "\\N", quote = '') %>%
  select(tconst, titleType, primaryTitle, originalTitle, startYear, endYear, runtimeMinutes, genres, isAdult) %>%
  filter((isAdult == 0) & titleType %in% c("movie", "tvMiniSeries", "tvSpecial", "tvMovie"))

#Title Crew
#df_title_crew <- read_tsv("title.crew.tsv", na = "\\N", quote = '') %>%
# separate("directors", "First_Director", sep = ","  )
#Title Principals 

df_title_principals <- read_tsv("title.principals.tsv", na = "\\N", quote = '') %>%
  select(tconst, nconst, category)%>%
  filter(category %in% c("director", "producer", "actor", "actress") & tconst %in% tconst_vec)

for(i in 1:10){
  for(j in i:10){
    if(df_title_principals[i, 1] == df_title_principals[j,1]){
      if(df_title_principals[j, 3] == "director"){
        df_title_principals[i,4] == df_title_principals[j,2]
      }else if(df_title_principals[j, 3] == "actor"){
        df_title_principals[i,5] == df_title_principals[j,2]
      }else if (df_title_principals[j, 3] == "actress"){
        df_title_principals[i,6] == df_title_principals[j,2]
      }else if (df_title_principals[j, 3] == "producer"){
        df_title_principals[i,7] == df_title_principals[j,2]
      }
    }
  }
}

unique_index <- order(df_title_principals$tconst)[!duplicated(sort(df_title_principals$tconst))]


#Names 
df_names <- read_tsv("name.basics.tsv", na = "\\N", quote = '')%>%
  select(nconst, primaryName, primaryProfession)

cast <- merge(df_title_principals, df_names, by.x = "nconst", by.y = "nconst")

#Title Rankings
df_title_ratings <- read_tsv("title.ratings.tsv", na = "\\N", quote = '')

#### Merging/Cleaning ####
df_full <- read_excel("Full_data.xlsx", col_names  = T)

df_phase_2 <- merge(df_title_basic, df_title_ratings, by.x = "tconst", by.y = "tconst") %>% 
  merge(df_full, by.x = "tconst", by.y = "ID") %>% 
  filter(titleType == "movie") 
id <- df_phase_2[, 1]
rest <- df_phase_2[,10:42]
df_phase_2 <- cbind(id, rest)

write.xlsx(df_phase_2, file = "Full_data.xlsx", append = T)

#df <- merge(df_title_aka, df_title_basic, by.x = "titleId", by.y = "tconst") %>%
# merge(df_title_ratings, by.x = "titleId", by.y = "tconst")%>%
#group_by(titleId) %>%
#summarise_each(funs(first(.[!is.na(.)])))%>%
#merge(cast, by.x = "titleId", by.y = "tconst") 



#df<- df %>% group_by(titleId) %>% summarise_each(funs(first(.[!is.na(.)])))
#tconst_vec <- as.vector(df[ ,1])


