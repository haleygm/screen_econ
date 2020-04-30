# IMDb Scrapper
library(tidyverse)
library(selectr)
library(XML)
library(xml2)
library(rvest)
library(stringr)
library(jsonlite)
library(readxl)
library(openxlsx)


# Disney ------------------------------------------------------------------

dis  <- read_excel("movies_to_merge_c2.xlsx", sheet = "DISNEY", col_names = T)
collection <- dis[,7]

df <- data.frame(id = as.character(), 
                 url = as.character(), 
                 title=as.character(),
                 year=as.character(), 
                 action=as.numeric(), 
                 animation=as.numeric(),
                 documentary =as.numeric(),
                 family=as.numeric(), 
                 bio = as.numeric(),
                 g = as.numeric(),
                 pg = as.numeric(),
                 pg13 = as.numeric(),
                 r = as.numeric(),
                 other = as.numeric(), 
                 budget = as.numeric(), 
                 gross_us = as.numeric(),
                 gross_ww = as.numeric(), 
                 runtime = as.numeric(), 
                 first_director = as.character(),
                 first_producer = as.character()) 

for(i in 1:nrow(collection)){
  url <- read_html(paste(collection[i, 1], sep = " ")) 
  id <- collection[i,1]%>%str_extract("tt\\d+")
  ty <- as.character(html_nodes(url,"h1") %>% html_text() %>% trimws())
  title <- as.character(str_sub(ty,  1,  str_length(ty)-7))
  year <- as.numeric(str_sub(ty, start = -6) %>% str_replace_all("[[:punct:]]", ""))
  headr <- html_nodes(url, ".subtext") %>% html_text() %>% str_replace_all("[\r\n]", "") %>% str_replace_all(" \\ ", "") %>% str_split( "\\|")
  rating <- headr[[1]][[1]] %>% trimws()
  g <- 0
  pg <- 0
  pg13 <- 0
  r <- 0
  other <- 0
  if(length(rating) != 0){
    if(rating == "G"){
      g <- 1
    }else if (rating == "PG"){
      pg <- 1
    }else if (rating == "PG-13"){
      pg13 <- 1
    }else if (rating == "R"){
      r <- 1
    }else if (rating == c("Approved", "Passed", "Not Rated")){
      other <- 1
    }
  }
  genres <- as.character(html_nodes(url, ".txt-block~ .canwrap a , .txt-block~ .canwrap .inline") %>% html_text() %>% trimws()) 
  genres <- as.character(html_nodes(url, ".subtext a+ a , .subtext a:nth-child(4)") %>% html_text() %>% trimws())
  genres <- genres[2:length(genres)]
  if(is.na(genres)){
    genres <- NA
  } 
  action <- 0
  animation <- 0
  family <- 0
  documentary <- 0
  bio <- 0
  for(i in 1:length(genres)){
    if((genres[i] == "Action") & (!is.na(genres[i]))){
      action <- 1
    }else if((genres[i] == "Animation") & (!is.na(genres[i]))){
      animation <- 1
    }else if ((genres[i] == "Family") & (!is.na(genres[i]))){
      family <- 1
    }else if((genres[i] == "Documentary") & (!is.na(genres[i]))){
      documentary <- 1
    }else if((genres[i] == "Biography") & (!is.na(genres[i]))){
      bio <- 1
    }
  }
  budget <- as.numeric(html_nodes(url, "#titleDetails .txt-block:nth-child(12)") %>% html_text() %>% parse_number())
  if(length(budget) == 0){
    budget <- NA
  }
  gross_us <- as.numeric(html_nodes(url, ".txt-block:nth-child(14)") %>% html_text() %>% parse_number())
  if(length(gross_us) == 0){
    gross_us <- NA
  }
  gross_ww <- as.numeric(html_nodes(url, ".txt-block:nth-child(15)") %>% html_text() %>% parse_number())
  if(length(gross_ww) == 0){
    gross_ww <- NA
  }
  runtime <- html_nodes(url, "time") %>% html_text()
  runtime <- as.numeric(runtime[2] %>% str_remove(" min"))
  if(length(runtime) == 0){
    runtime <- NA
  }
  first_director <- html_nodes(url, ".summary_text+ .credit_summary_item .inline+ a") %>% html_text()
  if(length(first_director) == 0){
    first_director <- NA
  }
  first_producer <- html_nodes(url, ".subheading+ .txt-block .inline+ a") %>% html_text() %>% trimws()
  if(length(first_producer) == 0){
    first_director <- NA
  } else if (length(first_producer) > 1){
    first_producer <- first_producer[1]
  }
  url <- collection[i,1]
  obs <- cbind(id, url, title, year, action, animation, documentary, family, bio, g, pg, pg13, r, other, budget, gross_us, gross_ww, runtime, first_director, first_producer)
  df <- rbind(df, obs)
}

write.xlsx(df, file = "Disney.xlsx")


# Universal ---------------------------------------------------------------
uni  <- read_excel("movies_to_merge_c2.xlsx", sheet = "UNIVERSAL", col_names = T)
collection <- uni[,7]

df2 <- data.frame(id = as.character(), 
                  url = as.character(), 
                  title=as.character(),
                  year=as.character(), 
                  action=as.numeric(), 
                  animation=as.numeric(),
                  documentary =as.numeric(),
                  family=as.numeric(), 
                  bio = as.numeric(),
                  g = as.numeric(),
                  pg = as.numeric(),
                  pg13 = as.numeric(),
                  r = as.numeric(),
                  other = as.numeric(), 
                  budget = as.numeric(), 
                  gross_us = as.numeric(),
                  gross_ww = as.numeric(), 
                  runtime = as.numeric(), 
                  first_director = as.character(),
                  first_producer = as.character()) 

collection <- uni[958:nrow(uni),7]

for(i in 1:nrow(collection)){
  url <- read_html(paste(collection[i, 1], sep = " ")) 
  id <- collection[i,1]%>%str_extract("tt\\d+")
  ty <- as.character(html_nodes(url,"h1") %>% html_text() %>% trimws())
  title <- as.character(str_sub(ty,  1,  str_length(ty)-7))
  year <- as.numeric(str_sub(ty, start = -6) %>% str_replace_all("[[:punct:]]", ""))
  headr <- html_nodes(url, ".subtext") %>% html_text() %>% str_replace_all("[\r\n]", "") %>% str_replace_all(" \\ ", "") %>% str_split( "\\|")
  rating <- headr[[1]][[1]] %>% trimws()
  g <- 0
  pg <- 0
  pg13 <- 0
  r <- 0
  other <- 0
  if(length(rating) != 0){
    if(rating == "G"){
      g <- 1
    }else if (rating == "PG"){
      pg <- 1
    }else if (rating == "PG-13"){
      pg13 <- 1
    }else if (rating == "R"){
      r <- 1
    }else if (rating == c("Approved", "Passed", "Not Rated")){
      other <- 1
    }
  }
  genres <- as.character(html_nodes(url, ".txt-block~ .canwrap a , .txt-block~ .canwrap .inline") %>% html_text() %>% trimws()) 
  genres <- as.character(html_nodes(url, ".subtext a+ a , .subtext a:nth-child(4)") %>% html_text() %>% trimws())
  genres <- genres[2:length(genres)]
  if(is.na(genres)){
    genres <- NA
  } 
  action <- 0
  animation <- 0
  family <- 0
  documentary <- 0
  bio <- 0
  for(i in 1:length(genres)){
    if((genres[i] == "Action") & (!is.na(genres[i]))){
      action <- 1
    }else if((genres[i] == "Animation") & (!is.na(genres[i]))){
      animation <- 1
    }else if ((genres[i] == "Family") & (!is.na(genres[i]))){
      family <- 1
    }else if((genres[i] == "Documentary") & (!is.na(genres[i]))){
      documentary <- 1
    }else if((genres[i] == "Biography") & (!is.na(genres[i]))){
      bio <- 1
    }
  }
  budget <- as.numeric(html_nodes(url, "#titleDetails .txt-block:nth-child(12)") %>% html_text() %>% parse_number())
  if(length(budget) == 0){
    budget <- NA
  }
  gross_us <- as.numeric(html_nodes(url, ".txt-block:nth-child(14)") %>% html_text() %>% parse_number())
  if(length(gross_us) == 0){
    gross_us <- NA
  }
  gross_ww <- as.numeric(html_nodes(url, ".txt-block:nth-child(15)") %>% html_text() %>% parse_number())
  if(length(gross_ww) == 0){
    gross_ww <- NA
  }
  runtime <- html_nodes(url, "time") %>% html_text()
  runtime <- as.numeric(runtime[2] %>% str_remove(" min"))
  if(length(runtime) == 0){
    runtime <- NA
  }
  first_director <- html_nodes(url, ".summary_text+ .credit_summary_item .inline+ a") %>% html_text()
  if(length(first_director) == 0){
    first_director <- NA
  }
  first_producer <- html_nodes(url, ".subheading+ .txt-block .inline+ a") %>% html_text() %>% trimws()
  if(length(first_producer) == 0){
    first_producer <- NA
  } else if (length(first_producer) > 1){
    first_producer <- first_producer[1]
  }
  url <- collection[i,1]
  obs <- cbind(id, url, title, year, action, animation, documentary, family, bio, g, pg, pg13, r, other, budget, gross_us, gross_ww, runtime, first_director, first_producer)
  df2 <- rbind(df2, obs)
}

write.xlsx(df2, file = "Universal.xlsx")


# Warner Bros -------------------------------------------------------------
warn  <- read_excel("movies_to_merge_c2.xlsx", sheet = "WB", col_names = T)
collection <- warn[,7]

df3 <- data.frame(id = as.character(), 
                  url = as.character(), 
                  title=as.character(),
                  year=as.character(), 
                  action=as.numeric(), 
                  animation=as.numeric(),
                  documentary =as.numeric(),
                  family=as.numeric(), 
                  bio = as.numeric(),
                  g = as.numeric(),
                  pg = as.numeric(),
                  pg13 = as.numeric(),
                  r = as.numeric(),
                  other = as.numeric(), 
                  budget = as.numeric(), 
                  gross_us = as.numeric(),
                  gross_ww = as.numeric(), 
                  runtime = as.numeric(), 
                  first_director = as.character(),
                  first_producer = as.character()) 


warn  <- read_excel("movies_to_merge_c2.xlsx", sheet = "WB", col_names = T)
collection <- warn[1336:nrow(warn),7]


for(i in 1:nrow(collection)){
  url <- read_html(paste(collection[i, 1], sep = " ")) 
  id <- collection[i,1]%>%str_extract("tt\\d+")
  ty <- as.character(html_nodes(url,"h1") %>% html_text() %>% trimws())
  title <- as.character(str_sub(ty,  1,  str_length(ty)-7))
  year <- as.numeric(str_sub(ty, start = -6) %>% str_replace_all("[[:punct:]]", ""))
  headr <- html_nodes(url, ".subtext") %>% html_text() %>% str_replace_all("[\r\n]", "") %>% str_replace_all(" \\ ", "") %>% str_split( "\\|")
  rating <- headr[[1]][[1]] %>% trimws()
  g <- 0
  pg <- 0
  pg13 <- 0
  r <- 0
  other <- 0
  if(length(rating) != 0){
    if(rating == "G"){
      g <- 1
    }else if (rating == "PG"){
      pg <- 1
    }else if (rating == "PG-13"){
      pg13 <- 1
    }else if (rating == "R"){
      r <- 1
    }else if (rating == c("Approved", "Passed", "Not Rated", "Unrated")){
      other <- 1
    }
  }
  genres <- as.character(html_nodes(url, ".txt-block~ .canwrap a , .txt-block~ .canwrap .inline") %>% html_text() %>% trimws()) 
  genres <- as.character(html_nodes(url, ".subtext a+ a , .subtext a:nth-child(4)") %>% html_text() %>% trimws())
  genres <- genres[2:length(genres)]
  if(is.na(genres)){
    genres <- NA
  } 
  action <- 0
  animation <- 0
  family <- 0
  documentary <- 0
  bio <- 0
  for(i in 1:length(genres)){
    if((genres[i] == "Action") & (!is.na(genres[i]))){
      action <- 1
    }else if((genres[i] == "Animation") & (!is.na(genres[i]))){
      animation <- 1
    }else if ((genres[i] == "Family") & (!is.na(genres[i]))){
      family <- 1
    }else if((genres[i] == "Documentary") & (!is.na(genres[i]))){
      documentary <- 1
    }else if((genres[i] == "Biography") & (!is.na(genres[i]))){
      bio <- 1
    }
  }
  budget <- as.numeric(html_nodes(url, "#titleDetails .txt-block:nth-child(12)") %>% html_text() %>% parse_number())
  if(length(budget) == 0){
    budget <- NA
  }
  gross_us <- as.numeric(html_nodes(url, ".txt-block:nth-child(14)") %>% html_text() %>% parse_number())
  if(length(gross_us) == 0){
    gross_us <- NA
  }
  gross_ww <- as.numeric(html_nodes(url, ".txt-block:nth-child(15)") %>% html_text() %>% parse_number())
  if(length(gross_ww) == 0){
    gross_ww <- NA
  }
  runtime <- html_nodes(url, "time") %>% html_text()
  runtime <- as.numeric(runtime[2] %>% str_remove(" min"))
  if(length(runtime) == 0){
    runtime <- NA
  }
  first_director <- html_nodes(url, ".summary_text+ .credit_summary_item .inline+ a") %>% html_text()
  if(length(first_director) == 0){
    first_director <- NA
  }
  first_producer <- html_nodes(url, ".subheading+ .txt-block .inline+ a") %>% html_text() %>% trimws()
  if(length(first_producer) == 0){
    first_producer <- NA
  } else if (length(first_producer) > 1){
    first_producer <- first_producer[1]
  }
  url <- collection[i,1]
  obs <- cbind(id, url, title, year, action, animation, documentary, family, bio, g, pg, pg13, r, other, budget, gross_us, gross_ww, runtime, first_director, first_producer)
  df3 <- rbind(df3, obs)
}


write.xlsx(df3, file = "Warner.xlsx")
