rm(list=ls())

library(httr)
library(jsonlite)
library(xml2)
library(scrapeR)
library(rvest)
library(tidyverse)
library(htm2txt)
library(readtext)



################################
# Get lists of all hearings IDs
#################################

# Import initial list of hearings 
initial <- read.csv('sample_jan19.csv')
initial_id <- as.data.frame(unique(initial$id))

# Import new list

sample2020 <- read.csv('sample_jan20.csv')
sample2020_id <- as.data.frame(unique(sample2020$id))

# Import update from 2019 hearings

update2020 <- read.csv('sample_jan20_update.csv')
update2020_id <- as.data.frame(unique(update2020$id))

################################
# Extract files for each hearing in the list - all policies
#################################

# Write a function, input an id, output a text file

function_text_extract <- function(id){
  # Get text from URL  
  data_text <- gettxt(paste("https://www.govinfo.gov/content/pkg/",id,"/html/",id,".htm", sep = ""))
  write.table(data_text, file = paste("texts_sample2020/",id,".text", sep = ""), sep = "\t")
  
}

# Get text not working in feb 2020
function_text_extract2 <- function(id){
  # Get text from URL  
  data_text <- download.file(paste("https://www.govinfo.gov/content/pkg/",id,"/html/",id,".htm", sep = ""),
                             destfile = paste("texts_sample_2020/",id,".text", sep = ""))}


function_text_extract2("CHRG-114shrg20445")



# Loop through all hearings


for (i in 1:nrow(sample2020_id)){
  # Get ID
  id_loop <- as.character(sample2020_id[i,1])
  # Feed this URL in the function - will save file
  indiv_hearing <- function_text_extract2(id_loop)
}




################################
# Extract files for each hearing in the list - hearings from 2019 update
#################################

function_text_extract_2019 <- function(id){
  # Get text from URL  
  data_text <- download.file(paste("https://www.govinfo.gov/content/pkg/",id,"/html/",id,".htm", sep = ""),
                             destfile = paste("texts_update_2020/",id,".text", sep = ""))}




for (i in 1:nrow(update2020_id)){
  # Get ID
  id_loop <- as.character(update2020_id[i,1])
  # Feed this URL in the function - will save file
  indiv_hearing <- function_text_extract_2019(id_loop)
}


################################
# In case of double entries/loop breaks
#################################

# One hearing has double entries, I'll deal with it outside of the loop
agri_id <- agri_id %>%
  filter (`unique(agri$id)`!='CHRG-115hhrg24725')


for (i in 1:nrow(agri_id)){
  tryCatch({
  # Get ID
  id_loop <- as.character(agri_id[i,1])
  cat(i, "page is done. \n")
  }, error=function(e){cat(i, "ERROR :",conditionMessage(e), "\n")})
  # Feed this URL in the function - will save file
  tryCatch({
  indiv_hearing <- function_text_extract_agri(id_loop)
  cat(i, "page is dowloaded. \n")
  }, error=function(e){cat(i, "ERROR :",conditionMessage(e), "\n")})
  }

# Loop skipped 1 hearing, CHRG-115hhrg24725, because there are 2 text files to extract
# https://www.govinfo.gov/content/pkg/CHRG-115hhrg24725/html/CHRG-115hhrg24725-pt1.htm
# https://www.govinfo.gov/content/pkg/CHRG-115hhrg24725/html/CHRG-115hhrg24725-pt2.htm

# Need to add them to corpus
text_p1 <- gettxt("https://www.govinfo.gov/content/pkg/CHRG-115hhrg24725/html/CHRG-115hhrg24725-pt1.htm", encoding = "UTF-8")
write.table(text_p1, file = 'texts_agri/CHRG-115hhrg24725-pt1.text', sep = "\t")

text_p2 <- gettxt("https://www.govinfo.gov/content/pkg/CHRG-115hhrg24725/html/CHRG-115hhrg24725-pt2.htm", encoding = "UTF-8")
write.table(text_p2, file = 'texts_agri/CHRG-115hhrg24725-pt2.text', sep = "\t")


