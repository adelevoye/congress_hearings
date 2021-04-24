rm(list=ls())


library(httr)
library(jsonlite)
library(xml2)
library(scrapeR)
library(rvest)
library(tidyverse)
library(rowr)


# This script scrapes information on the hearings based on the list of hearings IDs collected from the API
# Load list of Hearing IDs (from API extraction- vector of 28.000 IDs)

load(file = "Jan 22 2020 extraction/hearings_jan20_116.Rda")


# Get a test sample of those hearings: works with this

all_ids_jan20 <- all_ids_jan20 %>%
  select(ID)

# Looping through Hearings doesn't work. Loop only works with random numbering of rows
Hearings_random <- sample_n(all_ids_jan20, 528, replace = FALSE) %>%
  select(ID)
mean(duplicated(Hearings_random))

# Shorter test
Hearings_test <- sample_n(Hearings_random, 20)


# Hearings DF -------------------------------------------------------------

# Write a function, input an id, output a df with needed information from XML page

function_hearing_ID <- function(id){
  # Read the XML associated with the hearing ID  
  data_f <- read_xml(paste("https://www.govinfo.gov/metadata/pkg/",id,"/mods.xml", sep = ""))
  # Clean the XML file
  data_f <- xml_ns_strip(data_f)
  # Extract information from XML (use find first and not find all functions in XML2 package to make sure I only get 1 information, 
  # some XML files contain multiple dates or URLs etc). I need only one date/URL by hearing (when there are two in the XML file, it's a data entry mistake from them
  # and the two pieces of information are the same) 
  witness <- xml_find_all(data_f, ".//witness") %>%
    html_text %>%
    as.data.frame() %>%
    setNames("witness")
  info1 <- xml_find_first(data_f, ".//extension//waisDatabaseName") %>%
    html_text %>%
    as.data.frame() %>%
    setNames("info1")
  date <- xml_find_first(data_f, ".//extension//heldDate") %>%
    html_text %>%
    as.data.frame() %>%
    setNames("date")
  bill <- xml_find_first(data_f, ".//titleInfo//title") %>%
    html_text %>%
    as.data.frame() %>%
    setNames("bill")
  url <- xml_find_first(data_f, ".//location//url") %>%
    html_text %>%
    as.data.frame() %>%
    setNames("url")
  id <- xml_find_first(data_f, ".//extension//accessId") %>%
    html_text %>%
    as.data.frame() %>%
    setNames("id")
  chamber <- xml_find_first(data_f, ".//extension//chamber") %>%
    html_text %>%
    as.data.frame() %>%
    setNames("chamber")
  congress <- xml_find_first(data_f, ".//extension//congress") %>%
    html_text %>%
    as.data.frame() %>%
    setNames("chamber")
  session <- xml_find_first(data_f, ".//extension//session") %>%
    html_text %>%
    as.data.frame() %>%
    setNames("session")
  bill_nb <- xml_find_first(data_f, ".//extension//number") %>%
    html_text %>%
    as.data.frame() %>%
    setNames("number")
  committee_int <- xml_find_all(data_f, ".//congCommittee//name") %>%
    html_text %>%
    as.data.frame() 
  committee <- as.data.frame(committee_int[1,]) %>%
    setNames("committee")
  subcommittee <- as.data.frame(committee_int[2,]) %>%
    setNames("subcommittee")
  name_bill <- xml_find_first(data_f, ".//extension//searchTitle") %>%
    html_text %>%
    as.data.frame() %>%
    setNames("name_bill")
  # Regroup the information into a DF
  df_f <- witness %>%
    mutate (id = id$id,
            info = info1$info1,
            name_bill = name_bill$name_bill,
            date = date$date,
            congress = congress$chamber,
            chamber = chamber$chamber,
            committee=committee$committee,
            subcommittee=subcommittee$subcommittee,
            session = session$session,
            url = url$url
    )
  # Information on witnesses is all in 1 column. It needs to be parsed into multiple columns
  df2_f <- separate(df_f, witness, into = c("Name", "First Name","Position", "Organization","Other1", "Other2", "Other3", "Other4", "Other5"), sep=",") %>%
    mutate (Organization = if_else(is.na(Organization) == TRUE, Position, Organization))
  return(df2_f)
}



# Check function works
function_hearing_ID('CHRG-116hhrg36812')



# Also need a function which takes URL as input (rest is the same)

function_hearing_URL <- function(url){
  # Read the XML associated with the hearing ID  
  data_f2 <- read_xml(url)
  # Clean the XML file
  data_f2 <- xml_ns_strip(data_f2)
  # Extract information from XML (use find first and not find all functions in XML2 package to make sure I only get 1 information, 
  # some XML files contain multiple dates or URLs etc). I need only one date/URL by hearing (when there are two in the XML file, it's a data entry mistake from them
  # and the two pieces of information are the same) 
  # I do want all witnesses. 
  witness <- xml_find_all(data_f2, ".//witness") %>%
    html_text %>%
    as.data.frame() %>%
    setNames("witness")
  info1 <- xml_find_first(data_f2, ".//extension//waisDatabaseName") %>%
    html_text %>%
    as.data.frame() %>%
    setNames("info1")
  date <- xml_find_first(data_f2, ".//extension//heldDate") %>%
    html_text %>%
    as.data.frame() %>%
    setNames("date")
  bill <- xml_find_first(data_f2, ".//titleInfo//title") %>%
    html_text %>%
    as.data.frame() %>%
    setNames("bill")
  url <- xml_find_first(data_f2, ".//location//url") %>%
    html_text %>%
    as.data.frame() %>%
    setNames("url")
  id <- xml_find_first(data_f2, ".//extension//accessId") %>%
    html_text %>%
    as.data.frame() %>%
    setNames("id")
  chamber <- xml_find_first(data_f2, ".//extension//chamber") %>%
    html_text %>%
    as.data.frame() %>%
    setNames("chamber")
  congress <- xml_find_first(data_f2, ".//extension//congress") %>%
    html_text %>%
    as.data.frame() %>%
    setNames("chamber")
  session <- xml_find_first(data_f2, ".//extension//session") %>%
    html_text %>%
    as.data.frame() %>%
    setNames("session")
  bill_nb <- xml_find_first(data_f2, ".//extension//number") %>%
    html_text %>%
    as.data.frame() %>%
    setNames("number")
  committee_int <- xml_find_all(data_f2, ".//congCommittee//name") %>%
    html_text %>%
    as.data.frame() 
  committee <- as.data.frame(committee_int[1,]) %>%
    setNames("committee")
  subcommittee <- as.data.frame(committee_int[2,]) %>%
    setNames("subcommittee")
  name_bill <- xml_find_first(data_f2, ".//extension//searchTitle") %>%
    html_text %>%
    as.data.frame() %>%
    setNames("name_bill")
  # Regroup the information into a DF
  df_f2 <- witness %>%
    mutate (id = id$id,
            info = info1$info1,
            name_bill = name_bill$name_bill,
            date = date$date,
            congress = congress$chamber,
            chamber = chamber$chamber,
            committee=committee$committee,
            subcommittee=subcommittee$subcommittee,
            session = session$session,
            url = url$url
    )
  # Information on witnesses need to be parsed into multiple columns
  # Witnesses names and affiliations are not listed homogeneously, so I need to plan multiple empty cells
  df3_f <- separate(df_f2, witness, into = c("Name", "First Name","Position", "Organization", "Other1", "Other2", "Other3", "Other4", "Other5"), sep=",") %>%
    mutate (Organization = if_else(is.na(Organization) == TRUE, Position, Organization))
  return(df3_f)
}

function_hearing_URL("https://www.govinfo.gov/metadata/pkg/CHRG-116hhrg36812/mods.xml")


# Loop over all ids using the function in the loop, building a dataframe (code takes a long time to run, run it overnight)

hearings_wit = list()
for (i in 1:nrow(Hearings_random)){
  # Get each ID in the DF 
  id_loop <- Hearings_random[i,1]
  # Feed this ID in the function - gets me a list
  indiv_hearing <- function_hearing_ID(id_loop)
  # Add rows to main list through every iteration of the loop
  hearings_wit <- rbind(hearings_wit,indiv_hearing)
}

hearings_wit_df <- as_data_frame(hearings_wit)


save(hearings_wit_df, file = "/Volumes/GoogleDrive/My Drive/RW/5. Data analysis/1. Hearings and samples/3. Scrapping/Scrapping jan 20/hearings_wit_jan20.Rda")
write.csv(hearings_wit_df, file = "/Volumes/GoogleDrive/My Drive/RW/5. Data analysis/1. Hearings and samples/3. Scrapping/Scrapping jan 20/hearings_wit_jan20.csv")

