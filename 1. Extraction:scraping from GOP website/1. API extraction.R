rm(list=ls())

library(httr)
library(jsonlite)
library(dplyr)


# This script gives me a list of hearings IDs

# Get JSON link from API: https://api.govinfo.gov/docs/
# API limits # of extractions, so I have to operate by dates
# Previous Data collected: stopped at Jan 31 2019. 
# Therefore, I can only search for hearings under Congress 116 in my update

api_key=xxxx
# No data updated from Jan to July 2019
# Lots of updates happening in July 2019
# Don't forget to filter by Congress # (here I need 116 only)



# 2019-02-01T00:00:01Z-   2019-07-20T00:00:01Z

jul19_1 <- fromJSON("https://api.govinfo.gov/collections/CHRG/2019-02-01T00%3A00%3A01Z/2019-07-20T00%3A00%3A01Z?offset=0&pageSize=100&congress=116&api_key=api_key")
ID_0719_1 <- jul19_1$packages$packageId
ID_0719_1_df <- as.data.frame(ID_0719_1)
colnames(ID_0719_1_df) <- c("ID")


# 2019-07-20T00:00:02 -- 2019-08-31T00:00:01Z

jul19_2 <- fromJSON("https://api.govinfo.gov/collections/CHRG/2019-07-20T00%3A00%3A02Z/2019-08-31T00%3A00%3A01Z?offset=0&pageSize=100&congress=116&api_key=api_key")
ID_0719_2 <- jul19_2$packages$packageId
ID_0719_2_df <- as.data.frame(ID_0719_2)
colnames(ID_0719_2_df) <- c("ID")


all_ids <- rbind(ID_0719_1_df, ID_0719_2_df)



# 2019-08-31T00:00:02Z -- 2019-09-31T00:00:01Z

jul19_3 <- fromJSON("https://api.govinfo.gov/collections/CHRG/2019-08-31T00%3A00%3A02Z/2019-09-31T00%3A00%3A01Z?offset=0&pageSize=100&congress=116&api_key=api_key")
ID_0719_3 <- jul19_3$packages$packageId
ID_0719_3_df <- as.data.frame(ID_0719_3)
colnames(ID_0719_3_df) <- c("ID")


all_ids <- rbind(all_ids, ID_0719_3_df)



# 2019-09-31T00:00:02Z -- 2019-10-31T00:00:01Z

oct19 <- fromJSON("https://api.govinfo.gov/collections/CHRG/2019-09-31T00%3A00%3A02Z/2019-10-31T00%3A00%3A01Z?offset=0&pageSize=100&congress=116&api_key=api_key")
ID_1019 <- oct19$packages$packageId
ID_1019_df <- as.data.frame(ID_1019)
colnames(ID_1019_df) <- c("ID")


all_ids <- rbind(all_ids, ID_1019_df)




# 2019-10-31T00:00:02Z -- 2019-11-31T00:00:01Z

nov19 <- fromJSON("https://api.govinfo.gov/collections/CHRG/2019-10-31T00%3A00%3A02Z/2019-11-31T00%3A00%3A01Z?offset=0&pageSize=100&congress=116&api_key=api_key")
ID_1119 <- nov19$packages$packageId
ID_1119_df <- as.data.frame(ID_1119)
colnames(ID_1119_df) <- c("ID")


all_ids <- rbind(all_ids, ID_1119_df)


# 2019-11-31T00:00:02Z -- 2019-12-31T00:00:01Z

dec19 <- fromJSON("https://api.govinfo.gov/collections/CHRG/2019-11-31T00%3A00%3A02Z/2019-12-31T00%3A00%3A01Z?offset=0&pageSize=100&congress=116&api_key=api_key")
ID_1219 <- dec19$packages$packageId
ID_1219_df <- as.data.frame(ID_1219)
colnames(ID_1219_df) <- c("ID")


all_ids <- rbind(all_ids, ID_1219_df)





# 2019-12-31T00:00:02Z -- 2020-01-22T11:00:01Z

jan20 <- fromJSON("https://api.govinfo.gov/collections/CHRG/2019-12-31T00%3A00%3A02Z/2020-01-22T11%3A00%3A01Z?offset=0&pageSize=100&congress=116&api_key=api_key")
ID_0120 <- jan20$packages$packageId
ID_0120_df <- as.data.frame(ID_0120)
colnames(ID_0120_df) <- c("ID")


all_ids <- rbind(all_ids, ID_0120_df)

# CHeck for doubles
mean(duplicated(all_ids))


# Need to check for some overlaps with hearings scraped in jan 2019 that were already 116 congress

load(file = 'Jan 31 extraction/hearings.Rda')


all_ids_jan20 <- all_ids %>%
  mutate(already = ifelse(ID %in% Hearings, 1,0)) %>%
  filter (already == 0)


common <- intersect(all_ids$ID, Hearings$ID)  
common2 <- inner_join(all_ids,Hearings)
common3 <- setdiff(all_ids,Hearings)

# Does not seem to be any overlap



# Save the new hearingas update


save(all_ids_jan20, file = "Jan 22 2020 extraction/hearings_jan20_116.Rda")




