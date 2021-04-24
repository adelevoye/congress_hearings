rm(list=ls())
library(tidyverse)

# Upload clean df, wit and hearing levels
df_wit <- read.csv('df_ideo.csv')


df_hear <- read.csv('f_ideo_hearing.csv')

df_hear <- df_hear %>%
  select(id, Margin_prop, Margin_seats, balance_pers)


# Make sure I run race and copy paste in excel

df_wit$female <- as.numeric(as.character(df_wit$female))

# Need race and gender at hearing level
race <- df_wit %>%
  group_by (id) %>%
  summarize (female_hearing = mean(female, na.rm = T),
             white_hearing = mean(white, na.rm = T))


df_hear <- merge(df_hear, race, by = 'id')

# Also need party in control and other info
party <- df_wit %>%
  select(id, Party, year, chamber, committee)

party <- distinct(party)


df_hear <- merge(df_hear, party, by = 'id')





# Need average cf score of wit by hearings
ideo_hearing <- df_wit %>%
  group_by (id) %>%
  summarize (ideo_hearing = mean(cf_score, na.rm = T))


df_hear <- merge(df_hear, ideo_hearing, by = 'id')


write.csv(df_hear, 'df_hearing.csv')


