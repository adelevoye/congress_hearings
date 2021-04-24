rm(list=ls())


library(tidyverse)
library(ggplot2)
library(ggthemes)
library(tikzDevice)
library(xtable)


# This script cleans the data and extract basic descriptive information

# Data cleaning ---------------------------------------------------------


# Load list of Hearing IDs (from different API extractions over time)

load(file = "Scrapping/hearings_wit.Rda")

jan19_df <- hearings_wit_df



load(file = 'hearings_wit_jan20.Rda')
jan20_df <- hearings_wit_df

# Create one big df

hearings_wit_df <- rbind(jan19_df, jan20_df)


# Nbr of hearings
length(unique(hearings_wit_df$id))
length(unique(jan19_df$id))
length(unique(jan20_df$id))



# Add dates

date_all <- as.data.frame(hearings_wit_df$date) %>%
  setNames("date")
date_year <- separate(date_all, date, into = c("Year", "Month","Day"), sep="-") 

hearings_wit_df_dates <- hearings_wit_df %>%
  mutate (year = date_year$Year,
          month = date_year$Month,
          day = date_year$Day
          )


# Add parties in control

hearings_wit_clean <- hearings_wit_df_dates 
hearings_wit_clean$Party[hearings_wit_clean$info=="105_senate_hearings"] <- "R"
hearings_wit_clean$Party[hearings_wit_clean$info=="105_house_hearings"] <- "R"
hearings_wit_clean$Party[hearings_wit_clean$info=="106_senate_hearings"] <- "R"
hearings_wit_clean$Party[hearings_wit_clean$info=="106_house_hearings"] <- "R"
hearings_wit_clean$Party[hearings_wit_clean$info=="107_senate_hearings"] <- "D"
hearings_wit_clean$Party[hearings_wit_clean$info=="107_house_hearings"] <- "R"
hearings_wit_clean$Party[hearings_wit_clean$info=="108_senate_hearings"] <- "R"
hearings_wit_clean$Party[hearings_wit_clean$info=="108_house_hearings"] <- "R"
hearings_wit_clean$Party[hearings_wit_clean$info=="109_senate_hearings"] <- "R"
hearings_wit_clean$Party[hearings_wit_clean$info=="109_house_hearings"] <- "R"
hearings_wit_clean$Party[hearings_wit_clean$info=="110_senate_hearings"] <- "D"
hearings_wit_clean$Party[hearings_wit_clean$info=="110_house_hearings"] <- "D"
hearings_wit_clean$Party[hearings_wit_clean$info=="111_senate_hearings"] <- "D"
hearings_wit_clean$Party[hearings_wit_clean$info=="111_house_hearings"] <- "D"
hearings_wit_clean$Party[hearings_wit_clean$info=="112_senate_hearings"] <- "D"
hearings_wit_clean$Party[hearings_wit_clean$info=="112_house_hearings"] <- "R"
hearings_wit_clean$Party[hearings_wit_clean$info=="113_senate_hearings"] <- "D"
hearings_wit_clean$Party[hearings_wit_clean$info=="113_house_hearings"] <- "R"
hearings_wit_clean$Party[hearings_wit_clean$info=="114_senate_hearings"] <- "R"
hearings_wit_clean$Party[hearings_wit_clean$info=="114_house_hearings"] <- "R"
hearings_wit_clean$Party[hearings_wit_clean$info=="115_senate_hearings"] <- "R"
hearings_wit_clean$Party[hearings_wit_clean$info=="115_house_hearings"] <- "R"
hearings_wit_clean$Party[hearings_wit_clean$info=="116_senate_hearings"] <- "R"
hearings_wit_clean$Party[hearings_wit_clean$info=="116_house_hearings"] <- "D"


# Add party in White House
hearings_wit_clean$WH[hearings_wit_clean$info=="105_senate_hearings"] <- "D"
hearings_wit_clean$WH[hearings_wit_clean$info=="105_house_hearings"] <- "D"
hearings_wit_clean$WH[hearings_wit_clean$info=="106_senate_hearings"] <- "D"
hearings_wit_clean$WH[hearings_wit_clean$info=="106_house_hearings"] <- "D"
hearings_wit_clean$WH[hearings_wit_clean$info=="107_senate_hearings"] <- "R"
hearings_wit_clean$WH[hearings_wit_clean$info=="107_house_hearings"] <- "R"
hearings_wit_clean$WH[hearings_wit_clean$info=="108_senate_hearings"] <- "R"
hearings_wit_clean$WH[hearings_wit_clean$info=="108_house_hearings"] <- "R"
hearings_wit_clean$WH[hearings_wit_clean$info=="109_senate_hearings"] <- "R"
hearings_wit_clean$WH[hearings_wit_clean$info=="109_house_hearings"] <- "R"
hearings_wit_clean$WH[hearings_wit_clean$info=="110_senate_hearings"] <- "R"
hearings_wit_clean$WH[hearings_wit_clean$info=="110_house_hearings"] <- "R"
hearings_wit_clean$WH[hearings_wit_clean$info=="111_senate_hearings"] <- "D"
hearings_wit_clean$WH[hearings_wit_clean$info=="111_house_hearings"] <- "D"
hearings_wit_clean$WH[hearings_wit_clean$info=="112_senate_hearings"] <- "D"
hearings_wit_clean$WH[hearings_wit_clean$info=="112_house_hearings"] <- "D"
hearings_wit_clean$WH[hearings_wit_clean$info=="113_senate_hearings"] <- "D"
hearings_wit_clean$WH[hearings_wit_clean$info=="113_house_hearings"] <- "D"
hearings_wit_clean$WH[hearings_wit_clean$info=="114_senate_hearings"] <- "D"
hearings_wit_clean$WH[hearings_wit_clean$info=="114_house_hearings"] <- "D"
hearings_wit_clean$WH[hearings_wit_clean$info=="115_senate_hearings"] <- "R"
hearings_wit_clean$WH[hearings_wit_clean$info=="115_house_hearings"] <- "R"
hearings_wit_clean$WH[hearings_wit_clean$info=="116_senate_hearings"] <- "R"
hearings_wit_clean$WH[hearings_wit_clean$info=="116_house_hearings"] <- "R"





# Add margin of seats
# Dems margins: -5 = dems had minus 5 seats

hearings_wit_clean$Margin_seats[hearings_wit_clean$info=="105_senate_hearings"] <- -10
hearings_wit_clean$Margin_prop[hearings_wit_clean$info=="105_senate_hearings"] <- -10/100
hearings_wit_clean$Margin_seats[hearings_wit_clean$info=="105_house_hearings"] <- 206-228
hearings_wit_clean$Margin_prop[hearings_wit_clean$info=="105_house_hearings"] <- (206-228)/435
hearings_wit_clean$Margin_seats[hearings_wit_clean$info=="106_senate_hearings"] <- -10
hearings_wit_clean$Margin_prop[hearings_wit_clean$info=="106_senate_hearings"] <- -10/100
hearings_wit_clean$Margin_seats[hearings_wit_clean$info=="106_house_hearings"] <- 211-223
hearings_wit_clean$Margin_prop[hearings_wit_clean$info=="106_house_hearings"] <- (211-223)/435
hearings_wit_clean$Margin_seats[hearings_wit_clean$info=="107_senate_hearings"] <- 0
hearings_wit_clean$Margin_prop[hearings_wit_clean$info=="107_senate_hearings"] <- 0/100
hearings_wit_clean$Margin_seats[hearings_wit_clean$info=="107_house_hearings"] <- 211-222
hearings_wit_clean$Margin_prop[hearings_wit_clean$info=="107_house_hearings"] <- (211-222)/435
hearings_wit_clean$Margin_seats[hearings_wit_clean$info=="108_senate_hearings"] <- 48-51
hearings_wit_clean$Margin_prop[hearings_wit_clean$info=="108_senate_hearings"] <- (48-51)/100
hearings_wit_clean$Margin_seats[hearings_wit_clean$info=="108_house_hearings"] <- 205-229
hearings_wit_clean$Margin_prop[hearings_wit_clean$info=="108_house_hearings"] <- (205-229)/435
hearings_wit_clean$Margin_seats[hearings_wit_clean$info=="109_senate_hearings"] <- 44-55
hearings_wit_clean$Margin_prop[hearings_wit_clean$info=="109_senate_hearings"] <- (44-55)/100
hearings_wit_clean$Margin_seats[hearings_wit_clean$info=="109_house_hearings"] <- 202-232
hearings_wit_clean$Margin_prop[hearings_wit_clean$info=="109_house_hearings"] <- (202-232)/435
hearings_wit_clean$Margin_seats[hearings_wit_clean$info=="110_senate_hearings"] <- 51-49
hearings_wit_clean$Margin_prop[hearings_wit_clean$info=="110_senate_hearings"] <- (51-49)/100
hearings_wit_clean$Margin_seats[hearings_wit_clean$info=="110_house_hearings"] <- 233-202
hearings_wit_clean$Margin_prop[hearings_wit_clean$info=="110_house_hearings"] <- (233-202)/435
hearings_wit_clean$Margin_seats[hearings_wit_clean$info=="111_senate_hearings"] <- 60-40
hearings_wit_clean$Margin_prop[hearings_wit_clean$info=="111_senate_hearings"] <- (60-40)/100
hearings_wit_clean$Margin_seats[hearings_wit_clean$info=="111_house_hearings"] <- 257-178
hearings_wit_clean$Margin_prop[hearings_wit_clean$info=="111_house_hearings"] <- (257-178)/435
hearings_wit_clean$Margin_seats[hearings_wit_clean$info=="112_senate_hearings"] <- 53-47
hearings_wit_clean$Margin_prop[hearings_wit_clean$info=="112_senate_hearings"] <- (53-47)/100
hearings_wit_clean$Margin_seats[hearings_wit_clean$info=="112_house_hearings"] <- 193-242
hearings_wit_clean$Margin_prop[hearings_wit_clean$info=="112_house_hearings"] <- (193-242)/435
hearings_wit_clean$Margin_seats[hearings_wit_clean$info=="113_senate_hearings"] <- 55-45
hearings_wit_clean$Margin_prop[hearings_wit_clean$info=="113_senate_hearings"] <- (55-45)/100
hearings_wit_clean$Margin_seats[hearings_wit_clean$info=="113_house_hearings"] <- 201-234
hearings_wit_clean$Margin_prop[hearings_wit_clean$info=="113_house_hearings"] <- (201-234)/435
hearings_wit_clean$Margin_seats[hearings_wit_clean$info=="114_senate_hearings"] <- 55-45
hearings_wit_clean$Margin_prop[hearings_wit_clean$info=="114_senate_hearings"] <- (55-45)/100
hearings_wit_clean$Margin_seats[hearings_wit_clean$info=="114_house_hearings"] <- 188-247
hearings_wit_clean$Margin_prop[hearings_wit_clean$info=="114_house_hearings"] <- (188-247)/435
hearings_wit_clean$Margin_seats[hearings_wit_clean$info=="115_senate_hearings"] <- 48-52
hearings_wit_clean$Margin_prop[hearings_wit_clean$info=="115_senate_hearings"] <- (48-52)/100
hearings_wit_clean$Margin_seats[hearings_wit_clean$info=="115_house_hearings"] <- 194-241
hearings_wit_clean$Margin_prop[hearings_wit_clean$info=="115_house_hearings"] <- (194-241)/435
hearings_wit_clean$Margin_seats[hearings_wit_clean$info=="116_senate_hearings"] <- 47-53
hearings_wit_clean$Margin_prop[hearings_wit_clean$info=="116_senate_hearings"] <- (47-53)/100
hearings_wit_clean$Margin_seats[hearings_wit_clean$info=="116_house_hearings"] <- 236-199
hearings_wit_clean$Margin_prop[hearings_wit_clean$info=="116_house_hearings"] <- (236-199)/435



# Save updated df (full)

save(hearings_wit_clean, file = "hearings_wit_clean.Rda")

# Hearings wit clean has date and party in control

# Descriptive key numbers dataset -----------------------------------------


# Summary findings - individual witnesses level
congresses_sum <- hearings_wit_df_dates %>%
  group_by(congress) %>%
  summarise(group_size = n())

committee_sum <- hearings_wit_df_dates %>%
  group_by(subcommittee) %>%
  summarise(group_size = n())

chamber_sum <- hearings_wit_df_dates %>%
  group_by(chamber) %>%
  summarise(group_size = n())

year_sum <- hearings_wit_df_dates %>%
  group_by(year) %>%
  summarise(group_size = n())


# Graphs - individual level summaries
# Number of witnesses heard by year

plot_wit_chamber <- hearings_wit_clean %>%
  ggplot(aes(chamber)) +
  geom_bar(fill = "darkseagreen4") +
  ylab ("Number of witnesses") +
  xlab("Chamber") +
  ggtitle("Number of witnesses by Chamber", "N=61,612") +
  theme_pander()

plot_wit_all <- subset(hearings_wit_clean, chamber != 'JOINT' & !is.na(Party)) %>%
  ggplot(aes(year, fill = Party), alpha = 0.6) +
  geom_bar() +
  ylab ("Number of witnesses") +
  xlab("Year") +
  scale_fill_manual("Party in control", 
                    values = c("D" = "blue", "R" = "red")) + 
  facet_wrap(~chamber) +
  ggtitle("WIT-H dataset, number of witnesses","N = 61,612") +
  theme_bw() + 
  scale_x_discrete(breaks = 1997:2019, 
                   labels = c("", 1998, "", 2000, "", 2002, "",2004, "",2006, "",2008, "", 2010, "",2012, "",2014, "", 2016, "", 2018, "")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom")

plot_wit_all


# Summary findings - hearings level
# 11 770 hearings had witnesses listed out of my 28.000

# Number of hearings by year

year_sum2 <- hearings_wit_clean %>%
  group_by(year, chamber, Party, id) %>%
  summarise(group_size = n())

plot_hearing_all <- subset(year_sum2, chamber != 'JOINT' & !is.na(Party)) %>%
  ggplot(aes(year, fill = Party)) +
  geom_bar() +
  ylab ("Number of hearings") +
  xlab("Year") +
  facet_wrap(~chamber) +
  ggtitle("WIT-H dataset, number of hearings", "N = 11,770") +
  theme_bw() + 
  scale_fill_manual("Party in control", 
                    values = c("D" = "blue", "R" = "red")) + 
  scale_x_discrete(breaks = 1997:2019, 
                   labels = c("", 1998, "", 2000, "", 2002, "",2004, "",2006, "",2008, "", 2010, "",2012, "",2014, "", 2016, "", 2018, "")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom")

plot_hearing_all

# Number of hearings by Chamber

chamber_sum2 <- hearings_wit_df_dates %>%
  group_by(chamber,id) %>%
  summarise(group_size = n())

Hearings_with_wit <- as.data.frame(chamber_sum2$id) %>%
  set_names("ID")
save(Hearings_with_wit, file = "Hearings_with_wit.Rda")


plot_hearing_chamber <- chamber_sum2 %>%
  ggplot(aes(chamber)) +
  geom_bar(fill = "darkseagreen4") +
  ylab ("Number of hearings") +
  xlab("Chamber") +
  ggtitle("Number of hearings by Chamber","N=11,770") +
  theme_pander()
plot_hearing_chamber


# Nb of hearings by committee

committee_sum2 <- hearings_wit_df_dates %>%
  group_by(subcommittee,id) %>%
  summarise(group_size = n())

committee_sum3 <- committee_sum2 %>%
  select(subcommittee,id) %>%
 group_by(subcommittee) %>%
 summarise(group_size = n())




