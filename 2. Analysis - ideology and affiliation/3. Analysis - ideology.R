rm(list=ls())


library(readr)
library(ggplot2)
library(dplyr)
library(MultinomialCI)
library(ggthemes)
library(gdata)
library(RColorBrewer)
library(xtable)
library(gghighlight)
library(estimatr)
library(tidyr)



# Set up ------------------------------------------------------------------

# Load data with variables coded by RAs

df <- read.csv("sample_coded_final.csv", sep=",")
table(df$affiliation)



# Nbr of witnesses coded
N_samp <- nrow(df)

# Nbr of hearings coded
N_hear <- length(unique(df$id))

# N of witnesses without the NAs for CFScores
N_samp_noNA <- df[!is.na(df$cf_score),]

# N of hearingswithout the NAs for CFScores
N_hear_noNA <- length(unique(N_samp_noNA$id))



#####################
# SE functions for proportions and group means
#####################

# Write a function with fpc
binomial_se_fpc <- function (p,n,N) {
  sqrt(((p*(1-p))/n))* sqrt((N-n)/(N-1))   
}
#binomial_se_fpc(0.08,400,6353)

# Write a function without fpc
binomial_se <- function (p,n) {
  sqrt(((p*(1-p))/n))   
}


se <- function(x){sd(x, na.rm = T)/sqrt(length(x))}


#######################################
# Numbers in each categories of witnesses
#######################################

table(df$cf_score_code)

perct1 <- 2573/N_samp
perct0 <- 302/N_samp
perct2 <- 783/N_samp
perctNA <- (1-(perct1+perct0+perct2))



# CFScores by Party -------------------------------------------------------

# Only group means and CIs first (don't use that graph)


df$cf_score <- as.numeric(as.character(df$cf_score))

ideo_general <- df %>%
  group_by(Party) %>%
  summarise (Average_CF = mean(cf_score, na.rm = T),
             se = se(cf_score),
             ci_lower = Average_CF - 1.96*se,
             ci_upper = Average_CF + 1.96*se        
             ) %>%
  drop_na()


plot_ideo <- ggplot(ideo_general, aes(Average_CF, Party)) +
  geom_point(color=c("blue", "red")) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0,color=c("blue", "red")) +
  xlim(-0.5,0.5) +
  theme_pander() +
  xlab ("Average CF Scores of witnesses invited when party in control") +
  ylab ("Party in contol of the Chamber") +
  ggtitle("Average CF Scores of witnesses by party in control",
          "N=1824 (witnesses with CFScores of NAs excluded)") +
  theme(legend.position = "none",legend.text=element_text(size=10),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold")
  )


plot_ideo


# Now, more complete graph with complete distributions. Use that graph. 



fun_mean <- function(x){
  return(data.frame(y=mean(x),label=mean(x,na.rm=T)))}

plot_ideo_byparty <- ggplot(subset(df, !is.na(df$Party)), aes(Party, cf_score, color = Party)) +
  geom_point(alpha =.1) +
  scale_color_manual(name="Party in control of the Chamber",
                     values = c("blue", "red"))  +
  geom_boxplot(alpha = .7, width=0.3, outlier.shape = NA, coef = 0) +
  theme_bw() +
  ylab ("CF Scores") +
  xlab ("Party in control of the Chamber") +
  ylim(-2,2) +
  ggtitle("Distribution of witnesses CF-Scores",
          "N=3659 (witnesses with valid CF-Scores)") +
  theme(legend.position = "none",legend.text=element_text(size=10),
        legend.title = element_blank()
  )


plot_ideo_byparty <- plot_ideo_byparty + 
  coord_flip() +
  labs(caption="Boxplot shows 25th, 50th (median) and 75th percentiles. 
       Mean and 95% CIs also shown.") +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.1, size =.3)

plot_ideo_byparty




# CFScores by Congress/Year and Party -------------------------------------

# By year first

year_cf <- df %>%
  group_by(year, Party) %>%
  summarise (Average_CF = mean(cf_score, na.rm = T),
             se = se(cf_score),
             ci_lower = Average_CF - 1.96*se,
             ci_upper = Average_CF + 1.96*se        
  ) %>%
  drop_na()


congress_cf <- df %>%
  group_by(congress, Party) %>%
  summarise (Average_CF = mean(cf_score, na.rm = T),
             se = se(cf_score),
             ci_lower = Average_CF - 1.96*se,
             ci_upper = Average_CF + 1.96*se        
  ) %>%
  drop_na()


# PLot 1: gm and ci - by congress

plot_ideo_congress <- ggplot(congress_cf, aes(Average_CF, as.factor(congress), color = Party)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
  xlab ("Average CF") +
  ylab ("Congress") +
  scale_color_manual (values = c("blue", "red")) +
  theme_bw() +
  coord_flip() +
  labs(caption="Group means and 95% CIs") +
  ggtitle("Average CF-Scores by Congress and party",
          "N=3659 (witnesses with valid CFScores)") +
  theme(legend.position = "bottom",legend.text=element_text(size=10),
        legend.title = element_blank())


plot_ideo_congress


# Plot 2: averages
plot_ideo_year <- ggplot(year_cf, mapping = aes(x = year, y = Average_CF, color=Party)) + 
  geom_point() + 
  theme_bw() +
  stat_smooth(method="gam", formula = y~s(x, k = 6)) +
  ylab ("Average CFScore") +
  xlab ("Year") +
  ylim(-0.75,0.4) +
  ggtitle("Average CFScores by year and party in control of the Chamber",
          "N=3659 (witnesses with valid CFScores)") +
  scale_color_manual(name="Party in control of the Chamber",
                     values = c("blue", "red"))  +
  labs(caption="GAM fit with 6 degrees of freedom") +
  scale_shape(name = "Chamber holding the hearing") +
  theme(legend.position = "bottom",legend.text=element_text(size=10),
        plot.title = element_text(size = 15, face = "bold"))
plot_ideo_year






# Plot 3with all data
df$cf

plot_ideo_year_all <- ggplot(subset(df, !is.na(df$Party)), mapping = aes(x = year, y = cf_score, color=Party)) + 
  geom_point(alpha = .2) + 
  theme_bw() +
  stat_smooth(method="gam", formula = y~s(x, k = 6)) +
  ylab ("CFScores") +
  xlab ("Year") +
  ylim(-2,2) +
  ggtitle("CFScores of witnesses",
          "N=3659 (witnesses with valid CFScores)") +
  scale_color_manual(name="Party in control of the Chamber",
                     values = c("blue", "red"))  +
  labs(caption="GAM fit with 6 degrees of freedom") +
  scale_shape(name = "Chamber holding the hearing") +
  theme(legend.position = "bottom",legend.text=element_text(size=10),
        legend.title = element_blank())
plot_ideo_year_all



# Plot 3with all data by chamber
df$chamber

df2 <- df %>%
  filter(chamber != 'JOINT') %>%
  drop_na(chamber) %>%
  drop_na(Party)

plot_ideo_year_chamber_all <- ggplot(subset(df2), mapping = aes(x = year, y = cf_score, color=Party)) + 
  geom_point(alpha = .2) + 
  theme_bw() +
  stat_smooth(method="gam", formula = y~s(x, k = 4)) +
  ylab ("CFScores") +
  xlab ("Year") +
  ylim(-2,2) +
  facet_wrap(~chamber) +
  ggtitle("CF-Scores of witnesses",
          "N=3659 (witnesses with valid CFScores)") +
  scale_color_manual(name="Party in control of the Chamber",
                     values = c("blue", "red"))  +
  labs(caption="GAM fit with 4 degrees of freedom") +
  scale_shape(name = "Chamber holding the hearing") +
  theme(legend.position = "bottom",legend.text=element_text(size=10),
        legend.title = element_blank())
plot_ideo_year_chamber_all




# Distrib of CF Scores by Dems. by year

df_dem <- df %>%
  filter(Party == 'D')

distrib_year_D <- ggplot(df_dem, aes(x=cf_score)) +
  geom_density(bw = 0.2, color = "dodgerblue4", fill = "dodgerblue2") +
  facet_wrap(df_dem$year) +
  theme_bw() + 
  ylab ("Density") +
  xlab ("Distribution of CFScores") +
  xlim(-2,2) +
  ggtitle("Distribution of CF-Scores",
          "Witnesses invited by Democratic Chambers over the years") +
  theme(legend.position = "bottom",legend.text=element_text(size=10),
        legend.title = element_blank())

distrib_year_D


df_rep <- df %>%
  filter(Party == 'R')

distrib_year_R <- ggplot(df_rep, aes(x=cf_score)) +
  geom_density(bw = 0.2, color = "red4", fill = "red2") +
  facet_wrap(df_rep$year) +
  theme_bw() + 
  ylab ("Density") +
  xlab ("Distribution of CFScores") +
  xlim(-2,2) +
  ggtitle("Distribution of CF-Scores",
          "Witnesses invited by Republican Chambers over the years") +
  theme(legend.position = "bottom",legend.text=element_text(size=10),
        legend.title = element_blank())

distrib_year_R




# Distrib of CF Scores by affil and congress



plot_ideo_year_affil <- ggplot(subset(df, !is.na(df$Party)), mapping = aes(x = year, y = cf_score, color=Party)) + 
  geom_point(alpha = .2) + 
  theme_bw() +
  stat_smooth(method="gam", formula = y~s(x, k = 6)) +
  ylab ("CFScores") +
  xlab ("Year") +
  ylim(-2,2) +
  facet_wrap(~subset(df, !is.na(df$Party))$affiliation) +
  ggtitle("CF-Scores of witnesses, by affiliation",
          "N=3659 (witnesses with valid CFScores)") +
  scale_color_manual(name="Party in control of the Chamber",
                     values = c("blue", "red"))  +
  labs(caption="GAM fit with 6 degrees of freedom") +
  scale_shape(name = "Chamber holding the hearing") +
  theme(legend.position = "bottom",legend.text=element_text(size=10),
        legend.title = element_blank(),
        strip.text = element_text(size = 6))

plot_ideo_year_affil


# CFScores by Chamber -----------------------------------------------------


chamber_cf <- df %>%
  group_by(chamber, Party) %>%
  summarise (Average_CF = mean(cf_score, na.rm = T),
             se = se(cf_score),
             ci_lower = Average_CF - 1.96*se,
             ci_upper = Average_CF + 1.96*se        
  )

# Drop Joint committees

chamber_cf <-chamber_cf %>%
  filter (chamber != "JOINT")


labels <- c(D = "Democratic-Controlled Chambers", R = "Republican-Controlled Chambers")

plot_ideo_chamber <- ggplot(chamber_cf, aes(Average_CF, chamber)) +
  geom_point(color=c("blue", "red", "blue", "red")) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0,color=c("blue", "red", "blue", "red")) +
  facet_wrap(~Party, labeller=labeller(Party = labels))   +
  xlab ("Average CF-Score of witnesses") +
  ylab ("Chamber") +
  theme_bw() +
  labs(caption="Group means and 95% CIs plotted") +
  ggtitle("Average CF-Scores",
          "N=3659 (witnesses with valid CFScores)") +
  theme(legend.position = "bottom",legend.text=element_text(size=10),
        legend.title = element_blank())


plot_ideo_chamber



# Balanced perspective in hearing --------------------------------------------

#######################################
# Build perspective variable for each hearing
# Need to have a dataset with balanced perspective for each hearing (1 row = 1 hearing)
# all info on hearing needed too : chamber, party, congress etc
#######################################


# Get average perc of NAs, CFScores and sqrt of cf scores 
#for each hearings



df_hearing <- df %>%
  group_by(id) %>%
  summarize (perc_na = sum(is.na(cf_score)/n()),
             average = mean (cf_score, na.rm=T),
             variance = sd(cf_score, na.rm = T)) 



# WSome variances are 0 when only 1 witness

hist(df_hearing$variance)


# Code each hearing: whether one sided, two sided or neutral based on average and variance of CF scores

# Need NA for when only 1 wit (sd NA will take care of this) OR more than 20% witnesses in hearings with NAs for ideologies

df_hearing <- df_hearing %>%
  mutate (balance_pers = ifelse (perc_na > 0.2, NA,
                                 ifelse(abs(average)> 0.6, "one-sided",
                                        ifelse (variance > 0.9, "two_sided", 
                                         "neutral"))))




table(df_hearing$balance_pers)



# Now, need to add balance_pers column into main DF by matching on hearing id
# DF_info: number of witnesses in the eligible hearings 
df_info <- merge (df, df_hearing, by = "id")



table(df_info$balance_pers)
# Number of witnesses I have in this sample
wit_nbr <- nrow(df_info)


# Merge all those DF to get one DF with units = hearings
df_info_id <- df_info %>%
  select (Name, id, Party, chamber, committee, congress, year, balance_pers)

df_info_id <- df_info_id %>%
  group_by(id)  %>%
  summarise(congress = first(congress),
            chamber = first(chamber), 
            committee = first(committee),
            year = first(year),
            balance_pers = first(balance_pers),
            Party = first(Party))



# Info level - by party ---------------------------------------------------


table(df_info_id$Party)

D_hear <- as.numeric(table(df_info_id$Party)[1])
R_hear <- as.numeric(table(df_info_id$Party)[2])


df_info_party <- df_info_id %>%
  group_by(Party, balance_pers) %>%
  summarise (count = n()) %>%
  drop_na()
  
  
df_info_party <- df_info_party %>%
  mutate (proportion = ifelse (Party=="D", count/D_hear, count/R_hear),
          se = ifelse (Party=="D", binomial_se_fpc(proportion,913,28582),
                       binomial_se_fpc(proportion,913, 28582)),
          ci_lower = proportion - 1.96*se,
          ci_upper = proportion + 1.96*se)
    

df_info_party <- na.omit(df_info_party) 

  
  
plot_info_party <- ggplot(df_info_party, aes(proportion, Party, color = balance_pers)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
  xlab ("Proportion of hearings in the sample, among hearings organized by the same Party") +
  theme_bw() +
  scale_color_manual (values = c("gold2", "aquamarine2", "tomato2"),
                      name = "Type of hearing", labels = c("Two sided hearings", 
                                                           "One sided hearings", 
                                                           "Neutral hearings")) +
  ylab ("Party in control of the Chamber") +
  ggtitle("Balanced perspectives in hearings, by Party",
          "Hearings organized by one Party sum to 1, N=596 eligible hearings") +
  labs(caption="Proportion in the sample and binomial SEs for proportions, 95% CIs") +
  theme(legend.position = "bottom",legend.text=element_text(size=10),
        legend.title = element_blank())


plot_info_party




# Info level, by Chamber --------------------------------------------------

table(df_info_id$chamber)

H_hear <- as.numeric(table(df_info_id$chamber)[1])
S_hear <- as.numeric(table(df_info_id$chamber)[3])


df_info_chamber <- df_info_id %>%
  group_by(chamber, balance_pers) %>%
  summarise (count = n())


df_info_chamber <- df_info_chamber %>%
  filter (chamber != 'JOINT')

df_info_chamber <- df_info_chamber %>%
  mutate (proportion = ifelse (chamber=='HOUSE', count/H_hear, count/S_hear),
          se = ifelse (chamber=='HOUSE', binomial_se_fpc(proportion,913,28582),
                       binomial_se_fpc(proportion,913,28582)),
          ci_lower = proportion - 1.96*se,
          ci_upper = proportion + 1.96*se)


df_info_chamber <- na.omit(df_info_chamber) 


plot_info_chamber <- ggplot(df_info_chamber, aes(proportion, chamber, color = balance_pers)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
  xlab ("Proportion of hearings in the sample, among hearings organized by the same Chamber") +
  theme_bw() +
  scale_color_manual (values = c("gold2", "aquamarine2", "tomato2"),
                      name = "Type of hearing", labels = c("Two sided hearings", 
                                                           "One sided hearings", 
                                                           "Neutral hearings")) +
  ylab ("Chamber holding the hearingr") +
  ggtitle("Balanced perspectives in hearings, by Chamber",
          "Hearings organized by one Chamber sum to 1, N=596 eligible hearings") +
  labs(caption="Proportion in the sample and binomial SEs for proportions, 95% CIs") +
  theme(legend.position = "bottom",legend.text=element_text(size=10),
        legend.title = element_blank())

plot_info_chamber



# Info level by Congress and year -----------------------------------------

# Not enough data to get interesting stuff here
# By Congress 

table(df_info_id$congress)


# Need nbr of hearings for each Congress
hearings_by_congress <- df_info_id %>%
  group_by(congress) %>%
  summarise(n_hear = length(unique(id)))



# Now, add one column to dataframe for proportion. For each row, need to divide count by total nbr of witnesses for this congress (using witnesses_by_congress df)


df_info_congress <- df_info_id %>%
  group_by(congress, balance_pers) %>%
  summarise (count = n())

df_info_congress <- merge (df_info_congress, hearings_by_congress)


df_info_congress <- df_info_congress %>%
  mutate (proportion = count/n_hear,
          se = binomial_se_fpc(proportion,913,28582),
          ci_lower = proportion - 1.96*se,
          ci_upper = proportion + 1.96*se)


df_info_congress <- na.omit(df_info_congress) 

plot_info_congress <- ggplot(df_info_congress, aes(proportion, as.factor(congress), color = balance_pers)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
  xlab ("Proportion of hearings among hearings held during a given Congress") +
  theme_bw() +
  scale_color_manual (values = c("gold2", "aquamarine2", "tomato2"),
                      name = "Type of hearing", labels = c("Two-sided hearings", 
                                                           "One-sided hearings", 
                                                           "Neutral hearings")) +
  ylab ("Congress") +
  ggtitle("Balanced perspectives in hearings, by Congress",
          "N=596 eligible hearings") +
  labs(caption="") +
  theme(legend.position = "bottom",legend.text=element_text(size=10),
        legend.title = element_blank())


plot_info_congress





# By Year - CIs will be very large

df_info_year <- df_info %>%
  group_by(year, balance_pers) %>%
  summarise (count= n()
  ) 


# Need nbr of hearings for each year
hearings_by_year <- df_info %>%
  group_by(year) %>%
  summarise(n_hear = length(unique(id)))

# Need nbr of witneeses for each year
witnesses_by_year <- as.data.frame(table(df_info$year))
names(witnesses_by_year)[1] <- "year"
names(witnesses_by_year)[2] <- "n_wit"


# Now, add one column to dataframe for proportion. For each row, need to divide count by total nbr of witnesses for this congress (using witnesses_by_congress df)

df_info_year <- merge (df_info_year, witnesses_by_year, by = "year")

df_info_year <- df_info_year %>%
  mutate (proportion = count/n_wit)





plot_info_year <- ggplot(df_info_year, aes(year, proportion, color = balance_pers)) +
  geom_smooth(method = "lm_robust", formula = y ~ poly(x, 3)) +
  xlab ("Year") +
  theme_pander() +
  scale_color_manual (values = c("gold2", "aquamarine2", "tomato2"),
                      name = "Type of hearing", labels = c("Balanced hearings", 
                                                           "Biased hearings", 
                                                           "Neutral hearings")) +
  ylab ("Proportion of hearings") +
  labs(caption="OLS regression with robust SEs") +
  ggtitle("Balanced perspectives in hearings, by year",
          "All hearings held during a given year sum to 1. N=80") +
  theme(legend.position = "bottom",legend.text=element_text(size=10),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


plot_info_year




############################
# Alternative, info value
############################


df_hearing_alt <- df %>%
  group_by(id) %>%
  summarize (perc_na = sum(is.na(cf_score)/n()),
             count_lib = sum(cf_score < -0.5, na.rm = T),
             count_cons = sum(cf_score > 0.5, na.rm = T),
             count_total = n())



df_hearing_alt <- df_hearing_alt %>%
  mutate(perc_lib = count_lib/count_total,
         perc_cons = count_cons/count_total,
         perc_neutral = (count_total-(count_lib+count_cons))/count_total)





# Need NA for when only 1 wit (sd NA will take care of this) OR more than 20% NA



df_hearing_alt <- df_hearing_alt %>%
  mutate(
    balance_pers =
      case_when(
        perc_na > 0.2 ~ "NA",
        perc_lib >= 0.6 & count_cons == 0 ~ "Liberal only",
        perc_lib >= 0.6 & count_cons != 0 ~ "Liberal leaning",
        perc_cons >= 0.6 & count_lib == 0 ~ "Conservative only",
        perc_cons >= 0.6 & count_lib != 0 ~ "Conservative leaning",
        perc_neutral	>= 0.6 ~ "Neutral",
        TRUE ~ 'Other'
      ))
    

table(df_hearing_alt$balance_pers)



# Now, need to add balance_pers column into main DF by matching on hearing id
# DF_info: number of witnesses in the eligible hearings 
df_info_alt <- merge (df, df_hearing_alt, by = "id")



# Number of witnesses I have in this sample
wit_nbr <- nrow(df_info_alt)


# Merge all those DF to get one DF with units = hearings
df_info_id_alt <- df_info_alt %>%
  select (Name, id, Party, chamber, committee, congress, year, balance_pers)

df_info_id_alt <- df_info_id_alt %>%
  group_by(id)  %>%
  summarise(congress = first(congress),
            chamber = first(chamber), 
            committee = first(committee),
            year = first(year),
            balance_pers = first(balance_pers),
            Party = first(Party))



# Info level - by party ---------------------------------------------------



D_hear_alt <- as.numeric(table(df_info_id_alt$Party)[1])
R_hear_alt <- as.numeric(table(df_info_id_alt$Party)[2])


df_info_party_alt <- df_info_id_alt %>%
  group_by(Party, balance_pers) %>%
  summarise (count = n()) %>%
  drop_na()


df_info_party_alt <- df_info_party_alt %>%
  mutate (proportion = ifelse (Party=="D", count/D_hear_alt, count/R_hear_alt),
          se = ifelse (Party=="D", binomial_se_fpc(proportion,913,28582),
                       binomial_se_fpc(proportion,913, 28582)),
          ci_lower = proportion - 1.96*se,
          ci_upper = proportion + 1.96*se)


df_info_party_alt <- na.omit(df_info_party_alt) 

df_info_party_alt <- df_info_party_alt  %>%
  filter(balance_pers != 'NA') %>%
  filter(balance_pers != 'Other')


plot_info_party_alt <- ggplot(df_info_party_alt, aes(proportion, Party, color = balance_pers)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
  xlab ("Proportion of hearings in the sample, among hearings organized by the same Party") +
  theme_bw() +
  ylab ("Party in control of the Chamber") +
  ggtitle("Balanced perspectives in hearings, by Party",
          "Hearings organized by one Party sum to 1, N=913 eligible hearings") +
  labs(caption="Proportion in the sample and binomial SEs for proportions, 95% CIs") +
  theme(legend.position = "bottom",legend.text=element_text(size=7),
        legend.title = element_blank())


plot_info_party_alt






# Info level, by Chamber --------------------------------------------------


H_hear_alt <- as.numeric(table(df_info_id_alt$chamber)[1])
S_hear_alt <- as.numeric(table(df_info_id_alt$chamber)[3])


df_info_chamber_alt <- df_info_id_alt %>%
  group_by(chamber, balance_pers) %>%
  summarise (count = n())


df_info_chamber_alt <- df_info_chamber_alt %>%
  filter (chamber != 'JOINT')

df_info_chamber_alt <- df_info_chamber_alt %>%
  mutate (proportion = ifelse (chamber=='HOUSE', count/H_hear_alt, count/S_hear_alt),
          se = ifelse (chamber=='HOUSE', binomial_se_fpc(proportion,913,28582),
                       binomial_se_fpc(proportion,913,28582)),
          ci_lower = proportion - 1.96*se,
          ci_upper = proportion + 1.96*se)


df_info_chamber_alt <- na.omit(df_info_chamber_alt) 
df_info_chamber_alt <- df_info_chamber_alt  %>%
  filter(balance_pers != 'NA') %>%
  filter(balance_pers != 'Other')



plot_info_chamber_alt <- ggplot(df_info_chamber_alt, aes(proportion, chamber, color = balance_pers)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
  xlab ("Proportion of hearings in the sample, among hearings organized by the same Chamber") +
  theme_bw() +
  ylab ("Chamber holding the hearing") +
  ggtitle("Balanced perspectives in hearings, by Chamber",
          "Hearings organized by one Chamber sum to 1, N=913 eligible hearings") +
  labs(caption="Proportion in the sample and binomial SEs for proportions, 95% CIs") +
  theme(legend.position = "bottom",legend.text=element_text(size=7),
        legend.title = element_blank())

plot_info_chamber_alt




# Info level by Congress and year -----------------------------------------

# Not enough data to get interesting stuff here
# By Congress 



# Need nbr of hearings for each Congress
hearings_by_congress_alt <- df_info_id_alt %>%
  group_by(congress) %>%
  summarise(n_hear = length(unique(id)))



# Now, add one column to dataframe for proportion. For each row, need to divide count by total nbr of witnesses for this congress (using witnesses_by_congress df)


df_info_congress_alt <- df_info_id_alt %>%
  group_by(congress, balance_pers) %>%
  summarise (count = n())

df_info_congress_alt <- merge (df_info_congress_alt, hearings_by_congress_alt)


df_info_congress_alt <- df_info_congress_alt %>%
  mutate (proportion = count/n_hear,
          se = binomial_se_fpc(proportion,913,28582),
          ci_lower = proportion - 1.96*se,
          ci_upper = proportion + 1.96*se)


df_info_congress_alt <- na.omit(df_info_congress_alt) 

df_info_congress_alt <- df_info_congress_alt  %>%
  filter(balance_pers != 'NA') %>%
  filter(balance_pers != 'Other')
  

plot_info_congress_alt <- ggplot(df_info_congress_alt, aes(proportion, as.factor(congress), color = balance_pers)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
  xlab ("Proportion of hearings among hearings held during a given Congress") +
  theme_bw() +
  ylab ("Congress") +
  ggtitle("Balanced perspectives in hearings, by Congress",
          "N=593 eligible hearings") +
  labs(caption="") +
  theme(legend.position = "bottom",legend.text=element_text(size=7),
        legend.title = element_blank())


plot_info_congress_alt









# Average CFScores by affiliation -----------------------------------------
table(df$affiliation)

# Could be used as proxy to fill out certain missing CF Scores? At least for certains groups, when the distrib is narrow enough?. Also check correlation with Crosson et al
# For the appendix - will serve as a test that CFScores work
cf_affil <- df %>%
  group_by(affiliation) %>%
  summarise(av_cf = mean(cf_score, na.rm = T)) 



# Maybe exclude cf_score codes of 2 and NA before? 2 adds a big 0 for executive agencies distrib
df_affil <- df %>%
  filter (affiliation != 'Other') %>%
  drop_na(affiliation)

table(df$affiliation)

cf_affil_plot <- ggplot(df_affil, aes(x=cf_score)) +
  geom_density(bw = 0.17, color = "steelblue4", fill = "slategray2") +
  facet_wrap(~df_affil$affiliation) +
  theme_bw() + 
  ylab ("Density") +
  xlab ("Distribution of CFScores") +
  xlim(-2.5,2.5) +
  ggtitle("Distribution of CF-Scores, by affiliation") +
  theme(legend.position = "bottom",legend.text=element_text(size=10),
        legend.title = element_blank(),
        strip.text = element_text(size = 6))


cf_affil_plot 




table(df$affiliation)
# By margin of seats ------------------------------------------------------


df$Margin_seats[df$info=="105_senate_hearings"] <- -10
df$Margin_prop[df$info=="105_senate_hearings"] <- -10/100
df$Margin_seats[df$info=="105_house_hearings"] <- 206-228
df$Margin_prop[df$info=="105_house_hearings"] <- (206-228)/435
df$Margin_seats[df$info=="106_senate_hearings"] <- -10
df$Margin_prop[df$info=="106_senate_hearings"] <- -10/100
df$Margin_seats[df$info=="106_house_hearings"] <- 211-223
df$Margin_prop[df$info=="106_house_hearings"] <- (211-223)/435
df$Margin_seats[df$info=="107_senate_hearings"] <- 0
df$Margin_prop[df$info=="107_senate_hearings"] <- 0/100
df$Margin_seats[df$info=="107_house_hearings"] <- 211-222
df$Margin_prop[df$info=="107_house_hearings"] <- (211-222)/435
df$Margin_seats[df$info=="108_senate_hearings"] <- 48-51
df$Margin_prop[df$info=="108_senate_hearings"] <- (48-51)/100
df$Margin_seats[df$info=="108_house_hearings"] <- 205-229
df$Margin_prop[df$info=="108_house_hearings"] <- (205-229)/435
df$Margin_seats[df$info=="109_senate_hearings"] <- 44-55
df$Margin_prop[df$info=="109_senate_hearings"] <- (44-55)/100
df$Margin_seats[df$info=="109_house_hearings"] <- 202-232
df$Margin_prop[df$info=="109_house_hearings"] <- (202-232)/435
df$Margin_seats[df$info=="110_senate_hearings"] <- 51-49
df$Margin_prop[df$info=="110_senate_hearings"] <- (51-49)/100
df$Margin_seats[df$info=="110_house_hearings"] <- 233-202
df$Margin_prop[df$info=="110_house_hearings"] <- (233-202)/435
df$Margin_seats[df$info=="111_senate_hearings"] <- 60-40
df$Margin_prop[df$info=="111_senate_hearings"] <- (60-40)/100
df$Margin_seats[df$info=="111_house_hearings"] <- 257-178
df$Margin_prop[df$info=="111_house_hearings"] <- (257-178)/435
df$Margin_seats[df$info=="112_senate_hearings"] <- 53-47
df$Margin_prop[df$info=="112_senate_hearings"] <- (53-47)/100
df$Margin_seats[df$info=="112_house_hearings"] <- 193-242
df$Margin_prop[df$info=="112_house_hearings"] <- (193-242)/435
df$Margin_seats[df$info=="113_senate_hearings"] <- 55-45
df$Margin_prop[df$info=="113_senate_hearings"] <- (55-45)/100
df$Margin_seats[df$info=="113_house_hearings"] <- 201-234
df$Margin_prop[df$info=="113_house_hearings"] <- (201-234)/435
df$Margin_seats[df$info=="114_senate_hearings"] <- 44-54
df$Margin_prop[df$info=="114_senate_hearings"] <- (44-54)/100
df$Margin_seats[df$info=="114_house_hearings"] <- 188-247
df$Margin_prop[df$info=="114_house_hearings"] <- (188-247)/435
df$Margin_seats[df$info=="115_senate_hearings"] <- 48-52
df$Margin_prop[df$info=="115_senate_hearings"] <- (48-52)/100
df$Margin_seats[df$info=="115_house_hearings"] <- 194-241
df$Margin_prop[df$info=="115_house_hearings"] <- (194-241)/435
df$Margin_seats[df$info=="116_senate_hearings"] <- 47-53
df$Margin_prop[df$info=="116_senate_hearings"] <- (47-53)/100
df$Margin_seats[df$info=="116_house_hearings"] <- 236-199
df$Margin_prop[df$info=="116_house_hearings"] <- (236-199)/435



df_info$balance_pers


################
# Average CF score
#################

# Margin 

df_margin <- df %>%
  group_by(Margin_prop) %>%
  summarise(N_hear = n_distinct(id),
            av_cf = mean(cf_score, na.rm = T),
            se = se(cf_score),
            i_lower = av_cf - 1.96*se,
            ci_upper = av_cf + 1.96*se)



plot_margin_cf <- ggplot(df_margin, aes(x=Margin_prop, y = av_cf, color = Margin_prop)) +
  geom_point() +
  geom_jitter(width = 0.00, height = 0.05) +
  stat_smooth(method="gam", formula = y~s(x, k = 6), colour = 'grey27', 
              size =.3, alpha = .3, linetype = 'dashed') +
  scale_size(range = c(1,6)) + 
  scale_colour_gradient2(low = "red", high = "blue", guide = 'none') +
  ylim(-0.7,0.7) +
  theme_classic() +
  labs(x = "Margin of seats for Democrats (percentage)", y = "Average CF Score of witnesses",
       caption="GAM fit with 6 degrees of freedom") 
  


plot_margin_cf    


colnames(df)

test <- df %>%
  filter(Party == 'R' & Margin_prop >=0) %>%
  select(date, congress, chamber, year, month, day, Party, Margin_prop, Margin_seats)

# Same but with all the data
plot_margin_cf_all <- ggplot(subset(df, !is.na(df$Party)), mapping = aes(x = Margin_prop, y = cf_score, color=Margin_prop)) + 
  geom_point(alpha = .2) + 
  theme_bw() +
  stat_smooth(method="gam", formula = y~s(x, k = 6), colour = 'grey27', 
              size =.3, alpha = .3, linetype = 'dashed') +  
  ylab ("CFScore") +
  xlab ("Margin of seats for Democrats (percentage)") +
  ylim(-2,2) +
  facet_wrap(~Party, scales="free_x") +
  ggtitle("CF-Scores of witnesses, by margin of seats",
          "N=3659 (witnesses with valid CFScores)") +
  labs(caption="GAM fit with 6 degrees of freedom") +
  scale_colour_gradient2(low = "red", high = "blue", guide = 'none') +
  theme(legend.position = "none",legend.text=element_text(size=10),
        legend.title = element_blank())

plot_margin_cf_all








# Seats

df_seats <- df %>%
  group_by(Margin_seats) %>%
  summarise(N_hear = n_distinct(id),
            av_cf = mean(cf_score, na.rm = T),
            se = se(cf_score),
            i_lower = av_cf - 1.96*se,
            ci_upper = av_cf + 1.96*se)



plot_seats_cf <- ggplot(df_seats, aes(x=Margin_seats, y = av_cf, color = Margin_seats)) +
  geom_point() +
  geom_jitter(width = 0.00, height = 0.05) +
  stat_smooth(method="gam", formula = y~s(x, k = 6), colour = 'grey27', 
              size =.3, alpha = .5, linetype = 'dashed') +
  scale_size(range = c(1,6)) + 
  scale_colour_gradient2(low = "red", high = "blue", guide = 'none') +
  theme_classic() +
  ggtitle("CFScores of witnesses, by margin of seats",
          "N=3659 (witnesses with valid CFScores)") +
  ylim(-0.7,0.3) +
  labs(x = "Margin of seats for Democrats", y = "Average CF Score of witnesses",
       caption="GAM fit with 6 degrees of freedom") 



plot_seats_cf   




# Same but with all the data
plot_seats_cf_all <- ggplot(subset(df, !is.na(df$Party)), mapping = aes(x = Margin_seats, y = cf_score, color=Margin_seats)) + 
  geom_point(alpha = .2) + 
  theme_bw() +
  stat_smooth(method="gam", formula = y~s(x, k = 6), colour = 'grey27', 
              size =.3, alpha = .3, linetype = 'dashed') +  ylab ("CFScore") +
  xlab ("Margin of seats for Democrats") +
  ylim(-2,2) +
  ggtitle("CFScores of witnesses, by margin of seats",
          "N=3659 (witnesses with valid CFScores)") +
  labs(caption="GAM fit with 6 degrees of freedom") +
  scale_colour_gradient2(low = "red", high = "blue", guide = 'none') +
  theme(legend.position = "none",legend.text=element_text(size=10),
        legend.title = element_blank())

plot_seats_cf_all











################
# Balance of hearings
#################



df_hearing2 <- merge(df, df_hearing, by = 'id')

df_hearing2 <- df_hearing2 %>% 
  group_by (id) %>%
  summarize (Margin_prop = mean(Margin_prop),
             Margin_seats = mean(Margin_seats),
             balance_pers = first(balance_pers)
             )


df_margin_balance <- df_hearing2 %>%
  group_by(Margin_prop) %>%
  summarise(N_hear = n(),
            N_bias = sum(balance_pers == "one-sided", na.rm =T),
            N_balance = sum(balance_pers == "two-sided", na.rm =T),
            N_neutral = sum(balance_pers == "neutral", na.rm =T))


df_margin_balance <- df_margin_balance %>%
  mutate(bias_perc = N_bias/N_hear,
         balance_perc = N_balance/N_hear,
         neutral_perc = N_neutral/N_hear) 

df_margin_balance <- df_margin_balance %>%
  mutate(Party = ifelse(Margin_prop < 0, 'R', 'D')) %>%
  drop_na()


plot_margin_balance <- ggplot(df_margin_balance, aes(x=Margin_prop, y = bias_perc, color = Margin_prop)) +
  geom_point() +
  scale_size(range = c(1,6)) + 
  stat_smooth(method="gam", formula = y~s(x, k = 6), colour = 'grey27', 
              size =.3, alpha = .3, linetype = 'dashed') +
  scale_colour_gradient2(low = "red", high = "blue", guide = 'none') +
  facet_wrap(~Party, scales="free_x") +
  theme_classic() +  
  ggtitle("Percentage of one-sided hearings, by margin of seats",
          "N=3659 (witnesses with valid CFScores)") +
  labs(x = "Margin of seats for Democrats (percentage)", 
       y = "Percentage of one-sided hearings 
       (among all hearings with a given margin of seats)",
       caption="GAM fit with 6 degrees of freedom") 

plot_margin_balance









# Seats

df_seats_balance <- df_hearing2 %>%
  group_by(Margin_seats) %>%
  summarise(N_hear = n(),
            N_bias = sum(balance_pers == "one-sided", na.rm =T),
            N_balance = sum(balance_pers == "two-sided", na.rm =T),
            N_neutral = sum(balance_pers == "neutral", na.rm =T))


df_seats_balance <- df_seats_balance %>%
  mutate(bias_perc = N_bias/N_hear,
         balance_perc = N_balance/N_hear,
         neutral_perc = N_neutral/N_hear)



plot_seats_balance <- ggplot(df_seats_balance, aes(x=Margin_seats, y = bias_perc, color = Margin_seats)) +
  geom_point() +
  scale_size(range = c(1,6)) + 
  stat_smooth(method="gam", formula = y~s(x, k = 6), colour = 'grey27', 
              size =.3, alpha = .5, linetype = 'dashed') +
  scale_colour_gradient2(low = "red", high = "blue", guide = 'none') +
  theme_classic() +
  ggtitle("Percentage of biased hearings, by margin of seats",
          "N=3659 (witnesses with valid CFScores)") +
  labs(x = "Margin of seats for Democrats", y = "Percentage of biased hearings",
       caption="GAM fit with 6 degrees of freedom") 



plot_seats_balance



# Save df, wit level

df_ideo <- merge(df, df_hearing, by ='id')

write.csv(df_ideo, '/Volumes/GoogleDrive/My Drive/RW/5. Data analysis/1. Hearings and samples/4. Data cleaning analysis /2. Witnesses samples and graphs /Witnesses samples and graphs/9. Clean df for text anal/df_ideo')

# Save df, hearing level

# CHECK NBR OF HEARINGS REMAINS RIGHT

df_ideo_hearing <- merge(df_hearing, df_hearing2, by = 'id')

df_ideo_hearing <- df_ideo_hearing[,-4]

df_ideo_hearing <- df_ideo_hearing %>%
  rename (balance_pers = balance_pers.y)


write.csv(df_ideo_hearing, '/Volumes/GoogleDrive/My Drive/RW/5. Data analysis/1. Hearings and samples/4. Data cleaning analysis /2. Witnesses samples and graphs /Witnesses samples and graphs/9. Clean df for text anal/df_ideo_hearing')
