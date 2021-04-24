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
library(rowr)
library(estimatr)
library(tidyverse)

# Load dataset with variables coded by RAs
df <- read.csv("sample_coded_final.csv", sep=",")



N_samp <- nrow(df)


# SE functions ------------------------------------------------------------


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


######
#p has to be the proportion
# n is the size of the sample
# N is number of witnesses

# Need a set of colors for 14 variables
colourCount = length(unique(df$affiliation))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))



# Affiliations, general ---------------------------------------------------

# Get proportion of witnesses from each affiliation
df_count <- df %>%
  group_by(affiliation) %>%
  summarise (count= n(), proportion = n()/N_samp) 

df_count <- df_count %>%
  mutate (se = binomial_se(proportion,N_samp),
          ci_lower = proportion - 1.96*se,
          ci_upper = proportion + 1.96*se) 

df_count$affiliation

# Reorder variables
# Caution: name of the variable has to match the name in df_count exactly
affiliations_list <- c("Executive Agency","Other institutional - federal","Other institutional - state and local",
                       "Academic","Corporation", "Trade Association","Occupation Association",
                       "Public interest - liberal", "Public interest - neutral",'Public interest - conservative',	
                       "Identity Group", "Left leaning TT",'Moderate leaning TT',	
                       "Right leaning TT", "Other")
df_count$affiliation <- reorder.factor(df_count$affiliation, new.order=affiliations_list)
df_count <- df_count %>%
  arrange(affiliation) 

#Drop the NAs/Other (check that numbers are rigth)

df_count <- df_count %>%
  drop_na()


# Plot the proportions
plot_gen <- ggplot(df_count, aes(proportion, affiliation, color = affiliation)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
  theme_bw() +
  xlab ("Proportion of all witnesses") +
  ylab ("Affiliation") +
  labs(caption="Proportion in the sample and binomial SEs for proportions, 95% CIs") +
  ggtitle("Affiliations of witnesses, by affiliation",
          "Proportion of all witnesses in the sample, N=4569") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))



plot_gen


# Affiliations, by party --------------------------------------------------

table(df$Party)

df_party <- df %>%
  group_by(affiliation, Party) %>%
  summarise (count= n()) %>%
  drop_na(Party)

# find total number of D witnesses
D_wit <- with(df_party, sum(count[Party=="D"]))
R_wit <- with(df_party, sum(count[Party=="R"]))

df_party <- df_party %>%
  mutate (proportion = ifelse (Party=="D", count/D_wit, count/R_wit))

df_party <- df_party %>%
  mutate (se = ifelse (Party=="D", binomial_se_fpc(proportion,D_wit,61000),binomial_se_fpc(proportion,R_wit,61000)),
          ci_lower = proportion - 1.96*se,
          ci_upper = proportion + 1.96*se) 

df_party$affiliation <- reorder.factor(df_party$affiliation, new.order=affiliations_list)
df_party <- df_party %>%
  arrange(affiliation) 

# Get rid of NAs and others

df_party <- df_party %>%
  drop_na()

plot_party <- ggplot(df_party, aes(proportion, affiliation, color = Party)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
  theme_bw() +
  scale_color_manual (values = c("blue", "red")) +
  xlab ("Proportion in the sample, among witnesses invited by the same party") +
  ylab ("Affiliation") +
  labs(caption ="Proportion in the sample and binomial SEs for proportions, 95% CIs") +
  ggtitle("Affiliations of witnesses,by Party in control of the Chamber",
          "Witnesses invited by one party sum to 1, N=4569") +
  theme(legend.position = "bottom",
           legend.text=element_text(size=8,hjust = 0), 
           plot.caption = element_text(hjust = 0),
           legend.title = element_blank(),
           plot.title = element_text(size = 15, face = "bold"))

plot_party



# Affiliations, by committee, Charles Stewart typo----------------------------------------------

##########
# Using Charles Stewart's value of hearings 
# https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3153696&download=yes
###########

# Charles Stewart rank each committee by its value

# Committees put into buckets by tiers of value

df$chamber
table(df$subcommittee)
table(df$committee)

com_list <- data.frame(unique(df$committee))





df <- df %>%
  mutate (commit_rank = case_when(committee == 'Committee on Appropriations' & chamber == 'HOUSE' ~ 3,
                                  committee == 'Committee on Appropriations' & chamber == 'SENATE' ~ 2,
                                  committee == 'Committee on Small Business' & chamber == 'HOUSE' ~ 23,
                                  committee == 'Committee on Small Business' & chamber == 'SENATE' ~ 16,
                                  committee == 'Committee on the Judiciary' & chamber == 'HOUSE' ~ 8,
                                  committee == 'Committee on the Judiciary' & chamber == 'SENATE' ~ 12,
                                  committee == 'Joint Economic Committee' ~ -99,
                                  committee == 'Select Committee on Energy Independence and Global Warming' ~ -99,
                                  committee == 'Committee on Armed Services' ~ 5,
                                  committee == 'Committee on Ways and Means' ~ 1,
                                  committee == 'Committee on Financial Services' ~ 9,
                                  committee == 'Committee on Energy and Commerce' ~ 2,
                                  committee == 'Committee on Banking, Housing, and Urban Affairs' ~ 10,
                                  committee == 'Committee on Agriculture, Nutrition, and Forestry' ~ 13,
                                  committee == 'Committee on Homeland Security' & chamber == 'HOUSE' ~ 22,
                                  committee == 'Committee on Natural Resources' ~ 15,
                                  committee == 'Committee on Commerce, Science, and Transportation' ~ 4,
                                  committee == 'Committee on Education and the Workforce' ~ 17,
                                  committee == 'Committee on Judiciary' & chamber == 'HOUSE' ~ 8,
                                  committee == 'Committee on Judiciary' & chamber == 'SENATE' ~ 12,
                                  committee == 'Committee on Resources' ~ 14,
                                  committee == "Committee on Veterans' Affairs" & chamber == 'HOUSE' ~ 20,
                                  committee == "Committee on Veterans' Affairs" & chamber == 'SENATE' ~ 6,
                                  committee == "Committee on Foreign Affairs" ~ 6,
                                  committee == "Committee on Homeland Security and Governmental Affairs" ~ 8,
                                  committee == "Committee on Science, Space, and Technology" ~ 21,
                                  committee == "Committee on Transportation and Infrastructure" ~ 14,
                                  committee == "Committee on Indian Affairs" ~ 22,
                                  committee == "Committee on Science" ~ 21,
                                  committee == "Committee on Armed Services" & chamber == 'HOUSE' ~ 5,
                                  committee == "Committee on Armed Services" & chamber == 'SENATE' ~ 5,
                                  committee == "Committee on Education and Labor" ~ 17,
                                  committee == "Committee on Environment and Public Works" ~ 11,
                                  committee == "Committee on Governmental Affairs" ~ 8,
                                  committee == "Committee on Energy and Natural Resources" ~ 14,
                                  committee == "Committee on Oversight and Government Reform" ~ 18,
                                  committee == "Select Committee on Homeland Security"  & chamber == 'HOUSE' ~ 22,
                                  committee == "Select Committee on Homeland Security" & chamber == 'SENATE' ~ 8,
                                  committee == "Committee on Health, Education, Labor, and Pensions" ~ 7,
                                  committee == "Committee on Agriculture" & chamber == 'HOUSE' ~ 19,
                                  committee == "Committee on Agriculture" & chamber == 'SENATE' ~ 13,
                                  committee == "Committee on International Relations" & chamber == 'HOUSE' ~ 6,
                                  committee == "Committee on International Relations" & chamber == 'SENATE' ~ 9,
                                  committee == "Select Committee on Intelligence" ~ -99,
                                  committee == "Committee on Science and Technology" ~ 21,
                                  committee == "Committee on  Commerce, Science, and Transportation" ~ 4,
                                  committee == "Committee on Finance" & chamber == 'SENATE'~ 1,
                                  committee == "Committee on Small Business and Entrepreneurship" & chamber == 'HOUSE'~ 23,
                                  committee == "Committee on Small Business and Entrepreneurship" & chamber == 'HOUSE'~ 16,
                                  committee == "Committee on the Budget" & chamber == 'HOUSE'~ 13,
                                  committee == "Committee on the Budget" & chamber == 'SENATE'~ 15))


df <- df %>%
  mutate (commit_val = ifelse(commit_rank %in% c(1:5), 'high', 
                              ifelse (commit_rank %in% c(6:12), 'medium', 'low')))


table(df$commit_val)


df_committee <- df %>%
  group_by(affiliation, commit_val) %>%
  summarise (count= n()) 

# find total number of D witnesses
high_tot <- table(df$commit_val)[1]
medium_tot <- table(df$commit_val)[3]
low_tot <-table(df$commit_val)[2]

df_committee <- df_committee %>%
  mutate (proportion = ifelse (commit_val=="high", count/high_tot, 
                               ifelse (commit_val=="low",count/low_tot,count/medium_tot)))

df_committee <- df_committee %>%
  mutate (se = ifelse (commit_val=="high", binomial_se(proportion,high_tot),
                       ifelse (commit_val=="low",
                               binomial_se(proportion,low_tot),binomial_se(proportion,medium_tot))),
          ci_lower = proportion - 1.96*se,
          ci_upper = proportion + 1.96*se) 

# Re arrange order
df_committee$affiliation <- reorder.factor(df_committee$affiliation, new.order=affiliations_list)
df_committee <- df_committee %>%
  arrange(affiliation) 

# Get rid of NAs and others
df_committee <- df_committee %>%
  drop_na()


plot_committee <- ggplot(df_committee, aes(proportion, affiliation, color = commit_val)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
  theme_bw() +
  scale_color_manual (values = c("magenta2", "palegreen2", "skyblue2"), 
                      name = "Value of Committee") +
  xlab ("Proportion among witnesses invited by the same type of committee") +
  ylab ("Affiliation") +
  labs(caption="Proportion in the sample and binomial SEs for proportions, 95% CIs") +
  ggtitle("Affiliations of witnesses, by Committee value",
          "Witnesses invited by one type of committee sum to 1, N=4569") +
  theme(legend.position = "bottom",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        plot.title = element_text(size = 15, face = "bold"))


plot_committee



# Affiliations, by Chamber ------------------------------------------------

df_chamb <- df %>%
  group_by(affiliation, chamber) %>%
  summarise (count= n()) 

# Drop Joint committees
df_chamb <- df_chamb %>%
  filter (chamber != "JOINT")

# find total number of House witnesses
H_wit <- with(df_chamb, sum(count[chamber=="HOUSE"]))
S_wit <- with(df_chamb, sum(count[chamber=="SENATE"]))

df_chamb <- df_chamb %>%
  mutate (proportion = ifelse (chamber=="HOUSE", count/H_wit, count/S_wit))

df_chamb <- df_chamb %>%
  mutate (se = ifelse (chamber=="HOUSE", binomial_se_fpc(proportion,H_wit, 61000),binomial_se_fpc(proportion,S_wit, 61000)),
          ci_lower = proportion - 1.96*se,
          ci_upper = proportion + 1.96*se) 


df_chamb$affiliation <- reorder.factor(df_chamb$affiliation, new.order=affiliations_list)
df_chamb <- df_chamb %>%
  arrange(affiliation) 


plot_chamb <- ggplot(df_chamb, aes(proportion, affiliation, color = chamber)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
  theme_bw() +
  scale_color_manual (values = c("black", 'green')) +
  xlab ("Proportion among witnesses invited by the same chamber)") +
  ylab ("Affiliation") +
  labs(caption="Proportion in the sample and binomial SEs for proportions, 95% CIs") +
  ggtitle("Affiliations of witnesses, by Chamber",
          "Witnesses invited by one Chamber (House or Senate) sum to 1, N=4569") +
  theme(legend.position = "bottom",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


plot_chamb



# Affiliations, by divided gvt --------------------------------------------


df_exe <- df %>%
  group_by(affiliation, Party, WH) %>%
  summarise (count= n()) %>%
  drop_na(Party)


# find total number of witnesses in each combinations in df_exe
DD_wit <- with(df_exe, sum(count[Party=="D"&WH=="D"], na.rm=T))
DR_wit <- with(df_exe, sum(count[Party=="D"&WH=="R"], na.rm=T))
RD_wit <- with(df_exe, sum(count[Party=="R"&WH=="D"], na.rm=T))
RR_wit <- with(df_exe, sum(count[Party=="R"&WH=="R"], na.rm=T))

df_exe <- df_exe %>%
  mutate (proportion = if(Party=="D"&WH=="D"){count/DD_wit} else {
    if (Party=="D"&WH=="R"){count/DR_wit} else {
      if (Party=="R"&WH=="D"){count/RD_wit} else {
        if (Party=="R"&WH=="R"){count/RR_wit} else {
          NA
        }
      }}})




df_exe <- df_exe %>%
  mutate (se = if(Party=="D"&WH=="D"){binomial_se_fpc(proportion,DD_wit,61000)} else {
    if (Party=="D"&WH=="R"){binomial_se_fpc(proportion,DR_wit,61000)} else {
      if (Party=="R"&WH=="D"){binomial_se_fpc(proportion,RD_wit,61000)} else {
        if (Party=="R"&WH=="R"){binomial_se_fpc(proportion,RR_wit,61000)}
      }}})


df_exe <- df_exe %>%
  mutate (ci_lower = proportion - 1.96*se,
          ci_upper = proportion + 1.96*se) 

df_exe$affiliation <- reorder.factor(df_exe$affiliation, new.order=affiliations_list)
df_exe <- df_exe %>%
  arrange(affiliation) 



# need to change name of the legend for facet wrap (from D and R to "Democratic-Controlled Chamber")
# Same with legend of groups, not clear enough, I want 'Party in control of the WH"

party.labs <- list("Democratic-Controlled Chambers", "Republican-Controlled Chambers")
party_labeller <- function(variable,value){
  return(party.labs[value])
}

#plot_divided <- ggplot(df_exe, aes(proportion, affiliation, color = WH)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
  facet_wrap(df_exe$Party, labeller = party_labeller) +
  theme_bw() +
  xlab ("Proportion in the sample, among witnesses invited by same party and under same President's party") +
  ylab ("Affiliation") +
  labs(caption="Proportion in the sample and binomial SEs for proportions, 95% CIs") +
  ggtitle("Affiliations of witnesses, by Party in Control of the Chamber and the WH",
          "Witnesses invited by one Party and when President of one Party sum to 1, N=846") +
  scale_color_manual(name="Party in control of the WH",
                       values = c("blue", "red")) +
  theme(legend.position = "bottom",
          legend.text=element_text(size=10,hjust = 0), 
          plot.caption = element_text(hjust = 0),
          legend.title = element_blank(),
          plot.title = element_text(size = 15, face = "bold"))

#plot_divided


df_exe2 <- df_exe %>%
  filter(affiliation == 'Executive Agency')

plot_divided2 <- ggplot(df_exe2, aes(proportion, WH)) +
  geom_point(color = c('blue', 'red', 'blue', 'red')) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0, color = c('blue', 'red', 'blue', 'red')) +
  facet_wrap(df_exe2$Party, labeller = party_labeller)   +
  xlab ("Proportion of executive agency witnesses in the sample, 
        among witnesses invited by same party and under same President's party") +
  ylab ("Party in control of the White House") +
  ggtitle("Affiliations of witnesses, by Party in Control of the Chamber and the WH",
          "N=4569") +
  theme_bw()
  

plot_divided2




# Affiliations, by Year -----------------------------------------------


# Need to regroup TT and PI, regroup them in main DF

df2 <- df
df2$affiliation <- as.character(df2$affiliation)


df2$affiliation[df2$affiliation=="Left leaning TT"] <- "Think Tank"
df2$affiliation[df2$affiliation=="Right leaning TT"] <- "Think Tank"
df2$affiliation[df2$affiliation=="Moderate leaning TT"] <- "Think Tank"
df2$affiliation[df2$affiliation=="Public Interest - neutral"] <- "Public Interest"
df2$affiliation[df2$affiliation=="Public Interest - liberal"] <- "Public Interest"
df2$affiliation[df2$affiliation=="Public Interest - conservative"] <- "Public Interest"



df_year <- df2 %>%
  group_by(year, affiliation) %>%
  summarise (count= n()) %>%
  mutate (year_n = sum(count[affiliation==affiliation]),
          proportion = count/year_n)



# Start in 2001 bc lack of data earlier
df_year  <- df_year  %>%
  filter (year>2000)




# Then plot by groups of affilation based on substantive closenesss

df_year_insti <- df_year %>%
  filter (affiliation == "Other institutional - federal" |
            affiliation =="Other institutional - state and local" |
            affiliation == "Executive Agency")

plot_year_insti <- ggplot(df_year_insti,aes(x = year, y = proportion, color = affiliation)) + 
  geom_point() + 
  stat_smooth(method="lm_robust",formula=y ~ poly(x, 3)) +
  #stat_smooth(method="glm",family="binomial",link="logit",formula=y ~ poly(x, 3)) +
  #geom_smooth(method = glm, position ="identity", formula = y ~ poly(x, 3)) +
  theme_bw() +
  ylab ("Proportion among witnesses invited within the year") +
  xlab ("Year") +
  labs(caption="OLS regression with robust SEs") +
  ggtitle("Proportion among witnesses invited within the year",
          "Institutional witnesses, N=4569") +
  scale_color_manual(values = getPalette(colourCount)) + 
  theme(legend.position = "bottom",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))
        

plot_year_insti



df_year_private <- df_year %>%
  filter (affiliation == "Corporation" |
            affiliation =="Occupation Association" |
            affiliation == "Trade Association")



plot_year_private <- ggplot(df_year_private,aes(x = year, y = proportion, color = affiliation)) + 
  geom_point() + 
  stat_smooth(method="lm_robust",formula=y ~ poly(x, 3)) +
  #geom_smooth(method = glm, position ="identity", formula = y ~ poly(x, 3)) +
  theme_bw() +
  ylab ("Proportion among  witnesses invited within the year") +
  xlab ("Year") +
  labs(caption="OLS regression with robust SEs") +
  ggtitle("Proportion among witnesses invited within the year",
          "Private sector witnesses, N=4569") +
  scale_color_manual(values = getPalette(colourCount)) +
  theme(legend.position = "bottom",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))

plot_year_private



df_year_advo <- df_year %>%
  filter (affiliation == "Academic" |
            affiliation =="Think Tank" |
            affiliation == "Public Interest")
  
  
plot_year_advo <- ggplot(df_year_advo,aes(x = year, y = proportion, color = affiliation)) + 
  geom_point() + 
  stat_smooth(method="lm_robust",formula=y ~ poly(x, 3)) +
  #geom_smooth(method = glm, position ="identity", formula = y ~ poly(x, 3)) +
  theme_bw() +
  ylab ("Proportion among witnesses invited within the year") +
  xlab ("Year") +
  labs(caption="OLS regression with robust SEs") +
  ggtitle("Proportion among  witnesses invited within the year",
          "Advocacy group witnesses, N=4569") +
  scale_color_manual(values = getPalette(colourCount)) +
  theme(legend.position = "bottom",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))

plot_year_advo




df_year_interest <- df_year %>%
  filter (affiliation == "Executive Agency" |
            affiliation =="Other institutional - state and local" |
            affiliation == "Corporation" |
            affiliation == "Trade Association")


plot_year_interest <- ggplot(df_year_interest,aes(x = year, y = proportion, color = affiliation)) + 
  geom_point() + 
  stat_smooth(method="lm_robust",formula=y ~ poly(x, 3)) +
  #geom_smooth(method = glm, position ="identity", formula = y ~ poly(x, 3)) +
  theme_bw() +
  ylab ("Proportion among witnesses invited within the year") +
  xlab ("Year") +
  labs(caption="OLS regression with robust SEs") +
  ggtitle("Proportion among witnesses invited within the year",
          "N=4569") +
  scale_color_manual(values = getPalette(colourCount)) +
  theme(legend.position = "bottom",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))

plot_year_interest





# Before After elections --------------------------------------------------

df3 <- df

sapply(df3$month, class)

df3$month <- as.numeric(df3$month)
df3$year <- as.numeric(df3$year)

months_pre <- c(4,5,6,7,8,9,10)
months_post <- c(11,12)
month_post_year <- c(1,2,3,4,5)

months_pre_presi <- c(1,2,3,4,5,6,7,8,9,10)
months_post_presi <- c(11,12)
month_post_year_presi <- c(1,2,3,4,5,6,8,9,10)

election_years <- c(1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018)
presi_years <- c(1996,2000,2004,2008,2012,2016)



# Avril of election year to election : before.
# Nov of election year to july of post election year : post

df_elections <- df3 %>%
  mutate (election = ifelse(year %in% election_years & month %in% months_pre,"Pre",
                            ifelse(year %in% election_years & month %in% months_post,"Post",
                                   ifelse(year %in% (election_years+1) & month %in% month_post_year,"Post", "Between")))) 


df_presi <- df3 %>%
  mutate (election = ifelse(year %in% presi_years & month %in% months_pre_presi,"Pre",
                            ifelse(year %in% presi_years & month %in% months_post_presi,"Post",
                                   ifelse(year %in% (presi_years+1) & month %in% month_post_year_presi,"Post", "Between")))) 


##########
# All elections
##############

df_elec <- df_elections %>%
  group_by(affiliation, election) %>%
  summarise (count= n()) 


# find total number of House witnesses
pre_wit <- with(df_elec, sum(count[election=="Pre"]))
post_wit <- with(df_elec, sum(count[election=="Post"]))
btw_wit <- with(df_elec, sum(count[election=="Between"]))



df_elec <- df_elec %>%
  mutate (proportion = ifelse (election=="Pre", count/pre_wit, 
                               ifelse(election=="Post", count/post_wit,count/btw_wit)))

df_elec <- df_elec %>%
  mutate (se = ifelse (election=="Pre", binomial_se(proportion,pre_wit),
                       ifelse(election=="Post", binomial_se(proportion,post_wit),
                              binomial_se(proportion,btw_wit))),
          ci_lower = proportion - 1.96*se,
          ci_upper = proportion + 1.96*se) 


df_elec$affiliation <- reorder.factor(df_elec$affiliation, new.order=affiliations_list)
df_elec <- df_elec %>%
  arrange(affiliation) 


plot_elec <- ggplot(df_elec, aes(proportion, affiliation, color = election)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
  theme_bw() +
  scale_color_manual (values = c("lightskyblue3", 'salmon3', 'darkseagreen3')) +
  xlab ("Proportion among witnesses invited by the same chamber)") +
  ylab ("Affiliation") +
  labs(caption="Proportion in the sample and binomial SEs for proportions, 95% CIs") +
  ggtitle("Affiliations of witnesses, by Chamber",
          "Witnesses invited during one environment sum to 1, 
          N=1472") +
  theme(legend.position = "bottom",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


plot_elec






# By margin of seats ------------------------------------------------------
# Get margin of seats for Dems in each Congress

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
df$Margin_seats[df$info=="114_senate_hearings"] <- 55-45
df$Margin_prop[df$info=="114_senate_hearings"] <- (55-45)/100
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


hist(df$Margin_prop)


# LEt's look at acad, TT, public interest

df_thought <- df_margin %>%
  filter(affiliation == c('Academic', 'Left leaning TT', 'Moderate leaning TT', 'Right leaning TT'))

plot_margin <- ggplot(df_thought, aes(Margin_prop, count, color = affiliation)) +
  geom_point() 


plot_margin
