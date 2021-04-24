rm(list=ls())


library(tidyverse)
library(DeclareDesign)
library(tm)
library(tidytext)
library(stringr)
library(gridExtra)
library(textdata)
library(remotes)
library(gdata)

############
# Build tokens of words
############
se <- function(x){sd(x, na.rm = T)/sqrt(length(x))}


all_txts <- list.files(pattern = ".text$")



all_txts <- map_df(all_txts, ~ data_frame(txt = read_file(.x)) %>%
                     mutate(filename = basename(.x)))  %>%
  unnest_tokens(word, txt)


all_txts <- all_txts %>%
  mutate(id = str_remove_all(filename, ".text"))



all_txts <- all_txts %>%
  anti_join(stop_words)

all_txts <- all_txts %>%
  filter(!word %in% c('tiff', 'ms', 'mr', 'hon', 'html'))


most_words <- all_txts %>%
  count(word, sort = TRUE)




############
# Merge this with existing data on hearings
############
hearings <- read.csv('df_hearing.csv')



all_txts <- merge(all_txts, hearings, by ='id')

# Check that I didn't loose hearings
unique(all_txts$id)


# Evidence dic ------------------------------------------------------------



############
# Build dictionnary - evidence
############



# Use words I want in dictionary (using string rules, see regex metacharacters here or John Handerson's slides: https://r4ds.had.co.nz/strings.html)
evidence_dictionary <- 'evidence|evidence-based|research*|data|randomiz*|randomis*|
                        doctor|professor|trade-off|hypo*|study|science|scienti*|
                        evaluation|rigor*|experiment*|quantitative|methods|analytic*|
                        methodo*|measur*|results|expert*|model*|indicator*|analys*|
                        impact|studies|statistic*|empiric*|percent*'



# Then add a logical column for if each word in tokenized df is in the dictionnary 
all_txts$sc_word <- grepl(pattern = evidence_dictionary, all_txts$word)

# Also add a dummy variable
all_txts$sc_word_num <- as.numeric(all_txts$sc_word)

# Check that dummy and logical factor are the same thing
mean(all_txts$sc_word)
mean(all_txts$sc_word_num)


############
# By mean ideology of wit
# Proportion of words in hearing
############

unique(all_txts$id)

ideo_science <- all_txts %>%
  group_by(id) %>%
  summarize(science_perc = (mean(sc_word, na.rm = T)))

ideo_science <- merge(hearings,ideo_science)



############
# Regression
############
colnames(ideo_science)


############
# Standardize ind. var
############
ideo_science$ideo_hearing_s <- with(ideo_science,(ideo_hearing - mean(ideo_hearing, na.rm = T))/(sd(ideo_hearing, na.rm = T)))
ideo_science$Margin_prop_s <- with(ideo_science,(Margin_prop - mean(Margin_prop, na.rm = T))/(sd(Margin_prop, na.rm = T)))
ideo_science$female_hearing_s <- with(ideo_science,(female_hearing - mean(female_hearing, na.rm = T))/(sd(female_hearing, na.rm = T)))
ideo_science$white_hearing_s <- with(ideo_science,(white_hearing - mean(white_hearing, na.rm = T))/(sd(white_hearing, na.rm = T)))



# Replace NA by -99

fit_evidence <- lm(science_perc~ ideo_hearing_s + Margin_prop_s + balance_pers + female_hearing_s + white_hearing_s + 
     Party + year + chamber + committee, data = ideo_science)

summary(fit_evidence)

library(sandwich)
library(stargazer)

cov_evidence = vcovHC(fit_evidence, type = "HC") 
robust_se_evidence = sqrt(diag(cov_evidence))



coef_sc_ideo <- fit_evidence$coefficients[2]
robust_sc_ideo = sqrt(diag(cov_evidence))[2]

coef_sc_margin <- fit_evidence$coefficients[3]
robust_sc_margin = sqrt(diag(cov_evidence))[3]


coef_sc_balance1 <- fit_evidence$coefficients[4]
robust_sc_balance1 = sqrt(diag(cov_evidence))[4]


coef_sc_balance2 <- fit_evidence$coefficients[5]
robust_sc_balance2 = sqrt(diag(cov_evidence))[5]


coef_sc_female <- fit_evidence$coefficients[6]
robust_sc_female = sqrt(diag(cov_evidence))[6]


coef_sc_white <- fit_evidence$coefficients[7]
robust_sc_white = sqrt(diag(cov_evidence))[7]





# Pro-social dic ----------------------------------------------------------



############
# Build dictionnary - pro social
############

# https://www.pnas.org/content/pnas/112/21/6591.full.pdf

prosocial_dictionary <- 'accepting| accommodat*| affect*|
agreeable*| aid*| altruis*| appreciat*| approachable| assist*| benefit*|
  benevolen*| biodivers*| care| caring| charit*| collective*| commun*|
  compassion*|compliment| concern*| confide| conscien*| conservation*|
  considerate| contribut*| cooperat*| cope*| coping| courteous*| courtesy|
  defend*| dependab*|dignity| donat*| earth| ecolog*| education*| egalitar*|
  empath*| empower*|encourag*| environment*| equal*| ethic*| everybod*|
  everyone*| facilitat*|fair*| forgiv*| freed*| genero*| gentle*| genuin*| giv*|
  goodhearted*| greater good| guard*| harmon*| help*| helpful*| honest*|
  honorable| honourable|hospit*| human*| impartial*| inspiring| integrat*|
  integrity| interact*| invit*|involv*| justice| kids| kindness| listen*| loyal*|
  moral*| NGO*| nice*| nonjudgmental| nonprofit*| not-for-profit*| nurtur*|
  peace*| philanthrop*| prais*|prejud*| protect*| reciproc*| relia*| relied| 
rely| respectful*| responsib*| responsiv*| righteous*| rights| role model*| 
  selfless*| sensitiv*| serv*| share*| shari*| shield*| sincer*| societ*|
  solidarit*| support*| sustainab*| sympath*| taught|
teach*| team*| tender*| the people| therap*| thoughtful*| tolera*| trust*|
  tutor*|underst*| universal*| unprejudiced| upright| virtuous*| volunteer*'


# Then add a logical column for if each word in tokenized df is in the dictionnary 
all_txts$soc_word <- grepl(pattern = prosocial_dictionary, all_txts$word)

# Also add a dummy variable
all_txts$soc_word_num <- as.numeric(all_txts$soc_word)

# Check that dummy and logical factor are the same thing
mean(all_txts$soc_word)
mean(all_txts$soc_word_num)



unique(all_txts$id)

ideo_social <- all_txts %>%
  group_by(id) %>%
  summarize(social_perc = (mean(soc_word_num, na.rm = T)))

ideo_social <- merge(hearings,ideo_social)



ideo_social$ideo_hearing_s <- with(ideo_social,(ideo_hearing - mean(ideo_hearing, na.rm = T))/(sd(ideo_hearing, na.rm = T)))
ideo_social$Margin_prop_s <- with(ideo_social,(Margin_prop - mean(Margin_prop, na.rm = T))/(sd(Margin_prop, na.rm = T)))
ideo_social$female_hearing_s <- with(ideo_social,(female_hearing - mean(female_hearing, na.rm = T))/(sd(female_hearing, na.rm = T)))
ideo_social$white_hearing_s <- with(ideo_social,(white_hearing - mean(white_hearing, na.rm = T))/(sd(white_hearing, na.rm = T)))





colnames(ideo_social)

fit_social <- lm(social_perc~ ideo_hearing_s + Margin_prop_s + balance_pers + female_hearing_s + white_hearing_s + 
             Party + year + chamber + committee, data = ideo_social)

summary(fit_social)

cov_social = vcovHC(fit_social, type = "HC") 
robust_se_social = sqrt(diag(cov_social))



coef_soc_ideo <- fit_social$coefficients[2]
robust_soc_ideo = sqrt(diag(cov_social))[2]

coef_soc_margin <- fit_social$coefficients[3]
robust_soc_margin = sqrt(diag(cov_social))[3]

coef_soc_balance1 <- fit_social$coefficients[4]
robust_soc_balance1 = sqrt(diag(cov_social))[4]

coef_soc_balance2 <- fit_social$coefficients[5]
robust_soc_balance2 = sqrt(diag(cov_social))[5]

coef_soc_female <- fit_social$coefficients[6]
robust_soc_female = sqrt(diag(cov_social))[6]

coef_soc_white <- fit_social$coefficients[7]
robust_soc_white = sqrt(diag(cov_social))[7]




# Sentiment NRC dic ------------------------------------------------------------



sentiment_h <- all_txts %>%
  inner_join(get_sentiments("nrc")) 

table(sentiment_h$sentiment)

ideo_sentiment <- sentiment_h %>%
  group_by(id) %>%
  summarize (fear_prc = sum(sentiment == 'fear')/n(),
             anger_prc =  sum(sentiment == 'anger')/n(),
             anticipation_prc =  sum(sentiment == 'anticipation')/n(),
             disgust_prc =  sum(sentiment == 'disgust')/n(),
             joy_prc =  sum(sentiment == 'joy')/n(),
             negative_prc =  sum(sentiment == 'negative')/n(),
             positive_prc =  sum(sentiment == 'positive')/n(),
             sadness_prc =  sum(sentiment == 'sadness')/n(),
             surprise_prc =  sum(sentiment == 'surprise')/n(),
             trust_prc =  sum(sentiment == 'trust')/n())


ideo_sentiment <- merge(hearings,ideo_sentiment)


ideo_sentiment$ideo_hearing_s <- with(ideo_sentiment,(ideo_hearing - mean(ideo_hearing, na.rm = T))/(sd(ideo_hearing, na.rm = T)))
ideo_sentiment$Margin_prop_s <- with(ideo_sentiment,(Margin_prop - mean(Margin_prop, na.rm = T))/(sd(Margin_prop, na.rm = T)))
ideo_sentiment$female_hearing_s <- with(ideo_sentiment,(female_hearing - mean(female_hearing, na.rm = T))/(sd(female_hearing, na.rm = T)))
ideo_sentiment$white_hearing_s <- with(ideo_sentiment,(white_hearing - mean(white_hearing, na.rm = T))/(sd(white_hearing, na.rm = T)))




# Fear
fit_fear <- lm(fear_prc~ ideo_hearing_s + Margin_prop_s + balance_pers + female_hearing_s + white_hearing_s + 
                   Party + year + chamber + committee, data = ideo_sentiment)

summary(fit_fear)

cov_fear = vcovHC(fit_fear, type = "HC") 
robust_se_fear = sqrt(diag(cov_fear))


coef_fear_ideo <- fit_fear$coefficients[2]
robust_fear_ideo = sqrt(diag(cov_fear))[2]

coef_fear_margin <- fit_fear$coefficients[3]
robust_fear_margin = sqrt(diag(cov_fear))[3]

coef_fear_balance1 <- fit_fear$coefficients[4]
robust_fear_balance1 = sqrt(diag(cov_fear))[4]

coef_fear_balance2 <- fit_fear$coefficients[5]
robust_fear_balance2 = sqrt(diag(cov_fear))[5]

coef_fear_female <- fit_fear$coefficients[6]
robust_fear_female = sqrt(diag(cov_fear))[6]

coef_fear_white <- fit_fear$coefficients[7]
robust_fear_white = sqrt(diag(cov_fear))[7]



# Anger
fit_anger <- lm(anger_prc~ ideo_hearing_s + Margin_prop_s + balance_pers + female_hearing_s + white_hearing_s + 
                  Party + year + chamber + committee, data = ideo_sentiment)

summary(fit_anger)

cov_anger = vcovHC(fit_anger, type = "HC") 
robust_se_anger = sqrt(diag(cov_anger))

# Anticipation
fit_anticipation <- lm(anticipation_prc~ ideo_hearing_s + Margin_prop_s + balance_pers + female_hearing_s + white_hearing_s + 
                         Party + year + chamber + committee, data = ideo_sentiment)

summary(fit_anticipation)

cov_anticipation = vcovHC(fit_anticipation, type = "HC") 
robust_se_anticipation = sqrt(diag(cov_anticipation))


# Disgust
fit_disgust <- lm(disgust_prc~ ideo_hearing_s + Margin_prop_s + balance_pers + female_hearing_s + white_hearing_s + 
                    Party + year + chamber + committee, data = ideo_sentiment)

summary(fit_disgust)

cov_disgust = vcovHC(fit_disgust, type = "HC") 
robust_se_disgust = sqrt(diag(cov_disgust))



# Joy
fit_joy <- lm(joy_prc~ ideo_hearing_s + Margin_prop_s + balance_pers + female_hearing_s + white_hearing_s + 
                Party + year + chamber + committee, data = ideo_sentiment)

summary(fit_joy)

cov_joy = vcovHC(fit_joy, type = "HC") 
robust_se_joy = sqrt(diag(cov_joy))



# Negative
fit_negative <- lm(negative_prc ~ ideo_hearing_s + Margin_prop_s + balance_pers + female_hearing_s + white_hearing_s + 
                     Party + year + chamber + committee, data = ideo_sentiment)

summary(fit_negative)

cov_negative = vcovHC(fit_negative, type = "HC") 
robust_se_negative = sqrt(diag(cov_negative))


coef_negative_ideo <- fit_negative$coefficients[2]
robust_negative_ideo = sqrt(diag(cov_negative))[2]

coef_negative_margin <- fit_negative$coefficients[3]
robust_negative_margin = sqrt(diag(cov_negative))[3]

coef_negative_balance1 <- fit_negative$coefficients[4]
robust_negative_balance1 = sqrt(diag(cov_negative))[4]

coef_negative_balance2 <- fit_negative$coefficients[5]
robust_negative_balance2 = sqrt(diag(cov_negative))[5]

coef_negative_female <- fit_negative$coefficients[6]
robust_negative_female = sqrt(diag(cov_negative))[6]

coef_negative_white <- fit_negative$coefficients[7]
robust_negative_white = sqrt(diag(cov_negative))[7]



# Positive
fit_positive <- lm(positive_prc ~ ideo_hearing_s + Margin_prop_s + balance_pers + female_hearing_s + white_hearing_s + 
                     Party + year + chamber + committee, data = ideo_sentiment)

summary(fit_positive)

cov_positive = vcovHC(fit_positive, type = "HC") 
robust_se_positive = sqrt(diag(cov_positive))



coef_positive_ideo <- fit_positive$coefficients[2]
robust_positive_ideo = sqrt(diag(cov_positive))[2]

coef_positive_margin <- fit_positive$coefficients[3]
robust_positive_margin = sqrt(diag(cov_positive))[3]

coef_positive_balance1 <- fit_positive$coefficients[4]
robust_positive_balance1 = sqrt(diag(cov_positive))[4]

coef_positive_balance2 <- fit_positive$coefficients[5]
robust_positive_balance2 = sqrt(diag(cov_positive))[5]

coef_positive_female <- fit_positive$coefficients[6]
robust_positive_female = sqrt(diag(cov_positive))[6]

coef_positive_white <- fit_positive$coefficients[7]
robust_positive_white = sqrt(diag(cov_positive))[7]






# Sadness
fit_sadness <- lm(sadness_prc ~ ideo_hearing_s + Margin_prop_s + balance_pers + female_hearing_s + white_hearing_s + 
                    Party + year + chamber + committee, data = ideo_sentiment)

summary(fit_sadness)

cov_sadness = vcovHC(fit_sadness, type = "HC") 
robust_se_sadness = sqrt(diag(cov_sadness))



# Surprise
fit_surprise <- lm(surprise_prc ~ideo_hearing_s + Margin_prop_s + balance_pers + female_hearing_s + white_hearing_s + 
                     Party + year + chamber + committee, data = ideo_sentiment)

summary(fit_surprise)

cov_surprise = vcovHC(fit_surprise, type = "HC") 
robust_se_surprise = sqrt(diag(cov_surprise))



# Trust
fit_trust <- lm(trust_prc ~ ideo_hearing_s + Margin_prop_s + balance_pers + female_hearing_s + white_hearing_s + 
                  Party + year + chamber + committee, data = ideo_sentiment)

summary(fit_trust)

cov_trust = vcovHC(fit_trust, type = "HC") 
robust_se_trust = sqrt(diag(cov_trust))






# Table ----------------------------------------------------------

table_dic <- stargazer(fit_evidence, fit_social,
          se = list(robust_se_evidence, robust_se_social),
          dep.var.labels = c("Evidence-based","Pro-social"),
          align = TRUE,
          header = TRUE,
          single.row = TRUE,
          omit.stat = c("f", "rsq", "ser"),
          type = "latex")



table_nrc_pos <- stargazer(fit_anticipation,fit_joy,
                       fit_positive, fit_surprise,fit_trust,
                       se = list(robust_se_anticipation,
                                 robust_se_joy,
                                 robust_se_positive,robust_se_surprise,
                                 robust_se_trust),
                       align = TRUE,
                       header = TRUE,
                       single.row = TRUE,
                       omit.stat = c("f", "rsq", "ser"),
                       type = "latex")


table_nrc_neg <- stargazer(fit_fear, fit_anger, fit_disgust,
                           fit_negative,fit_sadness,
                           se = list(robust_se_fear, robust_se_anger,
                                     robust_se_disgust, robust_se_negative,
                                     robust_se_sadness
                                     ),
                           align = TRUE,
                           header = TRUE,
                           single.row = TRUE,
                           omit.stat = c("f", "rsq", "ser"),
                           type = "latex")






# Graphs - Ideo -----------------------------------------------------------------


# Scientific


Treatment <-  c('Ideology of witnesses', 'Margin of seats', 'One sided hearing', 'Two sided hearing')

Coefficient <- c(coef_sc_ideo, coef_sc_margin, coef_sc_balance1, coef_sc_balance2)
SE <- c(robust_sc_ideo, robust_sc_margin, robust_sc_balance1, robust_sc_balance2)

df_sc <- data.frame(Treatment, Coefficient, SE)

df_sc <- df_sc %>%
  mutate (ci_lower = Coefficient - 1.96*SE,
          ci_upper = Coefficient + 1.96*SE) 

# Coef graphs -------------------------------------------------------------
# Lock in order of treatments to avoid alphabetical plot
#df_reg$Treatment <- factor(df_reg$Treatment , levels = df_reg$Treatment)


# Plot this 
plot_sc <- ggplot(df_sc, aes(Coefficient, Treatment)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
  theme_bw() +
  xlab ("Coefficient in regression") +
  ylab ("Independent variable") +
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "red") +
  ggtitle("Proportion of evidence-based words in hearing") +
  theme(plot.title = element_text(size=8))



plot_sc


# Pro social



Coefficient_soc <- c(coef_soc_ideo, coef_soc_margin, coef_soc_balance1, 
                     coef_soc_balance2)
SE_soc <- c(robust_soc_ideo, robust_soc_margin, robust_soc_balance1, 
            robust_soc_balance2)

df_soc <- data.frame(Treatment, Coefficient_soc, SE_soc)

df_soc <- df_soc %>%
  mutate (ci_lower = Coefficient_soc - 1.96*SE_soc,
          ci_upper = Coefficient_soc + 1.96*SE_soc) 

# Coef graphs -------------------------------------------------------------
# Lock in order of treatments to avoid alphabetical plot
#df_reg$Treatment <- factor(df_reg$Treatment , levels = df_reg$Treatment)


# Plot this 
plot_soc <- ggplot(df_soc, aes(Coefficient_soc, Treatment)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
  theme_bw() +
  xlab ("Coefficient in regression") +
  ylab ("") +
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "red") +
  ggtitle("Proportion of prosocial words in hearing") +
  theme(plot.title = element_text(size=8))



plot_soc



# Fear



Coefficient_fear <- c(coef_fear_ideo, coef_fear_margin, coef_fear_balance1, 
                      coef_fear_balance2)
SE_fear <- c(robust_fear_ideo, robust_fear_margin, robust_fear_balance1, 
             robust_fear_balance2)

df_fear <- data.frame(Treatment, Coefficient_fear, SE_fear)

df_fear <- df_fear %>%
  mutate (ci_lower = Coefficient_fear - 1.96*SE_fear,
          ci_upper = Coefficient_fear + 1.96*SE_fear) 

# Coef graphs -------------------------------------------------------------
# Lock in order of treatments to avoid alphabetical plot
#df_reg$Treatment <- factor(df_reg$Treatment , levels = df_reg$Treatment)


# Plot this 
plot_fear <- ggplot(df_fear, aes(Coefficient_fear, Treatment)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
  theme_bw() +
  xlab ("Coefficient in regression") +
  ylab ("Independent variable") +
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "red") +
  ggtitle("Proportion of fear words in hearing") +
  theme(plot.title = element_text(size=8))



plot_fear






# Negative



Coefficient_neg <- c(coef_negative_ideo, coef_negative_margin, coef_negative_balance1, 
                     coef_negative_balance2)
SE_neg <- c(robust_negative_ideo, robust_negative_margin, robust_negative_balance1, 
            robust_negative_balance2)

df_neg <- data.frame(Treatment, Coefficient_neg, SE_neg)

df_neg <- df_neg %>%
  mutate (ci_lower = Coefficient_neg - 1.96*SE_neg,
          ci_upper = Coefficient_neg + 1.96*SE_neg) 

# Coef graphs -------------------------------------------------------------
# Lock in order of treatments to avoid alphabetical plot
#df_reg$Treatment <- factor(df_reg$Treatment , levels = df_reg$Treatment)


# Plot this 
plot_neg <- ggplot(df_neg, aes(Coefficient_neg, Treatment)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
  theme_bw() +
  xlab ("Coefficient in regression") +
  ylab ("") +
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "red") +
  ggtitle("Proportion of negative words in hearing") +
  theme(plot.title = element_text(size=8))



plot_neg




# Positive



Coefficient_pos <- c(coef_positive_ideo, coef_positive_margin, coef_positive_balance1, 
                     coef_positive_balance2)
SE_pos <- c(robust_positive_ideo, robust_positive_margin, robust_positive_balance1, 
            robust_positive_balance2)

df_pos <- data.frame(Treatment, Coefficient_pos, SE_pos)

df_pos <- df_pos %>%
  mutate (ci_lower = Coefficient_pos - 1.96*SE_pos,
          ci_upper = Coefficient_pos + 1.96*SE_pos) 

# Coef graphs -------------------------------------------------------------
# Lock in order of treatments to avoid alphabetical plot
#df_reg$Treatment <- factor(df_reg$Treatment , levels = df_reg$Treatment)


# Plot this 
plot_pos <- ggplot(df_pos, aes(Coefficient_pos, Treatment)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
  theme_bw() +
  xlab ("Coefficient in regression") +
  ylab ("") +
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "red") +
  labs(caption="Coefficients in a regression with controls, 95% confidence intervals with robust SEs") +
  ggtitle("Proportion of positive words in hearing") +
  theme(plot.title = element_text(size=8))



plot_pos



# PLot sum
library(patchwork)

plots_sum_ideo <- (plot_sc | plot_soc) / (plot_neg | plot_pos) 

plots_sum_ideo







# Graphs - Representation -----------------------------------------------------------------

# Scientific


Treatment2 <-  c('Percentage female witnesses', 'Percentage white witnesses')

Coefficient2 <- c(coef_sc_female, coef_sc_white)
SE2 <- c(robust_sc_female,robust_sc_white)

df_sc2 <- data.frame(Treatment2, Coefficient2, SE2)

df_sc2 <- df_sc2 %>%
  mutate (ci_lower = Coefficient2 - 1.96*SE2,
          ci_upper = Coefficient2 + 1.96*SE2) 

# Coef graphs -------------------------------------------------------------
# Lock in order of treatments to avoid alphabetical plot
#df_reg$Treatment <- factor(df_reg$Treatment , levels = df_reg$Treatment)


# Plot this 
plot_sc2 <- ggplot(df_sc2, aes(Coefficient2, Treatment2)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
  theme_bw() +
  xlab ("Coefficient in regression") +
  ylab ("Independent variable") +
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "red") +
  ggtitle("Proportion of evidence-based words in hearing") +
  theme(plot.title = element_text(size=8))



plot_sc2


# Pro social



Coefficient_soc2 <- c(coef_soc_female, coef_soc_white)
SE_soc2 <- c(robust_soc_female,robust_soc_white)

df_soc2 <- data.frame(Treatment2, Coefficient_soc2, SE_soc2)

df_soc2 <- df_soc2 %>%
  mutate (ci_lower = Coefficient_soc2 - 1.96*SE_soc2,
          ci_upper = Coefficient_soc2 + 1.96*SE_soc2) 

# Coef graphs -------------------------------------------------------------
# Lock in order of treatments to avoid alphabetical plot
#df_reg$Treatment <- factor(df_reg$Treatment , levels = df_reg$Treatment)


# Plot this 
plot_soc2 <- ggplot(df_soc2, aes(Coefficient_soc2, Treatment2)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
  theme_bw() +
  xlab ("Coefficient in regression") +
  ylab ("") +
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "red") +
  ggtitle("Proportion of prosocial words in hearing") +
  theme(plot.title = element_text(size=8))



plot_soc2



# Fear



# Coefficient_fear <- c(coef_fear_ideo, coef_fear_margin, coef_fear_balance1, 
#                      coef_fear_balance2, coef_fear_female, coef_fear_white)
# SE_fear <- c(robust_fear_ideo, robust_fear_margin, robust_fear_balance1, 
#             robust_fear_balance2, robust_fear_female,robust_fear_white)
# 
# df_fear <- data.frame(Treatment, Coefficient_fear, SE_fear)
# 
# df_fear <- df_fear %>%
#   mutate (ci_lower = Coefficient_fear - 1.96*SE_fear,
#           ci_upper = Coefficient_fear + 1.96*SE_fear) 
# 
# # Coef graphs -------------------------------------------------------------
# # Lock in order of treatments to avoid alphabetical plot
# #df_reg$Treatment <- factor(df_reg$Treatment , levels = df_reg$Treatment)
# 
# 
# # Plot this 
# plot_fear <- ggplot(df_fear, aes(Coefficient_fear, Treatment)) +
#   geom_point() +
#   geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
#   theme_bw() +
#   xlab ("Coefficient in regression") +
#   ylab ("Independent variable") +
#   geom_vline(xintercept = 0, linetype="dashed", 
#              color = "red") +
#   ggtitle("Proportion of fear words in hearing") +
#   theme(plot.title = element_text(size=8))
# 
# 
# 
# plot_fear






# Negative



Coefficient_neg2 <- c(coef_negative_female, coef_negative_white)
SE_neg2 <- c(robust_negative_female,robust_negative_white)

df_neg2 <- data.frame(Treatment2, Coefficient_neg2, SE_neg2)

df_neg2 <- df_neg2 %>%
  mutate (ci_lower = Coefficient_neg2 - 1.96*SE_neg2,
          ci_upper = Coefficient_neg2 + 1.96*SE_neg2) 

# Coef graphs -------------------------------------------------------------
# Lock in order of treatments to avoid alphabetical plot
#df_reg$Treatment <- factor(df_reg$Treatment , levels = df_reg$Treatment)


# Plot this 
plot_neg2 <- ggplot(df_neg2, aes(Coefficient_neg2, Treatment2)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
  theme_bw() +
  xlab ("Coefficient in regression") +
  ylab ("") +
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "red") +
  ggtitle("Proportion of negative words in hearing") +
  theme(plot.title = element_text(size=8))



plot_neg2




# Positive



Coefficient_pos2 <- c(coef_positive_female, coef_positive_white)
SE_pos2 <- c(robust_positive_female,robust_positive_white)

df_pos2 <- data.frame(Treatment2, Coefficient_pos2, SE_pos2)

df_pos2 <- df_pos2 %>%
  mutate (ci_lower = Coefficient_pos2 - 1.96*SE_pos2,
          ci_upper = Coefficient_pos2 + 1.96*SE_pos2) 

# Coef graphs -------------------------------------------------------------
# Lock in order of treatments to avoid alphabetical plot
#df_reg$Treatment <- factor(df_reg$Treatment , levels = df_reg$Treatment)


# Plot this 
plot_pos2 <- ggplot(df_pos2, aes(Coefficient_pos2, Treatment2)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0) +
  theme_bw() +
  xlab ("Coefficient in regression") +
  ylab ("") +
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "red") +
  labs(caption="Coefficients in a regression with controls (standardized variables), 95% confidence intervals with robust SEs") +
  ggtitle("Proportion of positive words in hearing") +
  theme(plot.title = element_text(size=8))



plot_pos2



# PLot sum

plots_sum_repre <- (plot_sc2 | plot_soc2) / (plot_neg2 | plot_pos2) 

plots_sum_repre













