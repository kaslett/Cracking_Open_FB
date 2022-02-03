library(dplyr)
library(data.table)
library(xtable)
library(data.table)

#Set working directory
setwd("/home/jovyan/New_Clean/Cracking_Open_Analysis/data")

NG_Rating <- read.csv('NewsGuard_Ratings.csv')

NG_Click_Rating_1 <- read.csv('New_Headlines_Click_NG_1.csv')
NG_Click_Rating_2 <- read.csv('New_Headlines_Click_NG_2.csv')
NG_Click_Rating_3 <- read.csv('New_Headlines_Click_NG_3.csv')
NG_Click_Rating_4 <- read.csv('New_Headlines_Click_NG_4.csv')
NG_Click_Rating_5 <- read.csv('New_Headlines_Click_NG_5.csv')
NG_Click_Rating_6 <- read.csv('Headlines_Click_FN.csv')

colnames(NG_Click_Rating_6) <- c('url_rid','Clickbait')


Clickbait_Ratings <- rbind(NG_Click_Rating_1,NG_Click_Rating_2,NG_Click_Rating_3,NG_Click_Rating_4,NG_Click_Rating_5,NG_Click_Rating_6)


NG_Click_Rating_1 <- NULL
NG_Click_Rating_2 <- NULL
NG_Click_Rating_3 <- NULL
NG_Click_Rating_4 <- NULL
NG_Click_Rating_5 <- NULL
NG_Click_Rating_6 <- NULL

NG_Pol_Rating_1 <- read.csv('Headlines_Pol_NG_1.csv')
NG_Pol_Rating_2 <- read.csv('Headlines_Pol_NG_2.csv')
NG_Pol_Rating_3 <- read.csv('Headlines_Pol_NG_3.csv')
NG_Pol_Rating_4 <- read.csv('Headlines_Pol_NG_4.csv')
NG_Pol_Rating_5 <- read.csv('Headlines_Pol_NG_5.csv')
NG_Pol_Rating_6 <- read.csv('Headlines_Pol_FN.csv')

Political_Ratings <- rbind(NG_Pol_Rating_1,NG_Pol_Rating_2,NG_Pol_Rating_3,NG_Pol_Rating_4,NG_Pol_Rating_5,NG_Pol_Rating_6)
Political_Ratings <- na.omit(Political_Ratings)
Political_Ratings <- unique(Political_Ratings)


NG_Pol_Rating_1 <- NULL
NG_Pol_Rating_2 <- NULL
NG_Pol_Rating_3 <- NULL
NG_Pol_Rating_4 <- NULL
NG_Pol_Rating_5 <- NULL
NG_Pol_Rating_6 <- NULL

Ratings <- merge(Political_Ratings,Clickbait_Ratings,by='url_rid')
n_occur <- data.frame(table(Clickbait_Ratings$url_rid))
Repeaters_1 <- n_occur[n_occur$Freq > 1,]
n_occur <- data.frame(table(Political_Ratings$url_rid))
Repeaters_2 <- n_occur[n_occur$Freq > 1,]
`%notin%` <- Negate(`%in%`)

Ratings <- Ratings[Ratings$url_rid %notin% Repeaters_1$Var1,]
Ratings <- Ratings[Ratings$url_rid %notin% Repeaters_2$Var1,]

Political_Ratings <- NULL
Clickbait_Ratings <- NULL

Headlines_1 <- as.data.frame(fread('Second_NewsG_VIEWS_AGE_PPA_Feb_2018.tsv'))
Headlines_2 <- as.data.frame(fread('Second_NewsG_VIEWS_AGE_PPA_Mar_2018.tsv'))
Headlines_3 <- as.data.frame(fread('Second_NewsG_VIEWS_AGE_PPA_Apr_2018.tsv'))
Headlines_4 <- as.data.frame(fread('Second_NewsG_VIEWS_AGE_PPA_May_2018.tsv'))
Headlines_5 <- as.data.frame(fread('Second_NewsG_VIEWS_AGE_PPA_Jun_2018.tsv'))
Headlines_6 <- as.data.frame(fread('Second_NewsG_VIEWS_AGE_PPA_Jul_2018.tsv'))
Headlines_7_1 <- as.data.frame(fread('Second_NewsG_VIEWS_AGE_PPA_Aug_2018_1.tsv'))
Headlines_7_2 <- as.data.frame(fread('Second_NewsG_VIEWS_AGE_PPA_Aug_2018_2.tsv'))
Headlines_7 <- rbind(Headlines_7_1,Headlines_7_2)
Headlines_8_1 <- as.data.frame(fread('Second_NewsG_VIEWS_AGE_PPA_Sep_2018_1.tsv'))
Headlines_8_2 <- as.data.frame(fread('Second_NewsG_VIEWS_AGE_PPA_Sep_2018_2.tsv'))
Headlines_8 <- rbind(Headlines_8_1,Headlines_8_2)
Headlines_9 <- as.data.frame(fread('Second_NewsG_VIEWS_AGE_PPA_Oct_2018.tsv'))
Headlines_10 <- as.data.frame(fread('Second_NewsG_VIEWS_AGE_PPA_Nov_2018.tsv'))
Headlines_11 <- as.data.frame(fread('Second_NewsG_VIEWS_AGE_PPA_Dec_2018.tsv'))
Headlines_12 <- as.data.frame(fread('Second_NewsG_VIEWS_AGE_PPA_Jan_2018.tsv'))

Headlines_1 <- Headlines_1 %>% filter(public_shares_top_country == 'US')
Headlines_2 <- Headlines_2 %>% filter(public_shares_top_country == 'US') 
Headlines_3 <- Headlines_3 %>% filter(public_shares_top_country == 'US')
Headlines_4 <- Headlines_4 %>% filter(public_shares_top_country == 'US') 
Headlines_5 <- Headlines_5 %>% filter(public_shares_top_country == 'US')
Headlines_6 <- Headlines_6 %>% filter(public_shares_top_country == 'US') 
Headlines_7 <- Headlines_7 %>% filter(public_shares_top_country == 'US')
Headlines_8 <- Headlines_8 %>% filter(public_shares_top_country == 'US') 
Headlines_9 <- Headlines_9 %>% filter(public_shares_top_country == 'US')
Headlines_10 <- Headlines_10 %>% filter(public_shares_top_country == 'US')
Headlines_11 <- Headlines_11 %>% filter(public_shares_top_country == 'US') 
Headlines_12 <- Headlines_12 %>% filter(public_shares_top_country == 'US')

Headlines_1 <- Headlines_1 %>% select(parent_domain,url_rid,total_shares,total_views,age_bracket,political_page_affinity)
Headlines_2 <- Headlines_2 %>% select(parent_domain,url_rid,total_shares,total_views,age_bracket,political_page_affinity)
Headlines_3 <- Headlines_3 %>% select(parent_domain,url_rid,total_shares,total_views,age_bracket,political_page_affinity)
Headlines_4 <- Headlines_4 %>% select(parent_domain,url_rid,total_shares,total_views,age_bracket,political_page_affinity)
Headlines_5 <- Headlines_5 %>% select(parent_domain,url_rid,total_shares,total_views,age_bracket,political_page_affinity)
Headlines_6 <- Headlines_6 %>% select(parent_domain,url_rid,total_shares,total_views,age_bracket,political_page_affinity)
Headlines_7 <- Headlines_7 %>% select(parent_domain,url_rid,total_shares,total_views,age_bracket,political_page_affinity)
Headlines_8 <- Headlines_8 %>% select(parent_domain,url_rid,total_shares,total_views,age_bracket,political_page_affinity)
Headlines_9 <- Headlines_9 %>% select(parent_domain,url_rid,total_shares,total_views,age_bracket,political_page_affinity)
Headlines_10 <- Headlines_10 %>% select(parent_domain,url_rid,total_shares,total_views,age_bracket,political_page_affinity)
Headlines_11 <- Headlines_11 %>% select(parent_domain,url_rid,total_shares,total_views,age_bracket,political_page_affinity)
Headlines_12 <- Headlines_12 %>% select(parent_domain,url_rid,total_shares,total_views,age_bracket,political_page_affinity)

Headlines_1 <- unique(Headlines_1)
Headlines_2 <- unique(Headlines_2)
Headlines_3 <- unique(Headlines_3)
Headlines_4 <- unique(Headlines_4)
Headlines_5 <- unique(Headlines_5)
Headlines_6 <- unique(Headlines_6)
Headlines_7 <- unique(Headlines_7)
Headlines_8 <- unique(Headlines_8)
Headlines_9 <- unique(Headlines_9)
Headlines_10 <- unique(Headlines_10)
Headlines_11 <- unique(Headlines_11)
Headlines_12 <- unique(Headlines_12)

January_Data <- merge(Headlines_12,Ratings,by='url_rid')
February_Data <- merge(Headlines_1,Ratings,by='url_rid')
March_Data <- merge(Headlines_2,Ratings,by='url_rid')
April_Data <- merge(Headlines_3,Ratings,by='url_rid')
May_Data <- merge(Headlines_4,Ratings,by='url_rid')
June_Data <- merge(Headlines_5,Ratings,by='url_rid')
July_Data <- merge(Headlines_6,Ratings,by='url_rid')
August_Data <- merge(Headlines_7,Ratings,by='url_rid')
September_Data <- merge(Headlines_8,Ratings,by='url_rid')
October_Data <- merge(Headlines_9,Ratings,by='url_rid')
November_Data <- merge(Headlines_10,Ratings,by='url_rid')
December_Data <- merge(Headlines_11,Ratings,by='url_rid')


Headlines_1 <- NULL
Headlines_2 <- NULL
Headlines_3 <- NULL
Headlines_4 <- NULL
Headlines_5 <- NULL
Headlines_6 <- NULL
Headlines_7 <- NULL
Headlines_8 <- NULL
Headlines_9 <- NULL
Headlines_10 <- NULL
Headlines_11 <- NULL
Headlines_12 <- NULL


NG_Rating <- NG_Rating %>% select(Domain,Score,Rating)


January_Data <- merge(January_Data,NG_Rating,by.x='parent_domain',by.y='Domain',all.x=T)
February_Data <- merge(February_Data,NG_Rating,by.x='parent_domain',by.y='Domain',all.x=T)
March_Data <- merge(March_Data,NG_Rating,by.x='parent_domain',by.y='Domain',all.x=T)
April_Data <- merge(April_Data,NG_Rating,by.x='parent_domain',by.y='Domain',all.x=T)
May_Data <- merge(May_Data,NG_Rating,by.x='parent_domain',by.y='Domain',all.x=T)
June_Data <- merge(June_Data,NG_Rating,by.x='parent_domain',by.y='Domain',all.x=T)
July_Data <- merge(July_Data,NG_Rating,by.x='parent_domain',by.y='Domain',all.x=T)
August_Data <- merge(August_Data,NG_Rating,by.x='parent_domain',by.y='Domain',all.x=T)
September_Data <- merge(September_Data,NG_Rating,by.x='parent_domain',by.y='Domain',all.x=T)
October_Data <- merge(October_Data,NG_Rating,by.x='parent_domain',by.y='Domain',all.x=T)
November_Data <- merge(November_Data,NG_Rating,by.x='parent_domain',by.y='Domain',all.x=T)
December_Data <- merge(December_Data,NG_Rating,by.x='parent_domain',by.y='Domain',all.x=T)

January_Data <- January_Data %>% mutate(Credible = ifelse(Score >= 60, 1,0))
February_Data <- February_Data %>% mutate(Credible = ifelse(Score >= 60, 1,0))
March_Data <- March_Data %>% mutate(Credible = ifelse(Score >= 60, 1,0))
April_Data <- April_Data %>% mutate(Credible = ifelse(Score >= 60, 1,0))
May_Data <- May_Data %>% mutate(Credible = ifelse(Score >= 60, 1,0))
June_Data <- June_Data %>% mutate(Credible = ifelse(Score >= 60, 1,0))
July_Data <- July_Data %>% mutate(Credible = ifelse(Score >= 60, 1,0))
August_Data <- August_Data %>% mutate(Credible = ifelse(Score >= 60, 1,0))
September_Data <- September_Data %>% mutate(Credible = ifelse(Score >= 60, 1,0))
October_Data <- October_Data %>% mutate(Credible = ifelse(Score >= 60, 1,0))
November_Data <- November_Data %>% mutate(Credible = ifelse(Score >= 60, 1,0))
December_Data <- December_Data %>% mutate(Credible = ifelse(Score >= 60, 1,0))

January_Data <- January_Data %>% filter(!is.na(Score))
February_Data <- February_Data %>% filter(!is.na(Score))
March_Data <- March_Data %>% filter(!is.na(Score))
April_Data <- April_Data %>% filter(!is.na(Score))
May_Data <- May_Data %>% filter(!is.na(Score))
June_Data <- June_Data %>% filter(!is.na(Score))
July_Data <- July_Data %>% filter(!is.na(Score))
August_Data <- August_Data %>% filter(!is.na(Score))
September_Data <- September_Data %>% filter(!is.na(Score))
October_Data <- October_Data %>% filter(!is.na(Score))
November_Data <- November_Data %>% filter(!is.na(Score))
December_Data <- December_Data %>% filter(!is.na(Score))

January_Data <- January_Data %>% filter(Rating != 'S')
February_Data <- February_Data %>% filter(Rating != 'S')
March_Data <- March_Data %>% filter(Rating != 'S')
April_Data <- April_Data %>% filter(Rating != 'S')
May_Data <- May_Data %>% filter(Rating != 'S')
June_Data <- June_Data %>% filter(Rating != 'S')
July_Data <- July_Data %>% filter(Rating != 'S')
August_Data <- August_Data %>% filter(Rating != 'S')
September_Data <- September_Data %>% filter(Rating != 'S')
October_Data <- October_Data %>% filter(Rating != 'S')
November_Data <- November_Data %>% filter(Rating != 'S')
December_Data <- December_Data %>% filter(Rating != 'S')

January_Data <- January_Data %>% mutate(Month = 'January')
February_Data <- February_Data %>% mutate(Month = 'February')
March_Data <- March_Data %>% mutate(Month = 'March')
April_Data <- April_Data %>% mutate(Month = 'April')
May_Data <- May_Data %>% mutate(Month = 'May')
June_Data <- June_Data %>% mutate(Month = 'June')
July_Data <- July_Data %>% mutate(Month = 'July')
August_Data <- August_Data %>% mutate(Month = 'August')
September_Data <- September_Data %>% mutate(Month = 'September')
October_Data <- October_Data %>% mutate(Month = 'October')
November_Data <- November_Data %>% mutate(Month = 'November')
December_Data <- December_Data %>% mutate(Month = 'December')




#January:

January_Data <- January_Data %>% group_by(url_rid,age_bracket,political_page_affinity) %>% mutate(tot_shares = sum(total_shares))
January_Data <- January_Data %>% group_by(url_rid,age_bracket,political_page_affinity) %>% mutate(tot_views = sum(total_views))


January_Data <- January_Data %>% select(parent_domain,
                                          url_rid,
                                          age_bracket,
                                          political_page_affinity,
                                          Political,
                                          Clickbait,
                                          Score,
                                          Rating,
                                          Credible,
                                          tot_shares,
                                          tot_views,
                                          Month)



#February


February_Data <- February_Data %>% group_by(url_rid,age_bracket,political_page_affinity) %>% mutate(tot_shares = sum(total_shares))
February_Data <- February_Data %>% group_by(url_rid,age_bracket,political_page_affinity) %>% mutate(tot_views = sum(total_views))


February_Data <- February_Data %>% select(parent_domain,
                                          url_rid,
                                          age_bracket,
                                          political_page_affinity,
                                          Political,
                                          Clickbait,
                                          Score,
                                          Rating,
                                          Credible,
                                          tot_shares,
                                          tot_views,
                                          Month)

#March
March_Data <- March_Data %>% group_by(url_rid,age_bracket,political_page_affinity) %>% mutate(tot_shares = sum(total_shares))
March_Data <- March_Data %>% group_by(url_rid,age_bracket,political_page_affinity) %>% mutate(tot_views = sum(total_views))


March_Data <- March_Data %>% select(parent_domain,
                                          url_rid,
                                          age_bracket,
                                          political_page_affinity,
                                          Political,
                                          Clickbait,
                                          Score,
                                          Rating,
                                          Credible,
                                          tot_shares,
                                          tot_views,
                                          Month)


#April
April_Data <- April_Data %>% group_by(url_rid,age_bracket,political_page_affinity) %>% mutate(tot_shares = sum(total_shares))
April_Data <- April_Data %>% group_by(url_rid,age_bracket,political_page_affinity) %>% mutate(tot_views = sum(total_views))


April_Data <- April_Data %>% select(parent_domain,
                                          url_rid,
                                          age_bracket,
                                          political_page_affinity,
                                          Political,
                                          Clickbait,
                                          Score,
                                          Rating,
                                          Credible,
                                          tot_shares,
                                          tot_views,
                                          Month)

#May
May_Data <- May_Data %>% group_by(url_rid,age_bracket,political_page_affinity) %>% mutate(tot_shares = sum(total_shares))
May_Data <- May_Data %>% group_by(url_rid,age_bracket,political_page_affinity) %>% mutate(tot_views = sum(total_views))


May_Data <- May_Data %>% select(parent_domain,
                                          url_rid,
                                          age_bracket,
                                          political_page_affinity,
                                          Political,
                                          Clickbait,
                                          Score,
                                          Rating,
                                          Credible,
                                          tot_shares,
                                          tot_views,
                                          Month)

#June
June_Data <- June_Data %>% group_by(url_rid,age_bracket,political_page_affinity) %>% mutate(tot_shares = sum(total_shares))
June_Data <- June_Data %>% group_by(url_rid,age_bracket,political_page_affinity) %>% mutate(tot_views = sum(total_views))


June_Data <- June_Data %>% select(parent_domain,
                                          url_rid,
                                          age_bracket,
                                          political_page_affinity,
                                          Political,
                                          Clickbait,
                                          Score,
                                          Rating,
                                          Credible,
                                          tot_shares,
                                          tot_views,
                                          Month)

#July
July_Data <- July_Data %>% group_by(url_rid,age_bracket,political_page_affinity) %>% mutate(tot_shares = sum(total_shares))
July_Data <- July_Data %>% group_by(url_rid,age_bracket,political_page_affinity) %>% mutate(tot_views = sum(total_views))


July_Data <- July_Data %>% select(parent_domain,
                                          url_rid,
                                          age_bracket,
                                          political_page_affinity,
                                          Political,
                                          Clickbait,
                                          Score,
                                          Rating,
                                          Credible,
                                          tot_shares,
                                          tot_views,
                                          Month)


#August
August_Data <- August_Data %>% group_by(url_rid,age_bracket,political_page_affinity) %>% mutate(tot_shares = sum(total_shares))
August_Data <- August_Data %>% group_by(url_rid,age_bracket,political_page_affinity) %>% mutate(tot_views = sum(total_views))


August_Data <- August_Data %>% select(parent_domain,
                                          url_rid,
                                          age_bracket,
                                          political_page_affinity,
                                          Political,
                                          Clickbait,
                                          Score,
                                          Rating,
                                          Credible,
                                          tot_shares,
                                          tot_views,
                                          Month)


#September
September_Data <- September_Data %>% group_by(url_rid,age_bracket,political_page_affinity) %>% mutate(tot_shares = sum(total_shares))
September_Data <- September_Data %>% group_by(url_rid,age_bracket,political_page_affinity) %>% mutate(tot_views = sum(total_views))


September_Data <- September_Data %>% select(parent_domain,
                                          url_rid,
                                          age_bracket,
                                          political_page_affinity,
                                          Political,
                                          Clickbait,
                                          Score,
                                          Rating,
                                          Credible,
                                          tot_shares,
                                          tot_views,
                                          Month)


#October
October_Data <- October_Data %>% group_by(url_rid,age_bracket,political_page_affinity) %>% mutate(tot_shares = sum(total_shares))
October_Data <- October_Data %>% group_by(url_rid,age_bracket,political_page_affinity) %>% mutate(tot_views = sum(total_views))


October_Data <- October_Data %>% select(parent_domain,
                                          url_rid,
                                          age_bracket,
                                          political_page_affinity,
                                          Political,
                                          Clickbait,
                                          Score,
                                          Rating,
                                          Credible,
                                          tot_shares,
                                          tot_views,
                                          Month)

#November:

November_Data <- November_Data %>% group_by(url_rid,age_bracket,political_page_affinity) %>% mutate(tot_shares = sum(total_shares))
November_Data <- November_Data %>% group_by(url_rid,age_bracket,political_page_affinity) %>% mutate(tot_views = sum(total_views))


November_Data <- November_Data %>% select(parent_domain,
                                          url_rid,
                                          age_bracket,
                                          political_page_affinity,
                                          Political,
                                          Clickbait,
                                          Score,
                                          Rating,
                                          Credible,
                                          tot_shares,
                                          tot_views,
                                          Month)

#December:

December_Data <- December_Data %>% group_by(url_rid,age_bracket,political_page_affinity) %>% mutate(tot_shares = sum(total_shares))
December_Data <- December_Data %>% group_by(url_rid,age_bracket,political_page_affinity) %>% mutate(tot_views = sum(total_views))


December_Data <- December_Data %>% select(parent_domain,
                                          url_rid,
                                          age_bracket,
                                          political_page_affinity,
                                          Political,
                                          Clickbait,
                                          Score,
                                          Rating,
                                          Credible,
                                          tot_shares,
                                          tot_views,
                                          Month)

January_Data <- unique(January_Data)
February_Data <- unique(February_Data)
March_Data <- unique(March_Data)
April_Data <- unique(April_Data)
May_Data <- unique(May_Data)
June_Data <- unique(June_Data)
July_Data <- unique(July_Data)
August_Data <- unique(August_Data)
September_Data <- unique(September_Data)
October_Data <- unique(October_Data)
November_Data <- unique(November_Data)
December_Data <- unique(December_Data)

All_Data <- rbind(January_Data,
                  February_Data,
                  March_Data,
                  April_Data,
                  May_Data,
                  June_Data,
                  July_Data,
                  August_Data,
                  September_Data,
                  October_Data,
                  November_Data,
                  December_Data)

All_Data <- All_Data %>% filter(age_bracket != '')
write.table(All_Data, file='All_Data_Ideology_2.tsv', quote=FALSE, sep='\t', col.names = NA)