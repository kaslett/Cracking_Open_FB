library(dplyr)
library(data.table)
library(xtable)
library(data.table)
#Analysis
setwd("/Users/kevinaslett/Documents/Headlines_Political_Classifier")

NG_Rating <- read.csv('NewsGuard_Ratings.csv')

setwd("/Users/kevinaslett/Documents/Headlines_Political_Classifier/Ratings_Click_Pol")

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

Headlines_1 <- as.data.frame(fread('NewsG_VIEWS_AGE_PPA_Feb_2018.tsv'))
Headlines_2 <- as.data.frame(fread('NewsG_VIEWS_AGE_PPA_Mar_2018.tsv'))
Headlines_3 <- as.data.frame(fread('NewsG_VIEWS_AGE_PPA_Apr_2018.tsv'))
Headlines_4 <- as.data.frame(fread('NewsG_VIEWS_AGE_PPA_May_2018.tsv'))
Headlines_5 <- as.data.frame(fread('NewsG_VIEWS_AGE_PPA_Jun_2018.tsv'))
Headlines_6 <- as.data.frame(fread('NewsG_VIEWS_AGE_PPA_Jul_2018.tsv'))
Headlines_7 <- as.data.frame(fread('NewsG_VIEWS_AGE_PPA_Aug_2018.tsv'))
Headlines_8 <- as.data.frame(fread('NewsG_VIEWS_AGE_PPA_Sep_2018.tsv'))
Headlines_9 <- as.data.frame(fread('NewsG_VIEWS_AGE_PPA_Oct_2018.tsv'))
Headlines_10 <- as.data.frame(fread('NewsG_VIEWS_AGE_PPA_Nov_2018.tsv'))
Headlines_11 <- as.data.frame(fread('NewsG_VIEWS_AGE_PPA_Dec_2018.tsv'))
Headlines_12 <- as.data.frame(fread('NewsG_VIEWS_AGE_PPA_Jan_2018.tsv'))

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

January_Data <- January_Data %>% mutate(Credible = ifelse(is.na(Score), 0,Credible))
February_Data <- February_Data %>% mutate(Credible = ifelse(is.na(Score), 0,Credible))
March_Data <- March_Data %>% mutate(Credible = ifelse(is.na(Score), 0,Credible))
April_Data <- April_Data %>% mutate(Credible = ifelse(is.na(Score), 0,Credible))
May_Data <- May_Data %>% mutate(Credible = ifelse(is.na(Score), 0,Credible))
June_Data <- June_Data %>% mutate(Credible = ifelse(is.na(Score), 0,Credible))
July_Data <- July_Data %>% mutate(Credible = ifelse(is.na(Score), 0,Credible))
August_Data <- August_Data %>% mutate(Credible = ifelse(is.na(Score), 0,Credible))
September_Data <- September_Data %>% mutate(Credible = ifelse(is.na(Score), 0,Credible))
October_Data <- October_Data %>% mutate(Credible = ifelse(is.na(Score), 0,Credible))
November_Data <- November_Data %>% mutate(Credible = ifelse(is.na(Score), 0,Credible))
December_Data <- December_Data %>% mutate(Credible = ifelse(is.na(Score), 0,Credible))


January_Data <- January_Data %>% mutate(Rating = ifelse(is.na(Score), 'N',Rating))
February_Data <- February_Data %>% mutate(Rating = ifelse(is.na(Score), 'N',Rating))
March_Data <- March_Data %>% mutate(Rating = ifelse(is.na(Score), 'N',Rating))
April_Data <- April_Data %>% mutate(Rating = ifelse(is.na(Score), 'N',Rating))
May_Data <- May_Data %>% mutate(Rating = ifelse(is.na(Score), 'N',Rating))
June_Data <- June_Data %>% mutate(Rating = ifelse(is.na(Score), 'N',Rating))
July_Data <- July_Data %>% mutate(Rating = ifelse(is.na(Score), 'N',Rating))
August_Data <- August_Data %>% mutate(Rating = ifelse(is.na(Score), 'N',Rating))
September_Data <- September_Data %>% mutate(Rating = ifelse(is.na(Score), 'N',Rating))
October_Data <- October_Data %>% mutate(Rating = ifelse(is.na(Score), 'N',Rating))
November_Data <- November_Data %>% mutate(Rating = ifelse(is.na(Score), 'N',Rating))
December_Data <- December_Data %>% mutate(Rating = ifelse(is.na(Score), 'N',Rating))

January_Data <- January_Data %>% mutate(Score = ifelse(is.na(Score), 0,Score))
February_Data <- February_Data %>% mutate(Score = ifelse(is.na(Score), 0,Score))
March_Data <- March_Data %>% mutate(Score = ifelse(is.na(Score), 0,Score))
April_Data <- April_Data %>% mutate(Score = ifelse(is.na(Score), 0,Score))
May_Data <- May_Data %>% mutate(Score = ifelse(is.na(Score), 0,Score))
June_Data <- June_Data %>% mutate(Score = ifelse(is.na(Score), 0,Score))
July_Data <- July_Data %>% mutate(Score = ifelse(is.na(Score), 0,Score))
August_Data <- August_Data %>% mutate(Score = ifelse(is.na(Score), 0,Score))
September_Data <- September_Data %>% mutate(Score = ifelse(is.na(Score), 0,Score))
October_Data <- October_Data %>% mutate(Score = ifelse(is.na(Score), 0,Score))
November_Data <- November_Data %>% mutate(Score = ifelse(is.na(Score), 0,Score))
December_Data <- December_Data %>% mutate(Score = ifelse(is.na(Score), 0,Score))


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


January_Data$Rating <- as.character(January_Data$Rating)
January_Data <- January_Data %>% filter(Rating != 'S')

February_Data$Rating <- as.character(February_Data$Rating)
February_Data <- February_Data %>% filter(Rating != 'S')

March_Data$Rating <- as.character(March_Data$Rating)
March_Data <- March_Data %>% filter(Rating != 'S')

April_Data$Rating <- as.character(April_Data$Rating)
April_Data <- April_Data %>% filter(Rating != 'S')

May_Data$Rating <- as.character(May_Data$Rating)
May_Data <- May_Data %>% filter(Rating != 'S')

June_Data$Rating <- as.character(June_Data$Rating)
June_Data <- June_Data %>% filter(Rating != 'S')

July_Data$Rating <- as.character(July_Data$Rating)
July_Data <- July_Data %>% filter(Rating != 'S')

August_Data$Rating <- as.character(August_Data$Rating)
August_Data <- August_Data %>% filter(Rating != 'S')

September_Data$Rating <- as.character(September_Data$Rating)
September_Data <- September_Data %>% filter(Rating != 'S')

October_Data$Rating <- as.character(October_Data$Rating)
October_Data <- October_Data %>% filter(Rating != 'S')


November_Data$Rating <- as.character(November_Data$Rating)
November_Data <- November_Data %>% filter(Rating != 'S')


December_Data$Rating <- as.character(December_Data$Rating)
December_Data <- December_Data %>% filter(Rating != 'S')

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


setwd("/Users/kevinaslett/Documents/Headlines_Political_Classifier/Ratings_Click_Pol")



Data_Ideology <- All_Data %>% filter(age_bracket != "")
Data_Ideology <- Data_Ideology %>% filter(Rating != 'N')
write.table(All_Data, file='All_Data_Ideology.tsv', quote=FALSE, sep='\t', col.names = NA)




unique(All_Data$Month)

#Type_News_1 <- read.csv('Headlines_Type_News_1.csv')
#Type_News_2 <- read.csv('Headlines_Type_News_2.csv')
#Type_News_3 <- read.csv('Headlines_Type_News_3.csv')
#Type_News_4 <- read.csv('Headlines_Type_News_4.csv')
#Type_News_5 <- read.csv('Headlines_Type_News_5.csv')
#Type_News_6 <- read.csv('Headlines_Type_News_FN.csv')


#Type_News <- rbind(Type_News_1,
#                   Type_News_2,
#                   Type_News_3,
#                   Type_News_4,
#                   Type_News_5,
#                   Type_News_6)

#Type_News <- Type_News %>% select(url_rid,pred_what_news_us)

#Type_News_1 <- NULL
#Type_News_2 <- NULL
#Type_News_3 <- NULL
#Type_News_4 <- NULL
#Type_News_5 <- NULL
#Type_News_6 <- NULL

#Soft_News_1 <- read.csv('Headlines_Soft_News_1.csv')
#Soft_News_2 <- read.csv('Headlines_Soft_News_2.csv')
#Soft_News_3 <- read.csv('Headlines_Soft_News_3.csv')
#Soft_News_4 <- read.csv('Headlines_Soft_News_4.csv')
#Soft_News_5 <- read.csv('Headlines_Soft_News_5.csv')
#Soft_News_6 <- read.csv('Headlines_Soft_News_FN.csv')

#Soft_News <- rbind(Soft_News_1,
#                   Soft_News_2,
#                   Soft_News_3,
#                   Soft_News_4,
#                   Soft_News_5,
#                   Soft_News_6)
#colnames(Soft_News)

#Soft_News_1 <- NULL
#Soft_News_2 <- NULL
#Soft_News_3 <- NULL
#Soft_News_4 <- NULL
#Soft_News_5 <- NULL
#Soft_News_6 <- NULL

#Video_Data <- read.csv('Video_Data.csv')

#Video_Data$url_rid <- as.character(Video_Data$url_rid)

#All_Data <- merge(Video_Data,All_Data,by='url_rid')
#All_Data <- merge(Type_News,All_Data,by='url_rid')
#All_Data <- merge(Soft_News,All_Data,by='url_rid')

#All_Data$X.x <- NULL
#All_Data$X.y <- NULL
#All_Data$X <- NULL
#All_Data <- unique(All_Data)



#write.csv(Weekly_Data_2,'All_Data_3_Ideology.csv')







###############################################################






##### MERGE TIME UNIX POST #############

setwd("/Users/kevinaslett/Documents/Headlines_Political_Classifier/Coding_Data")


Headlines_1 <- as.data.frame(fread('All_NewsGuard_Headlines_1.tsv')) 
Headlines_1  <- Headlines_1  %>% filter(public_shares_top_country == 'US')
Headlines_1 <- Headlines_1 %>% select(url_rid,first_post_time_unix)
Headlines_2 <- as.data.frame(fread('All_NewsGuard_Headlines_2.tsv')) 
Headlines_2  <- Headlines_2  %>% filter(public_shares_top_country == 'US')
Headlines_2 <- Headlines_2 %>% select(url_rid,first_post_time_unix)
Headlines_3 <- as.data.frame(fread('All_NewsGuard_Headlines_3.tsv')) 
Headlines_3  <- Headlines_3  %>% filter(public_shares_top_country == 'US')
Headlines_3 <- Headlines_3 %>% select(url_rid,first_post_time_unix)
Headlines_4 <- as.data.frame(fread('All_NewsGuard_Headlines_4.tsv')) 
Headlines_4  <- Headlines_4  %>% filter(public_shares_top_country == 'US')
Headlines_4 <- Headlines_4 %>% select(url_rid,first_post_time_unix)
Headlines_5 <- as.data.frame(fread('All_NewsGuard_Headlines_5.tsv')) 
Headlines_5  <- Headlines_5  %>% filter(public_shares_top_country == 'US')
Headlines_5 <- Headlines_5 %>% select(url_rid,first_post_time_unix)
Headlines_6 <- as.data.frame(fread('All_FN_Headlines_Big.tsv')) 
Headlines_6  <- Headlines_6  %>% filter(public_shares_top_country == 'US')
Headlines_6 <- Headlines_6 %>% select(url_rid,first_post_time_unix)


URL_Timestamp <- rbind(Headlines_1,
                       Headlines_2,
                       Headlines_3,
                       Headlines_4,
                       Headlines_5,
                       Headlines_6)

Weekly_Data <- merge(All_Data,URL_Timestamp,by='url_rid')

Headlines_1 <- NULL
Headlines_2 <- NULL
Headlines_3 <- NULL
Headlines_4 <- NULL
Headlines_5 <- NULL
Headlines_6 <- NULL
URL_Timestamp <- NULL
All_Data <- NULL

Weekly_Data$first_post_time_unix <- as.character(Weekly_Data$first_post_time_unix)
Weekly_Data$first_post_time_unix <- as.numeric(Weekly_Data$first_post_time_unix)

Weekly_Data$Week_prior_Election <- NA
#for loop
i = 0
iteration = 0
for (i in -40:8)
{
  iteration = iteration + 1
  x = i-1
  end = as.numeric(as.POSIXct("2018-11-07 00:00:00 EST")) + 608400*i
  start = as.numeric(as.POSIXct("2018-11-07 00:00:00 EST")) + 608400*x
  Weekly_Data$Week_prior_Election <- ifelse(Weekly_Data$first_post_time_unix < end & Weekly_Data$first_post_time_unix >= start,i,Weekly_Data$Week_prior_Election)
  print(iteration/length(-24:10))
}

write.csv(Weekly_Data, 'All_Data_2_Ideology.csv')

Weekly_Data$X.x <- NULL
Weekly_Data$X.y <- NULL
Weekly_Data$X <- NULL

Weekly_Data_2 <- unique(Weekly_Data)


write.csv(Weekly_Data_2,'All_Data_3_Ideology.csv')
