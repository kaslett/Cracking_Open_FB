
#Load in necessary libraries:
library(dplyr)
library(data.table)
library(ggplot2)
library(xtable)

#Create Function that produces confidence intervals:
nrows_to_std <- function(nrows,sigma) {
  #Create list of 
  x <- rep(sigma, each = nrows)
  std <- sqrt(sum(x^2))
  return(std)
}

#Set working directory
#Place here the working directory for where you place the folder:
master_directory = "/Users/kevinaslett/Documents/"
#This location file should not change:
location_data = 'Cracking_Open_The_News_Feed/data/'
location_data_2 = 'Cracking_Open_The_News_Feed/figures/'
data_directory = paste0(master_directory,location_data)
setwd(data_directory)
figure_directory = paste0(master_directory,location_data_2)

#NewsGuard Ratings of Sources in the NewsGuard database:
NG_Rating <- read.csv('NewsGuard_Ratings.csv')

#Load in data for URLs with just age brackets:
Age_Data <- as.data.frame(fread('All_Data_Age_Bracket.tsv')) 

#Create a maximum number of rows aggregated for each month the URL was first posted:
Age_Data <- Age_Data %>% mutate(agg_nrows = 285)
Age_Data <- Age_Data %>% mutate(agg_nrows = ifelse(Month == 'February',270,agg_nrows))
Age_Data <- Age_Data %>% mutate(agg_nrows = ifelse(Month == 'March',255,agg_nrows))
Age_Data <- Age_Data %>% mutate(agg_nrows = ifelse(Month == 'April',240,agg_nrows))
Age_Data <- Age_Data %>% mutate(agg_nrows = ifelse(Month == 'May',225,agg_nrows))
Age_Data <- Age_Data %>% mutate(agg_nrows = ifelse(Month == 'June',210,agg_nrows))
Age_Data <- Age_Data %>% mutate(agg_nrows = ifelse(Month == 'July',195,agg_nrows))
Age_Data <- Age_Data %>% mutate(agg_nrows = ifelse(Month == 'August',180,agg_nrows))
Age_Data <- Age_Data %>% mutate(agg_nrows = ifelse(Month == 'September',165,agg_nrows))
Age_Data <- Age_Data %>% mutate(agg_nrows = ifelse(Month == 'October',150,agg_nrows))
Age_Data <- Age_Data %>% mutate(agg_nrows = ifelse(Month == 'November',135,agg_nrows))
Age_Data <- Age_Data %>% mutate(agg_nrows = ifelse(Month == 'December',120,agg_nrows))


#Load in data for URLs with just age brackets and ideology:
Data_Ideology <- as.data.frame(fread('All_Data_Ideology.tsv')) 

#Create a maximum number of rows aggregated for each month the URL was first posted:
Data_Ideology <- Data_Ideology %>% mutate(agg_nrows = 57)
Data_Ideology <- Data_Ideology %>% mutate(agg_nrows = ifelse(Month == 'February',54,agg_nrows))
Data_Ideology <- Data_Ideology %>% mutate(agg_nrows = ifelse(Month == 'March',51,agg_nrows))
Data_Ideology <- Data_Ideology %>% mutate(agg_nrows = ifelse(Month == 'April',48,agg_nrows))
Data_Ideology <- Data_Ideology %>% mutate(agg_nrows = ifelse(Month == 'May',45,agg_nrows))
Data_Ideology <- Data_Ideology %>% mutate(agg_nrows = ifelse(Month == 'June',42,agg_nrows))
Data_Ideology <- Data_Ideology %>% mutate(agg_nrows = ifelse(Month == 'July',39,agg_nrows))
Data_Ideology <- Data_Ideology %>% mutate(agg_nrows = ifelse(Month == 'August',36,agg_nrows))
Data_Ideology <- Data_Ideology %>% mutate(agg_nrows = ifelse(Month == 'September',33,agg_nrows))
Data_Ideology <- Data_Ideology %>% mutate(agg_nrows = ifelse(Month == 'October',30,agg_nrows))
Data_Ideology <- Data_Ideology %>% mutate(agg_nrows = ifelse(Month == 'November',27,agg_nrows))
Data_Ideology <- Data_Ideology %>% mutate(agg_nrows = ifelse(Month == 'December',24,agg_nrows))


#Pull in data for ideology of mainstream and low-quality news:

#Read in Mainstream news Ideology Scores:
Mainstream_Scores <- read.csv('Media_Scores.csv')
Mainstream_Scores <- Mainstream_Scores %>% mutate(rating_our_score = ifelse(zeta > 0,'c','l'))
Mainstream_Scores <- Mainstream_Scores %>% select(domain,rating_our_score)
#Convert to lower case:
Mainstream_Scores$domain <- tolower(Mainstream_Scores$domain)

#Read in Low-Quality Ideology Scores:
FN_Scores <- read.csv('Fake_News_Websites_ratings.csv')
FN_Scores <- FN_Scores %>% filter(active_our_score == 'y')
FN_Scores <- FN_Scores %>% select(domain,rating_our_score)

#Combine mainstream and low-quality news:
Ideo_source <- rbind(Mainstream_Scores,FN_Scores)
#Remove duplicates:
Ideo_source <- unique(Ideo_source)
#Remove duplicates:
Ideo_source <- Ideo_source[-95,]
Ideo_source <- Ideo_source[-62,]

Ideo_source$rating_our_score <- ifelse(Ideo_source$rating_our_score == 'c','Conservative',Ideo_source$rating_our_score)
Ideo_source$rating_our_score <- ifelse(Ideo_source$rating_our_score == 'u','Unclear',Ideo_source$rating_our_score)
Ideo_source$rating_our_score <- ifelse(Ideo_source$rating_our_score == 'l','Liberal',Ideo_source$rating_our_score)

#Merge Ideological scores with larger dataset:
Ideo_Data <- merge(Data_Ideology,Ideo_source,by.x='parent_domain',by.y='domain')


########################################    FIGURES     #######################################################

#Figure 13: Histogram of NewsGuard Scores

#Set new data directory
figure_directory = paste0(master_directory,location_data_2)
setwd(figure_directory)
png(file="Histogram.png",
    width=600, height=350)
hist(NG_Rating$Score,col='grey')
dev.off()



#Figure 14: All News URLS Views by Ideology of User.  95 percent confidence intervals aredisplayed.


LQ_Ideo_Data <- Ideo_Data %>% filter(Credible == 1) 
LQ_Ideo_Data <- LQ_Ideo_Data %>% filter(rating_our_score != 'Unclear') 
Monthly_Data <- LQ_Ideo_Data %>% group_by(political_page_affinity,rating_our_score) %>% mutate(Total_Shares = sum(as.numeric(tot_shares)))
Monthly_Data <- Monthly_Data %>% group_by(political_page_affinity,rating_our_score) %>% mutate(Total_agg_nrows = sum(agg_nrows))
Monthly_Data <- Monthly_Data %>% select(political_page_affinity,Total_Shares,Total_agg_nrows,rating_our_score)
Monthly_Data <- unique(Monthly_Data)

Monthly_Data$Total_Shares <- Monthly_Data$Total_Shares/1000000

Monthly_Data$Ideology <- ifelse(Monthly_Data$political_page_affinity == -2, 'Very Lib.','Moderate')
Monthly_Data$Ideology <- ifelse(Monthly_Data$political_page_affinity == -1, 'Lib.',Monthly_Data$Ideology)
Monthly_Data$Ideology <- ifelse(Monthly_Data$political_page_affinity == 1, 'Cons.',Monthly_Data$Ideology)
Monthly_Data$Ideology <- ifelse(Monthly_Data$political_page_affinity == 2, 'Very Cons.',Monthly_Data$Ideology)


Monthly_Data$Ideology <- as.factor(Monthly_Data$Ideology)

Monthly_Data$Ideology <- factor(Monthly_Data$Ideology,levels=c('Very Lib.',
                                                               'Lib.',
                                                               'Moderate',
                                                               'Cons.',
                                                               'Very Cons.'))



Monthly_Data <- Monthly_Data %>% mutate(conf_int = ((nrows_to_std(Total_agg_nrows,14))/1000000)*2)
Monthly_Data <- Monthly_Data %>% mutate(upper = Total_Shares + conf_int)
Monthly_Data <- Monthly_Data %>% mutate(lower = Total_Shares - conf_int)



ggplot(Monthly_Data, aes(x=Ideology, y=Total_Shares)) + 
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.4,
                position=position_dodge(.9)) +
  scale_y_continuous(breaks=c(0,50,100,150),limits = c(-0.5,160)) +
  facet_wrap( ~ rating_our_score, ncol=2,scales = "free") +
  ylab('Total Shares (In Millions)\n') +
  xlab('\nIdeology of User') +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=10),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=10),
        title =element_text(size=14, face='bold'),
        legend.position = c(1, 0),
        legend.justification = c(1, 0),
        strip.text.x = element_text(size = 12, color = "blue", face = "bold"))
figure_directory = paste0(master_directory,location_data_2)
setwd(figure_directory)
ggsave("Credible_Shares_By_Ideology_By_Ideology.png",width=12)



#Figure 15: All News URLS Views by Ideology of User.  95 percent confidence intervals aredisplayed.


LQ_Ideo_Data <- Ideo_Data %>% filter(Credible == 1) 
LQ_Ideo_Data <- LQ_Ideo_Data %>% filter(rating_our_score != 'Unclear') 
Monthly_Data <- LQ_Ideo_Data %>% group_by(political_page_affinity,rating_our_score) %>% mutate(Total_Shares = sum(as.numeric(tot_views)))
Monthly_Data <- Monthly_Data %>% group_by(political_page_affinity,rating_our_score) %>% mutate(Total_agg_nrows = sum(agg_nrows))
Monthly_Data <- Monthly_Data %>% select(political_page_affinity,Total_Shares,Total_agg_nrows,rating_our_score)
Monthly_Data <- unique(Monthly_Data)

Monthly_Data$Total_Shares <- Monthly_Data$Total_Shares/1000000000

Monthly_Data$Ideology <- ifelse(Monthly_Data$political_page_affinity == -2, 'Very Lib.','Moderate')
Monthly_Data$Ideology <- ifelse(Monthly_Data$political_page_affinity == -1, 'Lib.',Monthly_Data$Ideology)
Monthly_Data$Ideology <- ifelse(Monthly_Data$political_page_affinity == 1, 'Cons.',Monthly_Data$Ideology)
Monthly_Data$Ideology <- ifelse(Monthly_Data$political_page_affinity == 2, 'Very Cons.',Monthly_Data$Ideology)


Monthly_Data$Ideology <- as.factor(Monthly_Data$Ideology)

Monthly_Data$Ideology <- factor(Monthly_Data$Ideology,levels=c('Very Lib.',
                                                               'Lib.',
                                                               'Moderate',
                                                               'Cons.',
                                                               'Very Cons.'))



Monthly_Data <- Monthly_Data %>% mutate(conf_int = ((nrows_to_std(Total_agg_nrows,14))/1000000000)*2)
Monthly_Data <- Monthly_Data %>% mutate(upper = Total_Shares + conf_int)
Monthly_Data <- Monthly_Data %>% mutate(lower = Total_Shares - conf_int)



ggplot(Monthly_Data, aes(x=Ideology, y=Total_Shares)) + 
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.4,
                position=position_dodge(.9)) +
  scale_y_continuous(breaks=c(0,5,10,15,20),limits = c(-0.1,20)) +
  facet_wrap( ~ rating_our_score, ncol=2,scales = "free") +
  ylab('Total Views (In Billions)\n') +
  xlab('\nIdeology of User') +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=10),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=10),
        title =element_text(size=14, face='bold'),
        legend.position = c(1, 0),
        legend.justification = c(1, 0),
        strip.text.x = element_text(size = 12, color = "blue", face = "bold"))

ggsave("Credible_Views_By_Ideology_By_Ideology.png",width=12)


#Figure 16:  All News URLS Shares by Whether the Article is Clickbait from Low-QualityNews Domains.  95 percent confidence intervals are displayed.


Monthly_Data <- Age_Data %>% filter(Credible == 0)
Monthly_Data <- Monthly_Data %>% group_by(age_bracket,Clickbait) %>% mutate(Total_Shares = sum(as.numeric(tot_shares)))
Monthly_Data <- Monthly_Data %>% group_by(age_bracket,Clickbait) %>% mutate(Total_agg_nrows = sum(agg_nrows))
Monthly_Data <- Monthly_Data %>% select(age_bracket,Clickbait,Total_Shares,Total_agg_nrows)
Monthly_Data <- unique(Monthly_Data)

Monthly_Data$String_1 <- ifelse(Monthly_Data$Clickbait == 1,'Clickbait','Non-Clickbait')
Monthly_Data$Type <- Monthly_Data$String_1

Monthly_Data <- na.omit(Monthly_Data)

Monthly_Data$Type <- as.factor(Monthly_Data$Type)

Monthly_Data$Type <- factor(Monthly_Data$Type,levels = c('Clickbait',
                                                         'Non-Clickbait'))


Monthly_Data$age_bracket <- as.character(Monthly_Data$age_bracket)

Monthly_Data$age_bracket <- factor(Monthly_Data$age_bracket, levels=c("18-24","25-34","35-44","45-54","55-64","65+"), ordered=TRUE)

Monthly_Data$Numbers <- as.numeric(Monthly_Data$age_bracket)


Monthly_Data$Total_Shares <- Monthly_Data$Total_Shares/1000000


Monthly_Data <- Monthly_Data %>% mutate(conf_int = ((nrows_to_std(Total_agg_nrows,14))/1000000)*2)
Monthly_Data <- Monthly_Data %>% mutate(upper = Total_Shares + conf_int)
Monthly_Data <- Monthly_Data %>% mutate(lower = Total_Shares - conf_int)

ggplot(Monthly_Data, aes(x=Numbers, y=Total_Shares, fill=Type)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.4,
                position=position_dodge(.9)) +
  scale_x_continuous(breaks=c(1:6),
                     labels=c("18-24","25-34","35-44","45-54","55-64","65+")) +
  scale_y_continuous(breaks=c(15,30,45),limits = c(0,45)) +
  ylab('Total Shares (In Millions)\n') +
  xlab('\nAge Brackets') +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12),
        title =element_text(size=14, face='bold'),
        legend.justification = c(1, 0),
        strip.text.x = element_text(size = 12, color = "blue", face = "bold"))

ggsave("Shares_By_Age_Less_Clickbait_LQ.png",width=12)



#Figure 17: All  News  URLS  Shares  by  Whether  the  Article  is  Political  from  Low-QualityNews Domains.  95 percent confidence intervals are displayed.


Monthly_Data <- Age_Data %>% filter(Credible == 0)
Monthly_Data <- Monthly_Data %>% group_by(age_bracket,Political) %>% mutate(Total_Shares = sum(as.numeric(tot_shares)))
Monthly_Data <- Monthly_Data %>% group_by(age_bracket,Political) %>% mutate(Total_agg_nrows = sum(agg_nrows))
Monthly_Data <- Monthly_Data %>% select(age_bracket,Political,Total_Shares,Total_agg_nrows)
Monthly_Data <- unique(Monthly_Data)

Monthly_Data$String_1 <- ifelse(Monthly_Data$Political == 1,'Political','Non-Political')
Monthly_Data$Type <- Monthly_Data$String_1

Monthly_Data <- na.omit(Monthly_Data)

Monthly_Data$Type <- as.factor(Monthly_Data$Type)

Monthly_Data$Type <- factor(Monthly_Data$Type,levels = c('Political',
                                                         'Non-Political'))


Monthly_Data$age_bracket <- as.character(Monthly_Data$age_bracket)

Monthly_Data$age_bracket <- factor(Monthly_Data$age_bracket, levels=c("18-24","25-34","35-44","45-54","55-64","65+"), ordered=TRUE)

Monthly_Data$Numbers <- as.numeric(Monthly_Data$age_bracket)


Monthly_Data$Total_Shares <- Monthly_Data$Total_Shares/1000000


Monthly_Data <- Monthly_Data %>% mutate(conf_int = ((nrows_to_std(Total_agg_nrows,14))/1000000)*2)
Monthly_Data <- Monthly_Data %>% mutate(upper = Total_Shares + conf_int)
Monthly_Data <- Monthly_Data %>% mutate(lower = Total_Shares - conf_int)


ggplot(Monthly_Data, aes(x=Numbers, y=Total_Shares, fill=Type)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.4,
                position=position_dodge(.9)) +
  scale_x_continuous(breaks=c(1:6),
                     labels=c("18-24","25-34","35-44","45-54","55-64","65+")) +
  scale_y_continuous(breaks=c(15,30,45),limits = c(0,45)) +
  ylab('Total Shares (In Millions)\n') +
  xlab('\nAge Brackets') +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=12),
        title =element_text(size=14, face='bold'),
        legend.justification = c(1, 0),
        strip.text.x = element_text(size = 12, color = "blue", face = "bold"))

ggsave("Shares_By_Age_Less_Political_LQ.png",width=12)







