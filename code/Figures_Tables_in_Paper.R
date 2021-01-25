

########################################################################################################################################################
### Title: Cracking Open the News Feed: Exploring What U.S. Facebook Users See and Share with Large-Scale Platform Data
### Authors: Andrew M. Guess, Kevin Aslett, Jonathan Nagler, Richard Bonneaua, and Joshua A. Tucker
### Purpose of code: Produce figures and tables that are displayed in the main text of the paper.


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

#Figure 1: News URL Shares by Source Credibility. 95% confidence intervals are displayed.

Monthly_Data <- Data_Ideology %>% group_by(Credible) %>% mutate(Total_Shares = sum(as.numeric(tot_shares)))
Monthly_Data <- Monthly_Data %>% group_by(Credible) %>% mutate(Total_agg_nrows = sum(agg_nrows))
Monthly_Data <- Monthly_Data %>% select(Credible,Total_Shares,Total_agg_nrows)
Monthly_Data <- unique(Monthly_Data)

Monthly_Data$String_1 <- ifelse(Monthly_Data$Credible == 1,'Credible','Low-Quality')
Monthly_Data$Type <- Monthly_Data$String_1
Monthly_Data <- na.omit(Monthly_Data)
Monthly_Data$Type <- as.factor(Monthly_Data$Type)
Monthly_Data$Type <- factor(Monthly_Data$Type,levels = c('Low-Quality',
                                                         'Credible'))


Monthly_Data$Total_Shares <- Monthly_Data$Total_Shares/1000000

#Create confidence intervals:
Monthly_Data <- Monthly_Data %>% mutate(conf_int = ((nrows_to_std(Total_agg_nrows,14))/1000000)*2)
Monthly_Data <- Monthly_Data %>% mutate(upper = Total_Shares + conf_int)
Monthly_Data <- Monthly_Data %>% mutate(lower = Total_Shares - conf_int)


#Proportion of shares that are from low-quality news:
Monthly_Data$Total_Shares[1]/(Monthly_Data$Total_Shares[2]+Monthly_Data$Total_Shares[1])

ggplot(Monthly_Data, aes(x=Type, y=Total_Shares,fill=Type)) + 
  geom_bar(stat='identity',width=.6) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.4,
                position=position_dodge(.9)) +
  scale_y_continuous(breaks=c(00,200,400,600,800,1000),limits = c(0,1000)) +
  ylab('Total Shares (In Millions)\n') +
  xlab('\nCredibility of News') +
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

figure_directory = paste0(master_directory,location_data_2)
setwd(figure_directory)
ggsave("Shares_Less_Low_Quality.png",width=12)





#Figure 2: News URL Views by Source Credibility. 95% confidence intervals are displayed.

Monthly_Data <- Data_Ideology %>% group_by(Credible) %>% mutate(Total_Shares = sum(as.numeric(tot_views)))
Monthly_Data <- Monthly_Data %>% group_by(Credible) %>% mutate(Total_agg_nrows = sum(agg_nrows))
Monthly_Data <- Monthly_Data %>% select(Credible,Total_Shares,Total_agg_nrows)
Monthly_Data <- unique(Monthly_Data)

Monthly_Data$String_1 <- ifelse(Monthly_Data$Credible == 1,'Credible','Low-Quality')
Monthly_Data$Type <- Monthly_Data$String_1
Monthly_Data <- na.omit(Monthly_Data)
Monthly_Data$Type <- as.factor(Monthly_Data$Type)
Monthly_Data$Type <- factor(Monthly_Data$Type,levels = c('Low-Quality',
                                                         'Credible'))


Monthly_Data$Total_Shares <- Monthly_Data$Total_Shares/1000000000

#Create confidence intervals:
Monthly_Data <- Monthly_Data %>% mutate(conf_int = ((nrows_to_std(Total_agg_nrows,14))/1000000000)*2)
Monthly_Data <- Monthly_Data %>% mutate(upper = Total_Shares + conf_int)
Monthly_Data <- Monthly_Data %>% mutate(lower = Total_Shares - conf_int)


#Proportion of views that are from low-quality news:
Monthly_Data$Total_Shares[1]/(Monthly_Data$Total_Shares[2]+Monthly_Data$Total_Shares[1])


ggplot(Monthly_Data, aes(x=Type, y=Total_Shares,fill=Type)) + 
  geom_bar(stat='identity',width=.6) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.4,
                position=position_dodge(.9)) +
  scale_y_continuous(breaks=c(00,30,60,90,120),limits = c(0,120)) +
  ylab('Total Views (In Billions)\n') +
  xlab('\nCredibility of News') +
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

figure_directory = paste0(master_directory,location_data_2)
setwd(figure_directory)
ggsave("Views_Less_Low_Quality.png",width=12)



#Figure 3: News URL Shares by User Ideology. 95% confidence intervals are displayed.

Monthly_Data <- Data_Ideology %>% group_by(political_page_affinity,Credible) %>% mutate(Total_Shares = sum(as.numeric(tot_shares)))
Monthly_Data <- Monthly_Data %>% group_by(political_page_affinity,Credible) %>% mutate(Total_agg_nrows = sum(agg_nrows))
Monthly_Data <- Monthly_Data %>% select(political_page_affinity,Credible,Total_Shares,Total_agg_nrows)
Monthly_Data <- unique(Monthly_Data)

Monthly_Data$String_1 <- ifelse(Monthly_Data$Credible == 1,'Credible','Low-Quality')
Monthly_Data$Type <- Monthly_Data$String_1
Monthly_Data <- na.omit(Monthly_Data)
Monthly_Data$Type <- as.factor(Monthly_Data$Type)
Monthly_Data$Type <- factor(Monthly_Data$Type,levels = c('Low-Quality',
                                                         'Credible'))


Monthly_Data$Ideology <- ifelse(Monthly_Data$political_page_affinity == -2, 'Very Lib.','Moderate')
Monthly_Data$Ideology <- ifelse(Monthly_Data$political_page_affinity == -1, 'Lib.',Monthly_Data$Ideology)
Monthly_Data$Ideology <- ifelse(Monthly_Data$political_page_affinity == 1, 'Cons.',Monthly_Data$Ideology)
Monthly_Data$Ideology <- ifelse(Monthly_Data$political_page_affinity == 2, 'Very Cons.',Monthly_Data$Ideology)


Monthly_Data$Ideology <- factor(Monthly_Data$Ideology,levels=c('Very Lib.',
                                                               'Lib.',
                                                               'Moderate',
                                                               'Cons.',
                                                               'Very Cons.'))

Monthly_Data$Numbers <- as.numeric(Monthly_Data$Ideology)


Monthly_Data$Total_Shares <- Monthly_Data$Total_Shares/1000000

#Create confidence intervals:
Monthly_Data <- Monthly_Data %>% mutate(conf_int = ((nrows_to_std(Total_agg_nrows,14))/1000000)*2)
Monthly_Data <- Monthly_Data %>% mutate(upper = Total_Shares + conf_int)
Monthly_Data <- Monthly_Data %>% mutate(lower = Total_Shares - conf_int)

Monthly_Data$Numbers <- as.numeric(Monthly_Data$Ideology)

#Shares of low-quality news by those that are very conservative
v_cons_data <- Monthly_Data %>% filter(Ideology == 'Very Cons.')
v_cons_data$Total_Shares[1]/(v_cons_data$Total_Shares[1]+v_cons_data$Total_Shares[2])

#Shares of low-quality news by those that are very liberal
v_libs_data <- Monthly_Data %>% filter(Ideology == 'Very Lib.')
v_libs_data$Total_Shares[1]/(v_libs_data$Total_Shares[1]+v_libs_data$Total_Shares[2])

ggplot(Monthly_Data, aes(x=Numbers, y=Total_Shares, fill=Type)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.4,
                position=position_dodge(.9)) +
  scale_x_continuous(breaks=c(1:5),
                     labels=c('Very Liberal','Liberal','Moderate','Conservative','Very Conservative')) +
  scale_y_continuous(breaks=c(50,150,250),limits = c(0,250)) +
  ylab('Total Shares (In Millions)\n') +
  xlab('\nIdeology of User') +
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

figure_directory = paste0(master_directory,location_data_2)
setwd(figure_directory)
ggsave("Shares_By_Ideology_Less_Low_Quality.png",width=12)



#Figure 4: News URL Views by User Ideology. 95% confidence intervals are displayed.


Monthly_Data <- Data_Ideology %>% group_by(political_page_affinity,Credible) %>% mutate(Total_Shares = sum(as.numeric(tot_views)))
Monthly_Data <- Monthly_Data %>% group_by(political_page_affinity,Credible) %>% mutate(Total_agg_nrows = sum(agg_nrows))
Monthly_Data <- Monthly_Data %>% select(political_page_affinity,Credible,Total_Shares,Total_agg_nrows)
Monthly_Data <- unique(Monthly_Data)

Monthly_Data$String_1 <- ifelse(Monthly_Data$Credible == 1,'Credible','Low-Quality')
Monthly_Data$Type <- Monthly_Data$String_1
Monthly_Data <- na.omit(Monthly_Data)
Monthly_Data$Type <- as.factor(Monthly_Data$Type)
Monthly_Data$Type <- factor(Monthly_Data$Type,levels = c('Low-Quality',
                                                         'Credible'))


Monthly_Data$Ideology <- ifelse(Monthly_Data$political_page_affinity == -2, 'Very Lib.','Moderate')
Monthly_Data$Ideology <- ifelse(Monthly_Data$political_page_affinity == -1, 'Lib.',Monthly_Data$Ideology)
Monthly_Data$Ideology <- ifelse(Monthly_Data$political_page_affinity == 1, 'Cons.',Monthly_Data$Ideology)
Monthly_Data$Ideology <- ifelse(Monthly_Data$political_page_affinity == 2, 'Very Cons.',Monthly_Data$Ideology)


Monthly_Data$Ideology <- factor(Monthly_Data$Ideology,levels=c('Very Lib.',
                                                               'Lib.',
                                                               'Moderate',
                                                               'Cons.',
                                                               'Very Cons.'))

Monthly_Data$Numbers <- as.numeric(Monthly_Data$Ideology)


Monthly_Data$Total_Shares <- Monthly_Data$Total_Shares/1000000000


Monthly_Data <- Monthly_Data %>% mutate(conf_int = ((nrows_to_std(Total_agg_nrows,14))/1000000000)*2)
Monthly_Data <- Monthly_Data %>% mutate(upper = Total_Shares + conf_int)
Monthly_Data <- Monthly_Data %>% mutate(lower = Total_Shares - conf_int)

Monthly_Data$Numbers <- as.numeric(Monthly_Data$Ideology)


#Viewss of low-quality news by those that are very conservative
v_cons_data <- Monthly_Data %>% filter(Ideology == 'Very Cons.')
v_cons_data$Total_Shares[1]/(v_cons_data$Total_Shares[1]+v_cons_data$Total_Shares[2])

#Views of low-quality news by those that are very liberal
v_libs_data <- Monthly_Data %>% filter(Ideology == 'Very Lib.')
v_libs_data$Total_Shares[1]/(v_libs_data$Total_Shares[1]+v_libs_data$Total_Shares[2])


ggplot(Monthly_Data, aes(x=Numbers, y=Total_Shares, fill=Type)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.4,
                position=position_dodge(.9)) +
  scale_x_continuous(breaks=c(1:5),
                     labels=c('Very Liberal','Liberal','Moderate','Conservative','Very Conservative')) +
  scale_y_continuous(breaks=c(5,15,25,35),limits = c(0,35)) +
  ylab('Total Views (In Billions)\n') +
  xlab('\nIdeology of User') +
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

figure_directory = paste0(master_directory,location_data_2)
setwd(figure_directory)
ggsave("Views_By_Ideology_Less_Low_Quality.png",width=12)



#Figure 5: Low-Quality News URL Shares by User Ideology and News Slant. 95% confidence intervals are displayed.

LQ_Ideo_Data <- Ideo_Data %>% filter(Credible == 0) 
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
  scale_y_continuous(breaks=c(0,5,10,15,20),limits = c(-0.5,20)) +
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
ggsave("LQ_Shares_By_Ideology_By_Ideology.png",width=12)


#Figure 6: Low-Quality News URL Views by User Ideology and News Slant. 95\% confidence intervals are displayed.

LQ_Ideo_Data <- Ideo_Data %>% filter(Credible == 0) 
Monthly_Data <- LQ_Ideo_Data %>% group_by(political_page_affinity,rating_our_score) %>% mutate(Total_Shares = sum(as.numeric(tot_views)))
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
  scale_y_continuous(breaks=c(0,500,1000,1500),limits = c(0,1500)) +
  facet_wrap( ~ rating_our_score, ncol=2,scales = "free") +
  ylab('Total Views (In Millions)\n') +
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
ggsave("LQ_Views_By_Ideology_By_Ideology.png",width=12)


#Figure 7: News URL Shares by Age Group. 95% confidence intervals are displayed.


Monthly_Data <- Age_Data %>% group_by(age_bracket,Credible) %>% mutate(Total_Shares = sum(as.numeric(tot_shares)))
Monthly_Data <- Monthly_Data %>% group_by(age_bracket,Credible) %>% mutate(Total_agg_nrows = sum(agg_nrows))
Monthly_Data <- Monthly_Data %>% select(age_bracket,Credible,Total_Shares,Total_agg_nrows)
Monthly_Data <- unique(Monthly_Data)

Monthly_Data$String_1 <- ifelse(Monthly_Data$Credible == 1,'Credible','Low-Quality')
Monthly_Data$Type <- Monthly_Data$String_1

Monthly_Data <- na.omit(Monthly_Data)

Monthly_Data$Type <- as.factor(Monthly_Data$Type)

Monthly_Data$Type <- factor(Monthly_Data$Type,levels = c('Low-Quality',
                                                         'Credible'))


Monthly_Data$age_bracket <- as.character(Monthly_Data$age_bracket)

Monthly_Data$age_bracket <- factor(Monthly_Data$age_bracket, levels=c("18-24","25-34","35-44","45-54","55-64","65+"), ordered=TRUE)

Monthly_Data$Numbers <- as.numeric(Monthly_Data$age_bracket)


Monthly_Data$Total_Shares <- Monthly_Data$Total_Shares/1000000


Monthly_Data <- Monthly_Data %>% mutate(conf_int = ((nrows_to_std(Total_agg_nrows,14))/1000000)*2)
Monthly_Data <- Monthly_Data %>% mutate(upper = Total_Shares + conf_int)
Monthly_Data <- Monthly_Data %>% mutate(lower = Total_Shares - conf_int)

#65+:
old_data <- Monthly_Data %>% filter(age_bracket == '65+')
old_data$Total_Shares[1]/(old_data$Total_Shares[1]+old_data$Total_Shares[2])
#18-24:
young_data <- Monthly_Data %>% filter(age_bracket == '18-24')
young_data$Total_Shares[1]/(young_data$Total_Shares[1]+young_data$Total_Shares[2])

ggplot(Monthly_Data, aes(x=Numbers, y=Total_Shares, fill=Type)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.4,
                position=position_dodge(.9)) +
  scale_x_continuous(breaks=c(1:6),
                     labels=c("18-24","25-34","35-44","45-54","55-64","65+")) +
  scale_y_continuous(breaks=c(50,150,250),limits = c(0,250)) +
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

figure_directory = paste0(master_directory,location_data_2)
setwd(figure_directory)
ggsave("Shares_By_Age_Less_Low_Quality.png",width=12)


#Figure 8: News URL Views by Age Group. 95% confidence intervals are displayed.

Monthly_Data <- Age_Data %>% group_by(age_bracket,Credible) %>% mutate(Total_Views = sum(as.numeric(tot_views)))
Monthly_Data <- Monthly_Data %>% group_by(age_bracket,Credible) %>% mutate(Total_agg_nrows = sum(agg_nrows))
Monthly_Data <- Monthly_Data %>% select(age_bracket,Credible,Total_Views,Total_agg_nrows)
Monthly_Data <- unique(Monthly_Data)

Monthly_Data$String_1 <- ifelse(Monthly_Data$Credible == 1,'Credible','Low-Quality')
Monthly_Data$Type <- Monthly_Data$String_1

Monthly_Data <- na.omit(Monthly_Data)

Monthly_Data$Type <- as.factor(Monthly_Data$Type)

Monthly_Data$Type <- factor(Monthly_Data$Type,levels = c('Low-Quality',
                                                         'Credible'))


Monthly_Data$age_bracket <- as.character(Monthly_Data$age_bracket)

Monthly_Data$age_bracket <- factor(Monthly_Data$age_bracket, levels=c("18-24","25-34","35-44","45-54","55-64","65+"), ordered=TRUE)

Monthly_Data$Numbers <- as.numeric(Monthly_Data$age_bracket)


Monthly_Data$Total_Views <- Monthly_Data$Total_Views/1000000000


Monthly_Data <- Monthly_Data %>% mutate(conf_int = ((nrows_to_std(Total_agg_nrows,2228))/1000000000)*2)
Monthly_Data <- Monthly_Data %>% mutate(upper = Total_Views + conf_int)
Monthly_Data <- Monthly_Data %>% mutate(lower = Total_Views - conf_int)

#65+:
old_data <- Monthly_Data %>% filter(age_bracket == '65+')
old_data$Total_Views[1]/(old_data$Total_Views[1]+old_data$Total_Views[2])
#18-24:
young_data <- Monthly_Data %>% filter(age_bracket == '18-24')
young_data$Total_Views[1]/(young_data$Total_Views[1]+young_data$Total_Views[2])


ggplot(Monthly_Data, aes(x=Numbers, y=Total_Views, fill=Type)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.4,
                position=position_dodge(.9)) +
  scale_x_continuous(breaks=c(1:6),
                     labels=c("18-24","25-34","35-44","45-54","55-64","65+")) +
  scale_y_continuous(breaks=c(10,20,30),limits = c(0,30)) +
  ylab('Total Views (In Billions)\n') +
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

figure_directory = paste0(master_directory,location_data_2)
setwd(figure_directory)
ggsave("Views_By_Age_Less_Low_Quality.png",width=12)



#Figure 9: News URL Shares by Ideology and Age Group. 95% confidence intervals are displayed.
Monthly_Data <- Data_Ideology %>% group_by(age_bracket,political_page_affinity,Credible) %>% mutate(Total_Shares = sum(as.numeric(tot_shares)))
Monthly_Data <- Monthly_Data %>% group_by(age_bracket,Credible,political_page_affinity) %>% mutate(Total_agg_nrows = sum(agg_nrows))
Monthly_Data <- Monthly_Data %>% select(age_bracket,political_page_affinity,Credible,Total_Shares,Total_agg_nrows)
Monthly_Data <- unique(Monthly_Data)

Monthly_Data$String_1 <- ifelse(Monthly_Data$Credible == 1,'Credible','Low-Quality')
Monthly_Data$Type <- Monthly_Data$String_1

Monthly_Data <- na.omit(Monthly_Data)

Monthly_Data$Type <- as.factor(Monthly_Data$Type)

Monthly_Data$Type <- factor(Monthly_Data$Type,levels = c('Low-Quality',
                                                         'Credible'))


Monthly_Data$age_bracket <- as.character(Monthly_Data$age_bracket)

Monthly_Data$age_bracket <- factor(Monthly_Data$age_bracket, levels=c("18-24","25-34","35-44","45-54","55-64","65+"), ordered=TRUE)

Monthly_Data$Numbers <- as.numeric(Monthly_Data$age_bracket)


Monthly_Data$Total_Shares <- Monthly_Data$Total_Shares/1000000



Monthly_Data$Ideology <- ifelse(Monthly_Data$political_page_affinity == -2, 'Extremely Liberal','Moderate')
Monthly_Data$Ideology <- ifelse(Monthly_Data$political_page_affinity == -1, 'Liberal',Monthly_Data$Ideology)
Monthly_Data$Ideology <- ifelse(Monthly_Data$political_page_affinity == 1, 'Conservative',Monthly_Data$Ideology)
Monthly_Data$Ideology <- ifelse(Monthly_Data$political_page_affinity == 2, 'Extremely Conservative',Monthly_Data$Ideology)


Monthly_Data$Ideology <- as.factor(Monthly_Data$Ideology)

Monthly_Data$Ideology <- factor(Monthly_Data$Ideology,levels=c('Extremely Liberal',
                                                               'Liberal',
                                                               'Moderate',
                                                               'Conservative',
                                                               'Extremely Conservative'))


Monthly_Data <- Monthly_Data %>% mutate(conf_int = ((nrows_to_std(Total_agg_nrows,14))/1000000)*2)
Monthly_Data <- Monthly_Data %>% mutate(upper = Total_Shares + conf_int)
Monthly_Data <- Monthly_Data %>% mutate(lower = Total_Shares - conf_int)



ggplot(Monthly_Data, aes(x=Numbers, y=Total_Shares, fill=Type)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.4,
                position=position_dodge(.9)) +
  scale_x_continuous(breaks=c(1:6),
                     labels=c("18-24","25-34","35-44","45-54","55-64","65+")) +
  scale_y_continuous(breaks=c(0,25,50,75),limits = c(0,75)) +
  facet_wrap( ~ Ideology, ncol=3,scales = "free") +
  ylab('Total Shares (In Millions)\n') +
  xlab('\nAge Brackets') +
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
ggsave("Shares_By_Age_Ideology_Credibility_Less.png",width=12)


#Create Data for Table 1:

#Create URL Data
URL_Data <- Age_Data %>% 
  ungroup() %>% 
  select(url_rid,
         Political,
         Clickbait,
         Credible)

URL_Data <- unique(URL_Data)


Cred_Click_Pol = nrow(URL_Data %>% filter(Credible == 1 & Clickbait == 1 & Political == 1))
Cred_Click_NPol = nrow(URL_Data %>% filter(Credible == 1 & Clickbait == 1 & Political == 0))
Cred_NClick_Pol = nrow(URL_Data %>% filter(Credible == 1 & Clickbait == 0 & Political == 1))
Cred_NClick_NPol = nrow(URL_Data %>% filter(Credible == 1 & Clickbait == 0 & Political == 0))


LQ_Click_Pol = nrow(URL_Data %>% filter(Credible == 0 & Clickbait == 1 & Political == 1))
LQ_Click_NPol = nrow(URL_Data %>% filter(Credible == 0 & Clickbait == 1 & Political == 0))
LQ_NClick_Pol = nrow(URL_Data %>% filter(Credible == 0 & Clickbait == 0 & Political == 1))
LQ_NClick_NPol = nrow(URL_Data %>% filter(Credible == 0 & Clickbait == 0 & Political == 0))


matrix(c('Yes','Yes',paste(round((Cred_Click_Pol/(Cred_Click_Pol+Cred_Click_NPol+Cred_NClick_Pol+Cred_NClick_NPol))*100,1),' %'),
  'Yes','No',paste0(round((Cred_Click_NPol/(Cred_Click_Pol+Cred_Click_NPol+Cred_NClick_Pol+Cred_NClick_NPol))*100,1),' %'),
  'No','Yes',paste0(round((Cred_NClick_Pol/(Cred_Click_Pol+Cred_Click_NPol+Cred_NClick_Pol+Cred_NClick_NPol))*100,1),' %'),
  'No','No',paste0(round((Cred_NClick_NPol/(Cred_Click_Pol+Cred_Click_NPol+Cred_NClick_Pol+Cred_NClick_NPol))*100,1),' %'),
  'Yes','Yes',paste0(round((LQ_Click_Pol/(LQ_Click_Pol+LQ_Click_NPol+LQ_NClick_Pol+LQ_NClick_NPol))*100,1),' %'),
  'Yes','No',paste0(round((LQ_Click_NPol/(LQ_Click_Pol+LQ_Click_NPol+LQ_NClick_Pol+LQ_NClick_NPol))*100,1),' %'),
  'No','Yes',paste0(round((LQ_NClick_Pol/(LQ_Click_Pol+LQ_Click_NPol+LQ_NClick_Pol+LQ_NClick_NPol))*100,1),' %'),
  'No','No',paste0(round((LQ_NClick_NPol/(LQ_Click_Pol+LQ_Click_NPol+LQ_NClick_Pol+LQ_NClick_NPol))*100,1),' %')),ncol=3,byrow=T)






#Figure 10: News URL Shares by Age Category and Clickbait. 95% confidence intervals are displayed.

Monthly_Data <- Age_Data %>% group_by(age_bracket,Clickbait) %>% mutate(Total_Shares = sum(as.numeric(tot_shares)))
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


#65+:
old_data <- Monthly_Data %>% filter(age_bracket == '65+')
old_data$Total_Shares[1]/(old_data$Total_Shares[1]+old_data$Total_Shares[2])
#18-24:
young_data <- Monthly_Data %>% filter(age_bracket == '18-24')
young_data$Total_Shares[1]/(young_data$Total_Shares[1]+young_data$Total_Shares[2])


ggplot(Monthly_Data, aes(x=Numbers, y=Total_Shares, fill=Type)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.4,
                position=position_dodge(.9)) +
  scale_x_continuous(breaks=c(1:6),
                     labels=c("18-24","25-34","35-44","45-54","55-64","65+")) +
  scale_y_continuous(breaks=c(50,150,250),limits = c(0,250)) +
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

figure_directory = paste0(master_directory,location_data_2)
setwd(figure_directory)
ggsave("Shares_By_Age_Less_Clickbait.png",width=12)

#Figure 11: News URL Shares by Age Category and Political Topic. 95% confidence intervals are displayed.

Monthly_Data <- Age_Data %>% group_by(age_bracket,Political) %>% mutate(Total_Shares = sum(as.numeric(tot_shares)))
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


#65+:
old_data <- Monthly_Data %>% filter(age_bracket == '65+')
old_data$Total_Shares[1]/(old_data$Total_Shares[1]+old_data$Total_Shares[2])
#18-24:
young_data <- Monthly_Data %>% filter(age_bracket == '18-24')
young_data$Total_Shares[1]/(young_data$Total_Shares[1]+young_data$Total_Shares[2])



ggplot(Monthly_Data, aes(x=Numbers, y=Total_Shares, fill=Type)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.4,
                position=position_dodge(.9)) +
  scale_x_continuous(breaks=c(1:6),
                     labels=c("18-24","25-34","35-44","45-54","55-64","65+")) +
  scale_y_continuous(breaks=c(50,150,250),limits = c(0,250)) +
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

figure_directory = paste0(master_directory,location_data_2)
setwd(figure_directory)
ggsave("Shares_By_Age_Less_Political.png",width=12)

#Figure 12: News URL Shares from Low-Quality News Domains. 95% confidence intervals are displayed.

Monthly_Data <- Age_Data %>% filter(Credible == 0)
Monthly_Data <- Monthly_Data %>% group_by(age_bracket,Clickbait,Political) %>% mutate(Total_Views = sum(as.numeric(tot_shares)))
Monthly_Data <- Monthly_Data %>% group_by(age_bracket,Clickbait,Political) %>% mutate(Total_agg_nrows = sum(agg_nrows))
Monthly_Data <- Monthly_Data %>% select(age_bracket,Clickbait,Political,Total_Views,Total_agg_nrows)
Monthly_Data <- unique(Monthly_Data)

Monthly_Data$String_1 <- ifelse(Monthly_Data$Political == 1,'Political ','Non-Political ')
Monthly_Data$String_2 <- ifelse(Monthly_Data$Clickbait == 1,'Clickbait','Non-Clickbait')
Monthly_Data$Type <- paste0(Monthly_Data$String_1,Monthly_Data$String_2)

Monthly_Data <- na.omit(Monthly_Data)

Monthly_Data$Type <- factor(Monthly_Data$Type, levels = c('Political Non-Clickbait',
                                                          'Political Clickbait',
                                                          'Non-Political Non-Clickbait',
                                                          'Non-Political Clickbait'))


Monthly_Data$age_bracket <- as.character(Monthly_Data$age_bracket)
Monthly_Data$age_bracket <- factor(Monthly_Data$age_bracket, levels=c("18-24","25-34","35-44","45-54","55-64","65+"), ordered=TRUE)
Monthly_Data$Numbers <- as.numeric(Monthly_Data$age_bracket)
Monthly_Data$Total_Views <- Monthly_Data$Total_Views/1000000
Monthly_Data$Type <- factor(Monthly_Data$Type,levels = c('Non-Political Clickbait',
                                                         'Non-Political Non-Clickbait',
                                                         'Political Clickbait',
                                                         'Political Non-Clickbait'))




Monthly_Data <- Monthly_Data %>% mutate(conf_int = ((nrows_to_std(Total_agg_nrows,14))/1000000)*2)
Monthly_Data <- Monthly_Data %>% mutate(upper = Total_Views + conf_int)
Monthly_Data <- Monthly_Data %>% mutate(lower = Total_Views - conf_int)





#65+ Political proportion:
old_data <- Monthly_Data %>% filter(age_bracket == '65+')
(old_data$Total_Views[1]+old_data$Total_Views[3])/(sum(old_data$Total_Views))
#18-24 Political proportion:
young_data <- Monthly_Data %>% filter(age_bracket == '18-24')
(young_data$Total_Views[1]+young_data$Total_Views[2])/(sum(young_data$Total_Views))

#65+ Clickbait proportion:
old_data <- Monthly_Data %>% filter(age_bracket == '65+')
(old_data$Total_Views[3]+old_data$Total_Views[4])/(sum(old_data$Total_Views))
#18-24 Clickbait proportion:
young_data <- Monthly_Data %>% filter(age_bracket == '18-24')
(young_data$Total_Views[2]+young_data$Total_Views[4])/(sum(young_data$Total_Views))

#65+ Clickbait proportion:
old_data <- Monthly_Data %>% filter(age_bracket == '65+')
(old_data$Total_Views[1])/(sum(old_data$Total_Views))
#18-24 Clickbait proportion:
young_data <- Monthly_Data %>% filter(age_bracket == '18-24')
(young_data$Total_Views[1])/(sum(young_data$Total_Views))


ggplot(Monthly_Data, aes(x = Numbers, y = Total_Views, fill=Type)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.4,
                position=position_dodge(.9)) +  
  scale_x_continuous(breaks=c(1:6),
                     labels=c("18-24","25-34","35-44","45-54","55-64","65+"), expand = c(0, 0)) +
  scale_y_continuous(breaks=c(0,10,20,30),limits = c(0,30)) +
  ylab('Total Shares (In Millions) \n') +
  xlab('\n Age Brackets') +
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
        legend.text = element_text(size=14))

figure_directory = paste0(master_directory,location_data_2)
setwd(figure_directory)
ggsave("Shares_By_Age_Clickbait_Political_LQ.png",width=12)







