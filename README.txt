################################################    README #########################################
### Title: Cracking Open the News Feed: Exploring What U.S. Facebook Users See and Share with Large-Scale Platform Data
### Authors: Andrew Guess, Kevin Aslett, Jonathan Nagler, Richard Bonneaua, and Joshua A. Tucker
###Abstract: In this study, we analyze for the first time newly available engagement data covering millions of web links shared on Facebook to describe how and by which categories of U.S. users different types of news are seen and shared on the platform. We focus on articles from ``fake news'' publishers, credible news sources, purveyors of clickbait, and news specifically about politics, which we identify through a combination of curated lists and supervised classifiers. Our results support recent findings that more fake news is shared by older users and conservatives and that both viewing and sharing patterns suggest a preference for ideologically congenial misinformation. We also find that fake news articles related to politics are more popular among older Americans than other types, while the youngest users share relatively more articles with clickbait headlines. Across the platform, however, articles from credible news sources are shared over 5 times more often and viewed over 7 times more often than articles from untrustworthy sources. These findings offer important context for researchers studying the spread and consumption of information --- including misinformation --- on social media.


Instructions:
(1) Load the full directory
(2) Open RStudio and make sure the environment in RStudio is empty. Then load the R files into RStudio and run each file.

There are four directories:
(1) code: In the code directory there lies two R files that produce important figures and tables in the main document and the supplementary methods and materials:
	(1) Figures_Tables_in_Paper.R: Produces figures for the main paper.
	(2) Figures_Tables_in_Supplementary_Materials.R: Produces figures and tables for the supplementary materials and methods.

There also lies one python file and two R files that collect and clean the data:
	(1) SS1_Data_Pull_Monthly_Views_Shares.py: Pulls the data from Social Science One dataset
	(2) Create_Age_Bracket_Data.R: Creates data aggregated by age bracket for each URL ID
	(3) Create_Age_Ideology_Data.R: Creates data aggregated by age bracket and policy page affinity for each URL ID 

(2) data: All the data needed to produce the figures and tables in the paper are located in this directory.

(3) figures: All the figures produced by the two R files in the code directory are located here.

(4) tables: All the tables (in .txt files) produced by the two R files in the code directory are located here.







 