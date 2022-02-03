

# Title: Cracking Open the News Feed: Exploring What U.S. Facebook Users See and Share with Large-Scale Platform Data

## Authors: Andrew Guess, Kevin Aslett, Jonathan Nagler, Richard Bonneau, and Joshua A. Tucker

### Abstract: 

In this study, we analyze for the first time newly available engagement data covering millions of web links shared on Facebook to describe how and by which categories of U.S. users different types of news are seen and shared on the platform. We focus on articles from ``fake news'' publishers, credible news sources, purveyors of clickbait, and news specifically about politics, which we identify through a combination of curated lists and supervised classifiers. Our results support recent findings that more fake news is shared by older users and conservatives and that both viewing and sharing patterns suggest a preference for ideologically congenial misinformation. We also find that fake news articles related to politics are more popular among older Americans than other types, while the youngest users share relatively more articles with clickbait headlines. Across the platform, however, articles from credible news sources are shared over 5 times more often and viewed over 7 times more often than articles from untrustworthy sources. These findings offer important context for researchers studying the spread and consumption of information --- including misinformation --- on social media.

Note: Due to privacy resrictions we cannot make the data publicly available, but we will make the code available.

### Two directories:

#### (1) code: In the code directory there lies two folders of code. One folder has the code used to produces the figures for the first draft and another folder that produces the figures for the second draft:
	
#### (1.1) First_Draft: All code used to create the figures in the first draft

(1) First_SS1_Data_Pull.py: Pulls the data from Social Science One dataset for the main draft.
		
(2) Main_Draft_Create_Age_Bracket_Data.py: Creates data aggregated by age bracket for each URL ID for the main paper.
		
(3) Main_Draft_Create_Age_Ideology_Data.py: Creates data aggregated by age bracket and policy page affinity for each URL ID for the main draft.
		
(4) Main_Text_Figures.R: Produces figures for the main paper.

#### (1.2) Second_Draft:  All code used to create the figures in the second draft

(1) Second_SS1_Data_Pull.py: Pulls the data from Social Science One dataset for the second draft
		
(2) Second_Draft_Create_Age_Bracket_Data.py: Creates data aggregated by age bracket for each URL ID for the main paper		(3) Second_Draft_Create_Age_Ideology_Data.py: Creates data aggregated by age bracket and policy page affinity for each URL ID for the second draft		(4)Second_Draft_Figures.R: Produces figures for the second paper.

#### (2) figures: All the figures produced by the two R files in the code directory are located here.

