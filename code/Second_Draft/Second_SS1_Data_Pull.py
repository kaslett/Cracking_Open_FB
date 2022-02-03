#Second_SS1_Data_Pull

"""""
Author: Kevin Aslett
Goal of Code: Produce view and share data for URLs from news domains with a NewsGuard ranking each month
INPUT: 
(1) NewsGuard_Ratings.csv
OUTPUT:
(1) Second_NewsG_VIEWS_AGE_PPA_Jan_2018.tsv
(2) Second_NewsG_VIEWS_AGE_PPA_Feb_2018.tsv
(3) Second_NewsG_VIEWS_AGE_PPA_Mar_2018.tsv
(4) Second_NewsG_VIEWS_AGE_PPA_Apr_2018.tsv
(5) Second_NewsG_VIEWS_AGE_PPA_May_2018.tsv
(6) Second_NewsG_VIEWS_AGE_PPA_Jun_2018.tsv
(7) Second_NewsG_VIEWS_AGE_PPA_Jul_2018.tsv
(8) Second_NewsG_VIEWS_AGE_PPA_Aug_2018_1.tsv
(8) Second_NewsG_VIEWS_AGE_PPA_Aug_2018_2.tsv
(9) Second_NewsG_VIEWS_AGE_PPA_Sep_2018_1.tsv
(9) Second_NewsG_VIEWS_AGE_PPA_Sep_2018_2.tsv
(10) Second_NewsG_VIEWS_AGE_PPA_Oct_2018.tsv
(11) Second_NewsG_VIEWS_AGE_PPA_Nov_2018.tsv
(12) Second_NewsG_VIEWS_AGE_PPA_Dec_2018.tsv
"""

#Import the libraries we need to use:
import pandas as pd
from fbri.private.sql.query import execute
import os

#Name the database that we are querying:
database = "fbri_prod_private"

#Name the url table we are querying:
url_table = "erc_condor_url_attributes_dp_final_v3" 

#Name the breakdown table we are querying:
bd_table = "erc_condor_url_breakdowns_dp_clean_partitioned_v2"

os.chdir('/home/jovyan/New_Clean/')


#Create a list of news domains with which we have NewsGuard Ratings. We will only pull data from news domains that have a NewsGuard rating
media_scores_df = pd.read_csv("NewsGuard_Ratings.csv")
target_domains = {tld.lower() for tld in media_scores_df["Domain"]}
print(len(target_domains))
target_domains = list(target_domains)
num = 0
for i in target_domains:
    target_domains[num] = i.strip()
    num = num + 1
print("Target Domains:", len(target_domains))
target_domains_sql = ",".join(["'%s'" % tld for tld in target_domains])



#Set the list of first post time so we can pull by month across 2018:
first_post_time_list = [1514764800,
                        1517461200,
                        1519880400,
                        1522555200,
                        1525147200,
                        1527825600,
                        1530417600,
                        1533096000,
                        1535774400,
                        1538366400,
                        1541044800,
                        1543622400,
                        1546300800]

#Set list of file names
file_name_list = ["Second_NewsG_VIEWS_AGE_PPA_Jan_2018.tsv",
                        "Second_NewsG_VIEWS_AGE_PPA_Feb_2018.tsv",
                        "Second_NewsG_VIEWS_AGE_PPA_Mar_2018.tsv",
                        "Second_NewsG_VIEWS_AGE_PPA_Apr_2018.tsv",
                        "Second_NewsG_VIEWS_AGE_PPA_May_2018.tsv",
                        "Second_NewsG_VIEWS_AGE_PPA_Jun_2018.tsv",
                        "Second_NewsG_VIEWS_AGE_PPA_Jul_2018.tsv",
                        "Second_NewsG_VIEWS_AGE_PPA_Aug_2018.tsv",
                        "Second_NewsG_VIEWS_AGE_PPA_Sep_2018.tsv",
                        "Second_NewsG_VIEWS_AGE_PPA_Oct_2018.tsv",
                        "Second_NewsG_VIEWS_AGE_PPA_Nov_2018.tsv",
                        "Second_NewsG_VIEWS_AGE_PPA_Dec_2018.tsv"]
                        

#Run loop that produces for every dataset we would like to pull for each month in 2018
for i in range(0,13): 
    x = i + 1
    sql = f"""
WITH aggregated_urlbds AS
    (SELECT
        urlbd.url_rid AS url_rid,
        urlbd.age_bracket AS age_bracket,
        urlbd.political_page_affinity AS political_page_affinity,
        SUM(urlbd.shares) AS total_shares,
        SUM(urlbd.views) AS total_views,
        SUM(urlbd.clicks) AS total_clicks
    FROM {database}.{bd_table} urlbd
    WHERE urlbd.c='US' AND urlbd.year_month='2018-01'
    OR urlbd.c='US' AND urlbd.year_month='2018-02'
    OR urlbd.c='US' AND urlbd.year_month='2018-03'
    OR urlbd.c='US' AND urlbd.year_month='2018-04'
    OR urlbd.c='US' AND urlbd.year_month='2018-05'
    OR urlbd.c='US' AND urlbd.year_month='2018-06'
    OR urlbd.c='US' AND urlbd.year_month='2018-07'
    OR urlbd.c='US' AND urlbd.year_month='2018-08'
    OR urlbd.c='US' AND urlbd.year_month='2018-09'
    OR urlbd.c='US' AND urlbd.year_month='2018-10'
    OR urlbd.c='US' AND urlbd.year_month='2018-11'
    OR urlbd.c='US' AND urlbd.year_month='2018-12'
    OR urlbd.c='US' AND urlbd.year_month='2019-01'
    OR urlbd.c='US' AND urlbd.year_month='2019-02'
    OR urlbd.c='US' AND urlbd.year_month='2019-03'
    OR urlbd.c='US' AND urlbd.year_month='2019-04'
    OR urlbd.c='US' AND urlbd.year_month='2019-05'
    OR urlbd.c='US' AND urlbd.year_month='2019-06'
    OR urlbd.c='US' AND urlbd.year_month='2019-07'
    GROUP BY urlbd.url_rid,urlbd.age_bracket,urlbd.political_page_affinity)
SELECT 
    url_rid,
    urlattr.parent_domain,
    urlattr.public_shares_top_country,
    aggregated_urlbds.total_shares,
    aggregated_urlbds.total_views,
    aggregated_urlbds.age_bracket,
    aggregated_urlbds.political_page_affinity
FROM 
    aggregated_urlbds JOIN
    {database}.{url_table} urlattr
        USING (url_rid)
WHERE
    LOWER(urlattr.parent_domain) IN ({target_domains_sql}) AND
    urlattr.first_post_time_unix>={first_post_time_list[i]} AND
    urlattr.first_post_time_unix<{first_post_time_list[x]}
    """
    err = None
    try:
        result = execute(sql, output_file=file_name_list[i])
        print(result)
    except Exception as e:
        print(e)
        err = e


#If the data pull for August and September times out then we split the data:


Create a list of news domains with which we have NewsGuard Ratings. We will only pull data from news domains that have a NewsGuard rating
media_scores_df = pd.read_csv("NewsGuard_Ratings.csv")
target_domains = {tld.lower() for tld in media_scores_df["Domain"]}
print(len(target_domains))
target_domains = list(target_domains)
num = 0
for i in target_domains:
    target_domains[num] = i.strip()
    num = num + 1
print("Target Domains:", len(target_domains))
target_domains_sql = ",".join(["'%s'" % tld for tld in target_domains])



#Set the list of first post time so we can pull by month across 2018:
first_post_time_list = [1533096000,
                        1534294800,
                        1535774400,
                        1536973200,
                        1538366400]

#Set list of file names
file_name_list = ["Second_NewsG_VIEWS_AGE_PPA_Aug_2018_1.tsv",
                  "Second_NewsG_VIEWS_AGE_PPA_Aug_2018_2.tsv",
                  "Second_NewsG_VIEWS_AGE_PPA_Sep_2018_1.tsv",
                  "Second_NewsG_VIEWS_AGE_PPA_Sep_2018_2.tsv"]
                        

#Run loop that produces for every dataset we would like to pull for each month in 2018
for i in range(0,5): 
    x = i + 1
    sql = f"""
WITH aggregated_urlbds AS
    (SELECT
        urlbd.url_rid AS url_rid,
        urlbd.age_bracket AS age_bracket,
        urlbd.political_page_affinity AS political_page_affinity,
        SUM(urlbd.shares) AS total_shares,
        SUM(urlbd.views) AS total_views,
        SUM(urlbd.clicks) AS total_clicks
    FROM {database}.{bd_table} urlbd
    WHERE urlbd.c='US' AND urlbd.year_month='2018-01'
    OR urlbd.c='US' AND urlbd.year_month='2018-02'
    OR urlbd.c='US' AND urlbd.year_month='2018-03'
    OR urlbd.c='US' AND urlbd.year_month='2018-04'
    OR urlbd.c='US' AND urlbd.year_month='2018-05'
    OR urlbd.c='US' AND urlbd.year_month='2018-06'
    OR urlbd.c='US' AND urlbd.year_month='2018-07'
    OR urlbd.c='US' AND urlbd.year_month='2018-08'
    OR urlbd.c='US' AND urlbd.year_month='2018-09'
    OR urlbd.c='US' AND urlbd.year_month='2018-10'
    OR urlbd.c='US' AND urlbd.year_month='2018-11'
    OR urlbd.c='US' AND urlbd.year_month='2018-12'
    OR urlbd.c='US' AND urlbd.year_month='2019-01'
    OR urlbd.c='US' AND urlbd.year_month='2019-02'
    OR urlbd.c='US' AND urlbd.year_month='2019-03'
    OR urlbd.c='US' AND urlbd.year_month='2019-04'
    OR urlbd.c='US' AND urlbd.year_month='2019-05'
    OR urlbd.c='US' AND urlbd.year_month='2019-06'
    OR urlbd.c='US' AND urlbd.year_month='2019-07'
    GROUP BY urlbd.url_rid,urlbd.age_bracket,urlbd.political_page_affinity)
SELECT 
    url_rid,
    urlattr.parent_domain,
    urlattr.public_shares_top_country,
    aggregated_urlbds.total_shares,
    aggregated_urlbds.total_views,
    aggregated_urlbds.age_bracket,
    aggregated_urlbds.political_page_affinity
FROM 
    aggregated_urlbds JOIN
    {database}.{url_table} urlattr
        USING (url_rid)
WHERE
    LOWER(urlattr.parent_domain) IN ({target_domains_sql}) AND
    urlattr.first_post_time_unix>={first_post_time_list[i]} AND
    urlattr.first_post_time_unix<{first_post_time_list[x]}
    """
    err = None
    try:
        result = execute(sql, output_file=file_name_list[i])
        print(result)
    except Exception as e:
        print(e)
        err = e