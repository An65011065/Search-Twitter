---
jupyter:
  language_info:
    name: python
  nbformat: 4
  nbformat_minor: 2
---

::: {.cell .markdown}
`<span style="color: red;">`{=html}Project Discontinuation: Twitter API
Changes`</span>`{=html}

> **Important Update**: Due to significant policy changes on pricing
> following Twitter\'s acquisition, we no longer have access to the new
> Twitter API. As a result, **we have discontinued the \"Search
> Twitter\" app project**. We apologize for any inconvenience and
> appreciate your understanding.

## Search Twitter

Welcome to the \"Search Twitter\" app repository -- a dynamic tool
designed for in-depth analysis and visualization of Twitter data. This
R-based application harnesses the power of Twitter\'s API and to provide
users with rich insights into Twitter trends and user behaviors.

Key Features: • User Tweet Analysis: Analyzes up to 3,200 recent tweets
from a specified Twitter user, offering insights into their most popular
tweets, frequent words, and hashtags used. • Data Extraction: Utilizes
the \'rtweet\' package for efficient extraction of data directly from
Twitter\'s database, ensuring a comprehensive and up-to-date analysis. •
Multi-Faceted Insights: From categorizing tweets into themes (like
\"Climate Advocate\" for environmental activists) to identifying the
most discussed topics, the app dives deep into the content of Twitter
conversations. • Tweet Finder: A specialized feature for identifying
tweets containing specific words. This function is particularly useful
for content monitoring and media analysis. • Sentiment Analysis:
Evaluates the sentiment of tweets, providing a monthly breakdown of
positive or negative connotations in the user\'s Twitter activity.

User Interface: The app presents an intuitive interface with various
widgets displaying different aspects of Twitter data: • Follower Counts
and Tweet Analysis: Displays basic stats like number of followers, and
categorizes tweets vs retweets. • Activity Insights: Showcases metrics
such as the most active hours of the user and the platforms
predominantly used for tweeting. • Interactive Graphs: Includes a \'Word
Usage Over Time\' graph and a sentiment analysis graph for deeper
content exploration. • Wordcloud Visualization: Offers a visual
representation of the most frequently used words in the user\'s tweets.

Usage: To use the app, enter a Twitter username (without the \'@\'
symbol). The app processes this input to render detailed visuals and
analytics, which may take a few moments.

Limitations: Please note that the analysis is limited to the latest
3,200 tweets per user, which may affect the comprehensiveness of
insights for less active Twitter accounts.

Important Notes: Update 03/2023:

    •	Changes in Twitter API Policy: Following the acquisition of Twitter,free users are now limited to accessing only 1,000 tweets. This change impacts the depth of analysis our app can offer. While the app initially analyzed up to 3,200 recent tweets from a user, this limit has been reduced to 1,000 tweets in accordance with the new policy. Users may notice that some sections of the app are less comprehensive than before due to this limitation.

    •	Discontinuation of the Corpus Package: The 'corpus' package, previously a key component of our app, has been discontinued. Consequently, it now requires installation from a local library. Users looking to utilize this app will need to manually install the 'corpus' package from a local source. Instructions for this process are provided in the installation guide.

    •	Privacy and Access Keys: In the interest of privacy and security, the source code provided in this repository does not include the access keys required for the Twitter API. Users will need to obtain their own API keys by registering for a Twitter developer account. This process is straightforward and can be completed on the Twitter developer platform.

Update 05/2022: • The app requires reloading for new username inputs to
optimize performance.

    •	Users may encounter errors for invalid or non-existent usernames, or if the specified account has no tweets.

    •	Some visualizations might not cover each month due to the limitation of 3,200 tweets.
:::
