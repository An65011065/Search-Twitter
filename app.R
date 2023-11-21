library(shiny)
library(shinydashboard)
library(lubridate)
library(tidytext)
library(stringr)
library(tidyverse)
library(corpus)
library(rtweet)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(rtweet)
library(twitteR)
library(shinycustomloader)



sentimentsToUse <- read_csv("afinnsentiments.csv")

header <- dashboardHeader(title = "Search Twitter")

sidebar<- dashboardSidebar(
    sidebarMenu(
        menuItem("Info", tabName = "model", icon = icon("user-plus")
              
        ),
        menuItem("Frequently Used Words", tabName = "used_words", icon = icon("clipboard-list"),
                 radioButtons(inputId = "radio1",
                              label = "Duration",
                              choices = c("Words","Hashtags", "@s")
                 ),
                 div(style="display: inline-block;vertical-align:top; width: 100px;",
                     actionButton("go1", "Search"))
        ),
        menuItem("Word Cloud", tabName = "cloud", icon = icon("cloud"),
                 #sliderInput("freq",
                 #           "Minimum Frequency:",
                 #          min = 1,  max = 50, value = 15),
                 sliderInput(inputId = "max",
                             label = "Maximum Number of Words:",
                             min = 1,  max = 300,  value = 100),
                 div(style="display: inline-block;vertical-align:top; width: 100px;",
                     actionButton("go2", "Show"))
        ),
        
        menuItem("Word Usage Over Time", tabName = "word_usage", icon = icon("hourglass-half"),
                 textInput(inputId = "wordInput",
                           label = "Enter a word(s) (no space between commas): "
                 ),
                 textInput(inputId = "year",
                           label = "Year: "
                 ),
                 div(style="display: inline-block;vertical-align:top; width: 100px;",
                     actionButton("go3", "Search"))
                 
        ),
        
        menuItem("Tweet Sentiments", tabName = "tweetSentiments", icon = icon("heartbeat")),
        
        menuItem("Tweet Finder", tabName = "tweetbyname", icon = icon("twitter"), badgeLabel = "new", badgeColor = "green"),
        
        menuItem("About App", tabName = "AboutApp", icon = icon("info-circle"))
        
    
    )
)


body <- dashboardBody(
    tabItems(
        
        tabItem(tabName = "model",
                h1(strong(textOutput("person"))),
                textInput(inputId = "nameInput",
                          label = "Input Twitter ID: "
                ),
                fluidRow(
                    infoBoxOutput("follower"), 
                    infoBoxOutput("tweetvsretweet"),
                    infoBoxOutput("stat"),
                    infoBoxOutput("interactions"),infoBoxOutput("sources"),infoBoxOutput("tweettime") ),
                div(style="display: inline-block;vertical-align:top; width: 100px;",
                    actionButton("go", "Search")),
                h2(textOutput(outputId = "text")),
                h3(withLoader(tableOutput(outputId = "checkingOutput"), type="html",loader="pacman")),
        
        fluidPage(
            
            tags$head(tags$style("#checkingOutput{color: black;
                                 font-size: 15px;
                                 font-family: Source Sans Pro
                                 }")),
            fluidRow(
                splitLayout(cellWidths = c("50%", "50%"),
                            plotOutput(outputId ="graph1"),
                            plotOutput(outputId = "plot")),
                plotOutput(outputId= "graph2")
            )
        )),
        
        tabItem(tabName = "tweetSentiments",
                div(style="display: inline-block;vertical-align:top; width: 100px;",
                    actionButton("showSentiments", "Reveal Sentiments")),
                plotOutput(outputId= "graph3")),
        
        tabItem(tabName = "tweetbyname",
                textInput(inputId = "nameInputforTweets",
                          label = "Input Word: "
                ),
                div(style="display: inline-block;vertical-align:top; width: 100px;",
                    actionButton("gofindtweet", "Search")),
                h2(textOutput(outputId = "tweetfindertext")),
                tableOutput(outputId = "tweetfinder")
        ),
        
        
        
        tabItem(tabName = "AboutApp",
                h1(id="app-heading", "Search Twitter"),
                tags$style(HTML("#app-heading{color: #008ABA;}")),
                tags$style(HTML("#app-heading{text-align:center}")),
                
                h3("Discover the pulse of Twitter with our 'Explore Twitter Trends' app. This tool delves into an individual's Twitter activity, analyzing up to 3,200 of their most recent tweets. It unveils a range of insights, from trending topics to popular hashtags, providing a comprehensive view of their digital footprint."),
                
                h4("Leveraging data directly from Twitter's database via a developer account, our app uses the 'rtweet' package for efficient data extraction. This approach allows for a versatile analysis of multiple Twitter users, offering a richer dataset compared to standard CSV files."),
                
                h4("Our goal is multifaceted: to unearth what Twitter users are discussing the most, to reveal popular words and hashtags, and to potentially categorize these insights into themes. Imagine assigning 'Climate Advocate' to Greta Thunberg or 'Political Leader' to Joe Biden, based on their tweet content."),
                
                h4("On a larger scale, the 'Tweet Finder' feature can serve as a tool for monitoring explicit content or for media analysis. Currently, it supports single-word searches, with plans for multi-word functionality in future updates."),
                
                h4("Please note, the analysis is confined to the latest 3,200 tweets per user. This means our insights are concentrated but may not encompass the entirety of a user's Twitter history, especially for less active accounts."),
                
                h4("To start exploring, simply enter a Twitter username (omit the '@' symbol) and let the app work its magic. The process may take some time, as it renders comprehensive visuals and analytics."),
                
                h4("Upon entering a username, six informative widgets will appear, each offering unique insights about the selected Twitter user. These range from follower counts to tweet versus retweet ratios, most active hours, and the platforms used for tweeting."),
                h4("Additional features include a 'Word Usage Over Time' graph, a sentiment analysis graph, and a 'Tweet Finder' for pinpointing specific words in tweets."),
                
                h2(id="note-heading", "Key Notes:"),
                tags$style(HTML("#note-heading{color: red;}")),
                h4("• To optimize performance, the app requires a reload for new username inputs."),
                h4("• Errors may arise if usernames are misspelled, non-existent, or if they have no tweets. In such cases, refreshing the app is necessary."),
                h4("• Some visualizations, like the 'Word Usage Over Time' graph, may not display data for each month due to the 3,200 tweet limit."),
                
                h3("Designed by:"),
                tag("a", list(href = "https://twitter.com/AnweshanAdhika3", "Anweshan Adhika")),
                tag("a", list(href = "http://www.linkedin.com/in/hedavam-solano/", "Hedavam Solano")),
                tag("a", list(href = "https://www.instagram.com/terryppark/", "Terry Park")))
                
    ),

    
    
)


ui <- dashboardPage(header, 
                    sidebar,
                    body)



server <- function(input, output, session) {
    
    person.data<- reactive(
        {
            
            isolate({
                withProgress({
                    setProgress(message = "Processing tweets...")
                })
            })
            my_authorization <- rtweet::create_token(app = "utd_an-twitter",
                                                     consumer_key = "#####",
                                                     consumer_secret = "######", access_token="#####", access_secret = "####",
                                                     set_renv=FALSE)
            datatweets <- rtweet::get_timeline(c({input$nameInput}), n = 3200, parse=T, token=my_authorization)
            rtweet::write_as_csv(datatweets, "twitterdata.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
            
            Texts <- readtext::readtext("twitterdata.csv", text_field = "text")
            
            return(Texts$name%>%head(1))
            
        }
    )
    
    observeEvent(input$go, {
        output$person <- renderText({
            isolate(paste("User Name: ",person.data()))
        })})
    

    
    forTableOutput <- reactive(
        {
            Texts <- readtext::readtext("twitterdata.csv", text_field = "text")
            
            
            testing<-Texts%>%
                group_by(text) %>%
                summarize(text, favorite_count) %>%
                arrange(desc(favorite_count)) %>%
                mutate(likes= favorite_count)%>%
                mutate(tweets=text)%>%
                head(5)
            
            testing$n<- NULL
            testing$favorite_count<-NULL
            testing$text<- NULL
            
            
            return(testing)
        }
    )
    
    observeEvent(input$go, {
        output$checkingOutput <- renderTable({
            isolate(forTableOutput())
        })})
    
    observeEvent(input$go, {
        output$text <- renderText({
            isolate("Top 5 Most Liked Tweets: ")
        })})
    
    GraphOutput <- reactive(
        {
            Texts <- readtext::readtext("twitterdata.csv", text_field = "text")
            
            pop.words <- Texts %>%
                mutate(Time = ymd_hms(created_at))
            
            remove_reg <- "&amp;|&lt;|&gt;"
            
            word_tweets <- pop.words %>%
                mutate(Tweet = str_remove_all(text, remove_reg)) %>%
                unnest_tokens(output=word, input = Tweet, token = "tweets") %>%
                filter(!word %in% stop_words$word)

            
            frequency.table<-word_tweets %>% 
                count(word, sort = TRUE)%>%
                mutate(cond1=substr(word,1,1)=="#")%>%
                mutate(cond2=substr(word,1,1)=="@")%>%
                filter(cond1=="FALSE" & cond2=="FALSE" )
            
            #frequency.table$word<-gsub("@\\w+ *", "", frequency.table$word)
                
            
            
            data.pop.words<-frequency.table%>%
                arrange(desc(n))%>%
                head(25)
            
            
            
            return(
                data.pop.words%>%
                    ggplot()+
                    geom_col(aes(y=reorder(word,-n), x= n, fill="#f5bc80"))+
                    labs(title=paste("Top 25 Words in ", input$nameInput, "tweets"),
                         y="Word", x="Count") +
                    theme(legend.position = "none",
                          aspect.ratio=0.8) 
            )
        }
    )
    
    
            
    GraphHashtags<- reactive(
        {
            Texts <- readtext::readtext("twitterdata.csv", text_field = "text")
            
            
            pop.words <- Texts %>%
                mutate(Time = ymd_hms(created_at))
            
            remove_reg <- "&amp;|&lt;|&gt;"
            
            word_tweets <- pop.words %>%
                mutate(Tweet = str_remove_all(text, remove_reg)) %>%
                mutate(month=month(created_at))%>%
                unnest_tokens(output=word, input = Tweet, token = "tweets") %>%
                filter(!word %in% stop_words$word)
            
            frequency.table<-word_tweets %>% 
                filter(substr(word,1,1)=="#")%>%
                count(word, sort = TRUE)
            
            
            data.pop.words<-frequency.table%>%
                head(25)

            
            
            
            return(
                data.pop.words%>%
                    ggplot()+
                    geom_col(aes(y=reorder(word,-n), x= n, fill="#f5bc80"))+
                    labs(title=paste("Most Used Hashtags in", input$nameInput, "Tweets"),
                         y="hastags", x="Count") +
                    theme(legend.position = "none")
            )
        }
    )
    
    GraphRates<- reactive(
        {
            Texts <- readtext::readtext("twitterdata.csv", text_field = "text")
            
            
            pop.words <- Texts %>%
                mutate(Time = ymd_hms(created_at))
            
            remove_reg <- "&amp;|&lt;|&gt;"
            
            word_tweets <- pop.words %>%
                mutate(Tweet = str_remove_all(text, remove_reg)) %>%
                mutate(month=month(created_at))%>%
                unnest_tokens(output=word, input = Tweet, token = "tweets") %>%
                filter(!word %in% stop_words$word)
            
            frequency.table<-word_tweets %>% 
                filter(substr(word,1,1)=="@")%>%
                count(word, sort = TRUE)
            
            
            data.pop.words<-frequency.table%>%
                head(25)
            data.pop.words
            
            
        
            return(
                data.pop.words%>%
                    ggplot()+
                    geom_col(aes(y=reorder(word,-n), x= n, fill="#f5bc80"))+
                    labs(title=paste("Most Used @s in", input$nameInput, "Tweets"),
                         y="@s", x="Count") +
                    theme(legend.position = "none")
            )
        }
    )
    
    observeEvent(input$go1, {output$graph1 <- renderPlot({
        if (input$radio1=="Words"){
            isolate(GraphOutput())
        }
        else if(input$radio1=="Hashtags"){ isolate(GraphHashtags())}
        else if(input$radio1=="@s"){ isolate(GraphRates())}
        
    })   
    })
    
    wordcloudd<- reactive(
        {
            
            Texts <- readtext::readtext("twitterdata.csv", text_field = "text")
            
            pop.words <- Texts %>%                                                  
                mutate(Time = ymd_hms(created_at))
            
            remove_reg <- "&amp;|&lt;|&gt;"
            
            word_tweets <- pop.words %>%
                mutate(Tweet = str_remove_all(text, remove_reg)) %>%
                mutate(month=month(created_at))%>%
                filter(month==12)%>%
                unnest_tokens(output=word, input = Tweet, token = "tweets") %>%
                filter(!word %in% stop_words$word)
            
            frequency.table<-word_tweets %>% 
                count(word, sort = TRUE)
            
            
            
            return(wordcloud(words = frequency.table$word, freq = frequency.table$n, min.freq = 1,
                             max.words=input$max, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2")))
            
        })
    
    observeEvent(input$go2, {
        output$plot <- renderPlot({
            isolate(wordcloudd())
        })})
    
    statistics <- reactive(
        {
            Texts <- readtext::readtext("twitterdata.csv", text_field = "text")
            
            Text <-  Texts %>%                                                  
                mutate(Time = ymd_hms(created_at))%>%
                mutate(month= month(created_at))%>%
                filter(month==12)%>%
                filter(is_retweet=="FALSE")
            
            
            # rtweet::write_as_csv(tweets, "tweets.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
            # 
            # Texts <- readtext::readtext("tweets.csv", text_field = "text")
            
            #nameInput <- (input$nameInput)
            
            
            
            return(nrow(Text))
        }
    )
    observeEvent(input$go, {
        output$stat <- renderInfoBox({
            isolate(infoBox("Tweets in December",statistics(),icon = icon("twitter"),
                    color = "light-blue", fill = TRUE))})
    })
    
    
    interaction <- reactive(
        {
            Texts <- readtext::readtext("twitterdata.csv", text_field = "text") 
            
            retweet= Texts %>%
                group_by(mentions_screen_name)%>%
                count(mentions_screen_name) %>%
                arrange(desc(n)) %>%
                mutate(case=mentions_screen_name=="")%>%
                filter(case=="FALSE")%>%
                head(1)
            
            retweet$n<- NULL
            retweet$case<-NULL
            
            
            
            return(retweet)
        }
    )
    observeEvent(input$go, {
        output$interactions <- renderInfoBox({
            isolate(infoBox("Most Interactions",interaction(),icon = icon("handshake"),
                    color = "red", fill = TRUE))})
    })
    
    
    source <- reactive(
        {
            Texts <- readtext::readtext("twitterdata.csv", text_field = "text")
            
            popular_source= Texts %>%
                group_by(source) %>%
                count(text) %>%
                count(sum(n)) %>%
                summarize(source,n)%>%
                arrange(desc(n))%>%
                head(1)
            
            popular_source$n<-NULL
            
            
            
            return(popular_source)
        }
    )
    observeEvent(input$go, {
        output$sources <- renderInfoBox({
            isolate(infoBox("Most Twitter Usage From", source(),icon = icon("laptop"),
                    color = "green", fill = TRUE))})
    })
    
    
    Followers <- reactive(
        {
            Texts <- readtext::readtext("twitterdata.csv", text_field = "text")
            
            Texts.Followcount= Texts %>%
                summarize(followers_count) %>%
                head(1)
            
            
            
            
            
            return(Texts.Followcount)
        }
    )
    observeEvent(input$go, {
        output$follower <- renderInfoBox({
            isolate(infoBox("Number of Followers ", Followers(),icon = icon("users"),
                    color = "navy", fill = TRUE))})
    })
    
    Tweets <- reactive(
        {
            Texts <- readtext::readtext("twitterdata.csv", text_field = "text")
            
            #How many Tweets Out of 3000 something
            Texts.Tweets= Texts %>%
                filter(is_retweet==FALSE) %>%
                count(text) %>%
                count(sum(n)) %>%
                summarize(n)
            
            #How many Retweets
            Texts.Retweets= Texts %>%
                filter(is_retweet==TRUE) %>%
                count(text) %>%
                count(sum(n)) %>%
                summarize(n)
            
            Tweet<- paste(Texts.Tweets, "VS", Texts.Retweets)
            
            
            
            return(Tweet)
        }
    )
    observeEvent(input$go, {
        output$tweetvsretweet <- renderInfoBox({
            isolate(infoBox("Tweets VS Retweets ", Tweets(),icon = icon("retweet"),
                    color = "purple", fill = TRUE))})
    })
    
    
    Time <- reactive(
        {
            Texts <- readtext::readtext("twitterdata.csv", text_field = "text")
            
            Texts.Time<- Texts%>%
                mutate(Time = ymd_hms(created_at))%>%
                mutate(hour= hour(created_at))%>%
                group_by(hour)%>%
                count(hour) %>%
                arrange(desc(n))%>%
                head(1)
            
            Texts.Time$n<-NULL
            
            
            
            
            return(Texts.Time)
        }
    )
    observeEvent(input$go, {
        output$tweettime <- renderInfoBox({
            isolate(infoBox("Preferred Tweeting Time ", paste(Time(),":00", "-", Time()+1,":00"),icon = icon("clock"),
                    color = "yellow", fill = TRUE))})
    })
    
    
    ##############3TERRY 
    
    GraphOutput3= reactive(
        {  Texts <- readtext::readtext("twitterdata.csv", text_field = "text")
        
        Text.sentiments<- Texts %>%
            mutate(year=year(created_at)) %>%
            group_by(month = month(created_at)) %>% 
            unnest_tokens(output=word, input =  text, token = "tweets")%>%
            count(word) %>%
            arrange(-n)%>%
            anti_join(stop_words, by = "word") %>%
            left_join(sentimentsToUse) %>%
            na.omit()
        
        
        collective.sentiment<-Text.sentiments %>%
            group_by(month)%>%
            summarize(total=mean(value*n)) %>%
            mutate(month=factor(month.abb[month],levels=month.abb))
        collective.sentiment
        
        collective.sentiment%>%
            ggplot()+
            geom_col(aes(y=reorder(month,total), x=total), fill="#f5bc80") +
            labs(title="Sentiments Over Time",
                 y="Month", x="Sentiment Value") +
            theme(plot.title = element_text(hjust = 0.5))
        
        }
    )
    
    observeEvent(input$showSentiments, {
        output$graph3 <- renderPlot({
            isolate(GraphOutput3()) })
        
    })
    
    OverTime <- reactive(
        { Texts <- readtext::readtext("twitterdata.csv", text_field = "text")
        
        word.over.time<-Texts%>%
            mutate(year=year(created_at))%>%
            mutate(month=month(created_at))%>%
            group_by(month)%>% 
            filter(year==input$year)%>%
            mutate(tweets.month=length(text)) %>%
            group_by(month,tweets.month)%>%
            unnest_tokens(output=word, input =  text, token = "tweets")%>%
            filter(!word %in% stop_words$word)%>%
            count(tweets.month,word, sort = TRUE)%>%
            mutate(usage.per.tweet=(n/tweets.month)*100)
        a <- input$wordInput
        b <- scan(text = a, sep = ",", what = "")
        return(word.over.time %>%
                   filter(word %in% b) %>%
                   ggplot(aes(x=month, y= usage.per.tweet, color=word))+
                   geom_point()+
                   geom_line(size=1)+
                   labs(title="Words Usage Over Time",
                        y="Usage per 100 tweets", x="Month") +
                   theme(plot.title = element_text(hjust = 0.5))
               
        )
        
        
        }
    )
    
    observeEvent(input$go3,{
        output$graph2 <- renderPlot({
            isolate(OverTime()) })
    })
    
    wordForTweets <- reactive(
        {
            Texts <- readtext::readtext("twitterdata.csv", text_field = "text")
            
            # rtweet::write_as_csv(tweets, "tweets.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
            # 
            # Texts <- readtext::readtext("tweets.csv", text_field = "text")
            
            #nameInput <- (input$nameInput)
            
            t.tweets<-Texts %>%
                mutate(Time = ymd_hms(created_at))
            
            remove_reg <- "&amp;|&lt;|&gt;"
            
            word_tweets <- t.tweets %>%
                mutate(Tweet = str_remove_all(text, remove_reg)) %>%
                unnest_tokens(output=word, input = Tweet, token = "tweets") %>%
                filter(!word %in% stop_words$word)
            
            
            scanner2 <- input$nameInputforTweets
            scanned2 <- scan(text = scanner2, sep = ",", what = "")
            
            
            #Terry's scanner
            
            
            TxttoDisplay <-  word_tweets %>%
                filter(word %in% scanned2) %>%
                summarize(text)
            
            
            
            return(TxttoDisplay)
        }
    )
    
    observeEvent(input$gofindtweet, {
        output$tweetfindertext <- renderText({
            isolate((paste("Tweets by ", (input$nameInput), "containing the word(s): ", (input$wordInput))))
        })})
    
    observeEvent(input$gofindtweet, {
        output$tweetfinder <- renderTable({
            wordForTweets() })
    })
    
    
}



shinyApp(ui, server)
