library(shiny)

library(twitteR)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(DT)

source("config.R")

setup_twitter_oauth(twitter_api_key,
                    twitter_api_secret,
                    twitter_access_token,
                    twitter_access_secret)

shinyServer(function(input, output) {
  
  sent_df <- reactive({
    tweets <- searchTwitter(input$hashtag, n=50)
    tweets <- strip_retweets(tweets, strip_manual=TRUE, strip_mt=TRUE)
    
    some_txt = sapply(tweets, function(x) x$getText())
    
    # remove at people
    some_txt = gsub("@\\w+", "", some_txt)
    # remove punctuation
    some_txt = gsub("[[:punct:]]", "", some_txt)
    # remove numbers
    some_txt = gsub("[[:digit:]]", "", some_txt)
    # remove html links
    some_txt = gsub("http\\w+", "", some_txt)
    
    # Strangely enough, it seems that tolower can't handle unicode.
    some_txt = sapply(some_txt, function(txt) {
      tryCatch(tolower(txt), error = function(e) { return("") })
    })
    
    # classify emotion
    class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
    # get emotion best fit
    emotion = class_emo[,7]
    # substitute NA's by "unknown"
    emotion[is.na(emotion)] = "unknown"
    
    # classify polarity
    class_pol = classify_polarity(some_txt, algorithm="bayes")
    # get polarity best fit
    polarity = class_pol[,4]
    
    # data frame with results
    sent_df = data.frame(tweet = sapply(tweets, function(x) x$getText()), 
                         text=some_txt, 
                         emotion=emotion,
                         polarity=polarity, stringsAsFactors=FALSE)
    
    # sort data frame
    sent_df = within(sent_df,
                     emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
  })
  
  output$emoPlot <- renderPlot({
    # plot distribution of emotions
    ggplot(sent_df(), aes(x=emotion)) +
      geom_bar(aes(y=..count.., fill=emotion)) +
      scale_fill_brewer(palette="Dark2") +
      labs(x="emotion categories", y="number of tweets") +
      ggtitle("Sentiment Analysis of Tweets\n(classification by emotion)")
  })
  
  output$polarityPlot <- renderPlot({
    ggplot(sent_df(), aes(x=polarity)) +
      geom_bar(aes(y=..count.., fill=polarity)) +
      scale_fill_brewer(palette="RdGy") +
      labs(x="polarity categories", y="number of tweets") +
      ggtitle("Sentiment Analysis of Tweets\n(classification by polarity)")
  })
  
  output$cloudPlot <- renderPlot({
    # separating text by emotion
    emos = levels(factor(sent_df()$emotion))
    nemo = length(emos)
    emo.docs = rep("", nemo)
    for (i in 1:nemo)
    {
      tmp = sent_df()$text[
        sent_df()$emotion == emos[i]
        ]
      emo.docs[i] = paste(tmp, collapse=" ")
    }
    
    # remove stopwords
    emo.docs = removeWords(emo.docs, stopwords("english"))
    # create corpus
    corpus = Corpus(VectorSource(emo.docs))
    tdm = TermDocumentMatrix(corpus)
    tdm = as.matrix(tdm)
    colnames(tdm) = emos
    
    # comparison word cloud
    comparison.cloud(tdm,
                     max.words = 50,
                     colors = brewer.pal(nemo, "Dark2"),
                     #scale = c(3,.5), 
                     random.order = FALSE,
                     #title.size = 1.5,
                     )
  })
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(
    data.frame(tweet=sent_df()$tweet, emotion=sent_df()$emotion, polarity=sent_df()$polarity)
    )

})
