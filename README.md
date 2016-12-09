# Twitter Sentiment Analysis (TSA)

My first R project - it's a mishmash of various guides found around the web. Buyer beware: This is crappy code!

## Install Instructions

Copy the example config file (config.R.sample) to config.R and update with your twitter API details.
    
    cp config.R.sample config.R
    vi config.R

Run the following in your R environment to install the dependancies.

    install.packages("devtools")
    require(devtools)
    
    # Because Sentiment is no longer maintained/available.
    install_url("ftp://cran.r-project.org/pub/R/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
    install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.1.tar.gz")
    install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
    
    install.packages("twitteR")
    install.packages("plyr")
    install.packages("ggplot2")
    install.packages("wordcloud")
    install.packages("RColorBrewer")
    install.packages("DT")

## Credits

A lot of this code is based on:

https://sites.google.com/site/miningtwitter/questions/sentiment/sentiment
https://sites.google.com/site/miningtwitter/questions/talking-about/wordclouds/comparison-cloud