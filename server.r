shinyServer(function(input, output) {

  library("shiny")
  library("tm")
  library("wordcloud")
  library("twitteR")
  library("plyr")
  library("stringr")
  library("caret")
  library("shinyIncubator")
  library("RColorBrewer")
  library("sentiment")
 

  api_key <- "te4eJxLeHFaM1aVJ0gUrAkUJ4"
  api_secret <- "enRZchObOyyfRphlShAQS9iQUeREFxYpSelCbAc9vjYor7DT2W"
  access_token <- "4043784380-jWYLgRlBIlk9IWtV4aQhn4DrvGmacXsDL7u37ko"
  access_token_secret <- "S9Gkv1CgPqQdLS3mGC4SjCBuPhZgO76K7qatyrzmRzbzh"
  
  setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
  
  observe({
    tweets= searchTwitter(input$text, n = input$n, lang= "en",  since= format(input$dateRange[1]),  until=format(input$dateRange[2]))
    tweetstext <- sapply(tweets, function(x) x$getText())
    
    catch.error = function(x)
    {
      # let us create a missing value for test purpose
      y = NA
      # Try to catch that error (NA) we just created
      catch_error = tryCatch(tolower(x), error=function(e) e)
      # if not an error
      if (!inherits(catch_error, "error"))
        y = tolower(x)
      # check result if error exists, otherwise the function works fine.
      return(y)
    }
    
    
    cleanTweets<- function(tweet){
      # Clean the tweet for sentiment analysis
      #  remove html links, which are not required for sentiment analysis
      tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
      # First we will remove retweet entities from the stored tweets (text)
      tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
      # Then remove all "#Hashtag"
      tweet = gsub("#\\w+", " ", tweet)
      # Then remove all "@people"
      tweet = gsub("@\\w+", " ", tweet)
      # Then remove all the punctuation
      tweet = gsub("[[:punct:]]", " ", tweet)
      # Then remove numbers, we need only text for analytics
      tweet = gsub("[[:digit:]]", " ", tweet)
      # finally, we remove unnecessary spaces (white spaces, tabs etc)
      tweet = gsub("[ \t]{2,}", " ", tweet)
      tweet = gsub("^\\s+|\\s+$", "", tweet)
      # if anything else, you feel, should be removed, you can. For example "slang words" etc using the above function and methods.
      # Next we'll convert all the word in lower case. This makes uniform pattern.
      tweet = catch.error(tweet)
      tweet
    }
    
    
    cleanTweetsAndRemoveNAs<- function(Tweets) {
      TweetsCleaned = sapply(Tweets, cleanTweets)
      # Remove the "NA" tweets from this tweet list
      TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
      names(TweetsCleaned) = NULL
      # Remove the repetitive tweets from this tweet list
      TweetsCleaned = unique(TweetsCleaned)
      TweetsCleaned
    }
    
    TweetsCleaned = cleanTweetsAndRemoveNAs( tweetstext)
     
     pos = scan('positive-words.txt', what = 'character', comment.char = ';')
     neg = scan('negative-words.txt', what = 'character', comment.char = ';')
    
       pos.words = pos
       neg.words = neg
    
    
    #Now, we create a function, score.sentiment(), which computes the raw sentiment based on the simple matching algorithm:
    getSentimentScore = function(sentences, words.positive, words.negative, .progress='none')
    {
      require(plyr)
      require(stringr)
      
      scores = laply(sentences, function(sentence, words.positive, words.negative) {
        
        # Let first remove the Digit, Punctuation character and Control characters:
        sentence = gsub('[[:cntrl:]]', '', gsub('[[:punct:]]', '', gsub('\\d+', '', sentence)))
        
        # Then lets convert all to lower sentence case:
        sentence = tolower(sentence)
        
        # Now lets split each sentence by the space delimiter
        words = unlist(str_split(sentence, '\\s+'))
        
        # Get the boolean match of each words with the positive & negative opinion-lexicon
        pos.matches = !is.na(match(words, words.positive))
        neg.matches = !is.na(match(words, words.negative))
        
        # Now get the score as total positive sentiment minus the total negatives
        score = sum(pos.matches) - sum(neg.matches)
        
        return(score)
      }, words.positive, words.negative, .progress=.progress )
      
      # Return a data frame with respective sentence and the score
      return(data.frame(text=sentences, score=scores))
    }
   
    
    
    TweetsClassEmo = classify_emotion(TweetsCleaned, algorithm="bayes", prior=1.0)
 
    #Let's substitute NA values with the word unknown to make further analysis easier:
    
    Emotion = TweetsClassEmo[,7]
    
    Emotion[is.na(Emotion)] = "unknown"
    
# cLASSIFY POLARITY TO 0,1,-1
    TweetsClassPol = classify_polarity(TweetsCleaned, algorithm="bayes")
   
    
    # we will fetch polarity category best_fit for our analysis purposes,
    Pol = TweetsClassPol[,4]
    # Let us now create a data frame with the above results
    SentimentDataFrame = data.frame(text=TweetsCleaned, emotion=Emotion, polarity=Pol, stringsAsFactors=FALSE)
    # rearrange data inside the frame by sorting it
    SentimentDataFrame = within(SentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
    
    
    #Function to plot sentiments in tweets
    
    plotSentiments1<- function (sentiment_dataframe,title) {
      library(ggplot2)
      ggplot(sentiment_dataframe, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) +
        scale_fill_brewer(palette="Dark2") +
        ggtitle(title) +
        theme(legend.position='right') + ylab('Number of Tweets') + xlab('Emotion Categories')
    }
    
     ## Similarly we will plot distribution of polarity in the tweets
    plotSentiments2 <- function (sentiment_dataframe,title) {
      library(ggplot2)
      ggplot(sentiment_dataframe, aes(x=polarity)) +
        geom_bar(aes(y=..count.., fill=polarity)) +
        scale_fill_brewer(palette="RdGy") +
        ggtitle(title) +
        theme(legend.position='right') + ylab('Number of Tweets') + xlab('Polarity Categories')
    }
    
    #Now lets make word cloud of tweets
    removeCustomeWords <- function (TweetsCleaned) {
      for(i in 1:length(TweetsCleaned)){
        TweetsCleaned[i] <- tryCatch({
          TweetsCleaned[i] =  removeWords(TweetsCleaned[i], c(stopwords("english"), "care", "guys", "can", "dis", "didn", "guy" ,"booked", "plz"))
          TweetsCleaned[i]
        }, error=function(cond) {
          TweetsCleaned[i]
        }, warning=function(cond) {
          TweetsCleaned[i]
        })
      }
      return(TweetsCleaned)
    }
    
    getWordCloud <- function (sentiment_dataframe, TweetsCleaned, Emotion) {
      emos = levels(factor(sentiment_dataframe$emotion))
      n_emos = length(emos)
      emo.docs = rep("", n_emos)
      TweetsCleaned = removeCustomeWords(TweetsCleaned)
      
      for (i in 1:n_emos){
        emo.docs[i] = paste(TweetsCleaned[Emotion == emos[i]], collapse=" ")
      }
      corpus = Corpus(VectorSource(emo.docs))
      tdm = TermDocumentMatrix(corpus)
      tdm = as.matrix(tdm)
      colnames(tdm) = emos
      require(wordcloud)
      suppressWarnings(comparison.cloud(tdm, colors = brewer.pal(n_emos, "Dark2"),  scale = c(3,.5), random.order = FALSE, title.size = 1.5))
    }
    
    ########## Plotting actual plots 
    
    output$plot1 <- renderPlot({
    
   
    Result = getSentimentScore(TweetsCleaned, pos.words , neg.words)
    
    
    hist(Result$score, main =paste("Sentiments of tweets Vs Frequency of sentiments" ), col = "green")
    output$text <- renderText({
      paste("Mean Sentiment score", mean(Result$score))
    })
    
   })
   
   output$plot2 <- renderPlot({
    
     plotSentiments1(SentimentDataFrame, 'Sentiment Analysis of Tweets on Twitter')
   })
    
   
   output$plot3 <- renderPlot({
     
     plotSentiments2(SentimentDataFrame, 'Polarity Analysis of Tweets on Twitter')
   })
   
   output$plot4 <- renderPlot({
     
     getWordCloud(SentimentDataFrame, TweetsCleaned, Emotion)
     
   })
  
 })
})
   
 
