shinyUI(fluidPage( 
  titlePanel("Sentiment Analysis of Topic Based Tweets"), #Title
  fluidRow(
    column(4, wellPanel(
      textInput("text", " What topic would you like to analyze?", "Modi"),
      sliderInput("n", " How many tweets do you want analyzed?", min= 100, max= 10000, value = 1000),
      dateRangeInput('dateRange',
                     label = 'Select Date Range for which tweets are to be Analyzed: ',
                     start = Sys.Date() - 5, end = Sys.Date() 
      ),
      submitButton("Fetch and Analyze!")
    ),
    strong("So, how does this work?"),
    p(" When the user enters topic, no of tweets to be analyzed, date range for which tweets are to be analyzed, application authenicates with twitter
      and fetches those tweets related to topic. Then analytical engine process those tweets and analyzes their sentiments. With the scoring algorithm, each tweet is given a 
      score based on emotionally positive or negative entities encountered. Histogram is created based on no tweets and sentiment score. Similarly sentiment plot and polarity plots
        is created with the help of bayes algorithm. The wordcloud is plotted to show most frequently used words in the tweets. If we see 'http______' in the word cloud, it means lot
       of people are sharing links about that topic through their tweets. This analytical engine could aid brands/organizations in finding out emotions of people towards their product or service.
      This can not only help the customer decide the trending business, but also the business itself can get a well-defined metric of its own 
      performance.")

    ), 
     column(8,
            mainPanel(
              tabsetPanel(type = "tabs",
                          tabPanel("Histogram", plotOutput("plot1"),verbatimTextOutput("text")),
                        
                          tabPanel("Sentiment Analysis", plotOutput("plot2")),
                          tabPanel("Polarity Analysis", plotOutput("plot3")),
                          tabPanel("Word-Cloud", plotOutput("plot4"))
                          
                           )
            )      
           )
    
  )
)

)







