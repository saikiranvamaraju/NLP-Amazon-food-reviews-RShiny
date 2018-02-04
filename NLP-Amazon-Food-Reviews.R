library(shiny)
library(magrittr)
library(ggplot2)
library(lattice)
library(tm)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)
library(NLP)
library(tidyr)
library(shinythemes)
library(RColorBrewer)
library(wordcloud)
library(dplyr)
library(tidytext)
library(DT)
library(plyr)



# Define UI for shiny

ui <- fluidPage(
  
  #theme shiny
  theme=shinytheme("slate"),
  
  titlePanel("Uploading Files"),
  
  
  sidebarLayout(
    
    
    sidebarPanel(
      
      # Inputing a file
      fileInput("file", "Choose CSV File",options(shiny.maxRequestSize=200*1024^2),
                multiple = TRUE),
      
      tags$hr(),
      
      # Input header option
      checkboxInput("header", "Header", TRUE),
      
      # Input separator options
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
    ),
    
    # Main panel
    mainPanel(
      
      
      tabsetPanel(
        tabPanel("Data",
                 h4("Table"),
                 tableOutput("first")
        ),
        tabPanel("Cleaned Reviews",DT::dataTableOutput("contents"),options = list(scrollX = TRUE)),
        tabPanel(" Term Count / Word Cloud",
                 tabsetPanel(
                   tabPanel("Initial table",  DT::renderDataTable("Freq")),
                   tabPanel("Initial Word Cloud", plotOutput("plot")),
                   tabPanel("Cleaned table", tableOutput("Freq2")),
                   tabPanel("Cleaned Word Cloud", plotOutput("plot2")))
        ),
        
        
        tabPanel("Sentiment Scores", tabsetPanel(
          tabPanel("Sentiment Scores",tableOutput("sentiment")))),
         
        tabPanel(" Product Table", tabsetPanel(
            tabPanel("Number of Users and Average Scores",tableOutput("tab1")),
            tabPanel("Top 6 Most Reviewed",tableOutput("tab2")))),
        tabPanel("Scatter plots",tableOutput("scp"))
        
          
       
      )
      
    )
    
  )
)

server <- function(input, output) {
  output$first <- renderTable({
    
    req(input$file)
    
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
 
  
  output$contents <-  DT::renderDataTable({
    
    req(input$file)
    
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep)
    
    corpus <- Corpus(VectorSource(df$Text))
    
    corpus.clean <- corpus %>%
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords(kind="en")) %>%
      tm_map(stripWhitespace)
    
    temp <- data.frame(text=sapply(corpus.clean, identity), 
                       stringsAsFactors=F)
    df$Text = temp
    names(df)[10]<-paste("Review")
    
    subdf <- df[,c(1:3,10)]
    
    return(subdf) 
})
  
  output$plot <- renderPlot({
    req(input$file)
    
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep)
    
    corpus <- Corpus(VectorSource(df$Text))
    dtm <- DocumentTermMatrix(corpus)
    dtm = removeSparseTerms(dtm, 0.99)
    findFreqTerms(dtm, 200)
    freq = data.frame(sort(colSums(as.matrix(dtm)), decreasing=TRUE))
    wordcloud(rownames(freq), freq[,1], max.words=200, colors=brewer.pal(8, "Dark2"), #plotting a wordcloud
              random.color=TRUE)
  })
  
  output$Freq <-  DT::renderDataTable({
    req(input$file)
    
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    #table1
    corpus <- Corpus(VectorSource(df$Text))
    
    dtm<- DocumentTermMatrix(corpus)
    freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
    table <- data.frame(word=names(freq), freq=freq)   
    return(table)
    
  })
  output$plot2 <- renderPlot({
    req(input$file)
    
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep)
    
    corpus <- Corpus(VectorSource(df$Text))
    
    corpus.clean <- corpus %>%       # removing punctuation, Numbers, Stopwords and White Spaces
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords(kind="en")) %>%
      tm_map(stripWhitespace)
    
    dtm <- DocumentTermMatrix(corpus.clean)
    dtm = removeSparseTerms(dtm, 0.99)
    findFreqTerms(dtm, 200)
    freq = data.frame(sort(colSums(as.matrix(dtm)), decreasing=TRUE))
    wordcloud(rownames(freq), freq[,1], max.words=200, colors=brewer.pal(8, "Dark2"), #plotting a wordcloud
              random.color=TRUE)
  })
  output$Freq2 <- renderTable({
    req(input$file)
    
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep)
    
    corpus <- Corpus(VectorSource(df$Text))
    
    corpus.clean <- corpus %>%
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords(kind="en")) %>%
      tm_map(stripWhitespace)
    
    dtm <- DocumentTermMatrix(corpus.clean)
    dtm = removeSparseTerms(dtm, 0.99)
    freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
    table <- data.frame(word=names(freq), freq=freq)   
    return(table)
    
  })
  #Sentiment Analysis
  output$sentiment <- renderTable({
    req(input$file)
    
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep)
  corpus <- Corpus(VectorSource(df$Text))
  
  corpus.clean <- corpus %>%  #removing punctuation, Numbers, Stopwords and White Spaces
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords(kind="en")) %>%
    tm_map(stripWhitespace)
  
  dtm <- DocumentTermMatrix(corpus.clean)
  t <- Terms(dtm)
  t_count <- tidy(dtm)
  ap_sentiments <- t_count %>%
    inner_join(get_sentiments("afinn"), by = c(term = "word"))
  g <- aggregate(ap_sentiments$score, by=list(Category=ap_sentiments$document), FUN=sum)
  g$Category <- as.numeric(as.character(g$Category))
  b <- g[order(g$Category) , ]
  names(b)[1]<-paste("Id")
  names(b)[2]<-paste("Sentiment_score")
  rownames(b) <- 1:nrow(b)
  dfscores <- merge(x = df, y = b, by.x = "row.names", by.y="Id", all.x = TRUE)
  dfscores$sentiment <- 'N/A'
  dfscores$sentiment <- ifelse(dfscores$Sentiment_score> 0, "Positive",ifelse(dfscores$Sentiment_score< 0, "Negative",NA))
  
  dfscores <- dfscores[complete.cases(dfscores), ]
  dfscores<-dfscores[,-1]
  })
  
  output$tab1 <- renderTable({
    req(input$file)
    
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep)
    corpus <- Corpus(VectorSource(df$Text))
    
    corpus.clean <- corpus %>%  #removing punctuation, Numbers, Stopwords and White Spaces
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords(kind="en")) %>%
      tm_map(stripWhitespace)
    
    dtm <- DocumentTermMatrix(corpus.clean)
    t <- Terms(dtm)
    t_count <- tidy(dtm)
    ap_sentiments <- t_count %>%
      inner_join(get_sentiments("afinn"), by = c(term = "word"))
    g <- aggregate(ap_sentiments$score, by=list(Category=ap_sentiments$document), FUN=sum)
    g$Category <- as.numeric(as.character(g$Category))
    b <- g[order(g$Category) , ]
    names(b)[1]<-paste("Id")
    names(b)[2]<-paste("Sentiment_score")
    rownames(b) <- 1:nrow(b)
    dfscores <- merge(x = df, y = b, by.x = "row.names", by.y="Id", all.x = TRUE)
    dfscores$sentiment <- 'N/A'
    dfscores$sentiment <- ifelse(dfscores$Sentiment_score> 0, "Positive",ifelse(dfscores$Sentiment_score< 0, "Negative",NA))
    
    dfscores <- dfscores[complete.cases(dfscores), ]
    dfscores<-dfscores[,-1]
    tab1<-ddply(dfscores,c(.(ProductId)),summarise,count=length(UserId),mean=mean(Sentiment_score))
    return(tab1)
    })
  output$tab2 <- renderTable({
    req(input$file)
    
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep)
    corpus <- Corpus(VectorSource(df$Text))
    
    corpus.clean <- corpus %>%  #removing punctuation, Numbers, Stopwords and White Spaces
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords(kind="en")) %>%
      tm_map(stripWhitespace)
    
    dtm <- DocumentTermMatrix(corpus.clean)
    t <- Terms(dtm)
    t_count <- tidy(dtm)
    ap_sentiments <- t_count %>%
      inner_join(get_sentiments("afinn"), by = c(term = "word"))
    g <- aggregate(ap_sentiments$score, by=list(Category=ap_sentiments$document), FUN=sum)
    g$Category <- as.numeric(as.character(g$Category))
    b <- g[order(g$Category) , ]
    names(b)[1]<-paste("Id")
    names(b)[2]<-paste("Sentiment_score")
    rownames(b) <- 1:nrow(b)
    dfscores <- merge(x = df, y = b, by.x = "row.names", by.y="Id", all.x = TRUE)
    dfscores$sentiment <- 'N/A'
    dfscores$sentiment <- ifelse(dfscores$Sentiment_score> 0, "Positive",ifelse(dfscores$Sentiment_score< 0, "Negative",NA))
    
    dfscores <- dfscores[complete.cases(dfscores), ]
    dfscores<-dfscores[,-1]
    tab1<-ddply(dfscores,c(.(ProductId)),summarise,count=length(UserId),mean=mean(Sentiment_score))
    tab2<-head(arrange(tab1,desc(count)), n = 6)
    return(tab2)
  })
  
  output$scp <- renderPlot({
    req(input$file)
    
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep)
    corpus <- Corpus(VectorSource(df$Text))
    
    corpus.clean <- corpus %>%  #removing punctuation, Numbers, Stopwords and White Spaces
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords(kind="en")) %>%
      tm_map(stripWhitespace)
    
    dtm <- DocumentTermMatrix(corpus.clean)
    t <- Terms(dtm)
    t_count <- tidy(dtm)
    ap_sentiments <- t_count %>%
      inner_join(get_sentiments("afinn"), by = c(term = "word"))
    g <- aggregate(ap_sentiments$score, by=list(Category=ap_sentiments$document), FUN=sum)
    g$Category <- as.numeric(as.character(g$Category))
    b <- g[order(g$Category) , ]
    names(b)[1]<-paste("Id")
    names(b)[2]<-paste("Sentiment_score")
    rownames(b) <- 1:nrow(b)
    dfscores <- merge(x = df, y = b, by.x = "row.names", by.y="Id", all.x = TRUE)
    dfscores$sentiment <- 'N/A'
    dfscores$sentiment <- ifelse(dfscores$Sentiment_score> 0, "Positive",ifelse(dfscores$Sentiment_score< 0, "Negative",NA))
    
    dfscores <- dfscores[complete.cases(dfscores), ]
    dfscores<-dfscores[,-1]
    tab1<-ddply(dfscores,c(.(ProductId)),summarise,count=length(UserId),mean=mean(Sentiment_score))
    tab2<-head(arrange(tab1,desc(count)), n = 6)
    scd<-merge(x = dfscores, y = tab2, by="ProductId", all.y =TRUE)
    p<-ggplot(scd, aes(x= scd$Sentiment_score, y=scd$Score, color=scd$ProductId)) +
      geom_point(aes(color=ProductId)) +
      facet_wrap( ~ scd$ProductId) +
      ggtitle(" Sentiment Score Vs User Rating  ") +
      labs(x ="Sentiment Scores", y = "User Ratings", fill = "ProductID") +
      theme(
        plot.title = element_text(color="Blue", size=20, face="bold"),
        axis.title.x = element_text(color="red", size=14, face="bold"),
        axis.title.y = element_text(color="red", size=14, face="bold")
      )
    print(p)
  })
  
  output$text <- renderUI({
    str1 <- paste("After Normalizing the data we can see Wordcloud shows much important terms")
    str2 <- paste("Sentiment Analysis could be improved",
                  input$range[1], "to", input$range[2])
    HTML(paste(str1, str2, sep = '<br/>'))})
}


# Running the Shiny App
shinyApp(ui = ui, server = server)
