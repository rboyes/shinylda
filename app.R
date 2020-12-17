library(shiny)
library(shinyjs)
library(LDAvis)
library(tm)
library(plyr)
library(ldatuning)
library(stringr)
library(topicmodels)
library(dplyr)
library(stringi)
library(NLP)
library(tibble)
library(tidytext)
library(tsne)

ui <- shinyUI(
  fluidPage(
    useShinyjs(),
    titlePanel(h1("Latent Dirichlet Allocation Visualiser"), windowTitle = "Latent Dirichlet Allocation Visualiser"),
    titlePanel(h5(HTML("<p>This Shiny app performs Latent Dirichlet Allocation on a set of text entries, initially using <a href='http://31.125.158.39/donald-trump-tweets.csv'>Donald Trump's Tweets from his 2016 election campaign</a>.</p>
    <p>You can upload your own text in a similar format, simply a CSV file with a line of text in each cell. 
                       You can also download the HTML visualisation so it can be used again as a static web page, as well as a CSV of the processed topics that each cell of text belongs to.</p><hr>"))),
    sidebarLayout(

      # Sidebar panel for inputs ----
      sidebarPanel(width = 2,
        
        
        selectInput("nTerms", "Topics:",
                    selected = "5",
                    c("2" = "2",
                      "5" = "5", "10" = "10", "20" = "20", "50" = "50")),
        # Input: Select a file ----
        fileInput("file1", "Input File:",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        
        div(style="display:inline-block; vertical-align:top;", downloadButton(outputId = "downloadHTML", label = "Download HTML")),
        div(style="display:inline-block; vertical-align:top;", downloadButton(outputId = "downloadCSV", label = "Download CSV")),
        
        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),
        
        # Input: Select separator ----
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","),
        
        # Input: Select quotes ----
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"')
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        
        # Output: Data file ----
        visOutput('myChart')
        
      )
    )
  )
)

server <- shinyServer(function(input, output, session) {
  
  ldaJsonString <- reactiveVal(value = "")
  shinyjs::disable(id="downloadHTML")
  shinyjs::disable(id="downloadCSV")
  shared_data <- reactiveValues()
  
  output$myChart <- renderVis({

    print("Running function")
    shinyjs::disable(id="downloadHTML")
    shinyjs::disable(id="downloadCSV")
    
    if(is.null(input$file1)) {
      text = read.csv("donald-trump-tweets.csv", 
                      header = TRUE, 
                      sep = ',', 
                      quote = "\"")
    }
    else {
      text = read.csv(input$file1$datapath,
                      header = input$header,
                      sep = input$sep,
                      quote = input$quote)
      req(input$file1)  
    }
    
    
    withProgress(message = "Calculating topics...", expr = {
        
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
      tryCatch(
      {
          textdf <- data.frame(text)
          df <- textdf[1]
          textBody2 <- sapply(df, as.character)
          
          incProgress(amount = 0.1)
          
          print(sprintf("Read in input csv %s", input$file1$datapath))
  
          textBody2 <- textBody2[!textBody2 %in% ""] %>% # remove empty lines
            iconv('UTF-8', 'ASCII') %>% # convert to 'ASCII'
            removeNumbers() %>% # remove digits
            removePunctuation(preserve_intra_word_contractions = TRUE, preserve_intra_word_dashes = FALSE) #remove punctuations (keeping intraword contractions)
          incProgress(amount = 0.1)
          
          textBody2 <- gsub("[[:cntrl:]]", " ", textBody2)  # replace control characters with space
          textBody2 <- gsub("^[[:space:]]+", "", textBody2) # remove whitespace at beginning of documents
          textBody2 <- gsub("[[:space:]]+$", "", textBody2) %>% # remove whitespace at end of documents
            tolower()  # force to lowercase
          textBody2 <- gsub("\\\\", "", textBody2) # remove slash
          incProgress(amount = 0.1)
          
          stop_words = stopwords(kind = "en")
          #remove stopwords and hashtags
          for (i in 1:length(textBody2)){
            
            textBody2[i] <- removeWords(textBody2[i], stop_words)# remove stopwords
              textBody2[i] <- gsub("\\btag", "", textBody2[i]) %>%  # remove (hash-)tag from the beginning of words
                str_replace_all("[[:punct:]]", " ") %>%  # remove all punctuation
                stemDocument(language = "english") # stem words
          }
          
          textBody2 <- gsub("^[[:space:]]+", "", textBody2) # remove whitespace at beginning of documents
          textBody2 <- gsub("[[:space:]]+$", "", textBody2) # remove whitespace at end of documents            
          
          #Get corpus and dtm
          
          textBody2.corpus = VCorpus(VectorSource(textBody2)) # create corpus
          textBody2.dtm = DocumentTermMatrix(textBody2.corpus) # create document-term matrix
          incProgress(amount = 0.1)
          
          BigramTokenizer <- function(x)
              unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
          
          textBody2.dtm2 <- DocumentTermMatrix(textBody2.corpus, control = list(tokenize = BigramTokenizer))
          textBody2.uniBiDtm = cbind(textBody2.dtm,textBody2.dtm2)
          #remove from dtm the documents for which the row total is 0 -- if not removed the lda function gives an error 
          
          uniq <- unique(textBody2.uniBiDtm$i)
          textBody2.dtmNew <- textBody2.uniBiDtm[uniq,]
  
          corpusNew <- textBody2.corpus$content[uniq]
          
          incProgress(amount = 0.1)
          
          # Fit the LDA model
          fitted <- LDA(textBody2.dtmNew, as.numeric(input$nTerms), method = "Gibbs", control = list(seed = 7, iter = 100, alpha = 0.02))
  
          incProgress(amount = 0.1)
          
          print("Fitted topics")
          
          # create the dataframe for the download csv button
          textdf2 <- data.frame(textdf[uniq,1])
          topic <- topics(fitted) %>% as.matrix()
          tterms <- terms(fitted,as.numeric(input$nTerms)) %>% t() %>% data.frame()
          
          topic_nums <- c(1:input$nTerms)
          tterms <- cbind(tterms, topic_nums) %>% data.frame()
          names(tterms) <- c(paste("word", 1:input$nTerms), "topic")
          colnames(tterms)[colnames(tterms) == 'topic_nums'] <- "topic"
          
          textdf3 <- cbind(textdf2, topic)
          final_df <- join(textdf3, tterms, type = "left", by = "topic")
          print(paste("nrow(final_df):", nrow(final_df)))
          topic_probs <- fitted@gamma
          print(paste("nrow(fitted@gamma):", nrow(fitted@gamma)))
          print(paste("nrow(topic_probs):", nrow(topic_probs)))
          names(topic_probs) <- paste("topic", 1:input$nTerms, "prob")
          print(paste("nrow(final_df):", nrow(final_df)))
          final_df <- cbind(final_df, topic_probs)
          colnames(final_df)[1] <- colnames(textdf)[1]
          
          shared_data$final_df <- final_df          
          
          createdTopics = TRUE
          
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
      
      if(createdTopics){
        
        phi <- posterior(fitted)$terms %>% as.matrix
        theta <- posterior(fitted)$topics %>% as.matrix
        vocab <- colnames(phi)
        doc_length <- vector()
        for (i in 1:length(corpusNew)) {
          temp <- paste(corpusNew[[i]], collapse = ' ')
          doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
        }
        # temp_frequency <- as.matrix(textBody2.dtmNew)
        # freq_matrix <- data.frame(ST = colnames(temp_frequency),
        #                           Freq = colSums(temp_frequency))
        # rm(temp_frequency)
        incProgress(amount = 0.1)
        
        freq_matrix <- tidy(textBody2.dtmNew)
        term_frequency <- aggregate(freq_matrix$value, by=list(freq_matrix$column), FUN=sum)
        svd_tsne <- function(x) {
          # Will not cause error number of topics is 2
          tsne(svd(x)$u)
        }
        print("Creating JSON")

        incProgress(amount = 0.1)
        json <- createJSON(phi = phi, theta = theta, vocab = vocab, doc.length = doc_length,
                           term.frequency = term_frequency$x, mds.method = svd_tsne)
        
        incProgress(amount = 0.1)
        
        shinyjs::enable(id="downloadHTML")
        shinyjs::enable(id="downloadCSV")
        
        ldaJsonString(json)
        
        return(json)
        
      }
    })
  })
  
  output$downloadHTML <- downloadHandler(
    filename = function() {
      paste("lda-visualisation", input$nTerms, ".html", sep = "")
    },
    content = function(file) {
      
      htmlTemplate <- "
      <link rel='stylesheet' type='text/css' href='https://cdn.rawgit.com/bmabey/pyLDAvis/files/ldavis.v1.0.0.css'>

      
      <div id='ldavis_el1242829485730617925476252464'></div>
      <script type='text/javascript'>
      
      var ldavis_el1242829485730617925476252464_data = %s

      function LDAvis_load_lib(url, callback){
        var s = document.createElement('script');
        s.src = url;
        s.async = true;
        s.onreadystatechange = s.onload = callback;
        s.onerror = function(){console.warn('failed to load library ' + url);};
        document.getElementsByTagName('head')[0].appendChild(s);
      }
      
      if(typeof(LDAvis) !== 'undefined'){
        // already loaded: just create the visualization
        !function(LDAvis){
          new LDAvis('#' + 'ldavis_el1242829485730617925476252464', ldavis_el1242829485730617925476252464_data);
        }(LDAvis);
      }else if(typeof define === 'function' && define.amd){
        // require.js is available: use it to load d3/LDAvis
        require.config({paths: {d3: 'https://cdnjs.cloudflare.com/ajax/libs/d3/3.5.5/d3.min'}});
        require(['d3'], function(d3){
          window.d3 = d3;
          LDAvis_load_lib('https://cdn.rawgit.com/bmabey/pyLDAvis/files/ldavis.v1.0.0.js', function(){
            new LDAvis('#' + 'ldavis_el1242829485730617925476252464', ldavis_el1242829485730617925476252464_data);
          });
        });
      }else{
        // require.js not available: dynamically load d3 & LDAvis
        LDAvis_load_lib('https://cdnjs.cloudflare.com/ajax/libs/d3/3.5.5/d3.min.js', function(){
          LDAvis_load_lib('https://cdn.rawgit.com/bmabey/pyLDAvis/files/ldavis.v1.0.0.js', function(){
            new LDAvis('#' + 'ldavis_el1242829485730617925476252464', ldavis_el1242829485730617925476252464_data);
          })
        });
      }
      </script>
      "
      
      writeLines(sprintf(htmlTemplate, ldaJsonString()), file)
    })
    
    output$downloadCSV <- downloadHandler(
      filename <- function(variables) {
        paste("lda-details_", input$nTerms, "_topics.csv", sep = "")
      },
      content <- function(file) {
        write.csv(shared_data$final_df, file, row.names = FALSE)
      }
    )
})

shinyApp(ui = ui, server = server)