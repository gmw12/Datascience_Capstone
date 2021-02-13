library(shiny)
library(stringr)
library(tm)
library(ggplot2)
library(ngram)
library(dplyr)
library(tidyr)

shinyServer(function(input, output) {
    
    profanity_list  <- read.csv("profanity.txt",header = FALSE,stringsAsFactors = FALSE)
    #word_list <- c(unlist(profanity_list), stopwords())
    word_list <- profanity_list
    
    predict_word <- function(corpus) {
        cat(file=stderr(), str_c("input corpus ---> ", corpus), "\n")
        corpus <- tolower(corpus)
        corpus <- gsub("’", "'", corpus) # fix contraction
        corpus <- gsub("n't", "n not", corpus) # fix contraction
        corpus <- gsub("'d", " would", corpus) # fix contraction
        corpus <- gsub("'ll", " will", corpus) # fix contraction
        corpus <- gsub("'ve", " have", corpus) # fix contraction
        corpus <- gsub("'s", "", corpus) # fix contraction
        
        corpus <- gsub("[0-9]", " ", corpus) # remove numbers
        #corpus <- removeWords(corpus, word_list)
        
        # replace all non-alphanumeric characters with a space at the beginning/end of a word.
        corpus <- gsub("^[^a-z0-9]+|[^a-z0-9]+$", " ", corpus) # at the beginning/end of a line
        corpus <- gsub("[^a-z0-9]+\\s", " ", corpus) # before space
        corpus <- gsub("\\s[^a-z0-9]+", " ", corpus) # after space
        corpus <- gsub("\\s+", " ", corpus) # remove multiple spaces
        corpus <- str_trim(corpus) # remove spaces at the beginning/end of the line
        
        word_count <- sapply(strsplit(corpus, " "), length)
        
        if (word_count > 2) {
            word1 <- word(corpus, word_count)
            word2 <- word(corpus, word_count-1)
            word3 <- word(corpus, word_count-2)
        
        
            df1 <- head(quadgram_df_final[quadgram_df_final$ngram1==word3 & quadgram_df_final$ngram2==word2 & quadgram_df_final$ngram3==word1 ,], 3)
            predicted <- head(df1$ngram0,3)
            if (nrow(df1) < 3){
                df2 <- head(trigram_df_final[trigram_df_final$ngram1==word2 & trigram_df_final$ngram2==word1 ,], 3)
                predicted <- c(predicted, head(df2$ngram0, 3) )
            }
            if ((nrow(df1) + nrow(df2)) < 3){
                df3 <- head(bigram_df_final[bigram_df_final$ngram1==word1 ,], 3)
                predicted <- c(predicted, head(df3$ngram0, 3) )
            }
            
            cat(file=stderr(), str_c(" ---> ", unlist(predicted)), "\n")
            return(predicted)
        
    }
    }
    
    
    #------------------------------
    word_out <<- ""
    
    test <- observe({
        cat(file=stderr(), str_c("observe:  ", input$word_input), "\n")
        word_out <<- predict_word(input$word_input)
        word_out <<- paste(word_out, collapse = ", ")
        output$prediction_output <- renderText({word_out})
    })
    

})