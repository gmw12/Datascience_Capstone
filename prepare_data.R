library(stringr)
library(tm)
library(ggplot2)
library(ngram)
library(dplyr)
library(tidyr)


con_blogs <- file("final/en_US/en_US.blogs.txt", "r")
blogs <- readLines(con_blogs)
close(con_blogs)

con_news <- file("final/en_US/en_US.news.txt", "r")
news <- readLines(con_news)
close(con_news)

con_twitter <- file("final/en_US/en_US.twitter.txt", "r")
twitter <- readLines(con_twitter)
close(con_twitter)


# Full file blog stats
con_stats <- function(type, text){
    lines <- length(text)
    mb <- round(object.size(text)/1000000, 0)
    words <- sum(sapply(strsplit(text,"\\s+"), length))
    words_line <- round(words/lines, 0)  
    return(c(type, lines, mb, words, words_line))
}

word_stats <- data.frame()
word_stats <- rbind(word_stats, con_stats("blogs", blogs))
word_stats <- rbind(word_stats, con_stats("news", news))
word_stats <- rbind(word_stats, con_stats("twitter", twitter))
colnames(word_stats) <- c("type", "lines", "size_mb", "words", "word_line")

corpus_all <- c(blogs, news, twitter)
rm(blogs, news, twitter, con_blogs, con_news, con_twitter)

profanity_list  <- read.csv("profanity.txt",header = FALSE,stringsAsFactors = FALSE)
#word_list <- c(unlist(profanity_list), stopwords())
word_list <- profanity_list

clean_text <- function(corpus) {
    corpus <- tolower(corpus)
    corpus <- gsub("’", "'", corpus) # fix contraction
    # split at all ".", "," and etc.
    corpus <- unlist(strsplit(corpus, "[.,:;!?(){}<>]+")) # 
    corpus <- removeWords(corpus, word_list)
    corpus <- gsub("n't", "n not", corpus) # fix contraction
    corpus <- gsub("'d", " would", corpus) # fix contraction
    corpus <- gsub("'ll", " will", corpus) # fix contraction
    corpus <- gsub("'ve", " have", corpus) # fix contraction
    corpus <- gsub("'s", "", corpus) # fix contraction
    corpus <- gsub("[0-9]", " ", corpus) # remove numbers
    
    
    # replace all non-alphanumeric characters with a space at the beginning/end of a word.
    corpus <- gsub("^[^a-z0-9]+|[^a-z0-9]+$", " ", corpus) # at the beginning/end of a line
    corpus <- gsub("[^a-z0-9]+\\s", " ", corpus) # before space
    corpus <- gsub("\\s[^a-z0-9]+", " ", corpus) # after space
    corpus <- gsub("\\s+", " ", corpus) # remove multiple spaces
    corpus <- str_trim(corpus) # remove spaces at the beginning/end of the line
    return(corpus)
}

start_time <- Sys.time()
corpus_clean <- clean_text(corpus_all)
saveRDS(corpus_clean, "data/corpus_clean.rds")
Sys.time() - start_time


corpus_clean <- corpus_clean[str_count(corpus_clean, "\\s+")>0] 
bigram <- ngram(corpus_clean, n=2); 
saveRDS(bigram, "data/bigram.rds")

corpus_clean <- corpus_clean[str_count(corpus_clean, "\\s+")>1] 
trigram <- ngram(corpus_clean, n=3)
saveRDS(trigram, "data/trigram.rds")

corpus_clean <- corpus_clean[str_count(corpus_clean, "\\s+")>2] 
quadgram <- ngram(corpus_clean, n=4)
saveRDS(quadgram, "data/quadgram.rds")

rm(corpus_clean)


bigram_df <- get.phrasetable(bigram); 
saveRDS(bigram_df, "data/bigram_df.rds")

trigram_df <- get.phrasetable(trigram); 
saveRDS(trigram_df, "data/trigram_df.rds")

quadgram_df <- get.phrasetable(quadgram); 
saveRDS(quadgram_df, "data/quadgram_df.rds")

rm(bigram, trigram, quadgram)

# bigram_df <- readRDS("data/bigram_df.rds")
# trigram_df <- readRDS("data/trigram_df.rds")
# quadgram_df <- readRDS("data/quadgram_df.rds")

bigram_df_final <- bigram_df[bigram_df$freq>1,]
trigram_df_final <- trigram_df[trigram_df$freq>1,]
quadgram_df_final <- quadgram_df[quadgram_df$freq>1,]
bigram_df_final$prop <- NULL
trigram_df_final$prop <- NULL
quadgram_df_final$prop <- NULL

rm(bigram_df, trigram_df, quadgram_df)

bigram_df_final$ngram0 <- word(bigram_df_final$ngrams, 2)
trigram_df_final$ngram0 <- word(trigram_df_final$ngrams, 3)
quadgram_df_final$ngram0 <- word(quadgram_df_final$ngrams, 4)

bigram_df_final <- bigram_df_final[nchar(bigram_df_final$ngram0) > 3, ]
trigram_df_final <- trigram_df_final[nchar(trigram_df_final$ngram0) > 3, ]
quadgram_df_final <- quadgram_df_final[nchar(quadgram_df_final$ngram0) > 3, ]

bigram_df_final$ngram1 <- word(bigram_df_final$ngrams, 1)
trigram_df_final$ngram1 <- word(trigram_df_final$ngrams, 1)
quadgram_df_final$ngram1 <- word(quadgram_df_final$ngrams, 1)

trigram_df_final$ngram2 <- word(trigram_df_final$ngrams, 2)
quadgram_df_final$ngram2 <- word(quadgram_df_final$ngrams, 2)

quadgram_df_final$ngram3 <- word(quadgram_df_final$ngrams, 3)

bigram_df_final$ngrams <- NULL
trigram_df_final$ngrams <- NULL
quadgram_df_final$ngrams <- NULL

bigram_df_final <- bigram_df_final[with(bigram_df_final, order(ngram1, -freq)), ]
trigram_df_final <- trigram_df_final[with(trigram_df_final, order(ngram1, ngram2, -freq)), ]
quadgram_df_final <- quadgram_df_final[with(quadgram_df_final, order(ngram1, ngram2, ngram3, -freq)), ]

saveRDS(bigram_df_final, "data/bigram_df_final.rds")
saveRDS(trigram_df_final, "data/trigram_df_final.rds")
saveRDS(quadgram_df_final, "data/quadgram_df_final.rds")

bigram_df_final <- readRDS("data/bigram_df_final.rds")
trigram_df_final <- readRDS("data/trigram_df_final.rds")
quadgram_df_final <- readRDS("data/quadgram_df_final.rds")

#----------------------------

predict_word <- function(corpus) {
    corpus <- tolower(corpus)
    corpus <- gsub("’", "'", corpus) # fix contraction
    corpus <- gsub("n't", "n not", corpus) # fix contraction
    corpus <- gsub("'d", " would", corpus) # fix contraction
    corpus <- gsub("'ll", " will", corpus) # fix contraction
    corpus <- gsub("'ve", " have", corpus) # fix contraction
    corpus <- gsub("'s", "", corpus) # fix contraction

    corpus <- gsub("[0-9]", " ", corpus) # remove numbers
    corpus <- removeWords(corpus, word_list)
    
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
    }
    
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
    
    
    return(predicted)
}

input_corpus <- "Every inch of you is perfect from the bottom to the"
predict_word(input_corpus)

