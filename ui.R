library(shiny)

shinyUI(fluidPage(
    titlePanel("Word Prediction"),
    
    navlistPanel(widths=c(2,10), "Menu",
                 
                 tabPanel("App",
                        h2("Welcome to the Word Prediction App!"),
                        br(),
                        br(),
                        h3("Instructions"),
                        br(),
                        h4("Start typing in the box below"),
                        h4("When three or more words are typed the App will start predicting the next word"),
                        h4("The top three choices will be presented below"),
                        
                        textInput("word_input", label = h3("Enter you text here"), width = 500),
                        br(),
                        br(),
                        textOutput("prediction_output" ),

                 ), 
                 
                 tabPanel("Information",     
                          
                          h2("App Basics..."),
                          br(),
                          h4("Tables were created for 2,3 and 4 ngrams"),
                          h4("The app will present the highest probability word, starting with 4ngram and working down until"),      
                          h4("there are three words"),
                          br(),
                          h4("Specific details and code can be found at:  ")
                 )
                         
)
))