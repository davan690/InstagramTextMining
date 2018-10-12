library(tm)
library(wordcloud)
library(memoise)
library("stringr")
library(rdrop2)

###Customizing Term Matrix function to work for  Arabic and Persian Unicode

TermMatrixFa <- function(textinput){
  textinput<-gsub( "[#,;@\n0-9]"," ",textinput)
  tex<-gsub("[^A-Za-z\U0622-\U063A\U0641-\U0649\U06AF\U0698\U067E\U0686\U064A\U06CC]"," ", textinput)
  tex<-chartr("[a-z]","[A-Z]",tex)
  CorpusInstagram = Corpus(VectorSource(tex))
    TDM<-TermDocumentMatrix(CorpusInstagram, control = list(minWordLength = 4))
    TDM<-as.matrix(TDM)
    sort(rowSums(TDM), decreasing = TRUE)
}


#Standard procedure to clean-up corpus

TermMatrixEn <- function(textinput){
  textinput = iconv(textinput, 'UTF-8', 'ASCII',sub='')
  CorpusInstagram = Corpus(VectorSource(textinput))
  CorpusInstagram = tm_map(CorpusInstagram, content_transformer(tolower))
  CorpusInstagram = tm_map(CorpusInstagram, removePunctuation)
  CorpusInstagram = tm_map(CorpusInstagram, removeNumbers)
  CorpusInstagram = tm_map(CorpusInstagram, removeWords,c(stopwords("english"),"the", "and", "but"))
  TDM = TermDocumentMatrix(CorpusInstagram, control = list(minWordLength = 3))
  TDM = as.matrix(TDM)
  sort(rowSums(TDM), decreasing = TRUE)
}



#Replace your token
token<-'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
tokenbox <- readRDS("droptoken.rds")

outputDir <- "ADDRESS"

saveData <- function(data) {
  data <- t(data)
  # unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
     write.csv(data, filePath, row.names = FALSE, quote = TRUE)
      drop_upload(filePath, dest = outputDir,dtoken = tokenbox)
  }