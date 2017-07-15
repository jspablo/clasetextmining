library(qdap)
library(XML)
library(tm)
library(splitstackshape)
library(caret)

GenerateVocabulary <- function(path, n = 1000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", verbose = TRUE) {
  setwd(path)
  
  files = list.files(pattern="*.xml")
  
  corpus.raw <- NULL
  i <- 0
  for (file in files) {
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
    i <- i + 1
    if (verbose) print(paste(i, " ", file))	
  }
  
  corpus.preprocessed <- corpus.raw
  
  
  if (lowcase) {
    if (verbose) print("Tolower...")
    corpus.preprocessed <- tolower(corpus.preprocessed)
  }	
  
  if (punctuations) {
    if (verbose) print("Removing punctuations...")		
    corpus.preprocessed <- removePunctuation(corpus.preprocessed)
  }
  
  if (numbers) {
    if (verbose) print("Removing numbers...")
    corpus.preprocessed <- removeNumbers(corpus.preprocessed)
  }
  
  if (whitespaces) {
    if (verbose) print("Stripping whitestpaces...")
    corpus.preprocessed <- stripWhitespace(corpus.preprocessed)
  }
  
  if (swlang!="")	{
    if (verbose) print(paste("Removing stopwords for language ", swlang , "..."))
    corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang))
  }
  
  #swlist <- c("si","q","gracias","hoy","ser","día","mejor","jaja","xd") # Para modelo variedad español
  if (swlist!="") {
    if (verbose) print("Removing provided stopwords...")
    corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
  }
  
  if (verbose) print("Generating frequency terms")
  
  corpus.frequentterms <- freq_terms(corpus.preprocessed, n)
  if (verbose) plot(corpus.frequentterms)
  
  return (corpus.frequentterms)
}


GenerateBoW <- function(path, vocabulary, n = 1000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", class="variety", verbose = TRUE) {
  setwd(path)
  
  truth <- read.csv("truth.txt", sep=":", header=FALSE)
  truth <- truth[,c(1,4,7)]
  colnames(truth) <- c("author", "gender", "variety")
  
  i <- 0
  bow <- NULL
  files = list.files(pattern="*.xml")
  for (file in files) {
    author <- gsub(".xml", "", file)
    variety <- truth[truth$author==author,"variety"]
    gender <- truth[truth$author==author,"gender"]
    
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    txtdata <- xpathApply(xmlfile, "//document", function(x) xmlValue(x))
    
    
    if (lowcase) {
      txtdata <- tolower(txtdata)
    }
    
    if (punctuations) {
      txtdata <- removePunctuation(txtdata)
    }
    
    if (numbers) {
      txtdata <- removeNumbers(txtdata)
    }
    
    if (whitespaces) {
      txtdata <- stripWhitespace(txtdata)
    }
    
    line <- author
    freq <- freq_terms(txtdata, n)
    for (word in vocabulary$WORD) {
      thefreq <- 0
      if (length(freq[freq$WORD==word,"FREQ"])>0) {
        thefreq <- freq[freq$WORD==word,"FREQ"]
      } 
      line <- paste(line, ",", thefreq, sep="")
    }
    if (class=="variety") {
      line <- paste(variety, ",", line, sep="")
    } else {
      line <- paste(gender, ",", line, sep="")
    }
    
    bow <- rbind(bow, line)
    
    i <- i + 1
    
    if (verbose) {
      if (class=="variety") {
        print(paste(i, author, variety))
      } else {
        print(paste(i, author, gender))
      }
    }
  }
  
  return (bow)
}


n <- 1000
path_training <- "/home/damc/Descargas/text_mining/pan-ap17-bigdata/training"	# Your training path
path_test <- "/home/damc/Descargas/text_mining/pan-ap17-bigdata/test"			# Your test path

vocabulary <- GenerateVocabulary(path_training, n, swlang="es")

bow_training <- GenerateBoW(path_training, vocabulary, n, class="variety")
bow_test <- GenerateBoW(path_test, vocabulary, n, class="variety")

training <- cSplit(bow_training, "V1", ",")
test <- cSplit(bow_test, "V1", ",")

training1 <- training[,1]
training2 <- training[,3:ncol(training)]
training <- cbind(training1,training2)
names(training)[1] <- "class"
truth  <- unlist(test[,1])
test <- test[,3:ncol(test)]

train_control <- trainControl( method="repeatedcv", number = 10 , repeats = 3) 
model_SVM <- train( class~., data= training, trControl = train_control, method = "svmLinear")
#print(model_SVM)

#train_control <- trainControl(method="none")
#model_SVM <- train( class~., data= training, trControl = train_control, method = "svmLinear")

pred_SVM <- predict(model_SVM, test)
confusionMatrix(pred_SVM, truth)





