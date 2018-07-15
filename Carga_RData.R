# Including needed libraries
library(XML)
library(tm)
library(splitstackshape)
library(caret)
library(qdap)

start.time <- Sys.time()

# Preparing parameters
vector <- c(10, 50, 100, 500, 1000, 5000, 10000) # palabras más frecuentes para la representación, para el corpus, y calcular caracteristicias
lang <- "es" # idioma

path_training <- "/home/usuario/big-data/Text Mining en Social Media/Entrega/training"
path_test <- "/home/usuario/big-data/Text Mining en Social Media/Entrega/test"

# Auxiliar functions
# * GenerateVocabulary: Given a corpus (training set), obtains the n most frequent words
# * GenerateBoW: Given a corpus (training or test), and a vocabulary, obtains the bow representation

# GenerateVocabulary: Given a corpus (training set), obtains the n most frequent words
# n = palabras mas frecuentes
# swlang = eliminar palabras vacías
# swlist = palabras a eliminar, se introducen manualmente como parametros
GenerateVocabulary <- function(path, n = 1000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", verbose = TRUE) {
  
  setwd(path)
  
  # Reading corpus list of files
  files = list.files(pattern="*.xml")
  
  # Reading files contents and concatenating into the corpus.raw variable
  corpus.raw <- NULL
  i <- 0
  for (file in files) {
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
    i <- i + 1
    if (verbose) print(paste(i, " ", file))
  }
  
  # Preprocessing the corpus
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
  
  if (swlist!="") {
    if (verbose) print("Removing provided stopwords...")
    corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
  }
  
  # Generating the vocabulary as the n most frequent terms
  if (verbose) print("Generating frequency terms")
  corpus.frequentterms <- freq_terms(corpus.preprocessed, n)
  if (verbose) plot(corpus.frequentterms)
  
  return (corpus.frequentterms)
}

# GenerateBoW: Given a corpus (training or test), and a vocabulary, obtains the bow representation
GenerateBoW <- function(path, vocabulary, n = 100000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", class="variety", verbose = TRUE) {
  setwd(path)
  
  # Reading the truth file
  truth <- read.csv("truth.txt", sep=":", header=FALSE)
  truth <- truth[,c(1,4,7)]
  colnames(truth) <- c("author", "gender", "variety")
  
  i <- 0
  bow <- NULL
  # Reading the list of files in the corpus
  files = list.files(pattern="*.xml")
  for (file in files) {
    # Obtaining truth information for the current author
    author <- gsub(".xml", "", file)
    variety <- truth[truth$author==author,"variety"]
    gender <- truth[truth$author==author,"gender"]
    
    # Reading contents for the current author
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    txtdata <- xpathApply(xmlfile, "//document", function(x) xmlValue(x))
    
    # Preprocessing the text
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
    
    # Building the vector space model. For each word in the vocabulary, it obtains the frequency of occurrence in the current author.
    line <- author
    freq <- freq_terms(txtdata, n)
    for (word in vocabulary$WORD) {
      thefreq <- 0
      if (length(freq[freq$WORD==word,"FREQ"])>0) {
        thefreq <- freq[freq$WORD==word,"FREQ"]
      }
      line <- paste(line, ",", thefreq, sep="")
    }
    
    # Concatenating the corresponding class: variety or gender
    if (class=="variety") {
      line <- paste(variety, ",", line, sep="")
    } else {
      line <- paste(gender, ",", line, sep="")
    }
    
    # New row in the vector space model matrix
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



for (n in 1:length(vector)) {
 
  # GENERATE VOCABULARY
  vocabulary <- GenerateVocabulary(path_training, vector[n], swlang=lang, swlist = "q")
  fichero<-paste("/home/usuario/big-data/Text Mining en Social Media/Entrega/vocabulary", as.character(vector[n]), ".Rdata", sep="")
  save(vocabulary, file = fichero)
  
  # GENDER IDENTIFICATION
  #######################
  # GENERATING THE BOW FOR THE GENDER SUBTASK FOR THE TRAINING SET
  bow_training_gender <- GenerateBoW(path_training, vocabulary, class="gender")
  fichero<-paste("/home/usuario/big-data/Text Mining en Social Media/Entrega/bow_training_gender", as.character(vector[n]), ".Rdata", sep="")
  save(bow_training_gender, file = fichero)
  
  # GENERATING THE BOW FOR THE GENDER SUBTASK FOR THE TEST SET
  bow_test_gender <- GenerateBoW(path_test, vocabulary, class="gender")
  fichero<-paste("/home/usuario/big-data/Text Mining en Social Media/Entrega/bow_test_gender", as.character(vector[n]), ".Rdata", sep="")
  save(bow_test_gender, file = fichero)
  
  
  # VARIETY IDENTIFICATION
  ########################
  # GENERATING THE BOW FOR THE GENDER SUBTASK FOR THE TRAINING SET
  bow_training_variety <- GenerateBoW(path_training, vocabulary, class="variety")
  fichero<-paste("/home/usuario/big-data/Text Mining en Social Media/Entrega/bow_training_variety", as.character(vector[n]), ".Rdata", sep="")
  save(bow_training_variety, file = fichero)
  
  # GENERATING THE BOW FOR THE GENDER SUBTASK FOR THE TEST SET
  bow_test_variety <- GenerateBoW(path_test, vocabulary, class="variety")
  fichero<-paste("/home/usuario/big-data/Text Mining en Social Media/Entrega/bow_test_variety", as.character(vector[n]), ".Rdata", sep="")
  save(bow_test_variety, file = fichero)

}