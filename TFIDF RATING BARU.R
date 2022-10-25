
install.packages("tm") # For text mining and data cleaning activities
install.packages ("SnowballC") # For stemming of text
install.packages("textstem") # For stemming of text
install.packages("readxl") # For reading excel files
install.packages("dplyr") # For filtering and selection of data
install.packages("wordcloud") # For generating word cloud
install.packages("RColorBrewer") # For colorful word cloud

# Loading Packages
library(tm) # For text mining and data cleaning activities
library(textstem) # For stemming of text
library(SnowballC) # For stemming of text
library(readxl) # For reading excel files
library(dplyr) # For filtering and selection of data
library(wordcloud) # Reading the dataset
library(RColorBrewer) # For colorful word cloud

datakedua<-read_excel("C:/Users/Win 10/Documents/SKRIPSI/data rating olah/data olah rating 2.xlsx")
# Filtering the dataset

Comment<-datakedua %>% filter (Classification=="Comment")

CorpusData<-Corpus(VectorSource(datakedua$Comment)) # Converts to a Corpus i.e. Collection of text
#str(CorpusData)
CorpusData$content[1]

# Data Cleaning using tm package

CorpusData<-tm_map(CorpusData,tolower) # to convert the text to lowercase
CorpusData <-tm_map(CorpusData, removeNumbers) # to remove numbers
CorpusData <- tm_map(CorpusData, removePunctuation) # to remove punctuation
CorpusData <- tm_map(CorpusData, stripWhitespace) # to remove extra spaces
CorpusData <- tm_map(CorpusData, removeWords,c('landline')) # To remove custom stop words
CorpusDataNew <- tm_map(CorpusData, content_transformer(gsub), pattern = "txt", replacement = "text", fixed=TRUE)

CorpusDataNew<-tm_map(CorpusDataNew,lemmatize_strings)

#Creating a term document matrix with tf-idf weight
tdm_tfidf <- TermDocumentMatrix(CorpusDataNew,
                                control = list(weighting = weightTfIdf)) #using td-idf method
#Other weightings available in tm package weightTf, weightTfIdf,weightBin, and weightSMART
# Link: https://cran.r-project.org/web/packages/tm/tm.pdf - page 41
m_tfidf <- as.matrix(tdm_tfidf)
headmatrix_tfidf<-head.matrix(m_tfidf,10)
CorpusData$content[1]
CorpusData$content[2]
v_tfidf <- sort(rowSums(m_tfidf),decreasing=TRUE)
head(v_tfidf, 10)
d_tfidf <- data.frame(word = names(v_tfidf),freq=v_tfidf)
head(d_tfidf, 10)
#row.names(d)

#Without tf-idf method

tdm_normal <- TermDocumentMatrix(CorpusDataNew)
m_normal <- as.matrix(tdm_normal)
headmatrix_normal<-head.matrix(m_normal,10)
v_normal <- sort(rowSums(m_normal),decreasing=TRUE)
head(v_normal, 10)
d_normal <- data.frame(word = names(v_normal),freq=v_normal)
head(d_normal, 10)

#row.names(d)

# Using bar graph

par(mfrow=c(1,2))

bp_tfidf<-barplot(d_tfidf[1:10,]$freq, las = 2, names.arg = d_tfidf[1:10,]$word,
                  col ="#2d4dd2", main ="Most important spam words
                  tf id weights",
                  ylab = "tf-idf weight",xlab="spam words",ylim=c(0,15))
text(x=bp_tfidf, y=d_tfidf[1:10,]$freq, labels=round(d_tfidf[1:10,]$freq,0), pos=1,family = "sans",col="#ffffff",font=1,cex=1)

bp_normal<-barplot(d_normal[1:10,]$freq, las = 2, names.arg = d_normal[1:10,]$word,
                   col ="#2d4dd2", main ="Most frequent spam words
                   without weights",
                   ylab = "term frequency",xlab="spam words",ylim=c(0,200))
text(x=bp_tfidf, y=d_normal[1:10,]$freq, labels=round(d_normal[1:10,]$freq,0), pos=1,family = "sans",col="#ffffff",font=1,cex=1)

write.csv(m_tfidf, file="m_tfidf2.csv")
write.csv(d_tfidf, file="d_tfidf2.csv")
write.csv(bp_tfidf, file="bp_tfidf2.csv")
