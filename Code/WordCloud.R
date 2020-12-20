# Loading Libaries 
library('wordcloud')
library(DT)
library('tm')


#import data
library(readxl)
wordcloud <- read_excel("C:/Users/alex7/Desktop/Data_WordCould.xls")

#preprocess data
news <- wordcloud$clean
news <- gsub(pattern = "\\W", replacement = " ", news)
news <- stripWhitespace(news)
news <- removeWords(news,c("say","says","said","first","second","will",
                          "year","years","now","one","may","just","two","five"))


# seperate Label 0 and 1
news1 <- news[wordcloud$Label==1]
news0 <- news[wordcloud$Label==0]

# Creating the corpus
myCorpus <- Corpus(VectorSource(news))
# Label 1
#myCorpus <- Corpus(VectorSource(news1))
# Label 0
#myCorpus <- Corpus(VectorSource(news0))


#BUILDING DTM          
myDtm <- TermDocumentMatrix(myCorpus, control = list(stopwords=TRUE,minWordLength = 3))
inspect(myDtm[266:270,31:40])


# calculate the dimensions of TDM
dim(myDtm)

# TDM frequency count
matrix_myDtm<-as.matrix(myDtm)
freq <- sort(rowSums(matrix_myDtm), decreasing=TRUE)
word_freq <- data.frame(word=names(freq), freq=freq)


# Frequency Table     

# create new dataframe of tdm
freq_words <- data.frame("Word"=word_freq[,1], "Frequency"= word_freq[,2])
# now sorting it and cutting it 
freq_words <- subset(freq_words[with(freq_words, order(-freq)),],freq>300)
#Frequency table
datatable(freq_words,options = list())



# Wordcloud (Label 1 VS Label 0)

# Label=1, news1
# repeat Dtm and Frequency Table steps

set.seed(1234)
wordcloud(words = freq_words$Word, freq = freq_words$Frequency, min.freq = 500,
          max.words=1500, random.order=FALSE, rot.per=0.3, 
          colors=brewer.pal(7, "Dark2"))


# Lable=0, news0
# repeat Dtm and Frequency Table steps 

set.seed(2334)
wordcloud(words = freq_words$Word, freq = freq_words$Frequency, min.freq = 500,
          max.words=1500, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(5, "Dark2"))




