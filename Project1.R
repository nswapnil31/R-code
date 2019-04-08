# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("dplyr")
install.packages("wordcloud2")
install.packages("rJava", type = "source")  
install.packages("RWeka", type = "source")
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("dplyr")
library("wordcloud2")
library("topicmodels")
library("tidytext")
library("janeaustenr")
library("ggplot2")
library("igraph")
library("ggraph")
library("RWeka")


getwd()
setwd("D:\\Winter 2019\\IST 650\\Project")
data <- read.csv("alldata.csv", header = TRUE)


Corpus<-Corpus(VectorSource(data$description))
inspect(Corpus[1:4])
print(lapply(Corpus[1:2], as.character))
Corpus <- tm_map(Corpus,tolower)
Corpus <- tm_map(Corpus, removePunctuation)
Corpus <- tm_map(Corpus,removeNumbers)
cleanset <- tm_map(Corpus,removeWords,c(stopwords("english"),"data","experiance","working","equal","employer","related","field","best","practices","qualified","applicant","action","employer","preferred","qualifications","high","quality","large","sets","every","day","receive","consideration","around","world","genetic","information",
                                        "track","record","regard","must","able","san","francisco","successful","candidate","attention","detail","experience","united","states","using","age","disability","relevant","applicants","join","us","reasonable","accommodation","every","day",
                                        "years","sexual","orientation","race","gender","identity","marital","status","veteran","without","regard","internal","external","religion","job","description","national","origin","sex","color","opportunity","experiance","will","work","time","team","skills","new"))


cleanset <- tm_map(cleanset,stripWhitespace)
#cleanset <- tm_map(cleanset, PlainTextDocument)

#cleanset <- tm_map(cleanset, stemDocument,language = "english")

minfreq_bigram<-9000

token_delim <- " \\t\\r\\n.!?,;\"()"
bitoken <- NGramTokenizer(cleanset, Weka_control(min=2,max=2, delimiters = token_delim))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq,decreasing=TRUE),]
wordcloud(sort_two$bitoken,sort_two$Freq,random.order=FALSE,scale = c(5,0.40),min.freq = minfreq_bigram,colors = brewer.pal(8,"Dark2"),max.words=70)



dtm <- TermDocumentMatrix(cleanset)

# dtm_DTM <- DocumentTermMatrix(cleanset)
# dtm_tfxidf <-weightTfIdf(dtm_DTM)
# m<- as.matrix(dtm_tfxidf)
# rownames(m) <- 1-nrow(m)
# norm_euc1 <-function(m)
#   m/apply(m,1,function(x) sum(x^2)^0.5)
# m_norm <- norm_euc1(m)
# results <- kmeans(m_norm, 12, 30)
# 
# clusters <-1:12
# for (i in clusters) {
#   cat("Clusters", i, ":" ,findFreqTerms(dtm_tfxidf[results$cluster==i],2),"\n\n")
# }
#   

dtm <- as.matrix(dtm)
dtm[1:10,1:5]
#  Start by removing sparse terms:   
# dtm <- removeSparseTerms(dtm,sparse = 0.95) # This makes a matrix that is 20% empty space, maximum.   
# dtm
termFrequency <- rowSums(dtm)
termFrequency <- subset(termFrequency, termFrequency>=7000)
termFrequency <- sort(termFrequency,decreasing = TRUE)
termFrequency
library(ggplot2)

barplot(termFrequency,las=2,col=rainbow(10))
  


dtm <- sort(rowSums(dtm),decreasing = TRUE)
set.seed(222)
wordcloud(words = names(dtm),
          freq = dtm,
          max.words = 100,
          random.order = F,
          colors = brewer.pal(6,'Dark2'),
          scale = c(5,0.7),
          rot.per = 0.4)


#k=5
#SEED = 1234
#description.lda <- LDA(dtm,k,control=list(seed=SEED))
#lda.topics <- as.matrix(topics(description.lda))
#lda.topics
#lda.terms <- terms(description.lda,7)
#lda.terms <- apply(lda.terms,MARGIN = 2,paste,collapse = ",") %>% print
#lda.terms


tableJE <- data.frame(words = names(dtm),
                      absolute.freq = dtm,
                      relative.freq = dtm/length(dtm))

rownames(tableJE) <- NULL

head(tableJE,20)

#p <- ggplot(subset(tableJE, termFrequency>50), aes(x = reorder(words, -termFrequency), y = termFrequency)) +
 # geom_bar(stat = "identity") + 
  #theme(axis.text.x=element_text(angle=45, hjust=1))
#p 

Corpus1<-Corpus(VectorSource(data$company))
View(Corpus1)

Corpus1 <- tm_map(Corpus1,tolower)
#Corpus1 <- tm_map(Corpus1, removePunctuation)
Corpus1 <- tm_map(Corpus1,removeNumbers)
cleanset1 <- tm_map(Corpus1,removeWords,stopwords("english"))
cleanset1 <- tm_map(cleanset1,stripWhitespace)

#cleanset1 <- tm_map(cleanset1, stemDocument)
#cleanset <- tm_map(cleanset,PlainTextDocument)

#minfreq_bigram<-30

#token_delim <- " \\t\\r\\n.!?,;\"() "
#bitoken <- NGramTokenizer(cleanset1, Weka_control(min=2,max=2, delimiters = token_delim))
#two_word <- data.frame(table(bitoken))
#sort_two <- two_word[order(two_word$Freq,decreasing=TRUE),]
#wordcloud(sort_two$bitoken,sort_two$Freq,random.order=FALSE,scale = c(2,0.35),min.freq = minfreq_bigram,colors = brewer.pal(8,"Dark2"),max.words=100)





dtm1 <- TermDocumentMatrix(cleanset1)
head(dtm1)
dtm1 <- as.matrix(dtm1)
dtm1
dtm1[1:10,1:5]
#  Start by removing sparse terms:   
# dtm1 <- removeSparseTerms(dtm1, 0.2) # This makes a matrix that is 20% empty space, maximum.   
# dtm1

termFrequency1 <- rowSums(dtm1)
termFrequency1 <- subset(termFrequency1, termFrequency1>=50)
termFrequency1 <- sort(termFrequency1,decreasing = TRUE)
termFrequency1
library(ggplot2)
barplot(termFrequency1,las=2,col=rainbow(20))

dtm1 <- sort(rowSums(dtm1),decreasing = TRUE)
set.seed(222)
wordcloud(words = names(dtm1),
          freq = dtm1,
          max.words = 100,
          random.order = F,
          colors = brewer.pal(8,'Dark2'),
          background_color='White',          ,
          scale = c(5,0.7),
          rot.per = 0.4)

Corpus2<-Corpus(VectorSource(data$position))

Corpus2 <- tm_map(Corpus2,tolower)
Corpus2 <- tm_map(Corpus2, removePunctuation)
Corpus2 <- tm_map(Corpus2,removeNumbers)
cleanset2 <- tm_map(Corpus2,removeWords,c(stopwords("english"),"machine","learning","data science","scientist data"))
cleanset2 <- tm_map(cleanset2,stripWhitespace)

#cleanset2 <- tm_map(cleanset2, stemDocument)
#cleanset <- tm_map(cleanset,PlainTextDocument)

minfreq_bigram<-60

token_delim <- "  \\t\\r\\n.!?,;\"()"
bitoken <- NGramTokenizer(cleanset2, Weka_control(min=2,max=2, delimiters = token_delim))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq,decreasing=TRUE),]
wordcloud(sort_two$bitoken,sort_two$Freq,random.order=FALSE,scale = c(6,0.35),min.freq = minfreq_bigram,colors = brewer.pal(8,"Dark2"),max.words=200)




 dtm2 <- TermDocumentMatrix(cleanset2)
dtm2 <- as.matrix(dtm2)
dtm2[1:10,1:5]
#  Start by removing sparse terms:   
# dtm2 <- removeSparseTerms(dtm2, 0.2) # This makes a matrix that is 20% empty space, maximum.   
# dtm2
termFrequency2 <- rowSums(dtm2)
termFrequency2 <- subset(termFrequency2, termFrequency2>=90)
termFrequency2 <- sort(termFrequency2,decreasing = TRUE)
library(ggplot2)
barplot(termFrequency2,las=2,col=rainbow(20))

# dtm2 <- sort(rowSums(dtm2),decreasing = TRUE)
# set.seed(222)
# wordcloud(words = names(dtm2),
#           freq = dtm2,
#           max.words = 200,
#           random.order = F,
#           colors = brewer.pal(8,'Dark2'),
#           scale = c(5,0.7),
#           rot.per = 0.4)

Corpus3<-Corpus(VectorSource(data$location))

Corpus3 <- tm_map(Corpus3,tolower)

Corpus3 <- tm_map(Corpus3, removePunctuation)
Corpus3 <- tm_map(Corpus3,removeNumbers)
cleanset3 <- tm_map(Corpus3,removeWords,stopwords("english"))
for (j in seq(cleanset3))
{
  cleanset3[[j]] <- gsub("cedar park", "cedar-park", cleanset3[[j]])
 cleanset3[[j]] <- gsub("round rock", "round-rock", cleanset3[[j]])
 cleanset3[[j]] <- gsub("los angeles", "los-angeles", cleanset3[[j]])
 cleanset3[[j]] <- gsub("mountain view", "mountain-view", cleanset3[[j]])
 cleanset3[[j]] <- gsub("new york", "new-york", cleanset3[[j]])
  cleanset3[[j]] <- gsub("jersey city", "jersey-city", cleanset3[[j]])
  cleanset3[[j]] <- gsub("murray hill", "murray-hill", cleanset3[[j]])
  cleanset3[[j]] <- gsub("south plainfield", "south-plainfield", cleanset3[[j]])
  cleanset3[[j]] <- gsub("troy hills", "troy-hills", cleanset3[[j]])
  cleanset3[[j]] <- gsub("fort lee", "fort-lee", cleanset3[[j]])
  cleanset3[[j]] <- gsub("east hanover", "east-hanover", cleanset3[[j]])
  cleanset3[[j]] <- gsub("long beach", "long-beach", cleanset3[[j]])
  
  cleanset3[[j]] <- gsub("san diego", "san-diego", cleanset3[[j]])
  cleanset3[[j]] <- gsub("new brunswick", "new-brunswick", cleanset3[[j]])
  cleanset3[[j]] <- gsub("san francisco", "san-francisco", cleanset3[[j]])
  cleanset3[[j]] <- gsub("mountain view", "mountain-view", cleanset3[[j]])
  cleanset3[[j]] <- gsub("redwood city", "redwood-city", cleanset3[[j]])
  cleanset3[[j]] <- gsub("san mateo", "san-mateo", cleanset3[[j]])
  cleanset3[[j]] <- gsub("menlo park", "menlo-park", cleanset3[[j]])
  cleanset3[[j]] <- gsub("foster city", "foster-city", cleanset3[[j]])
  cleanset3[[j]] <- gsub("union city", "union-city", cleanset3[[j]])
  cleanset3[[j]] <- gsub("san carlos", "san-carlos", cleanset3[[j]])
  cleanset3[[j]] <- gsub("san rafael", "san-rafael", cleanset3[[j]])
  cleanset3[[j]] <- gsub("pleasant hill", "pleasant-hill", cleanset3[[j]])
  
  cleanset3[[j]] <- gsub("san bruno", "san-bruno", cleanset3[[j]])
  cleanset3[[j]] <- gsub("san ramon", "san-ramon", cleanset3[[j]])
  cleanset3[[j]] <- gsub("little falls", "little-falls", cleanset3[[j]])
  cleanset3[[j]] <- gsub("florham park", "florham park", cleanset3[[j]])
  cleanset3[[j]] <- gsub("east hanover", "east-hanover", cleanset3[[j]])
  cleanset3[[j]] <- gsub("new hyde park", "new-hyde-park", cleanset3[[j]])
  cleanset3[[j]] <- gsub("daly city", "daly-city", cleanset3[[j]])
  cleanset3[[j]] <- gsub("mill valley", "mill-valley", cleanset3[[j]])
  cleanset3[[j]] <- gsub("walnut creek", "walnut-creek", cleanset3[[j]])
  
 }
cleanset3 <- tm_map(cleanset3,stripWhitespace)

#cleanset3 <- tm_map(cleanset3, stemDocument)
#cleanset <- tm_map(cleanset,PlainTextDocument)



dtm3 <- TermDocumentMatrix(cleanset3)
dtm3 <- as.matrix(dtm3)
dtm3[1:10,1:5]
#  Start by removing sparse terms:   
dtm3 <- removeSparseTerms(dtm3, 0.2) # This makes a matrix that is 20% empty space, maximum.   
dtm3
termFrequency3 <- rowSums(dtm3)
termFrequency3 <- subset(termFrequency3, termFrequency3>=20)
termFrequency3 <- sort(termFrequency3,decreasing = TRUE)
library(ggplot2)
barplot(termFrequency3,las=2,col=rainbow(20))

dtm3 <- sort(rowSums(dtm3),decreasing = TRUE)
set.seed(222)
wordcloud(words = names(dtm3),
          freq = dtm3,
          max.words = 500,
          random.order = F,
          colors = brewer.pal(8,'Dark2'),
          scale = c(5,0.7),
          rot.per = 0.4)


# library(fpc)   
# d <- dist(t(dtm), method="euclidian")   
# kfit <- kmeans(d, 2)   
# clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   

