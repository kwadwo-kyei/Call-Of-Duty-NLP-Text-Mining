#Get Working Directory
setwd("~/Desktop/R STUFF/hult_NLP_student/cases/Call of Duty E-Sport/teamTimeline")

#Library
library(tm)
library(qdap)
library(lexicon)
library(dplyr)
library(fst)
library(pbapply)
library(mgsub)
library(tidytext)
library(reshape2)
library(wordcloud)
library(viridisLite)

#Get Source

source("~/Desktop/R STUFF/hult_NLP_student/lessons/Z_otherScripts/ZZZ_supportingFunctions.R")
emoji <- read.csv('emojis.csv')
stops <- c(stopwords('SMART'),'atlanta','duty','faze','london','royalravens','ravens','game','videogame','video','warzone','cod','codleague','cdl','team','la','LAGuerrillas','laguerrillas','arrow<ef><b8><8f>') 
#Data



#Gsub texts
Team <- read_fst('student_TeamTimelines.fst')
'gsub("[^\x01-\x7F]","", Team$text)'
'gsub("\360\237\216\205","",Team$text)'
Team$text<-gsub("<fO><9f><94><a5>","",Team$text)
Team$text<-gsub("arrow<ef><b8><8f>","",Team$text)
Team$text<-gsub("<fO><9f><91><80>","",Team$text)

# substitute emojis in tweets
Team$text <- pbsapply(as.character(Team$text), mgsub, emoji$emoji, emoji$name)

# Naming Data
Atl<-subset(Team,screen_name=="ATLFaZe")
LON<-subset(Team,screen_name=="RoyalRavens")
LA<-subset(Team,screen_name=="LAGuerrillas")

#----------------------------------------------ATLANTA--------------------------
# Clean Atl Dataset
AtlClean <- VCorpus(VectorSource(Atl$text))
AtlClean <- cleanCorpus(AtlClean, stops)
content(AtlClean[[2]])
AtlMatrix  <- TermDocumentMatrix(AtlClean)
AtlMatrix <- as.matrix(AtlMatrix)

#------------------------------------------WordCloud for AtL---------------------
# Bigram token maker
bigramTokens1 <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}

# Row Sums & organize
AtlTDM <- sort(rowSums(AtlMatrix), decreasing = TRUE)
AtlDF1   <- data.frame(word = names(AtlTDM), freq = AtlTDM)

# Regular dynamic WC
pal <- brewer.pal(8, "Dark2")
wordcloud2(AtlDF1[1:50,], 
           color = pal, 
           backgroundColor = "lightgrey")


#-----------------------Polarity------------------------------------------------
# Extract the clean and subbed text to use polarity 
cleanAtl <- data.frame(document = seq_along(Atl$text), #simple id order
                         postID = Atl$text, # keep track of posts
                         text = unlist(sapply(AtlClean, `[`, "content")),stringsAsFactors=F)

polAtl <- polarity(cleanAtl$text)
polAtl$group

# Append to the clean data
cleanAtl$polarityValue <- polAtl$all$polarity

# Some documents returns NA from polarity, could be only stop words, screenshots etc, chg to 0 
cleanAtl$polarityValue[is.na(cleanAtl$polarityValue)] <- 0

# Classify the polarity scores
cleanAtl$polarityClass <- ifelse(cleanAtl$polarityValue>0, 'positive',
                                   ifelse(cleanAtl$polarityValue<0, 'negative', 'neutral'))

# Now let's assign an emotion 
AtlDTM   <- DocumentTermMatrix(VCorpus(VectorSource(cleanAtl$text)))
tidyAtl <- tidy(AtlDTM)
tidyAtl
dim(tidyAtl)

# Get bing lexicon- Sentiment
bing <- get_sentiments(lexicon = c("bing"))
head(bing)

# Perform Inner Join for Sentiments
bingSent <- inner_join(tidyAtl, bing, by=c('term' = 'word'))
bingSent

# Quick Analysis of Sentiments
table(bingSent$sentiment, bingSent$count)
aggregate(count~sentiment,bingSent, sum)

# Get afinn lexicon-Senriment Values
afinn<-get_sentiments(lexicon = c("afinn"))
head(afinn)

# Perform Inner Join for Sentiment Values
afinnSent <- inner_join(tidyAtl,afinn, by=c('term' = 'word'))
afinnSent

#----------------------------------Timeline of Identified Sentiment Words------
text <- Atl$text
text<-text[1:100]
textword <- data.frame(word = unlist(strsplit(text,' ')))
textword$word <- tolower(textword$word )
textword <- left_join(textword,afinn, by=c('word' = 'word'))
textword[is.na(textword$value),2] <- 0
plot(textword$value, type="l", main="Quick Timeline of Identified Words") 


#NRC Emotion Lexicon- eight basic emotions and two sentiments(negative & positive)
nrc     <- get_sentiments(lexicon = c("nrc"))
nrcAtl <- inner_join(tidyAtl,nrc, by=c('term' = 'word'))
nrcAtl


# Now group by document and select the most numerous 
grpAtl <- nrcAtl %>% group_by(document, sentiment) %>% summarise(n = sum(count))
grpAtl$document <- as.numeric(as.character(grpAtl$document))
grpAtl

# Cast to wide format
wideAtl <- dcast(grpAtl, document~sentiment,fun.aggregate = sum,value.var = "n")
head(wideAtl) #rowsum of 1 should be 6 based on grpSent
wideAtl[grep('\\b100\\b',wideAtl$document),] #"negative" should be 5 based on grpSent
wideAtl[1:50,]

# Drop positive/negative & get maximum column, but need to use  if else in case some docs were only pos/neg
wideAtl <- wideAtl[,-c(7,8)]
wideAtl$maxEmotion <- ifelse(rowSums(wideAtl[,2:ncol(wideAtl)])>0,
                              names(wideAtl)[2:ncol(wideAtl)][max.col(wideAtl[,2:ncol(wideAtl)])],
                              'noEmotion')
head(wideAtl)

# Some posts are neutral so you cant cbind, instead left_join
cleanAtl <- left_join(cleanAtl, wideAtl, by = c('document'='document'))
cleanAtl$maxEmotion[is.na(cleanAtl$maxEmotion)] <- 'noEmotion' #NA introduced from join on docs that had no emotion
cleanAtl[1:10,]

# Finally, a clean text, with ID, moderators, polarity, and emotional sentiment
head(cleanAtl)

#---------------------------Plot Wordcloud frequency-----------------------------
plot(freq_terms(cleanAtl$text, top=45, at.least=2, stopwords = stops))

#-------------------------- Polarity Comparison cloud----------------------------
polarityLst <- list()
for(i in 1:length(unique(cleanAtl$polarityClass))){
  x <- subset(cleanAtl$text, cleanAtl$polarityClass == unique(cleanAtl$polarityClass)[i])
  x <- paste(x, collapse = ' ')
  polarityLst[[unique(cleanAtl$polarityClass)[i]]] <- x
}

# Using the list
allPolarityClasses <- do.call(rbind, polarityLst)
allPolarityClasses <- VCorpus(VectorSource(allPolarityClasses))
allPolarityClasses <- TermDocumentMatrix(cleanCorpus(allPolarityClasses, stops))
allPolarityClasses <- as.matrix(allPolarityClasses)

colnames(allPolarityClasses) <- names(polarityLst)

allPolarityClasses=allPolarityClasses[,-c(4)]

# Make comparison cloud
comparison.cloud(allPolarityClasses, 
                 max.words=20, 
                 random.order=FALSE,
                 title.size=1,
                 colors=brewer.pal(ncol(allPolarityClasses),"Dark2"),
                 scale=c(4,0,0))
dev.off()

#-------------------------- Emotional Comparison Cloud-----------------------------------
emotionLst <- list()
for(i in 1:length(unique(cleanAtl$maxEmotion))){
  x <- subset(cleanAtl$text, cleanAtl$maxEmotion == unique(cleanAtl$maxEmotion)[i])
  x <- paste(x, collapse = ' ')
  emotionLst[[unique(cleanAtl$maxEmotion)[i]]] <- x
}

# Using the list
allEmotionClasses <- do.call(rbind, emotionLst)
allEmotionClasses <- VCorpus(VectorSource(allEmotionClasses))
allEmotionClasses <- TermDocumentMatrix(allEmotionClasses)
allEmotionClasses <- as.matrix(allEmotionClasses)


colnames(allEmotionClasses) <- names(emotionLst)

# Emotion comparison cloud
comparison.cloud(allEmotionClasses, 
                 max.words=100, 
                 random.order=FALSE,
                 title.size=1,
                 colors=viridis(10),
                 scale=c(5.5,0.1))






#----------------------------------Los Angeles----------------------------------
# Clean LA Dataset
LAClean <- VCorpus(VectorSource(LA$text))
LAClean <- cleanCorpus(LAClean, stops)
content(LAClean[[2]])
LAMatrix  <- TermDocumentMatrix(LAClean)
LAMatrix <- as.matrix(LAMatrix)


#---------------------------------WordCloud for LA------------------------------
# Bigram token maker
bigramTokens1 <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}

# Row Sums & organize
LATDM <- sort(rowSums(LAMatrix), decreasing = TRUE)
LADF1   <- data.frame(word = names(LATDM), freq = LATDM)

# LA Word Cloud
pal <- brewer.pal(8, "Dark2")
wordcloud2(LADF1[1:50,], 
           color = pal, 
           backgroundColor = "lightyellow")

#-----------------------Polarity for LA------------------------------------------------

# Extract the clean and subbed text to use polarity 
cleanLA <- data.frame(document = seq_along(LA$text), #simple id order
                       postID = LA$text, # keep track of posts
                       text = unlist(sapply(LAClean, `[`, "content")),stringsAsFactors=F)

polALA <- polarity(cleanLA$text)
polALA$group

# Append to the clean data
cleanLA$polarityValue <- polALA$all$polarity

# Some documents returns NA from polarity, could be only stop words, screenshots etc, chg to 0 
cleanLA$polarityValue[is.na(cleanLA$polarityValue)] <- 0

# Classify the polarity scores
cleanLA$polarityClass <- ifelse(cleanLA$polarityValue>0, 'positive',
                                 ifelse(cleanLA$polarityValue<0, 'negative', 'neutral'))

#----------------------Now Assign an emotion------------------------------------
LADTM   <- DocumentTermMatrix(VCorpus(VectorSource(cleanLA$text)))
tidyLA <- tidy(LADTM)
tidyLA
dim(tidyLA)

# Get bing lexicon- Sentiment
bing1 <- get_sentiments(lexicon = c("bing"))
head(bing1)

# Perform Inner Join for Sentiments
bingSent1 <- inner_join(tidyLA, bing1, by=c('term' = 'word'))
bingSent1

# Quick Analysis of Sentiments
table(bingSent1$sentiment, bingSent1$count)
aggregate(count~sentiment,bingSent1, sum)


# Get afinn lexicon-Sentiment Values
afinn1<-get_sentiments(lexicon = c("afinn"))
head(afinn1)

# Perform Inner Join for Sentiment Values
afinnSent1 <- inner_join(tidyLA,afinn1, by=c('term' = 'word'))
afinnSent1

#------------------------------Timeline of Identified Sentiment Words------------
text1 <- LA$text
text1<-text1[1:20]
textword1 <- data.frame(word = unlist(strsplit(text1,' ')))
textword1$word <- tolower(textword1$word )
textword1 <- left_join(textword1,afinn1, by=c('word' = 'word'))
textword1[is.na(textword1$value),2] <- 0
plot(textword1$value, type="l", main="Quick Timeline of Identified Words") 



#NRC Emotion Lexicon- eight basic emotions and two sentiments(negative & positive)
nrc1     <- get_sentiments(lexicon = c("nrc"))
nrcLA <- inner_join(tidyLA,nrc1, by=c('term' = 'word'))
nrcLA

# Group by document and select the most numerous 
grpLA <- nrcLA %>% group_by(document, sentiment) %>% summarise(n = sum(count))
grpLA$document <- as.numeric(as.character(grpLA$document))
grpLA

# Cast to wide format
wideLA <- dcast(grpLA, document~sentiment,fun.aggregate = sum,value.var = "n")
head(wideLA) #rowsum of 1 should be 6 based on grpSent
wideLA[grep('\\b100\\b',wideLA$document),] #"negative" should be 5 based on grpSent
wideLA[1:50,]

# Drop positive/negative & get maximum column
wideLA <- wideLA[,-c(7,8)]
wideLA$maxEmotion <- ifelse(rowSums(wideLA[,2:ncol(wideLA)])>0,
                             names(wideLA)[2:ncol(wideLA)][max.col(wideLA[,2:ncol(wideLA)])],
                             'noEmotion')
head(wideLA)

cleanLA <- left_join(cleanLA, wideLA, by = c('document'='document'))
cleanLA$maxEmotion[is.na(cleanLA$maxEmotion)] <- 'noEmotion' #NA introduced from join on docs that had no emotion
cleanLA[1:10,]


head(cleanLA)

#---------------------------Plot Wordcloud frequency---------------------------
plot(freq_terms(cleanLA$text, top=45, at.least=2, stopwords = stops))

#--------------------------Polarity Comparison cloud--------------------------- 
polarityLst1 <- list()
for(i in 1:length(unique(cleanLA$polarityClass))){
  x <- subset(cleanLA$text, cleanLA$polarityClass == unique(cleanLA$polarityClass)[i])
  x <- paste(x, collapse = ' ')
  polarityLst1[[unique(cleanLA$polarityClass)[i]]] <- x
}



allPolarityClasses1 <- do.call(rbind, polarityLst1)
allPolarityClasses1 <- VCorpus(VectorSource(allPolarityClasses1))
allPolarityClasses1 <- TermDocumentMatrix(cleanCorpus(allPolarityClasses1, stops))
allPolarityClasses1 <- as.matrix(allPolarityClasses1)

colnames(allPolarityClasses1) <- names(polarityLst1)

allPolarityClasses1=allPolarityClasses1[,-c(4)]

# Make Polarity comparison cloud
comparison.cloud(allPolarityClasses1, 
                 max.words=100, 
                 random.order=FALSE,
                 title.size=1,
                 colors=brewer.pal(ncol(allPolarityClasses1),"Dark2"),
                 scale=c(4,0,0))
dev.off()

# -------------------------------Emotional Comparison Cloud---------------------
emotionLst1 <- list()
for(i in 1:length(unique(cleanLA$maxEmotion))){
  x <- subset(cleanLA$text, cleanLA$maxEmotion == unique(cleanLA$maxEmotion)[i])
  x <- paste(x, collapse = ' ')
  emotionLst1[[unique(cleanLA$maxEmotion)[i]]] <- x
}

# Using the list
allEmotionClasses1 <- do.call(rbind, emotionLst1)
allEmotionClasses1 <- VCorpus(VectorSource(allEmotionClasses1))
allEmotionClasses1 <- TermDocumentMatrix(allEmotionClasses1)
allEmotionClasses1 <- as.matrix(allEmotionClasses1)

# Make sure order is the same as the c(objA, objB) on line ~80
colnames(allEmotionClasses1) <- names(emotionLst1)

# Make all emotion comparison cloud
comparison.cloud(allEmotionClasses1, 
                 max.words=100, 
                 random.order=FALSE,
                 title.size=1,
                 colors=viridis(10),
                 scale=c(3,0.1))




#------------------------------- LONDON---------------------------------------
#-------------------------------Clean LONDON DATASET-------------------------
LONClean <- VCorpus(VectorSource(LON$text))
LONClean <- cleanCorpus(LONClean, stops)
content(LONClean[[2]])
LONMatrix  <- TermDocumentMatrix(LONClean)
LONMatrix <- as.matrix(LONMatrix)

#---------------------------WordCloud for LON----------------------------------
# Bigram token maker
bigramTokens1 <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}

# London Row Sums & Dataframe
LONTDM <- sort(rowSums(LONMatrix), decreasing = TRUE)
LONDF1   <- data.frame(word = names(LONTDM), freq = LONTDM)

# London Word Cloud
pal <- brewer.pal(8, "Accent")
wordcloud2(LONDF1[1:30,], 
           color = pal, 
           backgroundColor = "lightblue")

-----------------------#Polarity for LON------------------------------------------------

# Extract the clean and subbed text to use polarity 
cleanLON <- data.frame(document = seq_along(LON$text), #simple id order
                      postID = LON$text, # keep track of posts
                      text = unlist(sapply(LONClean, `[`, "content")),stringsAsFactors=F)

polALON <- polarity(cleanLON$text)
polALON$group

# Append to the clean data
cleanLON$polarityValue <- polALON$all$polarity


# Some documents returns NA from polarity, could be only stop words, screenshots etc, chg to 0 
cleanLON$polarityValue[is.na(cleanLON$polarityValue)] <- 0

# Classify the polarity scores
cleanLON$polarityClass <- ifelse(cleanLON$polarityValue>0, 'positive',
                                ifelse(cleanLON$polarityValue<0, 'negative', 'neutral'))

# Now assign an emotion 
LONDTM   <- DocumentTermMatrix(VCorpus(VectorSource(cleanLON$text)))
tidyLON <- tidy(LONDTM)
tidyLON
dim(tidyLON)

# Get bing lexicon- Sentiments
bing2 <- get_sentiments(lexicon = c("bing"))
head(bing2)

# Perform Inner Join for Sentiments
bingSent2 <- inner_join(tidyLON, bing2, by=c('term' = 'word'))
bingSent2

# Quick Analysis of Srntiments
table(bingSent2$sentiment, bingSent2$count)
aggregate(count~sentiment,bingSent2, sum)

# Get afinn lexicon -Sentiment Values
afinn2<-get_sentiments(lexicon = c("afinn"))
head(afinn2)

# Perform Inner Join for sentiment Values
afinnSent2 <- inner_join(tidyLON,afinn2, by=c('term' = 'word'))
afinnSent2

#------------------------------Timeline of Identified Sentiment Words------------

text2 <- LON$text
text2<-text2[1:100]
textword2 <- data.frame(word = unlist(strsplit(text2,' ')))
textword2$word <- tolower(textword2$word )
textword2 <- left_join(textword2,afinn2, by=c('word' = 'word'))
textword2[is.na(textword2$value),2] <- 0
plot(textword2$value, type="l", main="London Royal") 

#NRC Emotion Lexicon- eight basic emotions and two sentiments(negative & positive)
nrc2     <- get_sentiments(lexicon = c("nrc"))
nrcLON <- inner_join(tidyLON,nrc1, by=c('term' = 'word'))
nrcLON

# Now group by document and select the most numerous 
grpLON <- nrcLON %>% group_by(document, sentiment) %>% summarise(n = sum(count))
grpLON$document <- as.numeric(as.character(grpLON$document))
grpLON

# Cast to wide format
wideLON <- dcast(grpLON, document~sentiment,fun.aggregate = sum,value.var = "n")
head(wideLON) #rowsum of 1 should be 6 based on grpSent
wideLON[grep('\\b100\\b',wideLON$document),] #"negative" should be 5 based on grpSent
wideLON[1:50,]

# Drop positive/negative & get maximum column
wideLON <- wideLON[,-c(7,8)]
wideLON$maxEmotion <- ifelse(rowSums(wideLON[,2:ncol(wideLON)])>0,
                            names(wideLON)[2:ncol(wideLON)][max.col(wideLON[,2:ncol(wideLON)])],
                            'noEmotion')

               
head(wideLON)

cleanLON <- left_join(cleanLON, wideLON, by = c('document'='document'))
cleanLON$maxEmotion[is.na(cleanLON$maxEmotion)] <- 'noEmotion' #NA introduced from join on docs that had no emotion
cleanLON[1:10,]

head(cleanLON)

#------------------------------Plot Wordcloud frequency--------------------
plot(freq_terms(cleanLON$text, top=45, at.least=2, stopwords = stops))

#------------------------------ Polarity comparison cloud----------------------- 
polarityLst2 <- list()
for(i in 1:length(unique(cleanLON$polarityClass))){
  x <- subset(cleanLON$text, cleanLA$polarityClass == unique(cleanLON$polarityClass)[i])
  x <- paste(x, collapse = ' ')
  polarityLst2[[unique(cleanLON$polarityClass)[i]]] <- x
}


allPolarityClasses2 <- do.call(rbind, polarityLst2)
allPolarityClasses2 <- VCorpus(VectorSource(allPolarityClasses2))
allPolarityClasses2 <- TermDocumentMatrix(cleanCorpus(allPolarityClasses2, stops))
allPolarityClasses2 <- as.matrix(allPolarityClasses2)

# Add the names from the list, get the order right!
colnames(allPolarityClasses2) <- names(polarityLst2)

allPolarityClasses2=allPolarityClasses2[,-c(4)]

#Comparison cloud
comparison.cloud(allPolarityClasses2, 
                 max.words=50, 
                 random.order=FALSE,
                 title.size=1,
                 colors=brewer.pal(ncol(allPolarityClasses2),"Set1"),
                 backgroundColor = "blue",
                 scale=c(6,0,0))
dev.off()

# Repeat for the max emotion
emotionLst2 <- list()
for(i in 1:length(unique(cleanLON$maxEmotion))){
  x <- subset(cleanLON$text, cleanLON$maxEmotion == unique(cleanLON$maxEmotion)[i])
  x <- paste(x, collapse = ' ')
  emotionLst2[[unique(cleanLON$maxEmotion)[i]]] <- x
}

# Using the list
allEmotionClasses2 <- do.call(rbind, emotionLst2)
allEmotionClasses2 <- VCorpus(VectorSource(allEmotionClasses2))
allEmotionClasses2 <- TermDocumentMatrix(allEmotionClasses2)
allEmotionClasses2 <- as.matrix(allEmotionClasses2)

colnames(allEmotionClasses2) <- names(emotionLst2)

# emotion comparison cloud
comparison.cloud(allEmotionClasses2, 
                 max.words=150, 
                 random.order=FALSE,
                 title.size=1,
                 colors=viridis(20),
                 scale=c(3,0.1))






