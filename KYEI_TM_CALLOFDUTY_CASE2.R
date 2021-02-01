# Library
library(tm)
library(lexicon)
library(tidytext)
library(dplyr)
library(reshape2)
library(radarchart)

# Bring in our supporting functions and WD
source('~/Desktop/R STUFF/hult_NLP_student/lessons/Z_otherScripts/ZZZ_supportingFunctions.R')
setwd("~/Desktop/R STUFF/hult_NLP_student/cases/Call of Duty E-Sport/teamFollowerTimelines")

# DataSets
ATL <-"student_2020-12-28_ATLFaZe2_followers_timelines.fst"
LON<- "student_2020-12-28_RoyalRavens2_followers_timelines.fst"
LA<-  "student_2020-12-28_LAGuerrillas2_followers_timelines.fst"


# Create custom stop words
stops <- c(stopwords('english'))

#Creating and Cleaning document term matrix
ATL1 <- cleanMatrix(pth             = ATL,
                         columnName      = 'text',
                         collapse        = T, 
                         customStopwords = stops,
                         type = 'DTM', # TDM or DTM
                         wgt = 'weightTf') # weightTfIdf or weightTf
LON1 <- cleanMatrix(pth             = LON,
                    columnName      = 'text',
                    collapse        = T, 
                    customStopwords = stops,
                    type = 'DTM', # TDM or DTM
                    wgt = 'weightTf') # weightTfIdf or weightTf
LA1 <- cleanMatrix(pth             = LA,
                    columnName      = 'text',
                    collapse        = T, 
                    customStopwords = stops,
                    type = 'DTM', # TDM or DTM
                    wgt = 'weightTf') # weightTfIdf or weightTf


#Atlanta EmojiChart


#dim(ATL1)

tmp      <- as.DocumentTermMatrix(ATL1, weighting = weightTf ) 
tidyATL <- tidy(tmp)
tidyATL
dim(tidyATL)


# NRC 
nrc <- nrc_emotions
terms <- subset(nrc, rowSums(nrc[,2:9])!=0)
sent  <- apply(terms[,2:ncol(terms)], 1, function(x)which(x>0))
head(sent)

# Reshape
nrcLex <- list()
for(i in 1:length(sent)){
  x <- sent[[i]]
  x <- data.frame(term      = terms[i,1],
                  sentiment = names(sent[[i]]))
  nrcLex[[i]] <- x
}
nrcLex <- do.call(rbind, nrcLex)
head(nrcLex)

# Perform Inner Join
nrcSent <- inner_join(tidyATL,nrcLex, by=c('term' = 'term'))
nrcSent=nrcSent[1:300,]

# Quick Analysis
emos <- aggregate(count ~ sentiment + document, nrcSent, sum)
emos$document <- NULL
chartJSRadar(scores = emos, labelSize = 10, showLegend = F)



#LON Emojichart
dim(LON1)

tmp1      <- as.DocumentTermMatrix(LON1, weighting = weightTf ) 
tidyLON <- tidy(tmp1)
tidyLON
dim(tidyLON)


#  NRC
nrc1 <- nrc_emotions
terms1 <- subset(nrc1, rowSums(nrc1[,2:9])!=0)
sent1  <- apply(terms1[,2:ncol(terms1)], 1, function(x)which(x>0))
head(sent1)

# Reshape
nrcLex1 <- list()
for(i in 1:length(sent1)){
  x <- sent1[[i]]
  x <- data.frame(term1      = terms[i,1],
                  sentiment = names(sent1[[i]]))
  nrcLex1[[i]] <- x
}
nrcLex1 <- do.call(rbind, nrcLex1)
head(nrcLex1)

# Perform Inner Join
nrcSent1 <- inner_join(tidyLON,nrcLex1, by=c('term' = 'term'))
nrcSent1
nrcSent1=nrcSent1[1:300,]

# Quick Analysis
emos1 <- aggregate(count ~ sentiment + document, nrcSent1, sum)
emos1$document <- NULL
chartJSRadar(scores = emos1, labelSize = 10, showLegend = F)



#LA EmojiChart
dim(LA1)

tmp2      <- as.DocumentTermMatrix(LA1, weighting = weightTf ) 
tidyLA <- tidy(tmp2)
tidyLA
dim(tidyLA)

#  NRC
nrc2 <- nrc_emotions
terms2 <- subset(nrc2, rowSums(nrc2[,2:9])!=0)
sent2 <- apply(terms2[,2:ncol(terms2)], 1, function(x)which(x>0))
head(sent2)

# Reshape
nrcLex2 <- list()
for(i in 1:length(sent2)){
  x <- sent2[[i]]
  x <- data.frame(term2      = terms[i,1],
                  sentiment = names(sent2[[i]]))
  nrcLex2[[i]] <- x
}
nrcLex2 <- do.call(rbind, nrcLex2)
head(nrcLex2)

# Perform Inner Join
nrcSent2 <- inner_join(tidyLA,nrcLex2, by=c('term' = 'term'))
nrcSent2
nrcSent2=nrcSent2[1:300,]

# Quick Analysis
emos2 <- aggregate(count ~ sentiment + document, nrcSent2, sum)
emos2$document <- NULL
chartJSRadar(scores = emos2, labelSize = 10, showLegend = F)

