


#########   PACKAGES REQUIRED 

needed <- c("tm","qdap","dapDictionaries","dplyr","RColorBrewer","ggplot2","scales","Rgraphvics","NLP","wordcloud","SnowballC","magrittr","openNLP","RWeka","fpc","RGraphics")
install.packages(needed,dependencies = TRUE) # INSTALLING PACKAGES

########### Important Package ################
library(tm) # Framework for text mining.
library(qdap) # Quantitative discourse analysis of transcripts.
library(qdapDictionaries) # About dictionaries of words.
library(dplyr) # Data wrangling, pipe operator %>%().
library(RColorBrewer) # Generate palette of colours for plots.
library(ggplot2) # Plot word frequencies.
library(scales) # Include commas in numbers.
library(RGraphics) # Correlation plots.
library(NLP)
library(SnowballC)
library(wordcloud)
library(magrittr)
library(openNLP)
library(RWeka)
library(cluster)
library(fpc)

library(e1071)


reuters <- read.csv("reutersCSV.csv", stringsAsFactors = FALSE)

reuters$docs <- paste(reuters$doc.title, reuters$doc.text, sep = " ")

# general text cleaning
reuters$docs <- tolower(reuters$docs)
reuters$docs <- removePunctuation(reuters$docs)
reuters$docs <- removeNumbers(reuters$docs)
reuters$docs <- removeWords(reuters$docs, stopwords("english"))
reuters$docs <- stripWhitespace(reuters$docs)

dtm.all <- DocumentTermMatrix(Corpus(VectorSource(reuters$docs)),
                              control = list(tokenize = scan_tokenizer,
                                             stemming = T))
# Not used bigram_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2))

class(dtm.all)

# replace document number with PID

dtm.all$dimnames$Docs

dtm.all$dimnames$Docs <- reuters$pid



dtm.all <- removeSparseTerms(dtm.all, .95)



lda.dtm <- reuters[(reuters$purpose == "test") & (reuters$group != 1),]
lda.dtm <- lda.dtm$pid

lda.dtmTest <- dtm.all[dtm.all$dimnames$Docs %in% lda.dtm,]

lda.dtmTrain <- reuters[(reuters$purpose == "train") & (reuters$group != 1),]
lda.dtmTrain <- lda.dtmTrain$pid


# lda.dtmTrain <- reuters$pid[reuters$pid %in% c(reuters$pid[reuters$purpose == "train" && reuters$group != 1 ])]
lda.dtmTrain <- dtm.all[dtm.all$dimnames$Docs %in% lda.dtmTrain,]



# apply(reuters[,c(4:138)],2,function(x)sum(x))

reuters$group <- v
reuters$pid[reuters$pid %in% c(reuters$pid[reuters$purpose == "test" && reuters$group != 1])]
labelgroup <- reuters[(reuters$purpose == "test") & (reuters$group != 1),] 
labelgroupv <- labelgroup$group

# TRAIN NAIVE BAYES MODEL USING trainmatrix DATA AND traindate$Journal_group CLASS VECTOR
Trainmat <- as.matrix(lda.dtmTrain)

model <- naiveBayes(as.matrix(lda.dtmTrain),as.factor(traindata$Journal_group))



for(i in 1:133)
{
  sample$group <- ifelse(sample[,]==1 & sample$group == 1000,colnames(sample[5]),ifelse(sample$group != 1000,sample$group[5],1000))
  
}

sample$group <- ifelse(sample[,6]==1 & sample$group == 1000,colnames(sample[6]),1000)



v <- rep(1,21578)


for(i in 1:21578)
  
{
  
  L1 <- reuternew[i,]
  
  for(j in 1:141)
  
  {
    if (L1[j]  == 1)
    {
      v[i] <- colnames(reuternew[j])
      
    }
  }
  
}


