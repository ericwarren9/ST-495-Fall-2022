# Purpose: Text analytics in R


# Load in libraries -------------------------------------------------------

library(tm)
library(SnowballC)
library(XML)
library(wordcloud)


# Start loading in text files ---------------------------------------------

# let's look at a simple text dataset
reut <- system.file("texts", "crude", package = "tm")
crude <- VCorpus(DirSource(reut), readerControl = list(reader  = readReut21578XMLasPlain))
crude[[1]]$content
crude[[1]]$meta 

# Let's look at another text corpus
reut <- system.file("texts", "acq", package = "tm")
acq <- VCorpus(DirSource(reut), readerControl = list(reader = readReut21578XMLasPlain))
acq[[3]]$content
acq[[3]]$meta 



# Make dataframes of the data ---------------------------------------------

# We will use two concepts that we discussed: the bag of words model and document-term matrices

dtm1 <- DocumentTermMatrix(crude, control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE), stopwords = TRUE))
dtm1
data1 <- data.frame(as.matrix(dtm1))


dtm2 <- DocumentTermMatrix(acq, control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE), stopwords = TRUE))
dtm2
data2 <- data.frame(as.matrix(dtm2))


# Do statistical analysis -------------------------------------------------

round(data1[1:5,501:509],3)

# find frequent terms
findFreqTerms(dtm1, 0.2)
findFreqTerms(dtm2, 0.9)

# find associated terms
findAssocs(dtm1, "crude", 0.7)
findAssocs(dtm2, "shares", 0.5)

# remove sparse terms
dtm1.1 <- removeSparseTerms(dtm1, 0.8)
dtm1.1

# construct wordcloud
m <- as.matrix(dtm1)
v <- sort(colSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, 
          freq = d$freq,
          min.freq = 1,
          max.words = 20, 
          random.order = FALSE, 
          rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))


# Latent Semantic Analysis (LSA) ------------------------------------------

library(lsa)
tdm <- TermDocumentMatrix(crude, control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE), wordLengths = c(2, Inf)))
lsaSpace <- lsa(tdm)  # create LSA space
lsak <- lsaSpace$dk #Docs  decomposed to k dimensions
dim(lsak)
lsaSpace$sk # these are like the variance
plot(lsaSpace$sk) # scree plot

# Goal: combine the two corpora, and develop a classifier algorithm that can detemine which article is related to crude oil and which to acquisitions

# combine the two corpora and add true labels
combined <- c(crude, acq)
dtm <- DocumentTermMatrix(combined, control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE),stopwords=TRUE))

dtm

true.labs <- c(rep("crude", 20), rep("acq", 50))
foo <- as.matrix(dtm)
data.reuter <- data.frame(true.labs, foo)

# remove sparse terms to make the data less sparse
dtmv2 <- removeSparseTerms(dtm,0.8)
foo <- as.matrix(dtmv2)
data.reuter <- data.frame(true.labs,foo)
data.reuter$true.labs <- as.factor(data.reuter$true.labs)


# Classification ----------------------------------------------------------

library(e1071)

train <- c(sample(50, 35), 50 + sample(20, 14))
data.train <- data.reuter[train, ]
data.test <- data.reuter[-train, ]


svm.model <- svm(true.labs ~ ., data = data.train, type = "C")
svm.pred <- predict(svm.model, data.test[ , -1])

table(svm.pred, data.test$true.labs)
mean(svm.pred != data.test$true.labs)

# Back to LSA
tdm <- TermDocumentMatrix(combined, control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE), wordLengths = c(2, Inf)))
lsaSpace <- lsa(tdm)  # create LSA space
lsak <- lsaSpace$dk #Docs decomposed to k dimensions
dim(lsak)
lsaSpace$sk # these are like the variance
plot(lsaSpace$sk) # scree plot
lsa.reuter <- data.frame(true.labs, lsak)
lsa.reuter$true.labs <- as.factor(lsa.reuter$true.labs)
dim(lsa.reuter)

train <- c(sample(50, 35), 50 + sample(20, 14))
data.train <- lsa.reuter[train, ]
data.test <- lsa.reuter[-train, ]

svm.model <- svm(true.labs~., data = data.train)
svm.pred <- predict(svm.model, data.test[ , -1])

table(svm.pred, data.test$true.labs)
mean(svm.pred != data.test$true.labs)