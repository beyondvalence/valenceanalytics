
#### Cluster Analysis of Terms for @TheEconomist tweets ####
# Thursday, 9 January, 2014

setwd("~/Documents/R/mining/twitter") 
require(twitteR)
require(ROAuth)
require(XML)

#\----- tweets to TD matrix ------\####

# setup OAuth
consumerKey <- "__"
consumerSecret <-"__"

# configure RCurl options
RCurlOptions <- list(capath=system.file("CurlSSL", "cacert.pem", package = "RCurl"),
                     ssl.verifypeer = FALSE)
options(RCurlOptions = RCurlOptions)

#use getTwitter Oauth
getTwitterOAuth(consumerKey, consumerSecret) # enter code

# retrieve 400 recent tweets for economist ####
econ.tw <- userTimeline("theeconomist", n=400, cainfo="cacert.pem")
save(econ.tw, file="econ.tw.RData")
length(econ.tw) # retrieved 400 tweets

# conversion ####
library(tm)

# to data.frame
econ.df <- do.call("rbind", lapply(econ.tw, as.data.frame))
dim(econ.df) # 400 rows, 16 columns
# to text corpus
econ.corpus <- Corpus(VectorSource(econ.df$text))
# transformations ####
# lower case
econ.corpus <- tm_map(econ.corpus, tolower)
# remove punctuation
econ.corpus <- tm_map(econ.corpus, removePunctuation)
# remove numbers
econ.corpus <- tm_map(econ.corpus, removeNumbers)
# remove URLs
removeURLs <- function(x) gsub("http[[:alnum:]]*", "", x)
econ.corpus <- tm_map(econ.corpus, removeURLs)
# add two stop words, 'available' and 'via'
myStopWords <- c(stopwords('english'), "available", "via")
# remove 'economy' and 'apps' from stopwords
myStopWords <- setdiff(myStopWords, c("economy", "apps"))
# remove stopwords from corpus
econ.corpus <- tm_map(econ.corpus, removeWords, myStopWords)
save("econ.corpus", file="econ.corpus.RData")
# view effect
writeLines(strwrap(econ.corpus[[4]], width=73))

# stemming ####
library(SnowballC)

# copy
econ.corpus.copy <- econ.corpus
# stem
econ.corpus <- tm_map(econ.corpus, stemDocument)
inspect(econ.corpus[4])
# stem completition
econ.corpus <- tm_map(econ.corpus, stemCompletion, 
                        dictionary=econ.corpus.copy)
save(econ.corpus, file="econ.corpus.SC.RData")

# to TD matrix ####
library(tm)

# tdm
econ.tdm <- TermDocumentMatrix(econ.corpus, control=list(wordLengths=c(1, Inf)))
# print terms
dimnames(econ.tdm)$Terms
save(econ.tdm, file="econ.tdm.RData")
# frequent terms
which(apply(econ.tdm, 1, sum) > 20)
findFreqTerms(econ.tdm, lowfreq=20)
findAssocs(econ.tdm, "mandela", 0.25)
findAssocs(econ.tdm, "recent", 0.25)

# word cloud ####
library(wordcloud)
econ.matrix <- as.matrix(econ.tdm)
wordFreq.sort <- sort(rowSums(econ.matrix), decreasing=T)
save(econ.matrix, file="econ.matrix.RData")
# wcloud
set.seed(1234)
grayLevels <- gray( (wordFreq.sort + 10) / (max(wordFreq.sort) + 10))
word.cloud <- wordcloud(words=names(wordFreq.sort), freq=wordFreq.sort, 
                        min.freq=3, random.order=F, colors=grayLevels)
# economist, weeks, america, new, year, china, mandela, december


#\-------------hclust-----------\####

# hierarchial clustering of terms ####
setwd("~/Documents/R/mining/twitter")
load("econ.tdm.RData")

# remove sparse terms ####
econ.tdm2 <- removeSparseTerms(econ.tdm, sparse=0.95)
econ.matrix2 <- as.matrix(econ.tdm2)

# hclust ####
distMatrix <- dist(scale(econ.matrix2))
econ.fit <- hclust(distMatrix, method="ward")

# plot dendrogram ####
plot(econ.fit, cex=0.9, hang=-1,
     main="Word Cluster Dendrogram")
# cut tree
rect.hclust(econ.fit, k=5)
(econ.groups <- cutree(econ.fit, k=5))

