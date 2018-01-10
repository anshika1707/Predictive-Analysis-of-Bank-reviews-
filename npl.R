# Library required
library(tm)
library(SnowballC)
library(caTools)
library(randomForest)
dataset = read.delim('Bank_Reviews.tsv', quote = '', stringsAsFactors = FALSE)
# Clearing the data
Corpus = VCorpus(VectorSource(dataset$Review))
Corpus = tm_map(Corpus, content_transformer(tolower))
Corpus = tm_map(corpus, removePunctuations)
corpus = tm_map(corpus, removeWords, stopwords())
Corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)
# Creating the bag of words model
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.999)
newdata = as.data.frame(as.matrix(dtm))
newdata$Liked = dataset$Liked

newdata$Liked = factor(newdata$Liked, levels = c(0,1))
set.seed(2018)
split = sample.split(newdata$Liked, SplitRatio = 0.80)
training_set = subset(newdata, split == TRUE)
test_set = subset(newdata, split == FALSE)
# Random Forest Classification
classifier = randomForest(x = training_set[-692],
                          y = training_set$Liked,
                          ntree = 100)

y_pred = predict(test_set[, 692])

cm = table(test_set[, 692], y_pred)