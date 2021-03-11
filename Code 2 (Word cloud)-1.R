# word cloud code
#-----------------------------------------------------------------------------
library(tm)
library(NLP)
library(wordcloud)
library(RColorBrewer)
library(tidytext)

# import dataset
text <- readLines(file.choose()) 
View(text)

# load data as corpus
doc <- Corpus(VectorSource(text)) 
inspect(doc)

#TermDocumentMatrix(x)
tdm <- TermDocumentMatrix(doc)  
m <- as.matrix(tdm)
f <- sort(rowSums(m),decreasing=TRUE)  
d <- data.frame(word = names(f),freq=f)  
head(d, 10)

# Inspect stopwords list (in english)  
stopwords("en")

# Remove english common stopwords
doc <- tm_map(doc, removeWords, stopwords("en"))
head(d, 20)

# Remove your own stop words (as character vector)  
doc <- tm_map(doc, removeWords, c("word1", "word2"))
head(d, 100)

# text transformation (those are examples, only use it if you need it! –> inspect first!)  
toSpace <- content_transformer(function(x,pattern) gsub(pattern," ",x))
doc <- tm_map(doc, toSpace, "/")  
doc <- tm_map(doc, toSpace, "@")  
doc <- tm_map(doc, toSpace, "-")
head(d, 20)

# Convert the text to lower case  
doc <- tm_map(doc, tolower)
# Remove numbers
#doc <- tm_map(doc, removeNumbers) 
# Remove extra words
doc <- tm_map(doc, removeWords, c("not","and","the","which","who","has","of","had","this","due","few"))
# Remove punctuations
doc <- tm_map(doc, removePunctuation)  
# Eliminate extra white spaces
doc <- tm_map(doc, stripWhitespace) 
# Text stemming
doc <- tm_map(doc, stemDocument)
head(d, 50) 

# text transformation (inspect first before use)  
toSpace <- content_transformer(function(x,pattern)  gsub(pattern," ",x))
doc <- tm_map(doc, toSpace, "/")
doc <- tm_map(doc, toSpace, "@")  
doc <- tm_map(doc, toSpace, "-")
head(d, 50)

# Convert the text to upper case
doc <- tm_map(doc, content_transformer(tolower))

# create wordcloud  
set.seed(1234)  
wordcloud(words = d$word, freq = d$freq, min.freq = 1,  max.words=200, random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))
title(main = "Word Cloud - AQI Index", font.main=1, cex.main=1.5)
