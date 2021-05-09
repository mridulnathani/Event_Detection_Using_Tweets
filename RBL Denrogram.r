# Read text file
library(tm) 
print(getwd())
tweets <- readLines("DAt.txt")

#build corpus

corpus <- Corpus(VectorSource(tweets))

# create term document matrix 

tdm <- TermDocumentMatrix(corpus, control = list(minWordLenth=c(1,Inf)))
t <- removeSparseTerms(tdm, sparse=0.98)

m <- as.matrix(t)

#Plot Frequent Terms

freq <- rowSums(m)
freq <- subset(freq, freq>-1)
barplot(freq, las=2, col=rainbow(25) )

#Hierarchical word/tweet clustering using dendrogram

distance <- dist(scale(m))
print(distance, digits =2)

hc <- hclust(distance, method="ward.D")
plot(hc, hang=-1)

rect.hclust(hc, k=12)

#Nonhierarchical k-means clustering of words/tweets

m1 <- t(m)
set.seed(222)
k <-12
kc <- kmeans(m1, k)


