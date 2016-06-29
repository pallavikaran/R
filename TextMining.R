library(quanteda)
sample.text<- "R is a programming language and software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing."
toks <- tokenize(sample.text, removePunct=TRUE)
toks
wordstem(toks)

removeFeatures(toks, stopwords("english"))
ngrams(toks, n=2)
testCorpus <-corpus(sample.text)
dfm(testCorpus, keptFeatures ="p*")

summary(inaugCorpus)
presCorpus <-subset(inaugCorpus, Year>1980)
presDfm <- dfm(presCorpus, ignoredFeatures=stopwords("english"))
head(presDfm)

topfeatures(presDfm, 20)
kwic(presCorpus, "war")

plot(presDfm, min.freq=6, random.order=FALSE)

cosine.sim<- similarity(presDfm, method="cosine")
obama.dist<- cosine.sim[["2009-Obama"]]
dotchart(obama.dist, xlab="Cosine Similarity")
